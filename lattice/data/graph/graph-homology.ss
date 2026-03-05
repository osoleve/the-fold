;;; lattice/data/graph/graph-homology.ss — Homology-Based Cycle Analysis for Graphs
;;; @module graph-homology
;;; @requires prelude topology/homology

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'homology)

(doc 'module 'graph-homology)
(doc 'bridges '(data topology))
(doc 'description "Algebraic topology tools for analyzing cycles in graphs using simplicial homology.
  H_0 (0-th homology) captures connected components; H_1 (1st homology) captures independent cycles.
  Betti numbers: beta_0 = number of connected components, beta_1 = number of independent cycles.
  All homology functions use canonical Z_2 implementation from topology/homology.ss.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'dependencies '(topology/homology topology/simplicial-complex))

;;; --- Graph to Simplicial Complex ---

(doc graph->simplicial-complex 'type '(-> (List Edge) (List Vertex) SC))
(doc graph->simplicial-complex 'description "Convert graph to 1-dimensional simplicial complex; vertices become 0-simplices, edges become 1-simplices")
(doc graph->simplicial-complex 'note "Edge format: (v1 . v2) or (v1 v2) - both are supported")
(doc graph->simplicial-complex 'export #t)
(define (graph->simplicial-complex edges vertices)
  (doc 'export #t)
  (let* ([vertex-simplices (map (lambda (v) (make-simplex (list v))) vertices)]
         [edge-simplices (map (lambda (e)
                                (let ([v1 (car e)]
                                      [v2 (if (pair? (cdr e))
                                              (cadr e)
                                              (cdr e))])
                                  (make-simplex (list v1 v2))))
                              edges)])
    (sc-from-simplices (append vertex-simplices edge-simplices))))

;;; graph-adjacency->simplicial-complex : AdjList → SC
;;; Convert adjacency list to simplicial complex.
;;; AdjList format: ((vertex neighbor1 neighbor2 ...) ...)
(doc graph-adjacency->simplicial-complex 'export #t)
(define (graph-adjacency->simplicial-complex adj)
  (doc 'export #t)
  (let* ([vertices (map car adj)]
         [edges '()])
    (for-each
     (lambda (entry)
       (let ([v (car entry)]
             [neighbors (cdr entry)])
         (for-each
          (lambda (n)
            (when (< v n)
              (set! edges (cons (cons v n) edges))))
          neighbors)))
     adj)
    (graph->simplicial-complex edges vertices)))

;;; --- Betti Numbers ---

(doc graph-betti-numbers 'type '(-> (List Edge) (List Vertex) (Pair Nat Nat)))
(doc graph-betti-numbers 'description "Compute Betti numbers using Z_2 homology: beta_0 (components), beta_1 (independent cycles)")
(doc graph-betti-numbers 'note "Uses canonical Z_2 homology implementation from topology/homology.ss with exact mod-2 arithmetic")
(doc graph-betti-numbers 'export #t)
(define (graph-betti-numbers edges vertices)
  (doc 'export #t)
  (let ([sc (graph->simplicial-complex edges vertices)])
    (if (null? edges)
        (cons (length vertices) 0)
        (let ([betti (sc-betti-numbers sc)])
          (cons (if (pair? betti) (car betti) 0)
                (if (and (pair? betti) (pair? (cdr betti)))
                    (cadr betti)
                    0))))))

;;; graph-betti-numbers-from-adjacency : AdjList → (beta0 . beta1)
;;; Compute Betti numbers from adjacency list representation.
(doc graph-betti-numbers-from-adjacency 'export #t)
(define (graph-betti-numbers-from-adjacency adj)
  (doc 'export #t)
  (let* ([vertices (map car adj)]
         [edges '()])
    (for-each
     (lambda (entry)
       (let ([v (car entry)]
             [neighbors (cdr entry)])
         (for-each
          (lambda (n)
            (when (< v n)
              (set! edges (cons (cons v n) edges))))
          neighbors)))
     adj)
    (graph-betti-numbers edges vertices)))

;;; --- Cycle Basis ---

(doc cycle-basis-homology 'type '(-> (List Edge) (List Vertex) (List Cycle)))
(doc cycle-basis-homology 'description "Compute basis for H_1 using Z_2 homology; returns fundamental cycles (number equals beta_1)")
(doc cycle-basis-homology 'note "Algorithm: Build Z_2 boundary matrix, find null space via z2-null-space, convert to edge lists")
(doc cycle-basis-homology 'export #t)
(define (cycle-basis-homology edges vertices)
  (doc 'export #t)
  (let* ([sc (graph->simplicial-complex edges vertices)]
         [n-edges (length edges)]
         [edge-list (sc-edges sc)])
    (if (= n-edges 0)
        '()
        (let* ([boundary-1 (sc-boundary-matrix sc 1)]
               [null-basis (z2-null-space boundary-1)])
          (map (lambda (null-vec)
                 (edges-from-z2-null-vector null-vec edge-list))
               null-basis)))))

;;; edges-from-z2-null-vector : (List {0,1}) × (List Simplex) → (List Edge)
;;; Convert a Z_2 null space vector to a list of edges.
(define (edges-from-z2-null-vector coeffs edge-simplices)
  (doc 'export #t)
  (let loop ([cs coeffs] [edges edge-simplices] [result '()])
    (if (or (null? cs) (null? edges))
        (reverse result)
        (let ([coeff (car cs)]
              [edge (car edges)])
          (if (= coeff 1)
              (let* ([vs (simplex-vertices edge)]
                     [v0 (car vs)]
                     [v1 (cadr vs)])
                (loop (cdr cs) (cdr edges) (cons (cons v0 v1) result)))
              (loop (cdr cs) (cdr edges) result))))))

;;; --- Convenience Functions ---

;;; graph-euler-characteristic : (List Edge) × (List Vertex) → Integer
;;; Compute Euler characteristic: χ = V - E
(doc graph-euler-characteristic 'export #t)
(define (graph-euler-characteristic edges vertices)
  (doc 'export #t)
  (- (length vertices) (length edges)))

;;; graph-cycle-rank : (List Edge) × (List Vertex) → Integer
;;; Compute the cycle rank (cyclomatic number): beta_1 = E - V + beta_0
(doc graph-cycle-rank 'export #t)
(define (graph-cycle-rank edges vertices)
  (doc 'export #t)
  (let ([betti (graph-betti-numbers edges vertices)])
    (cdr betti)))

;;; graph-is-tree? : (List Edge) × (List Vertex) → Boolean
;;; A graph is a tree iff it is connected (beta_0 = 1) and acyclic (beta_1 = 0).
(doc graph-is-tree? 'export #t)
(define (graph-is-tree? edges vertices)
  (doc 'export #t)
  (let ([betti (graph-betti-numbers edges vertices)])
    (and (= (car betti) 1)
         (= (cdr betti) 0))))

;;; graph-is-forest? : (List Edge) × (List Vertex) → Boolean
;;; A graph is a forest iff it is acyclic (beta_1 = 0).
(doc graph-is-forest? 'export #t)
(define (graph-is-forest? edges vertices)
  (doc 'export #t)
  (let ([betti (graph-betti-numbers edges vertices)])
    (= (cdr betti) 0)))
