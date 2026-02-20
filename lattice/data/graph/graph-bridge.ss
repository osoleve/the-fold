;;; lattice/data/graph/graph-bridge.ss — Bidirectional Graph-Matrix Bridge
;;; @module graph-bridge
;;; @requires prelude vec matrix sparse graph-matrix graph-laplacian

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'sparse)
(require 'graph-matrix)
(require 'graph-laplacian)

(doc 'module 'graph-bridge)
(doc 'bridges '(data linalg))
(doc 'description "Bidirectional bridge between graph and matrix representations.
  Provides a lightweight Graph record that carries metadata (node count,
  directedness, weight presence) alongside its edge list, and named
  conversion functions with round-trip guarantees.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Design rationale: graph-matrix.ss already converts edge lists
  to adjacency matrices and back.  graph-laplacian.ss already builds
  Laplacians from adjacency matrices.  This module closes the composition
  gap by providing:
    1. A Graph record that bundles edges with metadata
    2. Named bridge functions: graph->adjacency-matrix, adjacency-matrix->graph,
       graph->laplacian-matrix, graph->degree-matrix
    3. Round-trip identity: adjacency-matrix->graph(graph->adjacency-matrix(g)) = g
       (up to edge ordering)")

;;; ====
;;; Graph Record
;;; ====
;;;
;;; A Graph is: (graph edges node-count directed? weighted?)
;;;   edges      : (List Edge) — each edge is (from to) or (from to weight)
;;;   node-count : Nat — number of nodes (0-indexed)
;;;   directed?  : Boolean — if #f, edges are symmetric
;;;   weighted?  : Boolean — if #t, edges carry numeric weights

(doc graph? 'type '(-> Any Boolean))
(doc graph? 'description "Predicate for Graph records")
(define (graph? g)
  (and (pair? g)
       (eq? (car g) 'graph)
       (= (length g) 5)
       (list? (cadr g))
       (integer? (caddr g))
       (boolean? (cadddr g))
       (boolean? (car (cddddr g)))))

(doc graph-edges 'type '(-> Graph (List Edge)))
(define (graph-edges g) (cadr g))

(doc graph-node-count 'type '(-> Graph Nat))
(define (graph-node-count g) (caddr g))

(doc graph-directed? 'type '(-> Graph Boolean))
(define (graph-directed? g) (cadddr g))

(doc graph-weighted? 'type '(-> Graph Boolean))
(define (graph-weighted? g) (car (cddddr g)))

;;; ====
;;; Graph Construction
;;; ====

(doc make-graph 'type '(-> (List Edge) [Nat] [Boolean] [Boolean] Graph))
(doc make-graph 'description "Construct a Graph from an edge list.
  node-count defaults to inferred max + 1.
  directed? defaults to #t.
  weighted? is inferred from edge arity if not supplied.")
(define (make-graph edges . opts)
  (let* ([n (if (and (pair? opts) (car opts))
                (car opts)
                (infer-node-count edges))]
         [rest1 (if (pair? opts) (cdr opts) '())]
         [directed (if (pair? rest1) (car rest1) #t)]
         [rest2 (if (pair? rest1) (cdr rest1) '())]
         [weighted (if (pair? rest2)
                       (car rest2)
                       ;; Infer: if any edge has 3 elements, it's weighted
                       (and (pair? edges)
                            (= (length (car edges)) 3)))])
    (list 'graph edges n directed weighted)))

(doc make-graph-undirected 'type '(-> (List Edge) [Nat] Graph))
(doc make-graph-undirected 'description "Convenience: construct an undirected Graph")
(define (make-graph-undirected edges . opts)
  (let ([n (if (pair? opts) (car opts) (infer-node-count edges))])
    (make-graph edges n #f)))

;;; ====
;;; Graph -> Matrix Conversions
;;; ====

(doc graph->adjacency-matrix 'type '(-> Graph Matrix))
(doc graph->adjacency-matrix 'description "Convert Graph to dense adjacency matrix.
  For undirected graphs, both (i,j) and (j,i) are set.
  For weighted graphs, matrix entries are the edge weights.
  For unweighted graphs, matrix entries are 1.")
(define (graph->adjacency-matrix g)
  (let ([edges (graph-edges g)]
        [n (graph-node-count g)]
        [undirected (not (graph-directed? g))])
    (edges->adjacency-matrix edges n undirected)))

(doc graph->sparse-adjacency 'type '(-> Graph SparseCSR))
(doc graph->sparse-adjacency 'description "Convert Graph to sparse CSR adjacency matrix.
  Preferred for large, sparse graphs.")
(define (graph->sparse-adjacency g)
  (let ([edges (graph-edges g)]
        [n (graph-node-count g)]
        [undirected (not (graph-directed? g))])
    (edges->sparse-adjacency edges n undirected)))

(doc graph->degree-matrix 'type '(-> Graph [Symbol] Matrix))
(doc graph->degree-matrix 'description "Convert Graph to diagonal degree matrix.
  mode: 'out (default), 'in, or 'total.")
(define (graph->degree-matrix g . mode-opt)
  (let ([adj (graph->adjacency-matrix g)]
        [mode (if (pair? mode-opt) (car mode-opt) 'out)])
    (degree-matrix adj mode)))

(doc graph->laplacian-matrix 'type '(-> Graph Matrix))
(doc graph->laplacian-matrix 'description "Convert Graph directly to Laplacian matrix (L = D - A).
  Composes graph->adjacency-matrix with laplacian.
  For undirected graphs, L is symmetric positive semi-definite.")
(define (graph->laplacian-matrix g)
  (laplacian (graph->adjacency-matrix g)))

(doc graph->laplacian-normalized 'type '(-> Graph Matrix))
(doc graph->laplacian-normalized 'description "Convert Graph to normalized symmetric Laplacian.
  L_sym = I - D^(-1/2) A D^(-1/2).
  Eigenvalues in [0, 2] for undirected graphs.")
(define (graph->laplacian-normalized g)
  (laplacian-normalized (graph->adjacency-matrix g)))

(doc graph->laplacian-random-walk 'type '(-> Graph Matrix))
(doc graph->laplacian-random-walk 'description "Convert Graph to random-walk Laplacian.
  L_rw = I - D^(-1) A.
  Related to Markov chain transition probabilities.")
(define (graph->laplacian-random-walk g)
  (laplacian-random-walk (graph->adjacency-matrix g)))

;;; ====
;;; Matrix -> Graph Conversions
;;; ====

(doc adjacency-matrix->graph 'type '(-> Matrix [Boolean] [Boolean] Graph))
(doc adjacency-matrix->graph 'description "Reconstruct a Graph from a dense adjacency matrix.
  directed? defaults to #t.
  If directed? is #f, only upper-triangle edges are extracted
  (avoids duplicating symmetric edges).")
(define (adjacency-matrix->graph m . opts)
  (let* ([directed (if (pair? opts) (car opts) #t)]
         [rest1 (if (pair? opts) (cdr opts) '())]
         ;; Determine if weighted: check if any entry is not 0 or 1
         [auto-weighted (matrix-has-non-binary-entries? m)]
         [weighted (if (pair? rest1) (car rest1) auto-weighted)]
         [n (matrix-rows m)]
         [edges (if directed
                    (extract-directed-edges m n weighted)
                    (extract-undirected-edges m n weighted))])
    (list 'graph edges n directed weighted)))

(doc sparse-adjacency->graph 'type '(-> SparseCSR [Boolean] [Boolean] Graph))
(doc sparse-adjacency->graph 'description "Reconstruct a Graph from a sparse CSR adjacency matrix.")
(define (sparse-adjacency->graph m . opts)
  (let* ([directed (if (pair? opts) (car opts) #t)]
         [rest1 (if (pair? opts) (cdr opts) '())]
         [weighted-edges (sparse-adjacency->edges m #t)]
         ;; Check if any weight is non-{0,1}
         [auto-weighted (let loop ([es weighted-edges])
                          (cond
                            [(null? es) #f]
                            [(not (or (= (caddr (car es)) 0)
                                      (= (caddr (car es)) 1))) #t]
                            [else (loop (cdr es))]))]
         [weighted (if (pair? rest1) (car rest1) auto-weighted)]
         [n (sparse-csr-rows m)]
         [edges (if directed
                    ;; For directed, take all non-zero edges
                    (if weighted
                        weighted-edges
                        (map (lambda (e) (list (car e) (cadr e))) weighted-edges))
                    ;; For undirected, only take i < j edges
                    (filter-upper-triangle weighted-edges weighted))])
    (list 'graph edges n directed weighted)))

;;; ====
;;; Internal Helpers
;;; ====

;;; matrix-has-non-binary-entries? : Matrix -> Boolean
;;; Check if any entry in the matrix is neither 0 nor 1.
(define (matrix-has-non-binary-entries? m)
  (let ([data (matrix-data m)]
        [len (vector-length (matrix-data m))])
    (let loop ([i 0])
      (cond
        [(= i len) #f]
        [(let ([v (vector-ref data i)])
           (not (or (= v 0) (= v 1)))) #t]
        [else (loop (+ i 1))]))))

;;; extract-directed-edges : Matrix x Nat x Boolean -> (List Edge)
;;; Extract all non-zero entries as directed edges.
(define (extract-directed-edges m n weighted)
  (let loop ([i 0] [j 0] [acc '()])
    (cond
      [(= i n) (reverse acc)]
      [(= j n) (loop (+ i 1) 0 acc)]
      [else
       (let ([w (matrix-ref m i j)])
         (if (= w 0)
             (loop i (+ j 1) acc)
             (loop i (+ j 1)
                   (cons (if weighted
                             (list i j w)
                             (list i j))
                         acc))))])))

;;; extract-undirected-edges : Matrix x Nat x Boolean -> (List Edge)
;;; Extract only upper-triangle (i < j) non-zero entries.
;;; For undirected graphs, A is symmetric, so this avoids duplication.
(define (extract-undirected-edges m n weighted)
  (let loop ([i 0] [j 0] [acc '()])
    (cond
      [(= i n) (reverse acc)]
      [(= j n) (loop (+ i 1) (+ i 2) acc)]  ; j starts at i+1
      [else
       (let ([w (matrix-ref m i j)])
         (if (= w 0)
             (loop i (+ j 1) acc)
             (loop i (+ j 1)
                   (cons (if weighted
                             (list i j w)
                             (list i j))
                         acc))))])))

;;; filter-upper-triangle : (List Edge) x Boolean -> (List Edge)
;;; Keep only edges where from < to.
(define (filter-upper-triangle edges weighted)
  (filter (lambda (e) (< (car e) (cadr e)))
          (if weighted
              edges
              (map (lambda (e) (list (car e) (cadr e))) edges))))

;;; ====
;;; Graph Properties (delegating to graph-matrix)
;;; ====

(doc graph-edge-count 'type '(-> Graph Nat))
(doc graph-edge-count 'description "Number of edges in the graph.
  For undirected graphs, each edge is counted once.")
(define (graph-edge-count g)
  (length (graph-edges g)))

(doc graph-density 'type '(-> Graph Num))
(doc graph-density 'description "Edge density: edges / possible_edges.
  For directed: e / (n*(n-1)).  For undirected: e / (n*(n-1)/2).")
(define (graph-density g)
  (let ([n (graph-node-count g)]
        [e (graph-edge-count g)])
    (if (<= n 1)
        0
        (let ([possible (if (graph-directed? g)
                            (* n (- n 1))
                            (/ (* n (- n 1)) 2))])
          (/ e possible)))))

(doc graph-neighbors 'type '(-> Graph Nat (List Nat)))
(doc graph-neighbors 'description "Get neighbors of node i from the Graph's adjacency matrix")
(define (graph-neighbors g i)
  (adjacency-neighbors (graph->adjacency-matrix g) i))

;;; ====
;;; Round-Trip Utilities
;;; ====

(doc graph-equal? 'type '(-> Graph Graph Boolean))
(doc graph-equal? 'description "Structural equality for graphs: same node count, directedness,
  weight presence, and edge set (order-independent).
  Compares via adjacency matrices to normalize edge ordering.")
(define (graph-equal? g1 g2)
  (and (= (graph-node-count g1) (graph-node-count g2))
       (eq? (graph-directed? g1) (graph-directed? g2))
       (eq? (graph-weighted? g1) (graph-weighted? g2))
       (matrix-equal? (graph->adjacency-matrix g1)
                      (graph->adjacency-matrix g2))))

;;; ====
;;; Convenience: Spectral Analysis from Graph
;;; ====

(doc graph->algebraic-connectivity 'type '(-> Graph Num))
(doc graph->algebraic-connectivity 'description "Compute algebraic connectivity (Fiedler value) directly from Graph.
  lambda_2 = 0 iff the graph is disconnected.")
(define (graph->algebraic-connectivity g)
  (algebraic-connectivity (graph->adjacency-matrix g)))

(doc graph->fiedler-vector 'type '(-> Graph Vec))
(doc graph->fiedler-vector 'description "Compute the Fiedler vector directly from Graph.
  Useful for graph partitioning and spectral layout.")
(define (graph->fiedler-vector g)
  (fiedler-vector (graph->adjacency-matrix g)))

(doc graph->spectral-partition 'type '(-> Graph (Pair (List Nat) (List Nat))))
(doc graph->spectral-partition 'description "Partition graph into two groups using spectral bisection.")
(define (graph->spectral-partition g)
  (spectral-partition (graph->adjacency-matrix g)))
