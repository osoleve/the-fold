;;; lattice/data/graph/graph-filtration.ss — Bridge from graph edge-weight filtration to persistent homology
;;; @module graph-filtration
;;; @requires prelude graph-matrix simplicial-complex persistent

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'graph-matrix)
(require 'simplicial-complex)
(require 'persistent)

(doc 'module 'graph-filtration)
(doc 'bridges '(data topology))
(doc 'description "Bridges weighted graph analysis with persistent homology via edge-weight filtrations.
  Given a weighted adjacency matrix, produces filtrations that track how topological features
  (connected components, cycles) evolve as edges are added in order of increasing weight.
  Supports both simple weight filtration (edges only) and Vietoris-Rips filtration (clique expansion).")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'tier 1)
(doc 'dependencies '(data/graph/graph-matrix topology/simplicial-complex topology/persistent))

;;; ============================================================
;;; Weight Extraction
;;; ============================================================

(doc 'section 'weight-extraction)

(define (adjacency-unique-weights m)
  (doc 'type '(-> Matrix (List Number)))
  (doc 'description "Extract sorted unique positive edge weights from an adjacency matrix.
  Treats the matrix as undirected (only considers upper triangle i < j).")
  (let* ([n (matrix-rows m)]
         [weights '()])
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (do ([j (+ i 1) (+ j 1)])
          [(= j n)]
        (let ([w (matrix-ref m i j)])
          (when (> w 0)
            (set! weights (cons w weights))))))
    (unique-sorted-numbers (sort-by < weights))))

(define (unique-sorted-numbers lst)
  (doc 'type '(-> (List Number) (List Number)))
  (doc 'description "Remove duplicate numbers from a sorted list")
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) lst]
    [(= (car lst) (cadr lst))
     (unique-sorted-numbers (cdr lst))]
    [else
     (cons (car lst) (unique-sorted-numbers (cdr lst)))]))

;;; ============================================================
;;; Graph Weight Filtration
;;; ============================================================

(doc 'section 'weight-filtration)

(doc graph-weight-filtration 'export #t)
(doc graph-weight-filtration 'type '(-> Matrix Filtration))
(doc graph-weight-filtration 'description
  "Build a filtration from a weighted adjacency matrix.
  At threshold t, edge (i,j) is included if weight(i,j) <= t.
  Each edge is a 1-simplex; its endpoint vertices (0-simplices) are included
  at the same birth time as the edge (or earlier if already present).
  The matrix is treated as undirected (upper triangle only).")
(doc graph-weight-filtration 'note
  "Vertices that appear as endpoints of edges are born when their first
  incident edge appears. Isolated vertices (no edges) are not included.
  For graphs where all vertices should be present from the start,
  pre-add vertex simplices at time 0.")
(define (graph-weight-filtration m)
  (let* ([n (matrix-rows m)]
         [vertex-birth (make-vector n +inf.0)]
         [pairs '()])
    ;; Collect edges with weights, record earliest birth for each vertex
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (do ([j (+ i 1) (+ j 1)])
          [(= j n)]
        (let ([w (matrix-ref m i j)])
          (when (> w 0)
            (set! pairs (cons (cons (make-simplex (list i j)) w) pairs))
            (when (< w (vector-ref vertex-birth i))
              (vector-set! vertex-birth i w))
            (when (< w (vector-ref vertex-birth j))
              (vector-set! vertex-birth j w))))))
    ;; Add vertex simplices at their earliest edge birth time
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (let ([b (vector-ref vertex-birth i)])
        (when (< b +inf.0)
          (set! pairs (cons (cons (make-simplex (list i)) b) pairs)))))
    (make-filtration pairs)))

;;; ============================================================
;;; Graph Rips Filtration
;;; ============================================================

(doc 'section 'rips-filtration)

(doc graph-rips-filtration 'export #t)
(doc graph-rips-filtration 'type '(-> Matrix Integer Filtration))
(doc graph-rips-filtration 'description
  "Build a Vietoris-Rips filtration from a weighted adjacency matrix.
  At threshold t, a k-simplex [v0,...,vk] is included if every pairwise
  edge weight(vi,vj) <= t. This means triangles form when all 3 edges
  are present, tetrahedra when all 6 edges are present, etc.
  max-dim controls the maximum simplex dimension to compute.")
(doc graph-rips-filtration 'note
  "The birth time of a k-simplex is the maximum weight among its
  (k+1 choose 2) edges. Complexity is exponential in max-dim.")
(define (graph-rips-filtration m max-dim)
  (let* ([n (matrix-rows m)]
         [pairs '()])
    ;; Add vertices at time 0 (all vertices present from the start in Rips)
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (set! pairs (cons (cons (make-simplex (list i)) 0.0) pairs)))
    ;; Add k-simplices for k = 1 to max-dim
    (do ([k 1 (+ k 1)])
        [(> k max-dim)]
      (set! pairs (append pairs (rips-k-simplices-from-graph m n k))))
    (make-filtration pairs)))

(define (rips-k-simplices-from-graph m n k)
  (doc 'type '(-> Matrix Integer Integer (List (Pair Simplex Number))))
  (doc 'description "Find all k-simplices in the Rips complex of a graph.
  A k-simplex has k+1 vertices; its birth time is the max pairwise edge weight.")
  (let ([result '()])
    (graph-for-each-combination
      (iota n) (+ k 1)
      (lambda (vertices)
        (let ([max-w (max-pairwise-weight m vertices)])
          (when (< max-w +inf.0)
            (set! result (cons (cons (make-simplex vertices) max-w) result))))))
    result))

(define (max-pairwise-weight m vertices)
  (doc 'type '(-> Matrix (List Integer) Number))
  (doc 'description "Maximum pairwise edge weight among vertices.
  Returns +inf.0 if any pair has no edge (weight 0).")
  (let ([max-w 0.0])
    (let loop-i ([vs vertices])
      (if (null? vs)
          max-w
          (let ([i (car vs)])
            (let loop-j ([ws (cdr vs)])
              (if (null? ws)
                  (loop-i (cdr vs))
                  (let* ([j (car ws)]
                         [w (matrix-ref m i j)])
                    (if (= w 0)
                        +inf.0  ; No edge means this clique doesn't exist
                        (begin
                          (set! max-w (max max-w w))
                          (loop-j (cdr ws))))))))))))

(define (graph-for-each-combination lst k proc)
  (doc 'type '(-> (List a) Integer (-> (List a) Void) Void))
  (doc 'description "Call proc on each k-combination of lst")
  (when (>= (length lst) k)
    (if (= k 0)
        (proc '())
        (if (= k (length lst))
            (proc lst)
            (let ([first (car lst)]
                  [rest (cdr lst)])
              ;; Combinations including first
              (graph-for-each-combination rest (- k 1)
                (lambda (combo) (proc (cons first combo))))
              ;; Combinations not including first
              (graph-for-each-combination rest k proc))))))

;;; ============================================================
;;; Graph Persistent Homology
;;; ============================================================

(doc 'section 'persistent-homology)

(doc graph-persistent-homology 'export #t)
(doc graph-persistent-homology 'type '(-> Matrix Integer PersistenceDiagram))
(doc graph-persistent-homology 'description
  "Compute persistent homology of a weighted graph via Rips filtration.
  Returns a persistence diagram with (birth, death, dimension) points
  tracking how H_0 (components) and H_1 (cycles) evolve with edge weight threshold.
  max-dim controls the maximum homological dimension (typically 1 or 2).")
(define (graph-persistent-homology m max-dim)
  (let* ([filtration (graph-rips-filtration m max-dim)]
         [result (persistence-reduce filtration)]
         [diagram (make-persistence-diagram result)])
    diagram))

(doc graph-weight-persistent-homology 'export #t)
(doc graph-weight-persistent-homology 'type '(-> Matrix PersistenceDiagram))
(doc graph-weight-persistent-homology 'description
  "Compute persistent homology using simple weight filtration (edges only, no clique expansion).
  Faster than Rips but only captures H_0 (component merging). H_1 requires triangles
  to kill cycles, which weight filtration alone does not produce.")
(define (graph-weight-persistent-homology m)
  (let* ([filtration (graph-weight-filtration m)]
         [result (persistence-reduce filtration)]
         [diagram (make-persistence-diagram result)])
    diagram))

;;; ============================================================
;;; Betti Curve
;;; ============================================================

(doc 'section 'betti-curve)

(doc graph-betti-curve 'export #t)
(doc graph-betti-curve 'type '(-> PersistenceDiagram Integer (List Number) (List (Pair Number Integer))))
(doc graph-betti-curve 'description
  "Compute the Betti curve: Betti number as a function of filtration parameter.
  Given a persistence diagram, a homological dimension, and a list of threshold values,
  returns ((t0 . b0) (t1 . b1) ...) where bi is the Betti number at threshold ti.")
(define (graph-betti-curve diagram dim thresholds)
  (map (lambda (t)
         (cons t (diagram-betti diagram dim t)))
       thresholds))

(doc graph-betti-curve-auto 'export #t)
(doc graph-betti-curve-auto 'type '(-> Matrix Integer (List (Pair Number Integer))))
(doc graph-betti-curve-auto 'description
  "Compute Betti curve at the natural thresholds (unique edge weights) of a weighted graph.
  Uses Rips filtration with max-dim = dim + 1 to capture features up to dimension dim.")
(define (graph-betti-curve-auto m dim)
  (let* ([thresholds (adjacency-unique-weights m)]
         ;; Add epsilon-before and epsilon-after each threshold
         [sample-points (betti-sample-points thresholds)]
         [diagram (graph-persistent-homology m (+ dim 1))])
    (graph-betti-curve diagram dim sample-points)))

(define (betti-sample-points thresholds)
  (doc 'type '(-> (List Number) (List Number)))
  (doc 'description "Generate sample points: midpoints between consecutive thresholds plus
  points slightly after each threshold. This captures the Betti numbers
  at each transition.")
  (if (null? thresholds)
      '()
      (let ([eps 0.0001])
        (cons (- (car thresholds) eps)
              (let loop ([ts thresholds])
                (if (null? (cdr ts))
                    (list (+ (car ts) eps))
                    (let ([t1 (car ts)]
                          [t2 (cadr ts)])
                      (cons (+ t1 eps)
                            (cons (/ (+ t1 t2) 2.0)
                                  (loop (cdr ts)))))))))))

;;; ============================================================
;;; Persistence Diagram Extraction
;;; ============================================================

(doc 'section 'diagram-extraction)

(doc graph-persistence-pairs 'export #t)
(doc graph-persistence-pairs 'type '(-> PersistenceDiagram Integer (List (List Number Number))))
(doc graph-persistence-pairs 'description
  "Extract (birth, death) pairs for a specific homological dimension from a persistence diagram.
  Wraps diagram-points-dim for graph-oriented usage.")
(define (graph-persistence-pairs diagram dim)
  (diagram-points-dim diagram dim))

(doc graph-persistence-pairs-finite 'export #t)
(doc graph-persistence-pairs-finite 'type '(-> PersistenceDiagram Integer (List (List Number Number))))
(doc graph-persistence-pairs-finite 'description
  "Extract only finite (birth, death) pairs for a specific dimension.
  Excludes essential features (those with death = +inf).")
(define (graph-persistence-pairs-finite diagram dim)
  (filter (lambda (pt) (not (infinite? (cadr pt))))
          (diagram-points-dim diagram dim)))

(doc graph-persistence-pairs-essential 'export #t)
(doc graph-persistence-pairs-essential 'type '(-> PersistenceDiagram Integer (List (List Number Number))))
(doc graph-persistence-pairs-essential 'description
  "Extract only essential (birth, death = +inf) pairs for a specific dimension.
  These represent topological features that persist indefinitely.")
(define (graph-persistence-pairs-essential diagram dim)
  (filter (lambda (pt) (infinite? (cadr pt)))
          (diagram-points-dim diagram dim)))

(doc graph-max-persistence 'export #t)
(doc graph-max-persistence 'type '(-> PersistenceDiagram Integer Number))
(doc graph-max-persistence 'description
  "Compute the maximum finite persistence (death - birth) in a given dimension.
  Returns 0 if there are no finite persistence pairs.")
(define (graph-max-persistence diagram dim)
  (let ([finite (graph-persistence-pairs-finite diagram dim)])
    (if (null? finite)
        0
        (apply max (map (lambda (pt) (- (cadr pt) (car pt))) finite)))))
