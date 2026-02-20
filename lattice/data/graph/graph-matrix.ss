;;; lattice/data/graph/graph-matrix.ss — Graph Matrix Representation
;;; @module graph-matrix
;;; @requires prelude vec matrix sparse heap

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'sparse)
(require 'heap)

(doc 'module 'graph-matrix)
(doc 'bridges '(data linalg))
(doc 'purity 'total)
(doc 'description "Adjacency matrix graph representation for linear algebra graph algorithms")
(doc 'layer 'lattice)

(doc 'note "Matrix-based graph representation supporting both dense and sparse formats:
  - Nodes are numbered 0 to n-1
  - Edge (i,j) means node i connects to node j
  - For weighted graphs, A[i,j] = weight of edge (i,j)
  - For unweighted graphs, A[i,j] = 1 if edge exists, 0 otherwise")

(doc 'section 'edge-list-representation)
(doc 'description "Edge lists are input format for graph construction: unweighted ((from to) ...) or weighted ((from to weight) ...)")

(doc edge-list? 'type '(-> Any Boolean))
(doc edge-list? 'description "Check if value is a valid edge list")
(define (edge-list? edges)
  (and (list? edges)
       (or (null? edges)
           (let ([first (car edges)])
                (and (list? first)
                     (or (= (length first) 2)   ; Unweighted
                         (= (length first) 3))) ; Weighted
                (integer? (car first))
                (integer? (cadr first))))))

;;; edge-weighted? : Edge → Boolean
(define (edge-weighted? edge)
  (= (length edge) 3))

;;; edge-from : Edge → Nat
(define (edge-from edge) (car edge))

;;; edge-to : Edge → Nat
(define (edge-to edge) (cadr edge))

;;; edge-weight : Edge → Num
(define (edge-weight edge)
  (if (= (length edge) 3)
      (caddr edge)
      1))

;;; infer-node-count : (List Edge) → Nat
;;; Determine number of nodes from edge list (max node index + 1).
(define (infer-node-count edges)
  (if (null? edges)
      0
      (+ 1 (fold-left (lambda (acc edge)
                              (max acc (edge-from edge) (edge-to edge)))
                      0
                      edges))))

(doc 'constant '*dense-adjacency-max-nodes*)
(doc 'description "Maximum node count for dense adjacency matrices; beyond this, use sparse format")
(define *dense-adjacency-max-nodes* 10000)

(doc edges->adjacency-matrix 'type '(-> (List Edge) [Nat] [Boolean] Matrix))
(doc edges->adjacency-matrix 'description "Convert edge list to dense adjacency matrix")
(doc edges->adjacency-matrix 'param 'edges "Edge list")
(doc edges->adjacency-matrix 'param 'node-count "Optional node count (default: inferred from edges)")
(doc edges->adjacency-matrix 'param 'undirected "If #t, adds both (i,j) and (j,i) for each edge")
(doc edges->adjacency-matrix 'note "Raises error if n > *dense-adjacency-max-nodes* (use sparse matrix instead)")
(define (edges->adjacency-matrix edges . opts)
  (let* ([n (if (and (not (null? opts)) (car opts))
                (car opts)
                (infer-node-count edges))]
         [undirected (if (and (not (null? opts)) (not (null? (cdr opts))))
                         (cadr opts)
                         #f)]
         [_ (when (> n *dense-adjacency-max-nodes*)
                  (error 'edges->adjacency-matrix
                         (format "node count ~a exceeds dense matrix limit ~a; use edges->sparse-adjacency instead"
                                 n *dense-adjacency-max-nodes*)))]
         [data (make-vector (* n n) 0)])
        ;; Fill matrix
        (for-each
         (lambda (edge)
                 (let ([i (edge-from edge)]
                       [j (edge-to edge)]
                       [w (edge-weight edge)])
                      (vector-set! data (+ (* i n) j) w)
                      (when undirected
                            (vector-set! data (+ (* j n) i) w))))
         edges)
        (list 'matrix n n data)))

;;; adjacency-matrix->edges : Matrix × (Option Boolean) → (List Edge)
;;; Convert adjacency matrix back to edge list.
;;; If weighted is #t, includes weights; otherwise just (from to).
(define (adjacency-matrix->edges m . opts)
  (let* ([weighted (if (null? opts) #t (car opts))]
         [n (matrix-rows m)]
         [data (matrix-data m)])
        (let loop ([i 0] [j 0] [edges '()])
             (cond
              [(= i n) (reverse edges)]
              [(= j n) (loop (+ i 1) 0 edges)]
              [else
               (let ([w (vector-ref data (+ (* i n) j))])
                    (if (= w 0)
                        (loop i (+ j 1) edges)
                        (loop i (+ j 1)
                              (cons (if weighted
                                        (list i j w)
                                        (list i j))
                                    edges))))]))))

;;; make-adjacency-matrix : Nat → Matrix
;;; Create empty n×n adjacency matrix (all zeros).
(define (make-adjacency-matrix n)
  (make-matrix n n 0))

;;; adjacency-matrix-add-edge! : Matrix × Nat × Nat × (Option Num) → Void
;;; Add edge to adjacency matrix (mutates).
(define (adjacency-matrix-add-edge! m i j . weight-opt)
  (let ([w (if (null? weight-opt) 1 (car weight-opt))]
        [n (matrix-cols m)]
        [data (matrix-data m)])
       (vector-set! data (+ (* i n) j) w)))

;;; adjacency-matrix-remove-edge! : Matrix × Nat × Nat → Void
;;; Remove edge from adjacency matrix (mutates).
(define (adjacency-matrix-remove-edge! m i j)
  (let ([n (matrix-cols m)]
        [data (matrix-data m)])
       (vector-set! data (+ (* i n) j) 0)))

;;; adjacency-matrix-has-edge? : Matrix × Nat × Nat → Boolean
(define (adjacency-matrix-has-edge? m i j)
  (not (= (matrix-ref m i j) 0)))

;;; adjacency-matrix-edge-weight : Matrix × Nat × Nat → Num
(define (adjacency-matrix-edge-weight m i j)
  (matrix-ref m i j))

;;; ====
;;; Sparse Adjacency Matrix
;;; ====

;;; edges->sparse-adjacency : (List Edge) × (Option Nat) × (Option Boolean) → SparseCSR
;;; Convert edge list to sparse adjacency matrix (CSR format).
;;; Efficient for large, sparse graphs.
(define (edges->sparse-adjacency edges . opts)
  (let* ([n (if (and (not (null? opts)) (car opts))
                (car opts)
                (infer-node-count edges))]
         [undirected (if (and (not (null? opts)) (not (null? (cdr opts))))
                         (cadr opts)
                         #f)]
         ;; Build edge list with undirected duplication if needed
         [all-edges (if undirected
                        (fold-left
                         (lambda (acc edge)
                                 (let ([i (edge-from edge)]
                                       [j (edge-to edge)]
                                       [w (edge-weight edge)])
                                      (cons (list j i w)
                                            (cons (list i j w) acc))))
                         '()
                         edges)
                        edges)]
         ;; Convert to triplets for COO
         [triplets (map (lambda (e)
                                (list (edge-from e) (edge-to e) (edge-weight e)))
                        all-edges)]
         [coo (sparse-coo-from-triplets n n triplets)])
        (coo->csr coo)))

;;; sparse-adjacency->edges : SparseCSR × (Option Boolean) → (List Edge)
;;; Convert sparse adjacency matrix to edge list.
(define (sparse-adjacency->edges m . opts)
  (let* ([weighted (if (null? opts) #t (car opts))]
         [n (sparse-csr-rows m)]
         [row-ptrs (sparse-csr-row-ptrs m)]
         [col-idx (sparse-csr-col-indices m)]
         [vals (sparse-csr-values m)])
        (let loop ([i 0] [edges '()])
             (if (= i n)
                 (reverse edges)
                 (let ([start (vector-ref row-ptrs i)]
                       [end (vector-ref row-ptrs (+ i 1))])
                      (let inner ([k start] [edges edges])
                           (if (= k end)
                               (loop (+ i 1) edges)
                               (let ([j (vector-ref col-idx k)]
                                     [w (vector-ref vals k)])
                                    (inner (+ k 1)
                                           (cons (if weighted
                                                     (list i j w)
                                                     (list i j))
                                                 edges))))))))))

;;; ====
;;; Graph Properties from Adjacency Matrix
;;; ====

;;; adjacency-matrix-node-count : Matrix|SparseCSR → Nat
(define (adjacency-matrix-node-count m)
  (if (sparse-csr? m)
      (sparse-csr-rows m)
      (matrix-rows m)))

;;; adjacency-matrix-edge-count : Matrix|SparseCSR → Nat
;;; Count non-zero entries (edges).
(define (adjacency-matrix-edge-count m)
  (if (sparse-csr? m)
      (sparse-csr-nnz m)
      (let ([data (matrix-data m)]
            [n (vector-length (matrix-data m))])
           (let loop ([i 0] [count 0])
                (if (= i n)
                    count
                    (loop (+ i 1)
                          (if (= (vector-ref data i) 0)
                              count
                              (+ count 1))))))))

;;; adjacency-out-degree : Matrix|SparseCSR × Nat → Nat
;;; Out-degree of node i (number of outgoing edges).
(define (adjacency-out-degree m i)
  (if (sparse-csr? m)
      (let ([row-ptrs (sparse-csr-row-ptrs m)])
           (- (vector-ref row-ptrs (+ i 1))
              (vector-ref row-ptrs i)))
      (let ([n (matrix-cols m)]
            [data (matrix-data m)])
           (let loop ([j 0] [count 0])
                (if (= j n)
                    count
                    (loop (+ j 1)
                          (if (= (vector-ref data (+ (* i n) j)) 0)
                              count
                              (+ count 1))))))))

;;; adjacency-in-degree : Matrix|SparseCSR × Nat → Nat
;;; In-degree of node i (number of incoming edges).
(define (adjacency-in-degree m i)
  (if (sparse-csr? m)
      ;; For CSR, need to scan all values
      (let ([n (sparse-csr-rows m)]
            [row-ptrs (sparse-csr-row-ptrs m)]
            [col-idx (sparse-csr-col-indices m)]
            [nnz (sparse-csr-nnz m)])
           (let loop ([k 0] [count 0])
                (if (= k nnz)
                    count
                    (loop (+ k 1)
                          (if (= (vector-ref col-idx k) i)
                              (+ count 1)
                              count)))))
      (let ([n (matrix-rows m)]
            [data (matrix-data m)]
            [cols (matrix-cols m)])
           (let loop ([j 0] [count 0])
                (if (= j n)
                    count
                    (loop (+ j 1)
                          (if (= (vector-ref data (+ (* j cols) i)) 0)
                              count
                              (+ count 1))))))))

;;; adjacency-neighbors : Matrix|SparseCSR × Nat → (List Nat)
;;; Get list of neighbors (outgoing edges) of node i.
(define (adjacency-neighbors m i)
  (if (sparse-csr? m)
      (let ([row-ptrs (sparse-csr-row-ptrs m)]
            [col-idx (sparse-csr-col-indices m)]
            [start (vector-ref (sparse-csr-row-ptrs m) i)]
            [end (vector-ref (sparse-csr-row-ptrs m) (+ i 1))])
           (let loop ([k start] [result '()])
                (if (= k end)
                    (reverse result)
                    (loop (+ k 1)
                          (cons (vector-ref col-idx k) result)))))
      (let ([n (matrix-cols m)]
            [data (matrix-data m)])
           (let loop ([j 0] [result '()])
                (if (= j n)
                    (reverse result)
                    (loop (+ j 1)
                          (if (= (vector-ref data (+ (* i n) j)) 0)
                              result
                              (cons j result))))))))

;;; adjacency-neighbors-with-weights : Matrix|SparseCSR × Nat → (List (Cons Nat Num))
;;; Get list of (neighbor . weight) pairs for node i's outgoing edges.
;;; For unweighted graphs, weight is 1 for existing edges.
;;; This enables O(degree) iteration instead of O(V) for sparse graphs.
(define (adjacency-neighbors-with-weights m i)
  (if (sparse-csr? m)
      ;; Delegate to sparse-csr-row-alist for O(degree) access
      (sparse-csr-row-alist m i)
      ;; Dense matrix: scan row, skip zeros
      (let ([n (matrix-cols m)]
            [data (matrix-data m)])
           (let loop ([j 0] [result '()])
                (if (= j n)
                    (reverse result)
                    (let ([w (vector-ref data (+ (* i n) j))])
                         (loop (+ j 1)
                               (if (= w 0)
                                   result
                                   (cons (cons j w) result)))))))))

;;; ====
;;; Graph Transformations
;;; ====

;;; adjacency-transpose : Matrix|SparseCSR → Matrix|SparseCSR
;;; Transpose adjacency matrix (reverse all edges).
(define (adjacency-transpose m)
  (if (sparse-csr? m)
      (sparse-csr-transpose m)
      (matrix-transpose m)))

;;; adjacency-symmetrize : Matrix → Matrix
;;; Make graph undirected by adding A + A^T.
(define (adjacency-symmetrize m)
  (matrix-add m (matrix-transpose m)))

;;; adjacency-symmetrize-sparse : SparseCSR → SparseCSR
;;; Make sparse graph undirected.
(define (adjacency-symmetrize-sparse m)
  (sparse-csr-add m (sparse-csr-transpose m)))

;;; ====
;;; Degree Matrix
;;; ====

;;; degree-matrix : Matrix|SparseCSR × [Symbol] → Matrix
;;; Create diagonal degree matrix.
;;; mode: 'out (default), 'in, or 'total
(define (degree-matrix m . mode-opt)
  (let* ([mode (if (null? mode-opt) 'out (car mode-opt))]
         [n (adjacency-matrix-node-count m)]
         [degrees (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! degrees i
                         (case mode
                               [(out) (adjacency-out-degree m i)]
                               [(in) (adjacency-in-degree m i)]
                               [(total) (+ (adjacency-out-degree m i)
                                           (adjacency-in-degree m i))])))
        (diagonal degrees)))

;;; degree-matrix-sparse : Matrix|SparseCSR × [Symbol] → SparseCSR
;;; Create sparse diagonal degree matrix.
(define (degree-matrix-sparse m . mode-opt)
  (let* ([mode (if (null? mode-opt) 'out (car mode-opt))]
         [n (adjacency-matrix-node-count m)]
         [degrees (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! degrees i
                         (case mode
                               [(out) (adjacency-out-degree m i)]
                               [(in) (adjacency-in-degree m i)]
                               [(total) (+ (adjacency-out-degree m i)
                                           (adjacency-in-degree m i))])))
        (sparse-diagonal degrees)))

;;; ====
;;; Special Graph Matrices
;;; ====

;;; complete-graph : Nat × [Boolean] → Matrix
;;; Create adjacency matrix for complete graph K_n.
;;; If weighted is #t, all edges have weight 1.
(define (complete-graph n . opts)
  (let ([data (make-vector (* n n) 1)])
       ;; Zero out diagonal (no self-loops)
       (do ([i 0 (+ i 1)])
           ((= i n) (list 'matrix n n data))
           (vector-set! data (+ (* i n) i) 0))))

;;; cycle-graph : Nat × [Boolean] → Matrix
;;; Create adjacency matrix for cycle graph C_n.
;;; If directed is #f (default), creates undirected cycle.
(define (cycle-graph n . opts)
  (let ([directed (if (null? opts) #f (car opts))]
        [data (make-vector (* n n) 0)])
       (do ([i 0 (+ i 1)])
           ((= i n) (list 'matrix n n data))
           (let ([next (remainder (+ i 1) n)])
                (vector-set! data (+ (* i n) next) 1)
                (unless directed
                        (vector-set! data (+ (* next n) i) 1))))))

;;; path-graph : Nat × [Boolean] → Matrix
;;; Create adjacency matrix for path graph P_n.
;;; If directed is #f (default), creates undirected path.
(define (path-graph n . opts)
  (let ([directed (if (null? opts) #f (car opts))]
        [data (make-vector (* n n) 0)])
       (do ([i 0 (+ i 1)])
           ((= i (- n 1)) (list 'matrix n n data))
           (vector-set! data (+ (* i n) (+ i 1)) 1)
           (unless directed
                   (vector-set! data (+ (* (+ i 1) n) i) 1)))))

;;; star-graph : Nat → Matrix
;;; Create adjacency matrix for star graph S_n (center at node 0).
(define (star-graph n)
  (let ([data (make-vector (* n n) 0)])
       ;; Center (node 0) connects to all others
       (do ([i 1 (+ i 1)])
           ((= i n) (list 'matrix n n data))
           (vector-set! data i 1)         ; 0 -> i
           (vector-set! data (* i n) 1)))); i -> 0

;;; bipartite-graph : Nat × Nat × (List (Nat × Nat)) → Matrix
;;; Create bipartite graph with m nodes in first partition,
;;; n nodes in second partition, and given edges.
;;; Edges are (i, j) where i in [0,m), j in [0,n).
(define (bipartite-graph m n edges)
  (let* ([total (+ m n)]
         [data (make-vector (* total total) 0)])
        (for-each
         (lambda (edge)
                 (let ([i (car edge)]
                       [j (+ m (cadr edge))])  ; Offset second partition
                      (vector-set! data (+ (* i total) j) 1)
                      (vector-set! data (+ (* j total) i) 1)))
         edges)
        (list 'matrix total total data)))

;;; ====
;;; Matrix Powers for Graph Analysis
;;; ====

;;; adjacency-power : Matrix × Nat → Matrix
;;; Compute A^k (paths of length exactly k).
(define (adjacency-power m k)
  (if (= k 0)
      (matrix-identity (matrix-rows m))
      (let loop ([result m] [remaining (- k 1)])
           (if (= remaining 0)
               result
               (loop (matrix-mul result m) (- remaining 1))))))

;;; adjacency-reachability : Matrix × Nat → Matrix
;;; Compute reachability matrix (I + A + A^2 + ... + A^k).
;;; Entry (i,j) > 0 iff j is reachable from i in at most k steps.
(define (adjacency-reachability m k)
  (let ([n (matrix-rows m)])
       (let loop ([step 1] [result (matrix-identity n)] [power m])
            (if (> step k)
                result
                (loop (+ step 1)
                      (matrix-add result power)
                      (matrix-mul power m))))))

(doc matrix-power-fast 'type '(-> Matrix Nat Matrix))
(doc matrix-power-fast 'description "Compute A^k using repeated squaring (binary exponentiation)")
(doc matrix-power-fast 'note "Complexity: O(log k) matrix multiplications instead of O(k)")
(define (matrix-power-fast m k)
  (cond [(= k 0) (matrix-identity (matrix-rows m))]
        [(= k 1) m]
        [(even? k)
         (let ([half (matrix-power-fast m (quotient k 2))])
              (matrix-mul half half))]
        [else
         (matrix-mul m (matrix-power-fast m (- k 1)))]))

(doc 'constant '*infinity*)
(doc 'description "Representation of infinity for distance matrices")
(define *infinity* 1e308)

(doc transitive-closure 'type '(-> Matrix Matrix))
(doc transitive-closure 'description "Compute transitive closure using Warshall's algorithm; entry (i,j) = 1 iff j reachable from i")
(doc transitive-closure 'note "Complexity: O(n³)")
(define (transitive-closure adj)
  (let* ([n (matrix-rows adj)]
         [data (vector-copy (matrix-data adj))])
        ;; Set diagonal to 1 (self-reachable)
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! data (+ (* i n) i) 1))
        ;; Warshall's algorithm
        (do ([k 0 (+ k 1)])
            ((= k n))
            (do ([i 0 (+ i 1)])
                ((= i n))
                (do ([j 0 (+ j 1)])
                    ((= j n))
                    (let ([ij (+ (* i n) j)]
                          [ik (+ (* i n) k)]
                          [kj (+ (* k n) j)])
                         (when (and (> (vector-ref data ik) 0)
                                    (> (vector-ref data kj) 0))
                               (vector-set! data ij 1))))))
        (list 'matrix n n data)))

(doc floyd-warshall 'type '(-> Matrix Matrix))
(doc floyd-warshall 'description "All-pairs shortest paths using Floyd-Warshall algorithm")
(doc floyd-warshall 'param 'adj "Weighted adjacency matrix (0 means no edge, except diagonal)")
(doc floyd-warshall 'returns "Distance matrix where D[i,j] = shortest path length i→j; *infinity* for unreachable pairs")
(doc floyd-warshall 'note "Complexity: O(n³); detects negative cycles (diagonal becomes negative)")
(define (floyd-warshall adj)
  (let* ([n (matrix-rows adj)]
         [data (make-vector (* n n) *infinity*)])
        ;; Initialize distance matrix
        ;; For diagonal: use min(0, adj[i,i]) to detect negative self-loops
        (do ([i 0 (+ i 1)])
            ((= i n))
            (do ([j 0 (+ j 1)])
                ((= j n))
                (let ([idx (+ (* i n) j)]
                      [val (matrix-ref adj i j)])
                     (cond [(= i j) (vector-set! data idx (min 0 val))]
                           [(> val 0) (vector-set! data idx val)]))))
        ;; Floyd-Warshall dynamic programming
        (do ([k 0 (+ k 1)])
            ((= k n))
            (do ([i 0 (+ i 1)])
                ((= i n))
                (do ([j 0 (+ j 1)])
                    ((= j n))
                    (let* ([ij (+ (* i n) j)]
                           [ik (+ (* i n) k)]
                           [kj (+ (* k n) j)]
                           [d-ij (vector-ref data ij)]
                           [d-ik (vector-ref data ik)]
                           [d-kj (vector-ref data kj)]
                           [via-k (+ d-ik d-kj)])
                          (when (< via-k d-ij)
                                (vector-set! data ij via-k))))))
        (list 'matrix n n data)))

;;; has-negative-cycle? : Matrix → Boolean
;;; Check if graph has a negative cycle.
;;; Run floyd-warshall first, then check if any diagonal < 0.
(define (has-negative-cycle? dist)
  (let ([n (matrix-rows dist)])
       (let loop ([i 0])
            (cond [(= i n) #f]
                  [(< (matrix-ref dist i i) 0) #t]
                  [else (loop (+ i 1))]))))

(doc dijkstra 'type '(-> (Union Matrix SparseCSR) Nat (Pair Vector Vector)))
(doc dijkstra 'description "Compute single-source shortest paths using Dijkstra's algorithm with heap-based extraction")
(doc dijkstra 'param 'adj "Adjacency matrix (dense or sparse)")
(doc dijkstra 'param 'source "Source node")
(doc dijkstra 'returns "Pair of (distances . predecessors) vectors")
(doc dijkstra 'note "Time complexity: O((V + E) log V); for non-negative weights only")
(doc dijkstra 'note "Uses lazy deletion: inserts new (distance, node) pairs rather than updating")
(define (dijkstra adj source)
  (let* ([n (adjacency-matrix-node-count adj)]
         [dist (make-vector n *infinity*)]
         [pred (make-vector n -1)]
         [visited (make-vector n #f)]
         ;; Comparator: smaller distance first (min-heap by distance)
         [dist-cmp (lambda (a b) (<= (car a) (car b)))])
        ;; Initialize source
        (vector-set! dist source 0)
        ;; Main loop with heap-based extraction
        (let loop ([heap (heap-insert-by dist-cmp (cons 0 source) heap-empty)])
             (if (heap-empty? heap)
                 (cons dist pred)
                 (let* ([top (heap-value heap)]
                        [heap-rest (heap-delete-top-by dist-cmp heap)]
                        [u (cdr top)])
                       (if (vector-ref visited u)
                           ;; Already processed (lazy deletion) - skip
                           (loop heap-rest)
                           (begin
                             (vector-set! visited u #t)
                             ;; Relax neighbors - O(degree) instead of O(V)
                             (let relax ([neighbors (adjacency-neighbors-with-weights adj u)]
                                         [h heap-rest])
                                  (if (null? neighbors)
                                      (loop h)
                                      (let* ([edge (car neighbors)]
                                             [v (car edge)]
                                             [w (cdr edge)])
                                            (if (not (vector-ref visited v))
                                                (let ([alt (+ (vector-ref dist u) w)])
                                                     (if (< alt (vector-ref dist v))
                                                         (begin
                                                           (vector-set! dist v alt)
                                                           (vector-set! pred v u)
                                                           (relax (cdr neighbors)
                                                                  (heap-insert-by dist-cmp (cons alt v) h)))
                                                         (relax (cdr neighbors) h)))
                                                (relax (cdr neighbors) h))))))))))))

;;; dijkstra-naive : Matrix × Nat → (Vector Distance) × (Vector Predecessor)
;;; Original O(n²) implementation using linear scan for min extraction.
;;; Kept for comparison and cases where heap overhead isn't worth it.
(define (dijkstra-naive adj source)
  (let* ([n (matrix-rows adj)]
         [dist (make-vector n *infinity*)]
         [pred (make-vector n -1)]
         [visited (make-vector n #f)])
        (vector-set! dist source 0)
        (do ([iter 0 (+ iter 1)])
            ((= iter n))
            (let ([u (dijkstra-find-min dist visited n)])
                 (when (and u (< (vector-ref dist u) *infinity*))
                       (vector-set! visited u #t)
                       (do ([v 0 (+ v 1)])
                           ((= v n))
                           (let ([w (matrix-ref adj u v)])
                                (when (and (> w 0)
                                           (not (vector-ref visited v)))
                                      (let ([alt (+ (vector-ref dist u) w)])
                                           (when (< alt (vector-ref dist v))
                                                 (vector-set! dist v alt)
                                                 (vector-set! pred v u)))))))))
        (cons dist pred)))

;;; dijkstra-find-min : Vector × Vector × Nat → Nat | #f
;;; Find unvisited node with minimum distance. O(n) linear scan.
;;; Used by dijkstra-naive.
(define (dijkstra-find-min dist visited n)
  (let loop ([i 0] [min-idx #f] [min-val *infinity*])
       (if (= i n)
           min-idx
           (if (and (not (vector-ref visited i))
                    (< (vector-ref dist i) min-val))
               (loop (+ i 1) i (vector-ref dist i))
               (loop (+ i 1) min-idx min-val)))))

;;; dijkstra-path : Matrix × Nat × Nat → (List Nat) | #f
;;; Find shortest path from source to target using Dijkstra.
;;; Returns list of node indices, or #f if unreachable.
;;;
;;; Example:
;;;   (dijkstra-path adj 0 3) => (0 1 2 3)
(define (dijkstra-path adj source target)
  (let* ([result (dijkstra adj source)]
         [dist (car result)]
         [pred (cdr result)])
        (if (= (vector-ref dist target) *infinity*)
            #f  ; Unreachable
            ;; Reconstruct path from predecessors
            (let loop ([node target] [path '()])
                 (if (= node source)
                     (cons source path)
                     (loop (vector-ref pred node)
                           (cons node path)))))))

;;; dijkstra-distance : Matrix × Nat × Nat → Nat | Infinity
;;; Get shortest distance from source to target.
(define (dijkstra-distance adj source target)
  (let* ([result (dijkstra adj source)]
         [dist (car result)])
        (vector-ref dist target)))

;;; distance-matrix-unweighted : Matrix → Matrix
;;; Compute all-pairs shortest paths for unweighted graph.
;;; Uses BFS from each node. Complexity: O(n²) for sparse, O(n³) for dense.
;;; Returns distance matrix with *infinity* for unreachable pairs.
(define (distance-matrix-unweighted adj)
  (let* ([n (matrix-rows adj)]
         [adj-data (matrix-data adj)]
         [dist (make-vector (* n n) *infinity*)])
        ;; BFS from each source node
        (do ([src 0 (+ src 1)])
            ((= src n))
            ;; Initialize BFS for this source
            (vector-set! dist (+ (* src n) src) 0)
            (let ([queue (list src)]
                  [visited (make-vector n #f)])
                 (vector-set! visited src #t)
                 ;; BFS loop
                 (let bfs ([q queue])
                      (when (not (null? q))
                            (let* ([u (car q)]
                                   [d-u (vector-ref dist (+ (* src n) u))]
                                   [next-q (cdr q)])
                                  ;; Visit all neighbors of u
                                  (do ([v 0 (+ v 1)])
                                      ((= v n))
                                      (when (and (not (vector-ref visited v))
                                                 (> (vector-ref adj-data (+ (* u n) v)) 0))
                                            (vector-set! visited v #t)
                                            (vector-set! dist (+ (* src n) v) (+ d-u 1))
                                            (set! next-q (append next-q (list v)))))
                                  (bfs next-q))))))
        (list 'matrix n n dist)))

;;; graph-eccentricity : Matrix × Nat → Nat|Infinity
;;; Eccentricity of node i: max distance to any reachable node.
;;; Returns *infinity* if node is isolated (no reachable neighbors).
(define (graph-eccentricity dist i)
  (let ([n (matrix-rows dist)])
       (let loop ([j 0] [max-d 0] [found-any #f])
            (if (= j n)
                (if found-any max-d *infinity*)
                (let ([d (matrix-ref dist i j)])
                     (if (and (not (= i j))
                              (< d *infinity*))
                         (loop (+ j 1) (max max-d d) #t)
                         (loop (+ j 1) max-d found-any)))))))

;;; graph-diameter : Matrix → Nat|Infinity
;;; Diameter: maximum eccentricity (longest shortest path).
;;; Input should be a distance matrix.
(define (graph-diameter dist)
  (let ([n (matrix-rows dist)])
       (let loop ([i 0] [diam 0])
            (if (= i n)
                diam
                (let ([ecc (graph-eccentricity dist i)])
                     (loop (+ i 1)
                           (if (< ecc *infinity*)
                               (max diam ecc)
                               diam)))))))

;;; graph-radius : Matrix → Nat|Infinity
;;; Radius: minimum eccentricity.
;;; Input should be a distance matrix.
(define (graph-radius dist)
  (let ([n (matrix-rows dist)])
       (let loop ([i 0] [rad *infinity*])
            (if (= i n)
                rad
                (let ([ecc (graph-eccentricity dist i)])
                     (loop (+ i 1)
                           (if (and (< ecc *infinity*) (> ecc 0))
                               (min rad ecc)
                               rad)))))))

;;; graph-center : Matrix → (List Nat)
;;; Center: nodes with eccentricity equal to radius.
;;; Input should be a distance matrix.
(define (graph-center dist)
  (let* ([n (matrix-rows dist)]
         [rad (graph-radius dist)])
        (let loop ([i 0] [center '()])
             (if (= i n)
                 (reverse center)
                 (let ([ecc (graph-eccentricity dist i)])
                      (loop (+ i 1)
                            (if (= ecc rad)
                                (cons i center)
                                center)))))))

;;; ====
;;; Dense/Sparse Conversion
;;; ====

;;; adjacency-to-sparse : Matrix → SparseCSR
;;; Convert dense adjacency matrix to sparse.
(define (adjacency-to-sparse m)
  (dense->sparse-csr m))

;;; adjacency-to-dense : SparseCSR → Matrix
;;; Convert sparse adjacency matrix to dense.
(define (adjacency-to-dense m)
  (sparse-csr->dense m))

;;; adjacency-density : Matrix|SparseCSR → Num
;;; Compute edge density (edges / possible_edges).
(define (adjacency-density m)
  (let ([n (adjacency-matrix-node-count m)]
        [edges (adjacency-matrix-edge-count m)])
       (if (= n 0)
           0
           (/ edges (* n (- n 1))))))  ; Exclude self-loops
