;;; graph-matrix.ss — Adjacency Matrix Graph Representation
;;;
;;; Matrix-based graph representation for linear algebra graph algorithms.
;;; Supports both dense and sparse matrix formats.
;;;
;;; Graph representation:
;;;   - Nodes are numbered 0 to n-1
;;;   - Edge (i,j) means node i connects to node j
;;;   - For weighted graphs, A[i,j] = weight of edge (i,j)
;;;   - For unweighted graphs, A[i,j] = 1 if edge exists, 0 otherwise
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - matrix.ss
;;;   - sparse.ss

;;; ============================================================
;;; Edge List Representation
;;; ============================================================
;;;
;;; Edge lists are the input format for graph construction:
;;;   - Unweighted: ((from to) ...)
;;;   - Weighted: ((from to weight) ...)

;;; edge-list? : Any → Boolean
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

;;; ============================================================
;;; Dense Adjacency Matrix
;;; ============================================================

;;; Maximum node count for dense adjacency matrices.
;;; Beyond this threshold, use edges->sparse-adjacency instead.
(define *dense-adjacency-max-nodes* 10000)

;;; edges->adjacency-matrix : (List Edge) × [Nat] × [Boolean] → Matrix
;;; Convert edge list to dense adjacency matrix.
;;; Optional node-count overrides inferred count.
;;; If undirected is #t, adds both (i,j) and (j,i) for each edge.
;;; Raises error if n > *dense-adjacency-max-nodes* (use sparse matrix instead).
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

;;; adjacency-matrix->edges : Matrix × [Boolean] → (List Edge)
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

;;; adjacency-matrix-add-edge! : Matrix × Nat × Nat × [Num] → Void
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

;;; ============================================================
;;; Sparse Adjacency Matrix
;;; ============================================================

;;; edges->sparse-adjacency : (List Edge) × [Nat] × [Boolean] → SparseCSR
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

;;; sparse-adjacency->edges : SparseCSR × [Boolean] → (List Edge)
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

;;; ============================================================
;;; Graph Properties from Adjacency Matrix
;;; ============================================================

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

;;; ============================================================
;;; Graph Transformations
;;; ============================================================

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

;;; ============================================================
;;; Degree Matrix
;;; ============================================================

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

;;; ============================================================
;;; Special Graph Matrices
;;; ============================================================

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

;;; ============================================================
;;; Matrix Powers for Graph Analysis
;;; ============================================================

;;; adjacency-power : Matrix × Nat → Matrix
;;; Compute A^k (paths of length exactly k).
(define (adjacency-power m k)
  (if (= k 0)
      (identity (matrix-rows m))
      (let loop ([result m] [remaining (- k 1)])
           (if (= remaining 0)
               result
               (loop (matrix-mul result m) (- remaining 1))))))

;;; adjacency-reachability : Matrix × Nat → Matrix
;;; Compute reachability matrix (I + A + A^2 + ... + A^k).
;;; Entry (i,j) > 0 iff j is reachable from i in at most k steps.
(define (adjacency-reachability m k)
  (let ([n (matrix-rows m)])
       (let loop ([step 1] [result (identity n)] [power m])
            (if (> step k)
                result
                (loop (+ step 1)
                      (matrix-add result power)
                      (matrix-mul power m))))))

;;; ============================================================
;;; Dense/Sparse Conversion
;;; ============================================================

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
