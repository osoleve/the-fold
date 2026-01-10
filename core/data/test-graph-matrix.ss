;;; test-graph-matrix.ss — Tests for adjacency matrix graph representation
;;;
;;; Run with: scheme --script core/data/test-graph-matrix.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec.ss")
(load "core/linalg/matrix.ss")
(load "core/linalg/sparse.ss")
(load "core/data/graph-matrix.ss")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a~n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a~n" name)
       (printf "      Expected: ~s~n" expected)
       (printf "      Got:      ~s~n" actual))))

(define (test-approx name expected actual tol)
  (if (< (abs (- expected actual)) tol)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a~n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a~n" name)
       (printf "      Expected: ~a (±~a)~n" expected tol)
       (printf "      Got:      ~a~n" actual))))

(printf "~n=== Graph Matrix Tests ===~n~n")

;;; ============================================================
;;; Edge List Tests
;;; ============================================================

(printf "--- Edge List Utilities ---~n")

(test "edge-list? valid unweighted" #t (edge-list? '((0 1) (1 2) (2 0))))
(test "edge-list? valid weighted" #t (edge-list? '((0 1 1.5) (1 2 2.0))))
(test "edge-list? empty" #t (edge-list? '()))
(test "infer-node-count" 4 (infer-node-count '((0 1) (1 2) (2 3))))
(test "infer-node-count single edge" 2 (infer-node-count '((0 1))))
(test "infer-node-count gaps" 5 (infer-node-count '((0 4) (1 2))))

;;; ============================================================
;;; Dense Adjacency Matrix Tests
;;; ============================================================

(printf "~n--- Dense Adjacency Matrix ---~n")

;; Simple directed graph: 0->1, 1->2, 2->0
(let* ([edges '((0 1) (1 2) (2 0))]
       [m (edges->adjacency-matrix edges)])
      (test "directed: is matrix?" #t (matrix? m))
      (test "directed: shape" '(3 . 3) (matrix-shape m))
      (test "directed: edge 0->1" 1 (matrix-ref m 0 1))
      (test "directed: edge 1->2" 1 (matrix-ref m 1 2))
      (test "directed: edge 2->0" 1 (matrix-ref m 2 0))
      (test "directed: no edge 1->0" 0 (matrix-ref m 1 0)))

;; Undirected graph
(let* ([edges '((0 1) (1 2))]
       [m (edges->adjacency-matrix edges 3 #t)])
      (test "undirected: edge 0->1" 1 (matrix-ref m 0 1))
      (test "undirected: edge 1->0" 1 (matrix-ref m 1 0))
      (test "undirected: edge 1->2" 1 (matrix-ref m 1 2))
      (test "undirected: edge 2->1" 1 (matrix-ref m 2 1)))

;; Weighted graph
(let* ([edges '((0 1 2.5) (1 2 3.0))]
       [m (edges->adjacency-matrix edges)])
      (test "weighted: edge 0->1" 2.5 (matrix-ref m 0 1))
      (test "weighted: edge 1->2" 3.0 (matrix-ref m 1 2)))

;; Round-trip
(let* ([edges '((0 1 1) (1 2 1) (2 0 1))]
       [m (edges->adjacency-matrix edges)]
       [edges2 (adjacency-matrix->edges m #t)])
      (test "round-trip: same length" 3 (length edges2)))

;;; ============================================================
;;; Graph Properties Tests
;;; ============================================================

(printf "~n--- Graph Properties ---~n")

(let* ([edges '((0 1) (0 2) (1 2) (2 0))]
       [m (edges->adjacency-matrix edges)])
      (test "node-count" 3 (adjacency-matrix-node-count m))
      (test "edge-count" 4 (adjacency-matrix-edge-count m))
      (test "out-degree node 0" 2 (adjacency-out-degree m 0))
      (test "out-degree node 1" 1 (adjacency-out-degree m 1))
      (test "out-degree node 2" 1 (adjacency-out-degree m 2))
      (test "in-degree node 0" 1 (adjacency-in-degree m 0))
      (test "in-degree node 1" 1 (adjacency-in-degree m 1))
      (test "in-degree node 2" 2 (adjacency-in-degree m 2))
      (test "neighbors of 0" '(1 2) (adjacency-neighbors m 0))
      (test "neighbors of 1" '(2) (adjacency-neighbors m 1)))

;;; ============================================================
;;; Sparse Adjacency Matrix Tests
;;; ============================================================

(printf "~n--- Sparse Adjacency Matrix ---~n")

;; Simple directed graph
(let* ([edges '((0 1) (1 2) (2 0))]
       [m (edges->sparse-adjacency edges)])
      (test "sparse: is CSR?" #t (sparse-csr? m))
      (test "sparse: shape" '(3 . 3) (sparse-shape m))
      (test "sparse: edge 0->1" 1 (sparse-csr-ref m 0 1))
      (test "sparse: edge 1->2" 1 (sparse-csr-ref m 1 2))
      (test "sparse: edge 2->0" 1 (sparse-csr-ref m 2 0))
      (test "sparse: no edge 1->0" 0 (sparse-csr-ref m 1 0))
      (test "sparse: nnz" 3 (sparse-csr-nnz m)))

;; Sparse undirected
(let* ([edges '((0 1) (1 2))]
       [m (edges->sparse-adjacency edges 3 #t)])
      (test "sparse undirected: nnz" 4 (sparse-csr-nnz m))
      (test "sparse undirected: 0->1" 1 (sparse-csr-ref m 0 1))
      (test "sparse undirected: 1->0" 1 (sparse-csr-ref m 1 0)))

;; Sparse properties
(let* ([edges '((0 1) (0 2) (1 2))]
       [m (edges->sparse-adjacency edges)])
      (test "sparse: node-count" 3 (adjacency-matrix-node-count m))
      (test "sparse: edge-count" 3 (adjacency-matrix-edge-count m))
      (test "sparse: out-degree 0" 2 (adjacency-out-degree m 0))
      (test "sparse: neighbors 0" '(1 2) (adjacency-neighbors m 0)))

;; Sparse round-trip
(let* ([edges '((0 1 1) (1 2 1) (2 0 1))]
       [m (edges->sparse-adjacency edges)]
       [edges2 (sparse-adjacency->edges m #t)])
      (test "sparse round-trip: length" 3 (length edges2)))

;;; ============================================================
;;; Graph Transformations Tests
;;; ============================================================

(printf "~n--- Graph Transformations ---~n")

;; Transpose
(let* ([edges '((0 1) (0 2))]
       [m (edges->adjacency-matrix edges)]
       [mt (adjacency-transpose m)])
      (test "transpose: 1->0" 1 (matrix-ref mt 1 0))
      (test "transpose: 2->0" 1 (matrix-ref mt 2 0))
      (test "transpose: no 0->1" 0 (matrix-ref mt 0 1)))

;; Symmetrize
(let* ([edges '((0 1) (1 2))]
       [m (edges->adjacency-matrix edges)]
       [sym (adjacency-symmetrize m)])
      (test "symmetrize: 0->1" 1 (matrix-ref sym 0 1))
      (test "symmetrize: 1->0" 1 (matrix-ref sym 1 0))
      (test "symmetrize: symmetric" #t (matrix-symmetric? sym)))

;;; ============================================================
;;; Degree Matrix Tests
;;; ============================================================

(printf "~n--- Degree Matrix ---~n")

(let* ([edges '((0 1) (0 2) (1 2))]
       [m (edges->adjacency-matrix edges)]
       [d (degree-matrix m 'out)])
      (test "degree matrix: is diagonal" #t
            (and (= (matrix-ref d 0 0) 2)
                 (= (matrix-ref d 1 1) 1)
                 (= (matrix-ref d 2 2) 0)
                 (= (matrix-ref d 0 1) 0))))

;;; ============================================================
;;; Special Graphs Tests
;;; ============================================================

(printf "~n--- Special Graphs ---~n")

;; Complete graph K4
(let ([k4 (complete-graph 4)])
     (test "K4: shape" '(4 . 4) (matrix-shape k4))
     (test "K4: edge count" 12 (adjacency-matrix-edge-count k4))
     (test "K4: no self-loops" 0 (matrix-ref k4 0 0))
     (test "K4: has edge 0->1" 1 (matrix-ref k4 0 1)))

;; Cycle graph C4
(let ([c4 (cycle-graph 4)])
     (test "C4: shape" '(4 . 4) (matrix-shape c4))
     (test "C4: edge count" 8 (adjacency-matrix-edge-count c4))  ; Undirected
     (test "C4: has 0->1" 1 (matrix-ref c4 0 1))
     (test "C4: has 3->0" 1 (matrix-ref c4 3 0)))

;; Path graph P4
(let ([p4 (path-graph 4)])
     (test "P4: shape" '(4 . 4) (matrix-shape p4))
     (test "P4: edge count" 6 (adjacency-matrix-edge-count p4))  ; Undirected
     (test "P4: has 0->1" 1 (matrix-ref p4 0 1))
     (test "P4: no 3->0" 0 (matrix-ref p4 3 0)))

;; Star graph S4
(let ([s4 (star-graph 4)])
     (test "S4: shape" '(4 . 4) (matrix-shape s4))
     (test "S4: edge count" 6 (adjacency-matrix-edge-count s4))  ; 3 edges * 2
     (test "S4: center->1" 1 (matrix-ref s4 0 1))
     (test "S4: 2->center" 1 (matrix-ref s4 2 0)))

;; Bipartite graph
(let ([bp (bipartite-graph 2 3 '((0 0) (0 1) (1 2)))])
     (test "bipartite: shape" '(5 . 5) (matrix-shape bp))
     (test "bipartite: edge count" 6 (adjacency-matrix-edge-count bp)))

;;; ============================================================
;;; Matrix Powers Tests
;;; ============================================================

(printf "~n--- Matrix Powers ---~n")

;; Path graph: A^2 gives 2-hop reachability
(let* ([edges '((0 1) (1 2) (2 3))]
       [m (edges->adjacency-matrix edges 4)]
       [m2 (adjacency-power m 2)])
      (test "A^2: 0->2 reachable" 1 (matrix-ref m2 0 2))
      (test "A^2: 1->3 reachable" 1 (matrix-ref m2 1 3))
      (test "A^2: 0->3 not 2-hop" 0 (matrix-ref m2 0 3)))

;; Reachability matrix
(let* ([edges '((0 1) (1 2))]
       [m (edges->adjacency-matrix edges 3)]
       [reach (adjacency-reachability m 2)])
      (test "reachability: 0->0" 1 (matrix-ref reach 0 0))  ; Identity
      (test "reachability: 0->2" 1 (matrix-ref reach 0 2)))  ; 2-hop

;;; ============================================================
;;; Dense/Sparse Conversion Tests
;;; ============================================================

(printf "~n--- Dense/Sparse Conversion ---~n")

(let* ([edges '((0 1) (1 2) (2 0))]
       [dense (edges->adjacency-matrix edges)]
       [sparse (adjacency-to-sparse dense)]
       [dense2 (adjacency-to-dense sparse)])
      (test "dense->sparse->dense: equal" #t (matrix-equal? dense dense2)))

;; Density calculation
(let* ([edges '((0 1) (1 2))]
       [m (edges->adjacency-matrix edges 3)])
      (test-approx "density: 2 edges in 3 nodes" (/ 2 6) (adjacency-density m) 0.001))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(printf "~n--- Edge Cases ---~n")

;; Empty graph
(let ([m (edges->adjacency-matrix '() 3)])
     (test "empty: edge count" 0 (adjacency-matrix-edge-count m)))

;; Single node
(let ([m (edges->adjacency-matrix '() 1)])
     (test "single node: shape" '(1 . 1) (matrix-shape m)))

;; Self-loop
(let ([m (edges->adjacency-matrix '((0 0)) 1)])
     (test "self-loop: edge" 1 (matrix-ref m 0 0)))

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "~n=== Summary ===~n")
(printf "  Passed: ~a~n" tests-passed)
(printf "  Failed: ~a~n" tests-failed)
(printf "  Total:  ~a~n" (+ tests-passed tests-failed))
(if (= tests-failed 0)
    (printf "~n  All tests passed!~n~n")
    (printf "~n  Some tests failed.~n~n"))
