;;; test-graph-matrix.ss — Tests for adjacency matrix graph representation
;;;
;;; Run with: scheme --script core/data/test-graph-matrix.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/sparse.ss")
(load "lattice/data/graph-matrix.ss")

;;; ====
;;; Test Utilities
;;; ====

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

;;; ====
;;; Edge List Tests
;;; ====

(printf "--- Edge List Utilities ---~n")

(test "edge-list? valid unweighted" #t (edge-list? '((0 1) (1 2) (2 0))))
(test "edge-list? valid weighted" #t (edge-list? '((0 1 1.5) (1 2 2.0))))
(test "edge-list? empty" #t (edge-list? '()))
(test "infer-node-count" 4 (infer-node-count '((0 1) (1 2) (2 3))))
(test "infer-node-count single edge" 2 (infer-node-count '((0 1))))
(test "infer-node-count gaps" 5 (infer-node-count '((0 4) (1 2))))

;;; ====
;;; Dense Adjacency Matrix Tests
;;; ====

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

;;; ====
;;; Graph Properties Tests
;;; ====

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

;;; ====
;;; Sparse Adjacency Matrix Tests
;;; ====

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

;;; ====
;;; Graph Transformations Tests
;;; ====

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

;;; ====
;;; Degree Matrix Tests
;;; ====

(printf "~n--- Degree Matrix ---~n")

(let* ([edges '((0 1) (0 2) (1 2))]
       [m (edges->adjacency-matrix edges)]
       [d (degree-matrix m 'out)])
      (test "degree matrix: is diagonal" #t
            (and (= (matrix-ref d 0 0) 2)
                 (= (matrix-ref d 1 1) 1)
                 (= (matrix-ref d 2 2) 0)
                 (= (matrix-ref d 0 1) 0))))

;;; ====
;;; Special Graphs Tests
;;; ====

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

;;; ====
;;; Matrix Powers Tests
;;; ====

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

;;; ====
;;; Dense/Sparse Conversion Tests
;;; ====

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

;;; ====
;;; Matrix-Based Distance Algorithm Tests
;;; ====

(printf "~n--- Matrix-Based Distance Algorithms ---~n")

;; Binary exponentiation: verify matrix-power-fast matches adjacency-power
(let* ([edges '((0 1) (1 2) (2 0))]
       [m (edges->adjacency-matrix edges)]
       [slow (adjacency-power m 8)]
       [fast (matrix-power-fast m 8)])
      (test "power-fast: A^8 matches naive" #t (matrix-equal? slow fast))
      (test "power-fast: A^0 is identity" #t
            (matrix-equal? (matrix-power-fast m 0) (identity 3)))
      (test "power-fast: A^1 is A" #t (matrix-equal? (matrix-power-fast m 1) m)))

;; Transitive closure
(let* ([edges '((0 1) (1 2))]  ; Path: 0 -> 1 -> 2
       [m (edges->adjacency-matrix edges 3)]
       [tc (transitive-closure m)])
      (test "transitive: 0->0 self" 1 (matrix-ref tc 0 0))
      (test "transitive: 0->1 direct" 1 (matrix-ref tc 0 1))
      (test "transitive: 0->2 indirect" 1 (matrix-ref tc 0 2))
      (test "transitive: 2->0 no path" 0 (matrix-ref tc 2 0))
      (test "transitive: 1->0 no path" 0 (matrix-ref tc 1 0)))

;; Transitive closure with cycle
(let* ([edges '((0 1) (1 2) (2 0))]  ; Cycle
       [m (edges->adjacency-matrix edges)]
       [tc (transitive-closure m)])
      (test "transitive-cycle: all reachable" 1 (matrix-ref tc 0 2))
      (test "transitive-cycle: back edge" 1 (matrix-ref tc 2 0)))

;; Floyd-Warshall on weighted graph
(let* ([adj (matrix-from-lists '((0 1 0 0)    ; 0->1 weight 1
                                 (0 0 2 0)    ; 1->2 weight 2
                                 (0 0 0 1)    ; 2->3 weight 1
                                 (0 0 0 0)))] ; 3 sink
       [dist (floyd-warshall adj)])
      (test "floyd: 0->0 is 0" 0 (matrix-ref dist 0 0))
      (test "floyd: 0->1 is 1" 1 (matrix-ref dist 0 1))
      (test "floyd: 0->2 is 3" 3 (matrix-ref dist 0 2))  ; 1+2
      (test "floyd: 0->3 is 4" 4 (matrix-ref dist 0 3))  ; 1+2+1
      (test "floyd: 3->0 unreachable" #t (>= (matrix-ref dist 3 0) *infinity*)))

;; Floyd-Warshall with shortcut
(let* ([adj (matrix-from-lists '((0 10 3 0)   ; Direct 0->1=10, 0->2=3
                                 (0 0 0 0)
                                 (0 1 0 0)    ; Shortcut 2->1=1
                                 (0 0 0 0)))]
       [dist (floyd-warshall adj)])
      (test "floyd-shortcut: 0->1 via 2" 4 (matrix-ref dist 0 1)))  ; 3+1 < 10

;; Negative cycle detection (no negative cycle)
(let* ([adj (matrix-from-lists '((0 1 0) (0 0 2) (0 0 0)))]
       [dist (floyd-warshall adj)])
      (test "no-negative-cycle" #f (has-negative-cycle? dist)))

;; Distance matrix for unweighted graph
(let* ([edges '((0 1) (1 2) (2 3))]  ; Path of length 3
       [m (edges->adjacency-matrix edges 4)]
       [dist (distance-matrix-unweighted m)])
      (test "dist-unweighted: 0->0" 0 (matrix-ref dist 0 0))
      (test "dist-unweighted: 0->1" 1 (matrix-ref dist 0 1))
      (test "dist-unweighted: 0->2" 2 (matrix-ref dist 0 2))
      (test "dist-unweighted: 0->3" 3 (matrix-ref dist 0 3))
      (test "dist-unweighted: 3->0 unreachable" #t
            (>= (matrix-ref dist 3 0) *infinity*)))

;; Graph metrics: eccentricity, diameter, radius, center
(let* ([edges '((0 1) (1 2) (2 3) (3 0))]  ; Cycle of 4
       [m (edges->adjacency-matrix edges 4 #t)]  ; Undirected
       [dist (distance-matrix-unweighted m)])
      (test "eccentricity: node 0" 2 (graph-eccentricity dist 0))
      (test "diameter: cycle-4" 2 (graph-diameter dist))
      (test "radius: cycle-4" 2 (inexact->exact (graph-radius dist)))
      (test "center: all nodes" '(0 1 2 3) (graph-center dist)))

;; Path graph metrics
(let* ([edges '((0 1) (1 2) (2 3))]  ; Path 0-1-2-3
       [m (edges->adjacency-matrix edges 4 #t)]  ; Undirected
       [dist (distance-matrix-unweighted m)])
      (test "path-eccentricity: node 0" 3 (graph-eccentricity dist 0))
      (test "path-eccentricity: node 1" 2 (graph-eccentricity dist 1))
      (test "path-diameter" 3 (graph-diameter dist))
      (test "path-radius" 2 (inexact->exact (graph-radius dist)))
      (test "path-center" '(1 2) (graph-center dist)))

;;; ====
;;; Dijkstra's Algorithm
;;; ====

(printf "~n--- Dijkstra's Algorithm ---~n")

;; Weighted graph:
;;   0 --3-- 1
;;   |       |
;;   5       2
;;   |       |
;;   3 --1-- 2
(let* ([adj (make-adjacency-matrix 4)])
      (adjacency-matrix-add-edge! adj 0 1 3)
      (adjacency-matrix-add-edge! adj 0 3 5)
      (adjacency-matrix-add-edge! adj 1 2 2)
      (adjacency-matrix-add-edge! adj 2 3 1)
      ;; Make undirected
      (adjacency-matrix-add-edge! adj 1 0 3)
      (adjacency-matrix-add-edge! adj 3 0 5)
      (adjacency-matrix-add-edge! adj 2 1 2)
      (adjacency-matrix-add-edge! adj 3 2 1)

      ;; Test distances from node 0
      (let* ([result (dijkstra adj 0)]
             [dist (car result)])
            (test "dijkstra: 0->0" 0 (vector-ref dist 0))
            (test "dijkstra: 0->1" 3 (vector-ref dist 1))
            (test "dijkstra: 0->2" 5 (vector-ref dist 2))  ; 0->1->2
            (test "dijkstra: 0->3" 5 (vector-ref dist 3))) ; min(5, 3+2+1)=5

      ;; Test path reconstruction
      (test "dijkstra-path: 0->2" '(0 1 2) (dijkstra-path adj 0 2))
      (test "dijkstra-distance: 0->3" 5 (dijkstra-distance adj 0 3)))

;; Disconnected graph
(let* ([adj (make-adjacency-matrix 3)])
      (adjacency-matrix-add-edge! adj 0 1 1)
      (let ([dist (car (dijkstra adj 0))])
           (test "dijkstra: unreachable" *infinity* (vector-ref dist 2)))
      (test "dijkstra-path: unreachable" #f (dijkstra-path adj 0 2)))

;;; ====
;;; Edge Cases
;;; ====

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

;;; ====
;;; Summary
;;; ====

(printf "~n=== Summary ===~n")
(printf "  Passed: ~a~n" tests-passed)
(printf "  Failed: ~a~n" tests-failed)
(printf "  Total:  ~a~n" (+ tests-passed tests-failed))
(if (= tests-failed 0)
    (printf "~n  All tests passed!~n~n")
    (printf "~n  Some tests failed.~n~n"))
