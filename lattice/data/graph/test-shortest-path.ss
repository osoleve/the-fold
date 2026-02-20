;;; test-shortest-path.ss — Tests for shortest path algorithms
;;;
;;; Run with: scheme --script lattice/data/graph/test-shortest-path.ss

(load "core/lang/module.ss")
(load "lattice/data/graph/shortest-path.ss")
(load "core/testing/test-framework.ss")

(printf "\n=== Shortest Path Tests ===\n\n")

;;; ====
;;; Test Graph Construction
;;; ====
;;; We build several reference graphs and verify all three algorithms against them.
;;;
;;; Graph 1: Simple directed weighted graph (4 nodes)
;;;
;;;   0 --3--> 1 --1--> 3
;;;   |        ^
;;;   2        4
;;;   |        |
;;;   v        |
;;;   2 --5--> 3 (note: also 1->3 with weight 1)
;;;
;;; Edges: (0,1,3) (0,2,2) (2,3,5) (3,1,4) (1,3,1)
;;; Shortest paths from 0:
;;;   0->0: 0
;;;   0->1: 3 (direct)
;;;   0->2: 2 (direct)
;;;   0->3: 4 (0->1->3)

(define *g1-edges* '((0 1 3) (0 2 2) (2 3 5) (3 1 4) (1 3 1)))
(define *g1* (edges->adjacency-matrix *g1-edges* 4))

;;; Graph 2: Disconnected graph (3 nodes, only 0->1)
(define *g2-edges* '((0 1 5)))
(define *g2* (edges->adjacency-matrix *g2-edges* 3))

;;; Graph 3: Single node
(define *g3* (make-adjacency-matrix 1))

;;; Graph 4: Graph with negative edges (no negative cycle)
;;;   0 --4--> 1
;;;   0 --5--> 2
;;;   1 --(-2)--> 2
;;;   2 --3--> 3
;;;
;;; Shortest from 0:
;;;   0->0: 0
;;;   0->1: 4
;;;   0->2: 2  (0->1->2 via -2 edge)
;;;   0->3: 5  (0->1->2->3)

(define *g4* (make-adjacency-matrix 4))
(adjacency-matrix-add-edge! *g4* 0 1 4)
(adjacency-matrix-add-edge! *g4* 0 2 5)
(adjacency-matrix-add-edge! *g4* 1 2 -2)
(adjacency-matrix-add-edge! *g4* 2 3 3)

;;; Graph 5: Negative cycle
;;;   0 --1--> 1
;;;   1 --(-3)--> 2
;;;   2 --1--> 0
;;;
;;; Cycle 0->1->2->0 has weight 1+(-3)+1 = -1

(define *g5* (make-adjacency-matrix 3))
(adjacency-matrix-add-edge! *g5* 0 1 1)
(adjacency-matrix-add-edge! *g5* 1 2 -3)
(adjacency-matrix-add-edge! *g5* 2 0 1)

;;; ====
;;; Floyd-Warshall Tests
;;; ====

(test-group "floyd-warshall"

  (define-test "simple graph all-pairs distances"
    (let ([d (floyd-warshall *g1*)])
      (assert-equal 0 (matrix-ref d 0 0))
      (assert-equal 3 (matrix-ref d 0 1))
      (assert-equal 2 (matrix-ref d 0 2))
      (assert-equal 4 (matrix-ref d 0 3))
      ;; Self-distance is always 0
      (assert-equal 0 (matrix-ref d 2 2))))

  (define-test "simple graph distance 0 to 3"
    ;; 0->1->3 costs 3+1=4, 0->2->3 costs 2+5=7, so min=4
    (let ([d (floyd-warshall *g1*)])
      (assert-equal 4 (matrix-ref d 0 3))))

  (define-test "diagonal is zero"
    (let ([d (floyd-warshall *g1*)])
      (assert-equal 0 (matrix-ref d 0 0))
      (assert-equal 0 (matrix-ref d 1 1))
      (assert-equal 0 (matrix-ref d 2 2))
      (assert-equal 0 (matrix-ref d 3 3))))

  (define-test "disconnected graph has infinity"
    (let ([d (floyd-warshall *g2*)])
      (assert-equal 5 (matrix-ref d 0 1))
      (assert-equal *infinity* (matrix-ref d 0 2))
      (assert-equal *infinity* (matrix-ref d 1 0))
      (assert-equal *infinity* (matrix-ref d 2 0))
      (assert-equal *infinity* (matrix-ref d 2 1))))

  (define-test "single node"
    (let ([d (floyd-warshall *g3*)])
      (assert-equal 0 (matrix-ref d 0 0))))

  ;; Note: graph-matrix Floyd-Warshall uses convention 0=no edge, >0=weight.
  ;; Negative edge weights are invisible to it. Use bellman-ford for those.
  (define-test "negative edges ignored by floyd-warshall (use bellman-ford)"
    (let ([d (floyd-warshall *g4*)])
      (assert-equal 0 (matrix-ref d 0 0))
      (assert-equal 4 (matrix-ref d 0 1))
      ;; FW sees only positive edges, so 0->2 = 5 (direct), not 2 (via negative)
      (assert-equal 5 (matrix-ref d 0 2))
      (assert-equal 8 (matrix-ref d 0 3)))))

;;; ====
;;; Dijkstra Tests
;;; ====

(test-group "dijkstra"

  (define-test "simple graph single-source"
    (let* ([result (dijkstra *g1* 0)]
           [dist (car result)]
           [pred (cdr result)])
      (assert-equal 0 (vector-ref dist 0))
      (assert-equal 3 (vector-ref dist 1))
      (assert-equal 2 (vector-ref dist 2))
      (assert-equal 4 (vector-ref dist 3))))

  (define-test "matches floyd-warshall row 0"
    (let* ([fw (floyd-warshall *g1*)]
           [dijk (dijkstra *g1* 0)]
           [dist (car dijk)]
           [n (matrix-rows *g1*)])
      (do ([j 0 (+ j 1)])
          ((= j n))
        (assert-equal (matrix-ref fw 0 j) (vector-ref dist j)))))

  (define-test "matches floyd-warshall row 2"
    (let* ([fw (floyd-warshall *g1*)]
           [dijk (dijkstra *g1* 2)]
           [dist (car dijk)]
           [n (matrix-rows *g1*)])
      (do ([j 0 (+ j 1)])
          ((= j n))
        (assert-equal (matrix-ref fw 2 j) (vector-ref dist j)))))

  (define-test "disconnected graph"
    (let* ([result (dijkstra *g2* 0)]
           [dist (car result)])
      (assert-equal 0 (vector-ref dist 0))
      (assert-equal 5 (vector-ref dist 1))
      (assert-equal *infinity* (vector-ref dist 2))))

  (define-test "single node"
    (let* ([result (dijkstra *g3* 0)]
           [dist (car result)])
      (assert-equal 0 (vector-ref dist 0))))

  (define-test "path reconstruction"
    (let ([path (dijkstra-path *g1* 0 3)])
      ;; 0->1->3 (cost 4)
      (assert-equal '(0 1 3) path)))

  (define-test "unreachable path"
    (let ([path (dijkstra-path *g2* 0 2)])
      (assert-false path))))

;;; ====
;;; Bellman-Ford Tests
;;; ====

(test-group "bellman-ford"

  (define-test "simple graph single-source"
    (let ([result (bellman-ford *g1* 0)])
      (assert-equal 'ok (car result))
      (let ([dist (cadr result)])
        (assert-equal 0 (vector-ref dist 0))
        (assert-equal 3 (vector-ref dist 1))
        (assert-equal 2 (vector-ref dist 2))
        (assert-equal 4 (vector-ref dist 3)))))

  (define-test "matches dijkstra on non-negative graph"
    (let* ([bf (bellman-ford *g1* 0)]
           [dijk (dijkstra *g1* 0)]
           [bf-dist (cadr bf)]
           [dk-dist (car dijk)]
           [n (matrix-rows *g1*)])
      (assert-equal 'ok (car bf))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (assert-equal (vector-ref dk-dist j) (vector-ref bf-dist j)))))

  (define-test "negative edges without cycle"
    (let ([result (bellman-ford *g4* 0)])
      (assert-equal 'ok (car result))
      (let ([dist (cadr result)])
        (assert-equal 0 (vector-ref dist 0))
        (assert-equal 4 (vector-ref dist 1))
        (assert-equal 2 (vector-ref dist 2))   ; 0->1->2 via -2
        (assert-equal 5 (vector-ref dist 3))))) ; 0->1->2->3

  (define-test "negative edges predecessor"
    (let ([result (bellman-ford *g4* 0)])
      (assert-equal 'ok (car result))
      (let ([pred (cddr result)])
        ;; 0->1 direct
        (assert-equal 0 (vector-ref pred 1))
        ;; 0->1->2 (via negative edge)
        (assert-equal 1 (vector-ref pred 2))
        ;; 0->1->2->3
        (assert-equal 2 (vector-ref pred 3)))))

  (define-test "detects negative cycle"
    (let ([result (bellman-ford *g5* 0)])
      (assert-equal 'negative-cycle (car result))))

  (define-test "disconnected graph"
    (let ([result (bellman-ford *g2* 0)])
      (assert-equal 'ok (car result))
      (let ([dist (cadr result)])
        (assert-equal 0 (vector-ref dist 0))
        (assert-equal 5 (vector-ref dist 1))
        (assert-equal *infinity* (vector-ref dist 2)))))

  (define-test "single node"
    (let ([result (bellman-ford *g3* 0)])
      (assert-equal 'ok (car result))
      (let ([dist (cadr result)])
        (assert-equal 0 (vector-ref dist 0))))))

;;; ====
;;; Path Reconstruction Tests
;;; ====

(test-group "path-reconstruction"

  (define-test "bellman-ford path with negative edges"
    (let ([path (bellman-ford-path *g4* 0 3)])
      ;; 0->1->2->3
      (assert-equal '(0 1 2 3) path)))

  (define-test "bellman-ford path direct"
    (let ([path (bellman-ford-path *g4* 0 1)])
      (assert-equal '(0 1) path)))

  (define-test "bellman-ford unreachable"
    (let ([path (bellman-ford-path *g2* 0 2)])
      (assert-false path)))

  (define-test "bellman-ford negative cycle returns symbol"
    (let ([path (bellman-ford-path *g5* 0 1)])
      (assert-equal 'negative-cycle path)))

  (define-test "bellman-ford-distance"
    (assert-equal 5 (bellman-ford-distance *g4* 0 3)))

  (define-test "bellman-ford-distance unreachable"
    (assert-equal *infinity* (bellman-ford-distance *g2* 0 2)))

  (define-test "bellman-ford-distance negative cycle"
    (assert-equal 'negative-cycle (bellman-ford-distance *g5* 0 1)))

  (define-test "shortest-path-reconstruct source=target"
    (let ([pred (make-vector 3 -1)])
      (assert-equal '(0) (shortest-path-reconstruct pred 0 0)))))

;;; ====
;;; Cross-Algorithm Consistency
;;; ====

(test-group "cross-algorithm"

  (define-test "all three agree on simple graph"
    (let* ([fw (floyd-warshall *g1*)]
           [dijk (dijkstra *g1* 0)]
           [bf (bellman-ford *g1* 0)]
           [dk-dist (car dijk)]
           [bf-dist (cadr bf)]
           [n 4])
      (assert-equal 'ok (car bf))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (assert-equal (matrix-ref fw 0 j) (vector-ref dk-dist j))
        (assert-equal (matrix-ref fw 0 j) (vector-ref bf-dist j)))))

  (define-test "all three agree on disconnected graph"
    (let* ([fw (floyd-warshall *g2*)]
           [dijk (dijkstra *g2* 0)]
           [bf (bellman-ford *g2* 0)]
           [dk-dist (car dijk)]
           [bf-dist (cadr bf)]
           [n 3])
      (assert-equal 'ok (car bf))
      (do ([j 0 (+ j 1)])
          ((= j n))
        (assert-equal (matrix-ref fw 0 j) (vector-ref dk-dist j))
        (assert-equal (matrix-ref fw 0 j) (vector-ref bf-dist j))))))

(run-all-tests)
