;;; lattice/algebra/test-tropical-graph.ss — Tests for Tropical/Graph Bridge
;;; Run with: scheme --script lattice/algebra/test-tropical-graph.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/algebra/tropical-graph.ss")

;;; Helper: numeric equality (exact/inexact agnostic)
(define-syntax assert-num=
  (syntax-rules ()
    [(_ expected actual)
     (let ([e expected] [a actual])
       (assert-true (= e a)))]))

;;; ============================================================
;;; Section 1: Additional Semirings
;;; ============================================================

(test-group "graph-semirings"
  (define-test "boolean semiring is a valid semiring"
    (assert-true (semiring? boolean-semiring)))

  (define-test "boolean semiring: identities"
    ;; Additive identity: max(x, 0) = x
    (assert-equal 1 (tropical-add boolean-semiring 1 0))
    (assert-equal 0 (tropical-add boolean-semiring 0 0))
    ;; Multiplicative identity: x * 1 = x
    (assert-equal 1 (tropical-mul boolean-semiring 1 1))
    (assert-equal 0 (tropical-mul boolean-semiring 0 1)))

  (define-test "boolean semiring: annihilation"
    ;; 0 * x = 0
    (assert-equal 0 (tropical-mul boolean-semiring 0 1))
    (assert-equal 0 (tropical-mul boolean-semiring 0 0)))

  (define-test "min-max semiring is a valid semiring"
    (assert-true (semiring? min-max-semiring)))

  (define-test "min-max semiring: identities"
    ;; Additive identity: max(x, -inf) = x
    (assert-num= 5 (tropical-add min-max-semiring 5 -inf.0))
    ;; Multiplicative identity: min(x, +inf) = x
    (assert-num= 5 (tropical-mul min-max-semiring 5 +inf.0))))

;;; ============================================================
;;; Section 2: Shortest Paths via Tropical Closure
;;; ============================================================

(test-group "tropical-shortest-paths"
  (define-test "4-node DAG: known shortest paths"
    ;; 0->1: 3, 0->2: 8, 1->2: 2, 1->3: 5, 2->3: 1
    ;; Shortest: 0->0: 0, 0->1: 3, 0->2: 5 (via 1), 0->3: 6 (via 1->2->3)
    (let* ([adj (edges->adjacency-matrix
                 '((0 1 3) (0 2 8) (1 2 2) (1 3 5) (2 3 1)) 4)]
           [dist (tropical-shortest-paths adj)])
      (assert-num= 0 (tropical-distance dist 0 0))
      (assert-num= 3 (tropical-distance dist 0 1))
      (assert-num= 5 (tropical-distance dist 0 2))
      (assert-num= 6 (tropical-distance dist 0 3))
      ;; Unreachable: 3 can't reach anyone
      (assert-true (not (finite? (tropical-distance dist 3 0))))
      (assert-true (not (finite? (tropical-distance dist 3 1))))))

  (define-test "undirected cycle graph"
    ;; 4-node cycle: 0-1-2-3-0, all weights 1
    ;; Shortest path 0->2 should be 2 (either direction around the cycle)
    (let* ([adj (edges->adjacency-matrix
                 '((0 1 1) (1 2 1) (2 3 1) (3 0 1)
                   (1 0 1) (2 1 1) (3 2 1) (0 3 1)) 4)]
           [dist (tropical-shortest-paths adj)])
      (assert-num= 0 (tropical-distance dist 0 0))
      (assert-num= 1 (tropical-distance dist 0 1))
      (assert-num= 2 (tropical-distance dist 0 2))
      (assert-num= 1 (tropical-distance dist 0 3))))

  (define-test "disconnected graph: unreachable pairs"
    ;; Two components: {0,1} and {2,3}
    (let* ([adj (edges->adjacency-matrix
                 '((0 1 5) (1 0 5) (2 3 7) (3 2 7)) 4)]
           [dist (tropical-shortest-paths adj)])
      ;; Within component
      (assert-num= 5 (tropical-distance dist 0 1))
      (assert-num= 7 (tropical-distance dist 2 3))
      ;; Cross-component: unreachable
      (assert-true (not (finite? (tropical-distance dist 0 2))))
      (assert-true (not (finite? (tropical-distance dist 1 3))))))

  (define-test "single node graph"
    (let* ([adj (edges->adjacency-matrix '() 1)]
           [dist (tropical-shortest-paths adj)])
      (assert-num= 0 (tropical-distance dist 0 0)))))

;;; ============================================================
;;; Section 3: Transitive Closure via Boolean Semiring
;;; ============================================================

(test-group "tropical-transitive-closure"
  (define-test "DAG reachability"
    ;; 0->1->2->3 (linear chain)
    (let* ([adj (edges->adjacency-matrix
                 '((0 1) (1 2) (2 3)) 4)]
           [tc (tropical-transitive-closure adj)])
      ;; Forward reachability
      (assert-true (tropical-reachable? tc 0 1))
      (assert-true (tropical-reachable? tc 0 2))
      (assert-true (tropical-reachable? tc 0 3))
      (assert-true (tropical-reachable? tc 1 3))
      ;; Self-reachability
      (assert-true (tropical-reachable? tc 0 0))
      ;; Backward: NOT reachable
      (assert-false (tropical-reachable? tc 3 0))
      (assert-false (tropical-reachable? tc 2 0))))

  (define-test "cycle makes all nodes mutually reachable"
    ;; 0->1->2->0 (3-cycle)
    (let* ([adj (edges->adjacency-matrix
                 '((0 1) (1 2) (2 0)) 3)]
           [tc (tropical-transitive-closure adj)])
      (assert-true (tropical-reachable? tc 0 1))
      (assert-true (tropical-reachable? tc 0 2))
      (assert-true (tropical-reachable? tc 1 0))
      (assert-true (tropical-reachable? tc 2 0))
      (assert-true (tropical-reachable? tc 1 2))
      (assert-true (tropical-reachable? tc 2 1))))

  (define-test "disconnected components"
    ;; {0->1} and {2->3}, no cross edges
    (let* ([adj (edges->adjacency-matrix
                 '((0 1) (2 3)) 4)]
           [tc (tropical-transitive-closure adj)])
      (assert-true (tropical-reachable? tc 0 1))
      (assert-true (tropical-reachable? tc 2 3))
      (assert-false (tropical-reachable? tc 0 2))
      (assert-false (tropical-reachable? tc 0 3))
      (assert-false (tropical-reachable? tc 1 2))
      (assert-false (tropical-reachable? tc 3 0)))))

;;; ============================================================
;;; Section 4: Longest Paths via Max-Plus
;;; ============================================================

(test-group "tropical-longest-paths"
  (define-test "DAG critical path"
    ;; Task DAG: 0->1: 3, 0->2: 1, 1->3: 2, 2->3: 5
    ;; Longest path 0->3: max(3+2, 1+5) = max(5, 6) = 6
    (let* ([adj (edges->adjacency-matrix
                 '((0 1 3) (0 2 1) (1 3 2) (2 3 5)) 4)]
           [dist (tropical-longest-paths adj)])
      (assert-num= 0 (matrix-ref dist 0 0))
      (assert-num= 3 (matrix-ref dist 0 1))
      (assert-num= 1 (matrix-ref dist 0 2))
      (assert-num= 6 (matrix-ref dist 0 3))))

  (define-test "linear chain: longest = only path"
    ;; 0->1: 2, 1->2: 3
    ;; Longest 0->2 = 2+3 = 5 (same as shortest for a chain)
    (let* ([adj (edges->adjacency-matrix
                 '((0 1 2) (1 2 3)) 3)]
           [dist (tropical-longest-paths adj)])
      (assert-num= 5 (matrix-ref dist 0 2)))))

;;; ============================================================
;;; Section 5: Bottleneck Paths via Min-Max
;;; ============================================================

(test-group "tropical-bottleneck-paths"
  (define-test "widest path selection"
    ;; Two paths from 0 to 2:
    ;;   0->1: 10, 1->2: 3  => bottleneck = min(10, 3) = 3
    ;;   0->2: 5             => bottleneck = 5
    ;; Widest = max(3, 5) = 5
    (let* ([adj (edges->adjacency-matrix
                 '((0 1 10) (1 2 3) (0 2 5)) 3)]
           [dist (tropical-bottleneck-paths adj)])
      (assert-num= 5 (matrix-ref dist 0 2))))

  (define-test "chain: bottleneck is minimum edge"
    ;; 0->1: 8, 1->2: 2, 2->3: 6
    ;; Only path 0->3: bottleneck = min(8, 2, 6) = 2
    (let* ([adj (edges->adjacency-matrix
                 '((0 1 8) (1 2 2) (2 3 6)) 4)]
           [dist (tropical-bottleneck-paths adj)])
      (assert-num= 2 (matrix-ref dist 0 3)))))

;;; ============================================================
;;; Section 6: Verification (Agreement with Classical Algorithms)
;;; ============================================================

(test-group "agreement-verification"
  (define-test "tropical agrees with floyd-warshall on 4-node graph"
    (let ([adj (edges->adjacency-matrix
                '((0 1 3) (0 2 8) (1 2 2) (1 3 5) (2 3 1)) 4)])
      (assert-true (tropical-agrees-with-floyd-warshall? adj))))

  (define-test "tropical agrees with floyd-warshall on cycle"
    (let ([adj (edges->adjacency-matrix
                '((0 1 2) (1 2 3) (2 3 1) (3 0 4)
                  (1 0 2) (2 1 3) (3 2 1) (0 3 4)) 4)])
      (assert-true (tropical-agrees-with-floyd-warshall? adj))))

  (define-test "tropical agrees with floyd-warshall on disconnected graph"
    (let ([adj (edges->adjacency-matrix
                '((0 1 5) (2 3 7)) 4)])
      (assert-true (tropical-agrees-with-floyd-warshall? adj))))

  (define-test "tropical agrees with transitive-closure on DAG"
    (let ([adj (edges->adjacency-matrix
                '((0 1) (1 2) (2 3) (0 3)) 4)])
      (assert-true (tropical-agrees-with-transitive-closure? adj))))

  (define-test "tropical agrees with transitive-closure on cycle"
    (let ([adj (edges->adjacency-matrix
                '((0 1) (1 2) (2 0)) 3)])
      (assert-true (tropical-agrees-with-transitive-closure? adj))))

  (define-test "tropical agrees with transitive-closure on disconnected"
    (let ([adj (edges->adjacency-matrix
                '((0 1) (2 3)) 4)])
      (assert-true (tropical-agrees-with-transitive-closure? adj)))))

;;; ============================================================
;;; Section 7: Round-Trip and Edge Cases
;;; ============================================================

(test-group "round-trip"
  (define-test "graph -> tropical shortest paths -> graph preserves finite distances"
    ;; Build graph, compute shortest paths, convert back
    (let* ([adj (edges->adjacency-matrix
                 '((0 1 3) (1 2 2) (2 3 1)) 4)]
           [dist (tropical-shortest-paths adj)]
           [back (tropical-distance-to-graph dist)])
      ;; 0->1 shortest = 3 (tropical ops produce flonums via IEEE inf contagion)
      (assert-num= 3 (matrix-ref back 0 1))
      ;; 0->2 shortest = 5 (via 1)
      (assert-num= 5 (matrix-ref back 0 2))
      ;; 0->3 shortest = 6 (via 1->2)
      (assert-num= 6 (matrix-ref back 0 3))
      ;; Unreachable -> 0 in graph format
      (assert-num= 0 (matrix-ref back 3 0))))

  (define-test "empty graph: all self-distances zero"
    (let* ([adj (edges->adjacency-matrix '() 3)]
           [dist (tropical-shortest-paths adj)])
      (assert-num= 0 (tropical-distance dist 0 0))
      (assert-num= 0 (tropical-distance dist 1 1))
      (assert-num= 0 (tropical-distance dist 2 2))
      ;; All off-diagonal unreachable
      (assert-true (not (finite? (tropical-distance dist 0 1))))))

  (define-test "complete graph: shortest path is direct edge"
    ;; K3 with weights: 0->1: 1, 0->2: 3, 1->2: 1
    ;; Shortest 0->2 = min(3, 1+1) = 2
    (let* ([adj (edges->adjacency-matrix
                 '((0 1 1) (0 2 3) (1 0 1) (1 2 1)
                   (2 0 3) (2 1 1)) 3)]
           [dist (tropical-shortest-paths adj)])
      (assert-num= 1 (tropical-distance dist 0 1))
      (assert-num= 2 (tropical-distance dist 0 2)))))

(run-all-tests-and-exit)
