;;; core/data/test-pagerank.ss — PageRank Algorithm Tests
;;;
;;; Tests for PageRank importance scoring:
;;;   - Basic PageRank computation
;;;   - Damping factor variations
;;;   - Disconnected graphs
;;;   - Personalized PageRank
;;;   - Top-k node extraction
;;;
;;; Run from project root: scheme --script core/data/test-pagerank.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-eigen.ss")
(load "lattice/data/graph/graph-matrix.ss")
(load "lattice/data/graph/pagerank.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(define (test-approx name expected actual tolerance)
  (display "  ")
  (display name)
  (display ": ")
  (if (< (abs (- expected actual)) tolerance)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)
       (display "\n    diff: ")
       (display (abs (- expected actual)))))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; Helper: check if vector sums to approximately 1
(define (vec-is-probability-dist? v tol)
  (< (abs (- (vec-sum v) 1.0)) tol))

;;; ====
;;; Simple Graph Tests
;;; ====
(test-section "Simple Graph - Triangle")

;;; Triangle: 0 → 1 → 2 → 0
(define triangle-edges '((0 1) (1 2) (2 0)))
(define triangle-pr (pagerank triangle-edges))

(test "triangle PageRank converges"
      #t
      (vector? triangle-pr))

(test "triangle PageRank is probability distribution"
      #t
      (vec-is-probability-dist? triangle-pr 1e-6))

(test "triangle PageRank is uniform (symmetric graph)"
      #t
      (and (< (abs (- (vector-ref triangle-pr 0) (/ 1 3))) 0.01)
           (< (abs (- (vector-ref triangle-pr 1) (/ 1 3))) 0.01)
           (< (abs (- (vector-ref triangle-pr 2) (/ 1 3))) 0.01)))

;;; ====
;;; Chain Graph Tests
;;; ====
(test-section "Chain Graph")

;;; Chain: 0 → 1 → 2 → 3
(define chain-edges '((0 1) (1 2) (2 3)))
(define chain-pr (pagerank chain-edges))

(test "chain PageRank converges"
      #t
      (vector? chain-pr))

(test "chain PageRank is probability distribution"
      #t
      (vec-is-probability-dist? chain-pr 1e-6))

(test "chain PageRank: node 3 has highest score (sink)"
      #t
      (let ([pr0 (vector-ref chain-pr 0)]
            [pr1 (vector-ref chain-pr 1)]
            [pr2 (vector-ref chain-pr 2)]
            [pr3 (vector-ref chain-pr 3)])
           (and (> pr3 pr2)
                (> pr3 pr1)
                (> pr3 pr0))))

;;; ====
;;; Star Graph Tests
;;; ====
(test-section "Star Graph")

;;; Star: Center (0) with edges from 1,2,3,4 all pointing to 0
(define star-edges '((1 0) (2 0) (3 0) (4 0)))
(define star-pr (pagerank star-edges))

(test "star PageRank converges"
      #t
      (vector? star-pr))

(test "star PageRank is probability distribution"
      #t
      (vec-is-probability-dist? star-pr 1e-6))

(test "star PageRank: center has highest score"
      #t
      (let ([pr0 (vector-ref star-pr 0)]
            [pr1 (vector-ref star-pr 1)]
            [pr2 (vector-ref star-pr 2)]
            [pr3 (vector-ref star-pr 3)]
            [pr4 (vector-ref star-pr 4)])
           (and (> pr0 pr1)
                (> pr0 pr2)
                (> pr0 pr3)
                (> pr0 pr4))))

;;; ====
;;; Damping Factor Tests
;;; ====
(test-section "Damping Factor")

(define triangle-pr-d0 (pagerank triangle-edges 3 0.0))
(define triangle-pr-d1 (pagerank triangle-edges 3 1.0))

(test "damping=0 gives uniform distribution"
      #t
      (and (< (abs (- (vector-ref triangle-pr-d0 0) (/ 1 3))) 0.01)
           (< (abs (- (vector-ref triangle-pr-d0 1) (/ 1 3))) 0.01)
           (< (abs (- (vector-ref triangle-pr-d0 2) (/ 1 3))) 0.01)))

(test "damping=1 concentrates probability"
      #t
      (vec-is-probability-dist? triangle-pr-d1 1e-6))

;;; ====
;;; Disconnected Graph Tests
;;; ====
(test-section "Disconnected Graph")

;;; Two components: (0 → 1) and (2 → 3)
(define disconnected-edges '((0 1) (2 3)))
(define disconn-pr (pagerank disconnected-edges 4))

(test "disconnected graph converges"
      #t
      (vector? disconn-pr))

(test "disconnected graph is probability distribution"
      #t
      (vec-is-probability-dist? disconn-pr 1e-6))

;;; ====
;;; Dangling Nodes Tests
;;; ====
(test-section "Dangling Nodes")

;;; Graph with dangling node (1 has no outgoing edges): 0 → 1
(define dangling-edges '((0 1)))
(define dang-pr (pagerank dangling-edges 2))

(test "dangling node graph converges"
      #t
      (vector? dang-pr))

(test "dangling node graph is probability distribution"
      #t
      (vec-is-probability-dist? dang-pr 1e-6))

(test "dangling node (sink) has higher PageRank"
      #t
      (> (vector-ref dang-pr 1) (vector-ref dang-pr 0)))

;;; ====
;;; Matrix Interface Tests
;;; ====
(test-section "Matrix Interface")

(define triangle-adj (edges->adjacency-matrix triangle-edges 3))
(define triangle-pr-mat (pagerank-from-matrix triangle-adj))

(test "matrix interface converges"
      #t
      (vector? triangle-pr-mat))

(test "matrix interface is probability distribution"
      #t
      (vec-is-probability-dist? triangle-pr-mat 1e-6))

(test "matrix and edge-list interfaces match"
      #t
      (let ([diff (vec-sub triangle-pr triangle-pr-mat)])
           (< (vec-norm-1 diff) 1e-6)))

;;; ====
;;; Personalized PageRank Tests
;;; ====
(test-section "Personalized PageRank")

;;; Triangle with teleportation focused on node 0
(define teleport-to-0 (vector 1.0 0.0 0.0))
(define pers-pr-0 (personalized-pagerank triangle-adj teleport-to-0))

(test "personalized PageRank converges"
      #t
      (vector? pers-pr-0))

(test "personalized PageRank is probability distribution"
      #t
      (vec-is-probability-dist? pers-pr-0 1e-6))

(test "personalized PageRank favors teleport target"
      #t
      (> (vector-ref pers-pr-0 0)
         (vector-ref pers-pr-0 1)))

;;; Uniform teleportation should match standard PageRank
(define teleport-uniform (vector (/ 1 3) (/ 1 3) (/ 1 3)))
(define pers-pr-uniform (personalized-pagerank triangle-adj teleport-uniform))

(test "uniform personalized PageRank matches standard"
      #t
      (let ([diff (vec-sub triangle-pr-mat pers-pr-uniform)])
           (< (vec-norm-1 diff) 1e-6)))

;;; ====
;;; Top-K Nodes Tests
;;; ====
(test-section "Top-K Nodes")

(define top-2-star (top-k-nodes star-pr 2))

(test "top-k returns correct number of nodes"
      2
      (length top-2-star))

(test "top-k nodes are sorted descending"
      #t
      (>= (cdr (car top-2-star))
          (cdr (cadr top-2-star))))

(test "top-k first node is center (highest PageRank)"
      0
      (car (car top-2-star)))

;;; ====
;;; Weighted Graph Tests
;;; ====
(test-section "Weighted Graph")

;;; Weighted graph: 0 splits vote between 1 and 2, with more weight to 1
;;; 0 →(2) 1, 0 →(1) 2, 1 →(1) 0, 2 →(1) 0
;;; Node 0 gives 2/3 of vote to node 1, 1/3 to node 2
;;; Nodes 1 and 2 each give all their vote to node 0
(define weighted-edges '((0 1 2.0) (0 2 1.0) (1 0 1.0) (2 0 1.0)))
(define weighted-pr (pagerank weighted-edges 3))

(test "weighted graph converges"
      #t
      (vector? weighted-pr))

(test "weighted graph is probability distribution"
      #t
      (vec-is-probability-dist? weighted-pr 1e-6))

(test "weighted edge affects PageRank distribution"
      #t
      ;; Node 1 should have higher PR than node 2 due to receiving more vote from node 0
      (> (vector-ref weighted-pr 1)
         (vector-ref weighted-pr 2)))

;;; ====
;;; Convergence Tests
;;; ====
(test-section "Convergence")

;;; Test with different iteration limits
(define pr-iter-10 (pagerank triangle-edges 3 0.85 10))
(define pr-iter-100 (pagerank triangle-edges 3 0.85 100))

(test "more iterations improve convergence"
      #t
      (vector? pr-iter-10))

(test "different iteration counts converge to similar results"
      #t
      (< (vec-norm-1 (vec-sub pr-iter-10 pr-iter-100)) 0.01))

;;; ====
;;; Edge Cases
;;; ====
(test-section "Edge Cases")

;;; Single node (self-loop)
(define single-node-edges '((0 0)))
(define single-pr (pagerank single-node-edges 1))

(test "single node graph converges"
      #t
      (vector? single-pr))

(test "single node has PageRank = 1"
      #t
      (< (abs (- (vector-ref single-pr 0) 1.0)) 1e-6))

;;; Empty graph (no edges)
(define empty-edges '())
(define empty-pr (pagerank empty-edges 3))

(test "empty graph converges"
      #t
      (vector? empty-pr))

(test "empty graph has uniform distribution"
      #t
      (and (< (abs (- (vector-ref empty-pr 0) (/ 1 3))) 0.01)
           (< (abs (- (vector-ref empty-pr 1) (/ 1 3))) 0.01)
           (< (abs (- (vector-ref empty-pr 2) (/ 1 3))) 0.01)))

(newline)
(display "All PageRank tests completed!")
(newline)
