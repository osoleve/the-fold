;;; test-community-homology.ss — Tests for community homology analysis
;;;
;;; Tests the homology-based community quality metrics from graph-community.ss

(source-directories (cons "core" (source-directories)))
(load "lattice/data/graph-community.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

(define (round-3 x)
  (/ (round (* x 1000)) 1000.0))

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a (~a approx ~a)\n" name (round-3 expected) (round-3 actual)))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~a (tolerance ~a)\n" (round-3 expected) tolerance)
       (printf "      Got:      ~a\n" (round-3 actual)))))

(printf "\n")
(printf "====\n")
(printf "         COMMUNITY HOMOLOGY TESTS\n")
(printf "====\n\n")

;;; ====
;;; Test 1: Two well-separated cliques
;;; ====
;;; Graph: K_3 (triangle) + K_3 (triangle), no edges between
;;;
;;;   0 - 1      3 - 4
;;;    \ /        \ /
;;;     2          5
;;;
;;; Communities: {0,1,2} and {3,4,5}
;;; Each community: B_0=1 (connected), B_1=1 (one cycle)

(printf "Test 1: Two cliques (K_3 + K_3)\n")
(printf "---------------------------------\n")

;; Build adjacency matrix (6x6)
(define two-cliques (make-matrix 6 6 0))
;; Clique 1: 0-1, 1-2, 2-0
(matrix-set! two-cliques 0 1 1) (matrix-set! two-cliques 1 0 1)
(matrix-set! two-cliques 1 2 1) (matrix-set! two-cliques 2 1 1)
(matrix-set! two-cliques 2 0 1) (matrix-set! two-cliques 0 2 1)
;; Clique 2: 3-4, 4-5, 5-3
(matrix-set! two-cliques 3 4 1) (matrix-set! two-cliques 4 3 1)
(matrix-set! two-cliques 4 5 1) (matrix-set! two-cliques 5 4 1)
(matrix-set! two-cliques 5 3 1) (matrix-set! two-cliques 3 5 1)

;; Perfect labels: communities match cliques
(define perfect-labels (vector 0 0 0 1 1 1))

;; Test community-induced-edges
(define c1-edges (community-induced-edges two-cliques '(0 1 2)))
(test "clique 1 has 3 edges" 3 (length c1-edges))

;; Test community-betti-numbers for clique
(define c1-betti (community-betti-numbers two-cliques '(0 1 2)))
(test "clique 1 B_0 (connected)" 1 (car c1-betti))
(test "clique 1 B_1 (one cycle)" 1 (cdr c1-betti))

;; Test all-communities-betti preserves original labels
(define all-betti (all-communities-betti two-cliques perfect-labels))
(test "two communities detected" 2 (length all-betti))
(test "first community has original label 0" 0 (caar all-betti))
(test "second community has original label 1" 1 (caadr all-betti))

(printf "\n")

;;; ====
;;; Test 2: Fragmented community (bad partition)
;;; ====
;;; Same graph, but label nodes 0 and 3 in same "community"
;;; This creates a disconnected community: {0,3} with no edge between them

(printf "Test 2: Fragmented community detection\n")
(printf "--------------------------------------\n")

;; Bad labels: community 0 = {0,3}, community 1 = {1,2,4,5}
(define bad-labels (vector 0 1 1 0 1 1))

(define fragmented-betti (community-betti-numbers two-cliques '(0 3)))
(test "fragmented B_0 (disconnected)" 2 (car fragmented-betti))
(test "fragmented B_1 (no cycles)" 0 (cdr fragmented-betti))

;; Quality with new formula: sqrt(1/2) * 0.7 = 0.495
(define frag-quality (community-homology-quality 2 2 0))
(test-approx "fragmented quality penalty" 0.495 frag-quality 0.01)

(printf "\n")

;;; ====
;;; Test 3: Quality metric calculations
;;; ====

(printf "Test 3: Quality metrics\n")
(printf "-----------------------\n")

;; Perfect connected community with some cycles
;; Size 4, B_0=1 (connected), B_1=2 (two independent cycles)
;; max_b1 = (4-1)(4-2)/2 = 3, density = 2/3
;; quality = 0.7 * 1.0 + 0.3 * (2/3) = 0.7 + 0.2 = 0.9
(define q1 (community-homology-quality 4 1 2))
(test-approx "quality: connected with cycles" 0.9 q1 0.01)

;; Tree-like community: connected but no cycles
;; Size 4, B_0=1, B_1=0
;; quality = 0.7 * 1.0 + 0.3 * 0 = 0.7
(define q2 (community-homology-quality 4 1 0))
(test-approx "quality: tree-like community" 0.7 q2 0.01)

;; Heavily fragmented: 4 components, no cycles
;; connectivity = sqrt(1/4) = 0.5
;; quality = 0.7 * 0.5 + 0.3 * 0 = 0.35
(define q3 (community-homology-quality 4 4 0))
(test-approx "quality: heavily fragmented" 0.35 q3 0.01)

;; Single-node community should get modest score (0.5)
(define q-single (community-homology-quality 1 1 0))
(test-approx "quality: single-node community" 0.5 q-single 0.01)

(printf "\n")

;;; ====
;;; Test 4: Clique normalization (K_3 vs K_4 should score equally)
;;; ====
;;; This tests that cycle density is properly normalized to theoretical max

(printf "Test 4: Clique normalization\n")
(printf "----------------------------\n")

;; K_3: size=3, B_1=1, max_b1=(2*1)/2=1, density=1.0, quality=1.0
(define q-k3 (community-homology-quality 3 1 1))
(test-approx "K_3 quality (complete triangle)" 1.0 q-k3 0.01)

;; K_4: size=4, B_1=3, max_b1=(3*2)/2=3, density=1.0, quality=1.0
(define q-k4 (community-homology-quality 4 1 3))
(test-approx "K_4 quality (complete 4-clique)" 1.0 q-k4 0.01)

;; Both should be equal (size-invariant for cliques)
(test-approx "K_3 and K_4 equal quality" q-k3 q-k4 0.01)

(printf "\n")

;;; ====
;;; Test 5: Sparse labels (tests hashtable fix)
;;; ====

(printf "Test 5: Sparse labels\n")
(printf "---------------------\n")

;; Labels with large gaps: 0, 1000, 5000
;; This would crash with old make-vector approach
(define sparse-labels (vector 0 0 1000 1000 5000 5000))
(define sparse-betti (all-communities-betti two-cliques sparse-labels))

(test "sparse: three communities" 3 (length sparse-betti))
(test "sparse: first label is 0" 0 (caar sparse-betti))
(test "sparse: second label is 1000" 1000 (caadr sparse-betti))
(test "sparse: third label is 5000" 5000 (caaddr sparse-betti))

(printf "\n")

;;; ====
;;; Test 6: Aggregate quality comparison
;;; ====

(printf "Test 6: Aggregate quality\n")
(printf "-------------------------\n")

(define perfect-quality (aggregate-community-quality two-cliques perfect-labels))
(define bad-quality (aggregate-community-quality two-cliques bad-labels))

(printf "  Perfect partition quality: ~a\n" (round-3 perfect-quality))
(printf "  Bad partition quality:     ~a\n" (round-3 bad-quality))

;; Perfect partition should have higher quality
(if (> perfect-quality bad-quality)
    (begin
     (set! tests-passed (+ tests-passed 1))
     (printf "  [PASS] Perfect > Bad partition quality\n"))
    (begin
     (set! tests-failed (+ tests-failed 1))
     (printf "  [FAIL] Expected perfect > bad quality\n")))

;; Perfect partition of two K_3s should be 1.0
(test-approx "perfect partition is optimal" 1.0 perfect-quality 0.01)

(printf "\n")

;;; ====
;;; Test 7: Community homology report (visual inspection)
;;; ====

(printf "Test 7: Homology report output\n")
(printf "------------------------------\n")
(community-homology-report two-cliques perfect-labels)

;;; ====
;;; Test 8: Karate club-style graph
;;; ====
;;; A more realistic test: 8 nodes in 2 communities with some inter-community edges

(printf "Test 8: Realistic graph with inter-community edges\n")
(printf "--------------------------------------------------\n")

;; Community A: 0,1,2,3 (dense clique K_4)
;; Community B: 4,5,6,7 (star/tree)
;; Bridge: edge 3-4

(define realistic (make-matrix 8 8 0))

;; Community A: complete K_4 on 0,1,2,3
(matrix-set! realistic 0 1 1) (matrix-set! realistic 1 0 1)
(matrix-set! realistic 0 2 1) (matrix-set! realistic 2 0 1)
(matrix-set! realistic 0 3 1) (matrix-set! realistic 3 0 1)
(matrix-set! realistic 1 2 1) (matrix-set! realistic 2 1 1)
(matrix-set! realistic 1 3 1) (matrix-set! realistic 3 1 1)
(matrix-set! realistic 2 3 1) (matrix-set! realistic 3 2 1)

;; Community B: star/tree on 4,5,6,7
(matrix-set! realistic 4 5 1) (matrix-set! realistic 5 4 1)
(matrix-set! realistic 4 6 1) (matrix-set! realistic 6 4 1)
(matrix-set! realistic 4 7 1) (matrix-set! realistic 7 4 1)

;; Bridge edge
(matrix-set! realistic 3 4 1) (matrix-set! realistic 4 3 1)

;; True community labels
(define realistic-labels (vector 0 0 0 0 1 1 1 1))

;; Run label propagation and compare
(define detected-labels (label-propagation realistic 100 42))
(printf "  True labels:     ~a\n" (vector->list realistic-labels))
(printf "  Detected labels: ~a\n" (vector->list detected-labels))

;; Compute Betti numbers for true communities
(define comm-a-betti (community-betti-numbers realistic '(0 1 2 3)))
(define comm-b-betti (community-betti-numbers realistic '(4 5 6 7)))

(test "community A B_0 (connected)" 1 (car comm-a-betti))
(test "community A B_1 (K4 has 3 cycles)" 3 (cdr comm-a-betti))
(test "community B B_0 (connected)" 1 (car comm-b-betti))
(test "community B B_1 (tree, no cycles)" 0 (cdr comm-b-betti))

(printf "\n")
(community-homology-report realistic realistic-labels)

;;; ====
;;; Summary
;;; ====

(printf "\n====\n")
(printf "                    TEST RESULTS\n")
(printf "====\n\n")

(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n[SUCCESS] All community homology tests passed.\n\n")
    (printf "\n[FAILURE] Some tests failed.\n\n"))
