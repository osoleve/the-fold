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

;; Test all-communities-betti
(define all-betti (all-communities-betti two-cliques perfect-labels))
(test "two communities detected" 2 (length all-betti))

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

;; Quality should be low for fragmented community
(define frag-quality (community-homology-quality 2 2 0))
(test-approx "fragmented quality penalty" 0.35 frag-quality 0.01)
;; 0.7 * (1/2) + 0.3 * 0 = 0.35

(printf "\n")

;;; ====
;;; Test 3: Quality metric calculations
;;; ====

(printf "Test 3: Quality metrics\n")
(printf "-----------------------\n")

;; Perfect connected community with some cycles
;; Size 4, B_0=1 (connected), B_1=2 (two independent cycles)
(define q1 (community-homology-quality 4 1 2))
(test-approx "quality: connected with cycles" 0.9 q1 0.1)
;; 0.7 * 1.0 + 0.3 * (2/3) = 0.7 + 0.2 = 0.9

;; Tree-like community: connected but no cycles
;; Size 4, B_0=1, B_1=0
(define q2 (community-homology-quality 4 1 0))
(test-approx "quality: tree-like community" 0.7 q2 0.01)
;; 0.7 * 1.0 + 0.3 * 0 = 0.7

;; Heavily fragmented: 4 components, no cycles
(define q3 (community-homology-quality 4 4 0))
(test-approx "quality: heavily fragmented" 0.175 q3 0.01)
;; 0.7 * (1/4) + 0.3 * 0 = 0.175

(printf "\n")

;;; ====
;;; Test 4: Aggregate quality comparison
;;; ====

(printf "Test 4: Aggregate quality\n")
(printf "-------------------------\n")

(define perfect-quality (aggregate-community-quality two-cliques perfect-labels))
(define bad-quality (aggregate-community-quality two-cliques bad-labels))

(test-approx "perfect partition quality" 0.8 perfect-quality 0.15)
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

(printf "\n")

;;; ====
;;; Test 5: Community homology report (visual inspection)
;;; ====

(printf "Test 5: Homology report output\n")
(printf "------------------------------\n")
(community-homology-report two-cliques perfect-labels)

;;; ====
;;; Test 6: Karate club-style graph
;;; ====
;;; A more realistic test: 8 nodes in 2 communities with some inter-community edges

(printf "Test 6: Realistic graph with inter-community edges\n")
(printf "--------------------------------------------------\n")

;; Community A: 0,1,2,3 (dense clique + one chain)
;; Community B: 4,5,6,7 (less dense, tree-like)
;; Bridge: edge 3-4

(define realistic (make-matrix 8 8 0))

;; Community A: nearly complete on 0,1,2,3
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
