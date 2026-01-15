;;; lattice/fp/game/test-matching.ss — Tests for Matching Theory
;;;
;;; Tests stable matching (Gale-Shapley), assignment games, and
;;; optimal assignment algorithms.

(load "core/testing/test-framework.ss")
(load "lattice/fp/game/matching.ss")

;;; ============================================================================
;;; Helper Macros
;;; ============================================================================

;;; approximately-equal? : Real × Real × Real → Boolean
(define (approximately-equal? a b tolerance)
  (< (abs (- a b)) tolerance))

;;; ============================================================================
;;; Market Construction Tests
;;; ============================================================================

(test-group "Matching Market Construction"

  (define-test "create simple 3x3 market"
    (let* ([market (make-matching-market
                    '(m1 m2 m3)
                    '(w1 w2 w3)
                    '((m1 w1 w2 w3) (m2 w2 w1 w3) (m3 w1 w3 w2))
                    '((w1 m2 m1 m3) (w2 m1 m2 m3) (w3 m1 m3 m2)))])
      (assert-true (matching-market? market))
      (assert-equal 3 (market-num-proposers market))
      (assert-equal 3 (market-num-receivers market))))

  (define-test "retrieve proposer preferences"
    (let* ([market (make-matching-market
                    '(m1 m2)
                    '(w1 w2)
                    '((m1 w2 w1) (m2 w1 w2))
                    '((w1 m1 m2) (w2 m2 m1)))])
      (assert-equal '(w2 w1) (market-proposer-prefs market 'm1))
      (assert-equal '(w1 w2) (market-proposer-prefs market 'm2))))

  (define-test "retrieve receiver preferences"
    (let* ([market (make-matching-market
                    '(m1 m2)
                    '(w1 w2)
                    '((m1 w2 w1) (m2 w1 w2))
                    '((w1 m1 m2) (w2 m2 m1)))])
      (assert-equal '(m1 m2) (market-receiver-prefs market 'w1))
      (assert-equal '(m2 m1) (market-receiver-prefs market 'w2))))

  (define-test "unequal sides market (more proposers)"
    (let* ([market (make-matching-market
                    '(m1 m2 m3)
                    '(w1 w2)
                    '((m1 w1 w2) (m2 w2 w1) (m3 w1 w2))
                    '((w1 m1 m2 m3) (w2 m3 m2 m1)))])
      (assert-equal 3 (market-num-proposers market))
      (assert-equal 2 (market-num-receivers market))))

) ; end market construction tests

;;; ============================================================================
;;; Gale-Shapley Algorithm Tests
;;; ============================================================================

(test-group "Gale-Shapley Stable Matching"

  (define-test "classic 3x3 stable matching"
    ;; Classic example from Gale-Shapley paper
    (let* ([market (make-matching-market
                    '(m1 m2 m3)
                    '(w1 w2 w3)
                    '((m1 w1 w2 w3) (m2 w2 w1 w3) (m3 w1 w3 w2))
                    '((w1 m2 m1 m3) (w2 m1 m2 m3) (w3 m1 m3 m2)))]
           [matching (stable-match market 100)])
      (assert-equal 3 (length matching))
      ;; Verify it's a valid matching (each person appears once)
      (let ([props (map car matching)]
            [recvs (map cdr matching)])
        (assert-equal 3 (length (remove-duplicates props)))
        (assert-equal 3 (length (remove-duplicates recvs))))))

  (define-test "stable matching is stable"
    (let* ([market (make-matching-market
                    '(m1 m2 m3)
                    '(w1 w2 w3)
                    '((m1 w1 w2 w3) (m2 w2 w1 w3) (m3 w1 w3 w2))
                    '((w1 m2 m1 m3) (w2 m1 m2 m3) (w3 m1 m3 m2)))]
           [matching (stable-match market 100)])
      (assert-true (matching-stable? market matching))))

  (define-test "2x2 deterministic case"
    ;; m1 prefers w1, m2 prefers w2
    ;; w1 prefers m1, w2 prefers m2
    ;; Unique stable matching: (m1,w1), (m2,w2)
    (let* ((market (make-matching-market
                    '(m1 m2)
                    '(w1 w2)
                    '((m1 w1 w2) (m2 w2 w1))
                    '((w1 m1 m2) (w2 m2 m1))))
           (matching (stable-match market 100)))
      (assert-equal 2 (length matching))
      (assert-true (pair? (member '(m1 . w1) matching)))
      (assert-true (pair? (member '(m2 . w2) matching)))))

  (define-test "proposer-optimal matching"
    ;; Verify GS produces proposer-optimal result
    ;; m1: w1 > w2, m2: w1 > w2
    ;; w1: m2 > m1, w2: m1 > m2
    ;; GS execution: m1 proposes to w1, accepted. m2 proposes to w1,
    ;; w1 prefers m2 so rejects m1. m1 proposes to w2, accepted.
    ;; Result: m1-w2, m2-w1 (m2 gets his first choice via proposer-optimal)
    (let* ((market (make-matching-market
                    '(m1 m2)
                    '(w1 w2)
                    '((m1 w1 w2) (m2 w1 w2))
                    '((w1 m2 m1) (w2 m1 m2))))
           (matching (stable-match market 100)))
      ;; m2 gets w1 (his first choice), m1 gets w2
      (assert-true (pair? (member '(m2 . w1) matching)))
      (assert-true (pair? (member '(m1 . w2) matching)))
      ;; The matching should be stable
      (assert-true (matching-stable? market matching))))

  (define-test "unequal sides leaves some unmatched"
    (let* ([market (make-matching-market
                    '(m1 m2 m3)
                    '(w1 w2)
                    '((m1 w1 w2) (m2 w2 w1) (m3 w1 w2))
                    '((w1 m1 m2 m3) (w2 m2 m1 m3)))]
           [matching (stable-match market 100)])
      (assert-equal 2 (length matching))  ; only 2 receivers
      (assert-true (matching-stable? market matching))))

  (define-test "single pair market"
    (let* ([market (make-matching-market
                    '(m1)
                    '(w1)
                    '((m1 w1))
                    '((w1 m1)))]
           [matching (stable-match market 100)])
      (assert-equal 1 (length matching))
      (assert-equal '((m1 . w1)) matching)))

  (define-test "empty market"
    (let* ([market (make-matching-market
                    '()
                    '()
                    '()
                    '())]
           [matching (stable-match market 100)])
      (assert-equal 0 (length matching))))

) ; end GS tests

;;; ============================================================================
;;; Stability Verification Tests
;;; ============================================================================

(test-group "Stability Verification"

  (define-test "detect blocking pair in unstable matching"
    ;; Create a scenario where we have an unstable matching
    ;; m1: w2 > w1, m2: w2 > w1
    ;; w1: m1 > m2, w2: m1 > m2
    ;; Stable matching would be: m1-w2, m2-w1
    ;; Unstable: m1-w1, m2-w2 (m1,w2 would block: both prefer each other)
    (let* ((market (make-matching-market
                    '(m1 m2)
                    '(w1 w2)
                    '((m1 w2 w1) (m2 w2 w1))  ; both prefer w2
                    '((w1 m1 m2) (w2 m1 m2)))) ; both prefer m1
           ;; This is NOT the stable matching
           (unstable '((m1 . w1) (m2 . w2))))
      ;; (m1, w2) is blocking: m1 prefers w2 to w1, w2 prefers m1 to m2
      (assert-false (matching-stable? market unstable))))

  (define-test "verify stable matching passes check"
    (let* ([market (make-matching-market
                    '(m1 m2)
                    '(w1 w2)
                    '((m1 w2 w1) (m2 w1 w2))
                    '((w1 m1 m2) (w2 m2 m1)))]
           [stable '((m1 . w2) (m2 . w1))])
      (assert-true (matching-stable? market stable))))

  (define-test "empty matching is vacuously stable"
    (let* ([market (make-matching-market
                    '(m1)
                    '(w1)
                    '((m1 w1))
                    '((w1 m1)))])
      ;; Empty matching: no pairs, but is it stable?
      ;; Actually no - (m1, w1) would block since both unmatched
      (assert-false (matching-stable? market '()))))

) ; end stability tests

;;; ============================================================================
;;; Individual Rationality Tests
;;; ============================================================================
;;;
;;; Tests that agents are never matched to partners they find unacceptable
;;; (i.e., partners not in their preference list).

(test-group "Individual Rationality"

  (define-test "unacceptable proposer rejected by free receiver"
    ;; m1's only preference is w1, but w1 doesn't list m1 at all
    ;; m1 should remain unmatched (individual rationality)
    (let* ((market (make-matching-market
                    '(m1)
                    '(w1)
                    '((m1 w1))          ; m1 wants w1
                    '((w1))))           ; w1's list is empty - m1 unacceptable
           (matching (stable-match market 100)))
      ;; No one should be matched since w1 finds m1 unacceptable
      (assert-equal 0 (length matching))))

  (define-test "partial preferences with unacceptable agents"
    ;; m1: w1 > w2, m2: w2 > w1
    ;; w1 only accepts m1, w2 only accepts m2
    (let* ((market (make-matching-market
                    '(m1 m2)
                    '(w1 w2)
                    '((m1 w1 w2) (m2 w2 w1))
                    '((w1 m1) (w2 m2))))   ; partial preferences
           (matching (stable-match market 100)))
      ;; m1 matches w1, m2 matches w2
      (assert-equal 2 (length matching))
      (assert-true (pair? (member '(m1 . w1) matching)))
      (assert-true (pair? (member '(m2 . w2) matching)))))

  (define-test "all proposers unacceptable to all receivers"
    ;; Disjoint preferences: receivers don't list any proposers
    (let* ((market (make-matching-market
                    '(m1 m2)
                    '(w1 w2)
                    '((m1 w1 w2) (m2 w1 w2))  ; proposers want receivers
                    '((w1) (w2))))            ; receivers list no one
           (matching (stable-match market 100)))
      ;; No matching possible due to individual rationality
      (assert-equal 0 (length matching))))

  (define-test "mixed acceptable and unacceptable"
    ;; m1 acceptable to both, m2 acceptable to neither, m3 acceptable to w2 only
    (let* ((market (make-matching-market
                    '(m1 m2 m3)
                    '(w1 w2)
                    '((m1 w1 w2) (m2 w1 w2) (m3 w2 w1))
                    '((w1 m1) (w2 m1 m3))))  ; m2 not in any list
           (matching (stable-match market 100)))
      ;; m1 gets w1, m3 gets w2, m2 stays unmatched
      (assert-true (pair? (member '(m1 . w1) matching)))
      (assert-true (pair? (member '(m3 . w2) matching)))
      (assert-false (pair? (member '(m2 . w1) matching)))
      (assert-false (pair? (member '(m2 . w2) matching)))
      (assert-true (matching-stable? market matching))))

) ; end individual rationality tests

;;; ============================================================================
;;; Receiver-Optimal Matching Tests
;;; ============================================================================

(test-group "Receiver-Optimal Matching"

  (define-test "receiver-optimal vs proposer-optimal"
    ;; In this market, proposer-optimal and receiver-optimal are identical
    ;; because there's only one stable matching
    (let* ((market (make-matching-market
                    '(m1 m2)
                    '(w1 w2)
                    '((m1 w1 w2) (m2 w1 w2))
                    '((w1 m2 m1) (w2 m1 m2))))
           (prop-opt (stable-match market 100))
           (recv-opt (stable-match-receiver-optimal market 100)))
      ;; Both should produce: m1-w2, m2-w1
      (assert-true (pair? (member '(m2 . w1) prop-opt)))
      (assert-true (pair? (member '(m2 . w1) recv-opt)))
      ;; Both should be stable
      (assert-true (matching-stable? market prop-opt))
      (assert-true (matching-stable? market recv-opt))))

  (define-test "both optimal matchings have same agents"
    ;; Rural Hospital Theorem
    (let* ((market (make-matching-market
                    '(m1 m2 m3)
                    '(w1 w2)
                    '((m1 w1 w2) (m2 w2 w1) (m3 w1 w2))
                    '((w1 m1 m2 m3) (w2 m2 m1 m3))))
           (prop-opt (stable-match market 100))
           (recv-opt (stable-match-receiver-optimal market 100)))
      (assert-true (same-matched-agents? prop-opt recv-opt))))

) ; end receiver-optimal tests

;;; ============================================================================
;;; Assignment Game Tests
;;; ============================================================================

(test-group "Assignment Games"

  (define-test "create 2x2 assignment game"
    (let* ((valuation (lambda (i j)
                        (let ((vals '#(#(3 1)
                                       #(2 4))))
                          (vector-ref (vector-ref vals i) j))))
           (game (make-assignment-game 2 2 valuation)))
      (assert-true (coop-game? game))
      (assert-equal 4 (coop-game-players game))))

  (define-test "assignment game grand coalition value"
    ;; Value matrix:
    ;;   r0  r1
    ;; p0: 3   1
    ;; p1: 2   4
    ;; Optimal: p0-r0 (3) + p1-r1 (4) = 7
    (let* ([valuation (lambda (i j)
                        (let ([vals '#(#(3 1)
                                       #(2 4))])
                          (vector-ref (vector-ref vals i) j)))]
           [game (make-assignment-game 2 2 valuation)]
           [grand (coop-game-grand-coalition game)])
      ;; Grand coalition should have optimal assignment value
      (let ([v-grand (coop-game-value game grand)])
        (assert-true (approximately-equal? 7 v-grand 0.01)))))

  (define-test "assignment game singleton coalitions"
    ;; Single player coalitions should have value 0
    ;; (can't form any matching)
    (let* ([valuation (lambda (i j) 10)]  ; arbitrary positive values
           [game (make-assignment-game 2 2 valuation)])
      ;; Player 0 alone (just a proposer)
      (assert-equal 0 (coop-game-value game 1))
      ;; Player 2 alone (just a receiver, player index = 2)
      (assert-equal 0 (coop-game-value game 4))))

  (define-test "assignment game two-player coalition (one from each side)"
    ;; Coalition {p0, r0} = players {0, 2} = bitmask 5
    (let* ([valuation (lambda (i j)
                        (let ([vals '#(#(3 1)
                                       #(2 4))])
                          (vector-ref (vector-ref vals i) j)))]
           [game (make-assignment-game 2 2 valuation)])
      ;; {p0, r0} can match for value 3
      (let ([coalition-p0-r0 5])  ; binary 0101
        (let ([val (coop-game-value game coalition-p0-r0)])
          (assert-true (approximately-equal? 3 val 0.01))))))

  (define-test "shapley value sums to grand coalition value"
    (let* ([valuation (lambda (i j)
                        (let ([vals '#(#(3 1)
                                       #(2 4))])
                          (vector-ref (vector-ref vals i) j)))]
           [game (make-assignment-game 2 2 valuation)]
           [shapley (shapley-value game 10000)]
           [grand-val (coop-game-value game (coop-game-grand-coalition game))]
           [shapley-sum (allocation-total shapley)])
      (assert-true (approximately-equal? grand-val shapley-sum 0.01))))

) ; end assignment game tests

;;; ============================================================================
;;; Optimal Assignment Tests (including negative valuations)
;;; ============================================================================

(test-group "Optimal Assignment"

  (define-test "2x2 optimal assignment"
    (let* ([valuation (lambda (i j)
                        (let ([vals '#(#(3 1)
                                       #(2 4))])
                          (vector-ref (vector-ref vals i) j)))]
           [result (optimal-assignment 2 2 valuation)]
           [matches (car result)]
           [total (cdr result)])
      ;; Optimal: p0-r0 (3) + p1-r1 (4) = 7
      (assert-true (approximately-equal? 7 total 0.01))
      (assert-equal 2 (length matches))))

  (define-test "3x3 optimal assignment"
    ;; Value matrix:
    ;;     r0 r1 r2
    ;; p0:  5  4  3
    ;; p1:  4  5  3
    ;; p2:  3  3  5
    ;; Optimal: p0-r0, p1-r1, p2-r2 = 5+5+5 = 15
    (let* ([valuation (lambda (i j)
                        (let ([vals '#(#(5 4 3)
                                       #(4 5 3)
                                       #(3 3 5))])
                          (vector-ref (vector-ref vals i) j)))]
           [result (optimal-assignment 3 3 valuation)]
           [total (cdr result)])
      (assert-true (approximately-equal? 15 total 0.01))))

  (define-test "rectangular assignment (more proposers)"
    ;; 3x2 case
    (let* ([valuation (lambda (i j)
                        (let ([vals '#(#(5 1)
                                       #(2 4)
                                       #(3 3))])
                          (vector-ref (vector-ref vals i) j)))]
           [result (optimal-assignment 3 2 valuation)]
           [matches (car result)]
           [total (cdr result)])
      ;; Optimal: p0-r0 (5) + p1-r1 (4) = 9, p2 unmatched
      (assert-equal 2 (length matches))
      (assert-true (approximately-equal? 9 total 0.01))))

  (define-test "rectangular assignment (more receivers)"
    ;; 2x3 case
    (let* ([valuation (lambda (i j)
                        (let ([vals '#(#(5 1 2)
                                       #(2 4 3))])
                          (vector-ref (vector-ref vals i) j)))]
           [result (optimal-assignment 2 3 valuation)]
           [matches (car result)]
           [total (cdr result)])
      ;; Optimal: p0-r0 (5) + p1-r1 (4) = 9
      (assert-equal 2 (length matches))
      (assert-true (approximately-equal? 9 total 0.01))))

  (define-test "single pair assignment"
    (let* ([valuation (lambda (i j) 42)]
           [result (optimal-assignment 1 1 valuation)]
           [matches (car result)]
           [total (cdr result)])
      (assert-equal 1 (length matches))
      (assert-true (approximately-equal? 42 total 0.01))))

  (define-test "all zero values"
    (let* ([valuation (lambda (i j) 0)]
           [result (optimal-assignment 2 2 valuation)]
           [total (cdr result)])
      (assert-true (approximately-equal? 0 total 0.01))))

  (define-test "negative valuations leave pairs unmatched"
    ;; Negative valuations mean it's better to not match than to match
    ;; LP maximizes value, so should skip negative-valued pairs
    (let* ([valuation (lambda (i j)
                        (let ([vals '#(#(-5 2)
                                       #(3 -4))])
                          (vector-ref (vector-ref vals i) j)))]
           [result (optimal-assignment 2 2 valuation)]
           [matches (car result)]
           [total (cdr result)])
      ;; Best assignment: p0-r1 (2) + p1-r0 (3) = 5
      ;; Alternatives: p0-r0 + p1-r1 = -5 + -4 = -9 (worse)
      (assert-true (approximately-equal? 5 total 0.01))))

  (define-test "all negative valuations yield empty matching"
    ;; When all values are negative, optimal is to match no one
    (let* ([valuation (lambda (i j) -10)]
           [result (optimal-assignment 2 2 valuation)]
           [matches (car result)]
           [total (cdr result)])
      ;; No matches should be made - total value 0 beats -20
      (assert-equal 0 (length matches))
      (assert-true (approximately-equal? 0 total 0.01))))

  (define-test "mixed positive and negative with optimal subset"
    ;; Only some pairs are worth matching
    ;; Matrix:
    ;;     r0  r1  r2
    ;; p0:  5  -1  -2
    ;; p1: -1   4  -3
    ;; p2: -2  -3   6
    ;; Optimal: p0-r0 (5), p1-r1 (4), p2-r2 (6) = 15
    (let* ([valuation (lambda (i j)
                        (let ([vals '#(#(5 -1 -2)
                                       #(-1 4 -3)
                                       #(-2 -3 6))])
                          (vector-ref (vector-ref vals i) j)))]
           [result (optimal-assignment 3 3 valuation)]
           [total (cdr result)])
      (assert-true (approximately-equal? 15 total 0.01))))

) ; end optimal assignment tests

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (x) (not (equal? x (car lst))))
                                       (cdr lst))))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(display "Running matching theory tests...\n")
(newline)
(run-all-tests)
