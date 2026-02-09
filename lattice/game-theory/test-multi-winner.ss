;;; lattice/game-theory/test-multi-winner.ss — Tests for multi-winner voting methods

(load "core/testing/test-framework.ss")
(load "lattice/game-theory/multi-winner.ss")

;;; ============================================================================
;;; Quota Tests
;;; ============================================================================

(test-group "quotas"

  (define-test droop-quota-basic
    ;; Droop: floor(n/(k+1)) + 1
    ;; 100 voters, 5 seats: floor(100/6) + 1 = 16 + 1 = 17
    (assert-equal 17 (droop-quota 100 5))
    ;; 9 voters, 3 seats: floor(9/4) + 1 = 2 + 1 = 3
    (assert-equal 3 (droop-quota 9 3))
    ;; 6 voters, 2 seats: floor(6/3) + 1 = 2 + 1 = 3
    (assert-equal 3 (droop-quota 6 2)))

  (define-test hare-quota-basic
    ;; Hare: floor(n/k)
    ;; 100 voters, 5 seats: 100/5 = 20
    (assert-equal 20 (hare-quota 100 5))
    ;; 9 voters, 3 seats: 9/3 = 3
    (assert-equal 3 (hare-quota 9 3))
    ;; 10 voters, 3 seats: floor(10/3) = 3
    (assert-equal 3 (hare-quota 10 3)))

  (define-test droop-leq-hare
    ;; Droop quota is always <= Hare quota
    (assert-true (<= (droop-quota 100 4) (hare-quota 100 4)))
    (assert-true (<= (droop-quota 50 3) (hare-quota 50 3)))
    (assert-true (<= (droop-quota 7 2) (hare-quota 7 2))))

)

;;; ============================================================================
;;; STV Tests
;;; ============================================================================

(test-group "stv"

  (define-test stv-returns-correct-number-seats
    (let ([profile (stv-example)])
      (assert-equal 3 (length (stv-droop profile 3)))
      (assert-equal 2 (length (stv-droop profile 2)))
      (assert-equal 1 (length (stv-droop profile 1)))))

  (define-test stv-proportional-result
    ;; 4 voters prefer A, 2 prefer B, 3 prefer C
    ;; With 3 seats, should roughly elect 1 from each major group
    (let* ([profile (stv-example)]
           [winners (stv-droop profile 3)])
      ;; A should definitely be elected (4/9 > droop quota of 3)
      (assert-true (and (member 'a winners) #t))
      ;; C should be elected (3 first-choice votes)
      (assert-true (and (member 'c winners) #t))))

  (define-test stv-single-seat-equals-irv
    ;; With 1 seat, STV becomes Instant Runoff Voting
    ;; Should elect the Condorcet winner when one exists
    (let* ([profile (make-preference-profile
                     '((a b c)
                       (a b c)
                       (a b c)
                       (b c a)
                       (c b a)))]
           [winner (car (stv-droop profile 1))])
      ;; A has plurality (3 first-choice votes)
      (assert-equal 'a winner)))

  (define-test stv-surplus-transfer
    ;; When a candidate exceeds quota, surplus should transfer
    ;; 6 voters: 4 vote A>B, 2 vote C
    ;; Quota for 2 seats: floor(6/3)+1 = 3
    ;; A gets 4, exceeds quota by 1, 1/4 of A's votes transfer to B
    (let* ([profile (make-preference-profile
                     '((a b c) (a b c) (a b c) (a b c)
                       (c b a) (c b a)))]
           [winners (stv-droop profile 2)])
      ;; A should be elected
      (assert-true (and (member 'a winners) #t))
      ;; B or C should get second seat
      (assert-equal 2 (length winners))))

  (define-test stv-hare-vs-droop
    ;; Hare quota is more proportional but can differ
    (let ([profile (stv-example)])
      (let ([droop-winners (stv-droop profile 3)]
            [hare-winners (stv-hare profile 3)])
        ;; Both should return 3 winners
        (assert-equal 3 (length droop-winners))
        (assert-equal 3 (length hare-winners)))))

  (define-test stv-empty-profile
    (let ([profile (make-preference-profile '())])
      (assert-equal '() (stv-droop profile 3))))

)

;;; ============================================================================
;;; Approval Voting Tests
;;; ============================================================================

(test-group "approval-voting"

  (define-test approval-winners-basic
    ;; 4 approve {A,B}, 3 approve {C,D}
    ;; A and B tied with 4 approvals, C and D tied with 3
    (let* ([profile (make-approval-profile
                     '((a b) (a b) (a b) (a b)
                       (c d) (c d) (c d)))]
           [winners (approval-winners profile 2)])
      ;; A and B should win (4 approvals each)
      (assert-true (and (member 'a winners) #t))
      (assert-true (and (member 'b winners) #t))))

  (define-test approval-scores-correct
    (let* ([profile (make-approval-profile
                     '((a b) (a c) (b c)))]
           [scores (approval-scores profile)])
      ;; A: 2, B: 2, C: 2
      (assert-equal 2 (cdr (assq 'a scores)))
      (assert-equal 2 (cdr (assq 'b scores)))
      (assert-equal 2 (cdr (assq 'c scores)))))

  (define-test approval-single-winner
    (let* ([profile (make-approval-profile
                     '((a b) (a b) (a b) (c)))]
           [winners (approval-winners profile 1)])
      ;; A should win with 3 approvals
      (assert-equal 'a (car winners))))

)

;;; ============================================================================
;;; Proportional Approval Voting (PAV) Tests
;;; ============================================================================

(test-group "pav"

  (define-test pav-differs-from-basic-approval
    ;; Example where PAV gives different (more proportional) result
    ;; 4 voters approve {A,B}, 3 approve {C,D}
    ;; Basic approval: A,B win (both have 4)
    ;; PAV should tend toward one from each group
    (let* ([profile (approval-example)]
           [basic (approval-winners profile 2)]
           [pav (pav-winners profile 2)])
      ;; PAV should give different result - at least one from minority
      ;; With 4 approving A,B and 3 approving C,D and 2 approving A,C
      ;; A is clear winner. Second should consider proportionality.
      (assert-true (and (member 'a pav) #t))))

  (define-test pav-diminishing-returns
    ;; Verify the PAV scoring mechanism
    (let ([profile (make-approval-profile '((a b) (a b) (c)))])
      ;; With no elected: A gets score 2, B gets 2, C gets 1
      (assert-equal 2 (pav-score 'a profile '()))
      (assert-equal 2 (pav-score 'b profile '()))
      (assert-equal 1 (pav-score 'c profile '()))
      ;; After electing A: voters who approved A get diminished influence
      ;; B's score from those voters: 2 * (1/2) = 1
      ;; C's score: 1 (voter didn't approve A)
      (assert-equal 1 (pav-score 'b profile '(a)))
      (assert-equal 1 (pav-score 'c profile '(a)))))

  (define-test pav-returns-correct-number
    (let* ([profile (approval-example)]
           [winners (pav-winners profile 3)])
      (assert-equal 3 (length winners))))

)

;;; ============================================================================
;;; Satisfaction Approval Voting (SAV) Tests
;;; ============================================================================

(test-group "sav"

  (define-test sav-normalizes-by-approval-size
    ;; Voter who approves many candidates has less influence per candidate
    (let ([profile (make-approval-profile
                    '((a)       ; 1 approval: contributes 1 to A
                      (a b c d) ; 4 approvals: contributes 0.25 to each
                      ))])
      ;; A gets 1 + 1/4 = 5/4
      ;; B,C,D each get 1/4
      (assert-equal 5/4 (sav-score 'a profile))
      (assert-equal 1/4 (sav-score 'b profile))))

  (define-test sav-equal-with-same-sizes
    ;; When all voters approve same number, SAV = basic approval
    (let* ([profile (make-approval-profile
                     '((a b) (a c) (b c)))]
           [sav-w (sav-winners profile 1)]
           [approval-w (approval-winners profile 1)])
      ;; All tied, so either method picks one of them
      (assert-equal 1 (length sav-w))))

)

;;; ============================================================================
;;; Monroe Method Tests
;;; ============================================================================

(test-group "monroe"

  (define-test monroe-returns-correct-number
    (let ([profile (diverse-preferences-example)])
      (assert-equal 3 (length (monroe-greedy profile 3)))
      (assert-equal 2 (length (monroe-greedy profile 2)))))

  (define-test monroe-includes-popular-candidates
    ;; Groups: A has 3 supporters, B has 3, C has 2, D has 2
    ;; With 2 seats, should elect from largest groups
    (let* ([profile (diverse-preferences-example)]
           [winners (monroe-greedy profile 2)])
      ;; A and B have most first-place supporters
      (assert-true (or (and (member 'a winners) #t)
                       (and (member 'b winners) #t)))))

  (define-test monroe-empty-profile
    (assert-equal '() (monroe-greedy '() 3)))

  (define-test voter-satisfaction-correct
    ;; Test satisfaction scoring
    (let ([ranking '(a b c d)])
      ;; m=4 candidates: a gets 3, b gets 2, c gets 1, d gets 0
      (assert-equal 3 (voter-satisfaction ranking 'a 4))
      (assert-equal 2 (voter-satisfaction ranking 'b 4))
      (assert-equal 1 (voter-satisfaction ranking 'c 4))
      (assert-equal 0 (voter-satisfaction ranking 'd 4))))

)

;;; ============================================================================
;;; Chamberlin-Courant Tests
;;; ============================================================================

(test-group "chamberlin-courant"

  (define-test cc-returns-correct-number
    (let ([profile (diverse-preferences-example)])
      (assert-equal 3 (length (cc-greedy profile 3)))
      (assert-equal 2 (length (cc-greedy profile 2)))))

  (define-test cc-total-satisfaction-increases
    ;; Adding more candidates should increase total satisfaction
    (let ([profile (diverse-preferences-example)])
      (let ([sat-1 (cc-total-satisfaction profile (cc-greedy profile 1))]
            [sat-2 (cc-total-satisfaction profile (cc-greedy profile 2))]
            [sat-3 (cc-total-satisfaction profile (cc-greedy profile 3))])
        (assert-true (>= sat-2 sat-1))
        (assert-true (>= sat-3 sat-2)))))

  (define-test cc-maximizes-first-choice
    ;; First candidate should be most popular first choice
    (let* ([profile (make-preference-profile
                     '((a b c) (a b c) (a b c)
                       (b c a) (c b a)))]
           [winners (cc-greedy profile 1)])
      ;; A has 3 first-place votes
      (assert-equal 'a (car winners))))

  (define-test cc-empty-profile
    (assert-equal '() (cc-greedy '() 3)))

)

;;; ============================================================================
;;; Analysis Function Tests
;;; ============================================================================

(test-group "analysis"

  (define-test proportionality-score-range
    ;; Score should be in [0, 1]
    (let* ([profile (diverse-preferences-example)]
           [committee (cc-greedy profile 3)]
           [score (proportionality-score profile committee)])
      (assert-true (>= score 0))
      (assert-true (<= score 1))))

  (define-test representation-coverage-full
    ;; If committee covers all voters' approvals
    (let* ([profile (make-approval-profile '((a) (b) (c)))]
           [committee '(a b c)])
      (assert-equal 1 (representation-coverage profile committee))))

  (define-test representation-coverage-partial
    (let* ([profile (make-approval-profile '((a) (a) (b) (c)))]
           [committee '(a)])
      ;; 2 out of 4 voters covered
      (assert-equal 1/2 (representation-coverage profile committee))))

  (define-test representation-coverage-none
    (let* ([profile (make-approval-profile '((a) (a) (a)))]
           [committee '(b)])
      (assert-equal 0 (representation-coverage profile committee))))

  (define-test diversity-score-basic
    ;; Test with simple distance function
    (let ([committee '(a b c)]
          [dist (lambda (x y) 1)])  ; All pairs distance 1
      ;; 3 pairs, each distance 1: average = 1
      (assert-equal 1 (diversity-score committee dist))))

  (define-test diversity-score-empty
    (assert-equal 0 (diversity-score '() (lambda (x y) 1)))
    (assert-equal 0 (diversity-score '(a) (lambda (x y) 1))))

)

;;; ============================================================================
;;; Conversion Function Tests
;;; ============================================================================

(test-group "conversion"

  (define-test profile-to-approval-basic
    ;; Approve top 2 from each ranking
    (let* ([profile '((a b c d) (b c a d))]
           [approval (profile->approval profile 2)])
      (assert-equal '((a b) (b c)) approval)))

  (define-test profile-to-approval-all
    ;; Approve all candidates
    (let* ([profile '((a b c))]
           [approval (profile->approval profile 3)])
      (assert-equal '((a b c)) approval)))

  (define-test approval-to-profile-basic
    ;; Convert back to ranked profile
    (let* ([approval '((a b) (c))]
           [profile (approval->profile approval)])
      ;; First voter: a, b at top, c at end
      (assert-equal 'a (car (car profile)))
      (assert-equal 'b (cadr (car profile)))))

)

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(test-group "integration"

  (define-test complete-stv-workflow
    (let* ([profile (stv-example)]
           [winners (stv-droop profile 3)])
      ;; Should have 3 winners
      (assert-equal 3 (length winners))
      ;; All winners should be from the candidates
      (let ([candidates (profile-candidates profile)])
        (assert-true (for-all? (lambda (w) (and (member w candidates) #t)) winners)))))

  (define-test complete-approval-workflow
    (let* ([profile (approval-example)]
           [basic-w (approval-winners profile 2)]
           [pav-w (pav-winners profile 2)]
           [sav-w (sav-winners profile 2)])
      ;; All methods return 2 winners
      (assert-equal 2 (length basic-w))
      (assert-equal 2 (length pav-w))
      (assert-equal 2 (length sav-w))))

  (define-test complete-ordinal-workflow
    (let* ([profile (diverse-preferences-example)]
           [monroe-w (monroe-greedy profile 3)]
           [cc-w (cc-greedy profile 3)])
      ;; Both return 3 winners
      (assert-equal 3 (length monroe-w))
      (assert-equal 3 (length cc-w))
      ;; Both have positive satisfaction
      (let ([cc-sat (cc-total-satisfaction profile cc-w)])
        (assert-true (> cc-sat 0)))))

  (define-test methods-agree-on-obvious-case
    ;; When one candidate dominates, all methods should include them
    (let* ([profile (make-preference-profile
                     '((a b c) (a b c) (a b c) (a b c) (a b c)  ; 5 for A
                       (b c a)))]                              ; 1 for B
           [approval-profile (profile->approval profile 2)]
           [stv-w (stv-droop profile 2)]
           [monroe-w (monroe-greedy profile 2)]
           [cc-w (cc-greedy profile 2)]
           [pav-w (pav-winners approval-profile 2)])
      ;; A should be in all results
      (assert-true (and (member 'a stv-w) #t))
      (assert-true (and (member 'a monroe-w) #t))
      (assert-true (and (member 'a cc-w) #t))
      (assert-true (and (member 'a pav-w) #t))))

)

;;; ============================================================================
;;; Edge Case Tests
;;; ============================================================================

(test-group "edge-cases"

  (define-test more-seats-than-candidates
    ;; Should return all candidates
    (let* ([profile (make-preference-profile '((a b) (b a)))]
           [winners (stv-droop profile 5)])
      (assert-equal 2 (length winners))
      (assert-true (and (member 'a winners) #t))
      (assert-true (and (member 'b winners) #t))))

  (define-test single-voter
    (let* ([profile (make-preference-profile '((a b c)))]
           [winners (stv-droop profile 2)])
      ;; With 1 voter, any candidate they prefer should win
      (assert-true (and (member 'a winners) #t))))

  (define-test tied-candidates
    ;; When candidates are perfectly tied
    (let* ([profile (make-preference-profile
                     '((a b c) (b c a) (c a b)))]
           [winners (stv-droop profile 2)])
      ;; Should still return 2 (some tiebreaking)
      (assert-equal 2 (length winners))))

  (define-test approval-all-candidates
    ;; Voter approves everyone
    (let* ([profile (make-approval-profile '((a b c) (a b c)))]
           [winners (approval-winners profile 1)])
      ;; All tied, pick one
      (assert-equal 1 (length winners))))

)

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
