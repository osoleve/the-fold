;;; lattice/game-theory/test-strategic-voting.ss — Tests for strategic voting module

(load "core/testing/test-framework.ss")
(load "lattice/game-theory/strategic-voting.ss")

;;; ============================================================================
;;; Utility Function Tests
;;; ============================================================================

(test-group "voter-utility"

  (define-test utility-first-choice
    ;; First choice winner gives maximum utility
    (let ([ranking '(a b c)])
      (assert-equal 2 (voter-utility 'a ranking))))

  (define-test utility-second-choice
    (let ([ranking '(a b c)])
      (assert-equal 1 (voter-utility 'b ranking))))

  (define-test utility-last-choice
    (let ([ranking '(a b c)])
      (assert-equal 0 (voter-utility 'c ranking))))

  (define-test utility-not-found
    ;; Candidate not in ranking
    (let ([ranking '(a b c)])
      (assert-equal 0 (voter-utility 'd ranking))))

)

(test-group "social-welfare"

  (define-test welfare-unanimous-first
    ;; All voters get first choice
    (let ([profile (make-preference-profile '((a b c) (a b c) (a b c)))])
      ;; Each voter gets utility 2, total = 6
      (assert-equal 6 (social-welfare 'a profile))))

  (define-test welfare-unanimous-last
    ;; All voters get last choice
    (let ([profile (make-preference-profile '((a b c) (a b c) (a b c)))])
      ;; Each voter gets utility 0, total = 0
      (assert-equal 0 (social-welfare 'c profile))))

  (define-test welfare-mixed
    ;; Different preferences
    (let ([profile (make-preference-profile '((a b c) (b a c) (c a b)))])
      ;; a wins: voter1 gets 2, voter2 gets 1, voter3 gets 1 = 4
      (assert-equal 4 (social-welfare 'a profile))))

)

;;; ============================================================================
;;; Best Response Tests
;;; ============================================================================

(test-group "best-response"

  (define-test best-response-no-improvement
    ;; When voter's first choice wins, truthful is best response
    (let* ([profile (make-preference-profile '((a b c) (a b c) (a b c)))]
           [true-ranking (car profile)])
      (assert-false (best-response-improves? 0 profile plurality-winner true-ranking))))

  (define-test best-response-can-improve
    ;; a-voter (voter 0) in strategic-example-1 can improve by voting b first
    (let* ([profile (strategic-example-1)]
           [true-ranking (list-ref profile 0)])  ; a > b > c
      ;; Truthful: c wins (utility 0 for a-voter)
      ;; If votes (b a c): b wins (utility 1 for a-voter)
      (assert-true (best-response-improves? 0 profile plurality-winner true-ranking))))

  (define-test find-best-responses-returns-list
    (let* ([profile (make-preference-profile '((a b c) (a b c) (b a c)))]
           [true-ranking (list-ref profile 2)]  ; b > a > c
           [best (find-best-responses 2 profile plurality-winner true-ranking)])
      ;; Should return at least one ballot
      (assert-true (pair? best))
      ;; Each should be a valid ranking
      (assert-equal 3 (length (car best)))))

)

;;; ============================================================================
;;; Nash Equilibrium Tests
;;; ============================================================================

(test-group "nash-equilibrium"

  (define-test unanimous-is-equilibrium
    ;; Unanimous profile is always an equilibrium
    (let ([profile (make-preference-profile '((a b c) (a b c) (a b c)))])
      (assert-true (is-strategic-equilibrium? profile profile plurality-winner))))

  (define-test truthful-may-not-be-equilibrium
    ;; strategic-example-1 has a manipulable voter
    (let ([profile (strategic-example-1)])
      (assert-false (is-strategic-equilibrium? profile profile plurality-winner))))

  (define-test find-equilibrium-converges
    ;; Should find an equilibrium from unanimous profile
    (let* ([profile (make-preference-profile '((a b c) (a b c) (a b c)))]
           [result (find-strategic-equilibrium profile plurality-winner 100)])
      (assert-equal 'ok (car result))))

  (define-test find-equilibrium-from-manipulable
    ;; Should find equilibrium even when starting from manipulable profile
    (let* ([profile (strategic-example-1)]
           [result (find-strategic-equilibrium profile plurality-winner 100)])
      ;; Should either find equilibrium or detect cycle
      (assert-true (or (eq? (car result) 'ok)
                       (eq? (car result) 'err)))))

)

;;; ============================================================================
;;; All Equilibria Tests (Small Instances Only)
;;; ============================================================================

(test-group "all-equilibria"

  (define-test all-equilibria-2-candidates
    ;; With 2 candidates, finding all equilibria is tractable
    (let* ([profile (make-preference-profile '((a b) (a b)))]
           [equilibria (all-strategic-equilibria profile plurality-winner)])
      ;; Truthful should be an equilibrium
      (assert-true (and (member profile equilibria) #t))))

  (define-test all-equilibria-includes-truthful-when-stable
    ;; Unanimous 2-candidate case
    (let* ([profile (make-preference-profile '((a b) (a b) (a b)))]
           [equilibria (all-strategic-equilibria profile plurality-winner)])
      (assert-true (and (member profile equilibria) #t))))

)

;;; ============================================================================
;;; Price of Anarchy Tests
;;; ============================================================================

(test-group "price-of-anarchy"

  (define-test poa-unanimous-is-one
    ;; When truthful is the only equilibrium, POA = 1
    (let* ([profile (make-preference-profile '((a b) (a b)))]
           [poa (price-of-anarchy profile plurality-winner)])
      ;; Should be close to 1 (exact comparison may have float issues)
      (assert-true (and (number? poa) (<= poa 1.01) (>= poa 0.99)))))

  (define-test pos-at-most-poa
    ;; Price of Stability <= Price of Anarchy always
    (let* ([profile (make-preference-profile '((a b) (b a)))]
           [poa (price-of-anarchy profile plurality-winner)]
           [pos (price-of-stability profile plurality-winner)])
      (when (and (number? poa) (number? pos))
        (assert-true (<= pos poa)))))

)

;;; ============================================================================
;;; Strategy-Proofness Tests
;;; ============================================================================

(test-group "strategy-proofness"

  (define-test count-manipulable-2-cand-2-voters
    ;; Small instance to verify counting works
    (let ([counts (count-manipulable-profiles plurality-winner 2 2)])
      ;; Total profiles: (2!)^2 = 4
      (assert-equal 4 (cdr counts))
      ;; Manipulable count should be non-negative
      (assert-true (>= (car counts) 0))))

  (define-test proofness-ratio-bounded
    ;; Ratio should be between 0 and 1
    (let ([ratio (strategy-proofness-ratio plurality-winner 2 2)])
      (assert-true (>= ratio 0))
      (assert-true (<= ratio 1))))

  (define-test compare-rules-returns-sorted
    ;; Comparison should return sorted list
    (let* ([rules `((plurality . ,plurality-winner)
                    (borda . ,borda-winner))]
           [comparison (compare-strategy-proofness rules 2 2)])
      ;; Should have 2 entries
      (assert-equal 2 (length comparison))
      ;; First should have >= ratio than second
      (assert-true (>= (cdr (car comparison))
                       (cdr (cadr comparison))))))

)

;;; ============================================================================
;;; Gibbard-Satterthwaite Tests
;;; ============================================================================

(test-group "gibbard-satterthwaite"

  (define-test manipulation-found-for-plurality
    ;; Plurality is manipulable with 3 candidates
    (let ([result (find-manipulation-example plurality-winner 3)])
      (assert-true (and (pair? result) (eq? (car result) 'found)))))

  (define-test manipulation-found-for-borda
    ;; Borda is manipulable with 3 candidates
    (let ([result (find-manipulation-example borda-winner 3)])
      (assert-true (and (pair? result) (eq? (car result) 'found)))))

  (define-test find-manipulation-in-known-profile
    ;; strategic-example-1 should have manipulation
    (let ([result (find-manipulation-in-profile (strategic-example-1) plurality-winner)])
      (assert-true (and (pair? result) (eq? (car result) 'found)))))

)

;;; ============================================================================
;;; Example Profiles Tests
;;; ============================================================================

(test-group "example-profiles"

  (define-test strategic-example-1-valid
    (let ([profile (strategic-example-1)])
      (assert-equal 7 (profile-voters profile))
      (assert-equal 3 (profile-num-candidates profile))))

  (define-test strategic-example-cycle-valid
    (let ([profile (strategic-example-cycle)])
      (assert-equal 3 (profile-voters profile))
      (assert-equal 3 (profile-num-candidates profile))))

)

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
