;;; lattice/fp/game/test-voting.ss — Tests for voting theory module

(load "core/testing/test-framework.ss")
(load "lattice/fp/game/voting.ss")

;;; ============================================================================
;;; Preference Profile Tests
;;; ============================================================================

(test-group "preference-profiles"

  (define-test empty-profile
    (let ([profile (make-preference-profile '())])
      (assert-equal 0 (profile-voters profile))
      (assert-equal 0 (profile-num-candidates profile))))

  (define-test simple-profile
    (let ([profile (make-preference-profile '((a b c) (b a c) (c b a)))])
      (assert-equal 3 (profile-voters profile))
      (assert-equal 3 (profile-num-candidates profile))
      (assert-true (and (member 'a (profile-candidates profile)) #t))
      (assert-true (and (member 'b (profile-candidates profile)) #t))
      (assert-true (and (member 'c (profile-candidates profile)) #t))))

  (define-test unanimous-profile
    (let ([profile (make-preference-profile '((x y z) (x y z) (x y z)))])
      (assert-equal 3 (profile-voters profile))))

)

;;; ============================================================================
;;; Plurality Tests
;;; ============================================================================

(test-group "plurality"

  (define-test unanimous-plurality
    ;; All voters agree: a > b > c
    (let ([profile (make-preference-profile '((a b c) (a b c) (a b c)))])
      (assert-equal 'a (plurality-winner profile))))

  (define-test plurality-basic
    ;; 2 vote a first, 1 votes b first
    (let ([profile (make-preference-profile '((a b c) (a c b) (b a c)))])
      (assert-equal 'a (plurality-winner profile))))

  (define-test plurality-tie-breaker
    ;; 1 each for a, b, c - winner depends on implementation (first found)
    (let ([profile (make-preference-profile '((a b c) (b c a) (c a b)))])
      (let ([winner (plurality-winner profile)])
        (assert-true (and (member winner '(a b c)) #t)))))

  (define-test plurality-scores
    (let ([profile (make-preference-profile '((a b c) (a b c) (b a c)))])
      (let ([scores (plurality-scores-all profile)])
        (assert-equal 2 (cdr (assq 'a scores)))
        (assert-equal 1 (cdr (assq 'b scores)))
        (assert-equal 0 (cdr (assq 'c scores))))))

)

;;; ============================================================================
;;; Borda Count Tests
;;; ============================================================================

(test-group "borda"

  (define-test borda-unanimous
    (let ([profile (make-preference-profile '((a b c) (a b c) (a b c)))])
      (assert-equal 'a (borda-winner profile))))

  (define-test borda-scores-calculation
    ;; 3 candidates: scores are 2,1,0 for positions 1st,2nd,3rd
    ;; Voter 1 (a>b>c): a=2, b=1, c=0
    ;; Voter 2 (a>b>c): a=2, b=1, c=0
    ;; Voter 3 (b>a>c): b=2, a=1, c=0
    ;; Totals: a=5, b=4, c=0
    (let ([profile (make-preference-profile '((a b c) (a b c) (b a c)))])
      (let ([scores (borda-scores-all profile)])
        (assert-equal 5 (cdr (assq 'a scores)))
        (assert-equal 4 (cdr (assq 'b scores)))
        (assert-equal 0 (cdr (assq 'c scores))))))

  (define-test borda-different-from-plurality
    ;; Classic example where Borda and plurality can differ
    ;; 2 voters: a>b>c
    ;; 1 voter:  b>c>a
    ;; 1 voter:  c>b>a
    ;; Plurality: a=2, b=1, c=1 -> a wins
    ;; Borda: a=2*2+0+0=4, b=2*1+2+1=5, c=0+1+2=3 -> b wins
    (let ([profile (make-preference-profile '((a b c) (a b c) (b c a) (c b a)))])
      (assert-equal 'a (plurality-winner profile))
      (assert-equal 'b (borda-winner profile))))

)

;;; ============================================================================
;;; Condorcet Tests
;;; ============================================================================

(test-group "condorcet"

  (define-test pairwise-margin-basic
    (let ([profile (make-preference-profile '((a b c) (a b c) (b a c)))])
      ;; a vs b: 2 prefer a, 1 prefers b -> margin = 1
      (assert-equal 1 (pairwise-margin 'a 'b profile))
      ;; b vs a: margin = -1
      (assert-equal -1 (pairwise-margin 'b 'a profile))))

  (define-test condorcet-winner-exists
    ;; a beats both b and c
    (let ([profile (make-preference-profile '((a b c) (a b c) (a c b)))])
      (assert-true (condorcet-winner? 'a profile))
      (assert-equal 'a (condorcet-winner profile))))

  (define-test condorcet-cycle-no-winner
    ;; Classic Condorcet cycle: a>b>c, b>c>a, c>a>b
    (let ([profile (condorcet-cycle-example)])
      (assert-false (condorcet-winner profile))))

  (define-test condorcet-beats-relation
    (let ([profile (make-preference-profile '((a b c) (a b c) (b a c)))])
      (assert-true (pairwise-beats? 'a 'b profile))
      (assert-false (pairwise-beats? 'b 'a profile))))

)

;;; ============================================================================
;;; Copeland Method Tests
;;; ============================================================================

(test-group "copeland"

  (define-test copeland-basic
    ;; a beats everyone: score = 2
    ;; b beats c: score = 0 (loses to a, beats c)
    ;; c loses to everyone: score = -2
    (let ([profile (make-preference-profile '((a b c) (a b c) (a c b)))])
      (assert-equal 2 (copeland-score 'a profile))
      (assert-equal 'a (copeland-winner profile))))

  (define-test copeland-cycle
    ;; In a cycle, all have score 0
    (let ([profile (condorcet-cycle-example)])
      (assert-equal 0 (copeland-score 'a profile))
      (assert-equal 0 (copeland-score 'b profile))
      (assert-equal 0 (copeland-score 'c profile))))

)

;;; ============================================================================
;;; Schulze Method Tests
;;; ============================================================================

(test-group "schulze"

  (define-test schulze-condorcet-winner
    ;; When Condorcet winner exists, Schulze should find it
    (let ([profile (make-preference-profile '((a b c) (a b c) (a c b)))])
      (assert-equal 'a (schulze-winner profile))))

  (define-test schulze-cycle-resolution
    ;; Schulze resolves Condorcet cycles deterministically
    (let ([profile (condorcet-cycle-example)])
      (let ([winner (schulze-winner profile)])
        (assert-true (and (member winner '(a b c)) #t)))))

  (define-test schulze-ranking
    (let ([profile (make-preference-profile '((a b c) (a b c) (a c b)))])
      (let ([ranking (schulze-ranking profile)])
        (assert-equal 'a (car ranking)))))

)

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(test-group "edge-cases"

  (define-test two-candidates
    (let ([profile (make-preference-profile '((a b) (a b) (b a)))])
      (assert-equal 'a (plurality-winner profile))
      (assert-equal 'a (borda-winner profile))
      (assert-equal 'a (condorcet-winner profile))
      (assert-equal 'a (copeland-winner profile))))

  (define-test single-voter
    (let ([profile (make-preference-profile '((x y z)))])
      (assert-equal 'x (plurality-winner profile))
      (assert-equal 'x (borda-winner profile))
      (assert-equal 'x (condorcet-winner profile))))

)

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
