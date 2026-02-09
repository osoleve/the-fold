;;; lattice/game-theory/test-voting-games.ss — Tests for voting-games bridge module

(load "core/testing/test-framework.ss")
(load "lattice/game-theory/voting-games.ss")

;;; ============================================================================
;;; Profile to Game Conversion Tests
;;; ============================================================================

(test-group "profile-to-game"

  (define-test majority-game-basic
    ;; 3 voters: coalition needs 2+ to be winning
    (let* ([profile (make-preference-profile '((a b) (a b) (b a)))]
           [game (profile->majority-game profile)])
      ;; Empty coalition not winning
      (assert-equal 0 (coop-game-value game 0))
      ;; Single voters not winning (need 2 of 3)
      (assert-equal 0 (coop-game-value game 1))  ; {0}
      (assert-equal 0 (coop-game-value game 2))  ; {1}
      (assert-equal 0 (coop-game-value game 4))  ; {2}
      ;; Pairs are winning
      (assert-equal 1 (coop-game-value game 3))  ; {0,1}
      (assert-equal 1 (coop-game-value game 5))  ; {0,2}
      (assert-equal 1 (coop-game-value game 6))  ; {1,2}
      ;; Grand coalition is winning
      (assert-equal 1 (coop-game-value game 7)))) ; {0,1,2}

  (define-test majority-game-even-voters
    ;; 4 voters: coalition needs 3+ to be winning (strict majority)
    (let* ([profile (make-preference-profile '((a b) (a b) (b a) (b a)))]
           [game (profile->majority-game profile)])
      ;; 2-voter coalitions not winning
      (assert-equal 0 (coop-game-value game 3))  ; {0,1}
      ;; 3-voter coalitions winning
      (assert-equal 1 (coop-game-value game 7))  ; {0,1,2}
      (assert-equal 1 (coop-game-value game 11)) ; {0,1,3}
      ;; Grand coalition winning
      (assert-equal 1 (coop-game-value game 15)))) ; all 4

  (define-test weighted-voting-game-basic
    ;; Weighted voting: voters have weights 3, 2, 1
    ;; Total = 6, quota = floor(6/2) + 1 = 4 (strict majority)
    (let* ([profile (make-preference-profile '((a b) (a b) (b a)))]
           [game (profile->weighted-voting-game profile '(3 2 1))])
      ;; Voter 0 (weight 3) alone not winning (3 < 4)
      (assert-equal 0 (coop-game-value game 1))  ; {0} weight=3
      ;; Voter 1 (weight 2) alone not winning
      (assert-equal 0 (coop-game-value game 2))  ; {1} weight=2
      ;; Voters 0+1 (weights 3+2=5) is winning (5 >= 4)
      (assert-equal 1 (coop-game-value game 3))  ; {0,1}
      ;; Voters 1+2 (weights 2+1=3) not winning (3 < 4)
      (assert-equal 0 (coop-game-value game 6))  ; {1,2}
      ;; Voters 0+2 (weights 3+1=4) is winning (4 >= 4)
      (assert-equal 1 (coop-game-value game 5)))) ; {0,2}

  (define-test game-is-simple
    ;; Majority game should be a simple game (values in {0,1})
    (let* ([profile (make-preference-profile '((a b c) (b a c) (c a b)))]
           [game (profile->majority-game profile)])
      (assert-true (coop-game-simple? game))))

)

;;; ============================================================================
;;; Power Index Tests
;;; ============================================================================

(test-group "power-indices"

  (define-test shapley-shubik-equal-voters
    ;; 3 equal voters: each should have power 1/3
    (let* ([profile (make-preference-profile '((a b) (a b) (b a)))]
           [ss (shapley-shubik-index profile)])
      (assert-equal 3 (vector-length ss))
      ;; All voters have equal power
      (assert-true (< (abs (- (vector-ref ss 0) 1/3)) 1e-10))
      (assert-true (< (abs (- (vector-ref ss 1) 1/3)) 1e-10))
      (assert-true (< (abs (- (vector-ref ss 2) 1/3)) 1e-10))))

  (define-test shapley-shubik-sum-to-one
    ;; Shapley values should sum to 1 for efficiency
    (let* ([profile (make-preference-profile '((a b) (a b) (b a) (b a)))]
           [ss (shapley-shubik-index profile)]
           [total (+ (vector-ref ss 0) (vector-ref ss 1)
                     (vector-ref ss 2) (vector-ref ss 3))])
      (assert-true (< (abs (- total 1)) 1e-10))))

  (define-test banzhaf-equal-voters
    ;; 3 equal voters: each should have equal Banzhaf power
    (let* ([profile (make-preference-profile '((a b) (a b) (b a)))]
           [bz (banzhaf-voting-power profile)])
      (assert-equal 3 (vector-length bz))
      ;; All normalized powers equal
      (assert-true (< (abs (- (vector-ref bz 0) (vector-ref bz 1))) 1e-10))
      (assert-true (< (abs (- (vector-ref bz 1) (vector-ref bz 2))) 1e-10))))

  (define-test banzhaf-sum-to-one
    ;; Normalized Banzhaf should sum to 1
    (let* ([profile (make-preference-profile '((a b) (a b) (a b) (b a)))]
           [bz (banzhaf-voting-power profile)]
           [total (apply + (vector->list bz))])
      (assert-true (< (abs (- total 1)) 1e-10))))

  (define-test weighted-power-different
    ;; With weights 3,1,1, voter 0 has more power
    (let* ([profile (make-preference-profile '((a b) (a b) (b a)))]
           [ss (shapley-shubik-weighted profile '(3 1 1))])
      ;; Voter 0 should have more than 1/3 power
      (assert-true (> (vector-ref ss 0) 1/3))
      ;; Voters 1 and 2 should have equal power
      (assert-true (< (abs (- (vector-ref ss 1) (vector-ref ss 2))) 1e-10))))

)

;;; ============================================================================
;;; Power Analysis Tests
;;; ============================================================================

(test-group "power-analysis"

  (define-test no-dictator-in-majority
    ;; In majority voting with 3 equal voters, no one is a dictator
    (let ([profile (make-preference-profile '((a b) (a b) (b a)))])
      (assert-false (voter-is-dictator? profile 0))
      (assert-false (voter-is-dictator? profile 1))
      (assert-false (voter-is-dictator? profile 2))))

  (define-test no-dummy-voters-equal
    ;; With equal weights, no one is a dummy
    (let ([profile (make-preference-profile '((a b) (a b) (b a)))])
      (assert-false (voter-is-dummy? profile 0))
      (assert-false (voter-is-dummy? profile 1))
      (assert-false (voter-is-dummy? profile 2))))

  (define-test no-veto-in-majority
    ;; In majority with 3 voters, no single voter has veto
    ;; (the other 2 can still win without them)
    (let ([profile (make-preference-profile '((a b) (a b) (b a)))])
      (assert-false (voter-has-veto? profile 0))
      (assert-false (voter-has-veto? profile 1))
      (assert-false (voter-has-veto? profile 2))))

  (define-test two-voters-both-have-veto
    ;; With 2 voters, each has veto (need both to win)
    (let ([profile (make-preference-profile '((a b) (b a)))])
      (assert-true (voter-has-veto? profile 0))
      (assert-true (voter-has-veto? profile 1))))

  (define-test minimal-winning-3-voters
    ;; With 3 voters, minimal winning coalitions are pairs
    (let* ([profile (make-preference-profile '((a b) (a b) (b a)))]
           [mwc (minimal-winning-coalitions profile)])
      ;; Should have 3 minimal winning coalitions: {0,1}, {0,2}, {1,2}
      (assert-equal 3 (length mwc))
      (assert-true (and (member 3 mwc) #t))  ; {0,1}
      (assert-true (and (member 5 mwc) #t))  ; {0,2}
      (assert-true (and (member 6 mwc) #t)))) ; {1,2}

)

;;; ============================================================================
;;; Power Distribution Tests
;;; ============================================================================

(test-group "power-distribution"

  (define-test concentration-equal-power
    ;; 3 equal voters: HHI = 3*(1/3)^2 = 1/3
    (let ([power (vector 1/3 1/3 1/3)])
      (assert-true (< (abs (- (power-concentration power) 1/3)) 1e-10))))

  (define-test concentration-monopoly
    ;; Single voter with all power: HHI = 1
    (let ([power (vector 1 0 0)])
      (assert-equal 1 (power-concentration power))))

  (define-test gini-equal-power
    ;; Equal power distribution: Gini = 0
    (let ([power (vector 1/3 1/3 1/3)])
      (assert-equal 0 (power-gini power))))

  (define-test gini-unequal-power
    ;; Very unequal: one voter has all power
    ;; For [0, 0, 1] sorted: (2*1-3-1)*0 + (2*2-3-1)*0 + (2*3-3-1)*1 = 2
    ;; Gini = 2 / (3 * 1) = 2/3
    (let ([power (vector 1 0 0)])
      (assert-true (> (power-gini power) 0.5))))

)

;;; ============================================================================
;;; Electoral College Tests
;;; ============================================================================

(test-group "electoral-college"

  (define-test simple-electoral-college
    ;; 3 states with votes: 5, 3, 2 (total 10, quota 6)
    (let* ([states '((A . 5) (B . 3) (C . 2))]
           [game (make-electoral-college-game states)])
      ;; A alone is not winning (5 < 6)
      (assert-equal 0 (coop-game-value game 1))
      ;; A+B is winning (5+3=8 >= 6)
      (assert-equal 1 (coop-game-value game 3))
      ;; A+C is winning (5+2=7 >= 6)
      (assert-equal 1 (coop-game-value game 5))
      ;; B+C not winning (3+2=5 < 6)
      (assert-equal 0 (coop-game-value game 6))))

  (define-test electoral-power-larger-state-more-power
    ;; State A (5 votes) should have more power than C (2 votes)
    (let* ([states '((A . 5) (B . 3) (C . 2))]
           [power (electoral-college-power states)])
      (let ([pA (cdr (assq 'A power))]
            [pC (cdr (assq 'C power))])
        (assert-true (> pA pC)))))

)

;;; ============================================================================
;;; UN Security Council Tests
;;; ============================================================================

(test-group "un-security-council"

  (define-test unsc-requires-all-p5
    (let ([game (un-security-council)])
      ;; All 15 members but missing one P5 member (say 0) - not winning
      ;; Grand coalition minus player 0 = all bits except bit 0
      (let ([all-but-0 (- (expt 2 15) 1 1)])  ; 32766
        (assert-equal 0 (coop-game-value game all-but-0)))
      ;; All P5 + 4 temporary (only 9 total) - winning
      (let ([p5-plus-4 (+ 31 ; P5 (bits 0-4)
                          (+ 32 64 128 256))]) ; 4 temporary (bits 5-8)
        (assert-equal 1 (coop-game-value game p5-plus-4)))))

  (define-test unsc-p5-have-veto
    ;; Each P5 member (0-4) should have veto power
    (let ([game (un-security-council)])
      ;; Check: removing any P5 from grand coalition makes it losing
      (let ([grand (- (expt 2 15) 1)])
        (assert-equal 1 (coop-game-value game grand))
        ;; Remove P5 member 0
        (assert-equal 0 (coop-game-value game (- grand 1)))
        ;; Remove P5 member 1
        (assert-equal 0 (coop-game-value game (- grand 2)))
        ;; Remove P5 member 2
        (assert-equal 0 (coop-game-value game (- grand 4))))))

)

;;; ============================================================================
;;; Strategic Analysis Tests
;;; ============================================================================

(test-group "strategic-analysis"

  (define-test decisive-coalitions-majority
    ;; Under plurality, a majority can elect any candidate
    (let* ([profile (make-preference-profile '((a b) (a b) (b a)))]
           [decisive-a (decisive-for-candidate profile plurality-winner 'a)])
      ;; Any majority coalition (size >= 2) should be decisive for a
      ;; Coalitions of size >= 2: 3={0,1}, 5={0,2}, 6={1,2}, 7={0,1,2}
      (assert-true (and (member 3 decisive-a) #t))
      (assert-true (and (member 5 decisive-a) #t))
      (assert-true (and (member 6 decisive-a) #t))
      (assert-true (and (member 7 decisive-a) #t))))

  (define-test blocking-coalitions-majority
    ;; A majority can block any candidate
    (let* ([profile (make-preference-profile '((a b) (a b) (b a)))]
           [blocking-a (blocking-for-candidate profile plurality-winner 'a)])
      ;; Any majority can block a (by voting for b)
      (assert-true (and (member 3 blocking-a) #t))  ; {0,1}
      (assert-true (and (member 5 blocking-a) #t))  ; {0,2}
      (assert-true (and (member 6 blocking-a) #t)))) ; {1,2}

)

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(test-group "integration"

  (define-test complete-workflow
    ;; Test the complete workflow: profile -> game -> power -> analysis
    (let* ([profile (simple-3-voter-example)]
           [game (profile->majority-game profile)]
           [ss-power (shapley-shubik-index profile)]
           [bz-power (banzhaf-voting-power profile)]
           [mwc (minimal-winning-coalitions profile)])
      ;; Profile has 3 voters
      (assert-equal 3 (profile-voters profile))
      ;; Game is simple
      (assert-true (coop-game-simple? game))
      ;; Powers are defined
      (assert-equal 3 (vector-length ss-power))
      (assert-equal 3 (vector-length bz-power))
      ;; 3 minimal winning coalitions (the pairs)
      (assert-equal 3 (length mwc))))

  (define-test us-electoral-college-example
    ;; Test with actual electoral college data subset
    (let* ([states (us-electoral-college-2020)]
           [power (electoral-college-power states)])
      ;; Should have 10 states
      (assert-equal 10 (length power))
      ;; California (55 votes) should have highest power
      (let ([ca-power (cdr (assq 'CA power))])
        (assert-true (> ca-power 0)))))

)

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
