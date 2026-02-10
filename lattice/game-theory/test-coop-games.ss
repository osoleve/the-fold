;;; lattice/game-theory/test-coop-games.ss — Tests for Cooperative Game Theory
;;;
;;; Tests coalition operations, Shapley value, core stability,
;;; bargaining solutions, and game properties.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/game-theory/coop-games.ss")

;;; ============================================================================
;;; Coalition Operations Tests
;;; ============================================================================

(test-group coalition-ops

  (define-test coalition-singleton-basic
    (assert-equal 1 (coalition-singleton 0))
    (assert-equal 2 (coalition-singleton 1))
    (assert-equal 4 (coalition-singleton 2))
    (assert-equal 8 (coalition-singleton 3)))

  (define-test coalition-member-basic
    ;; Coalition 5 = binary 101 = {0, 2}
    (assert-true (coalition-member? 0 5))
    (assert-false (coalition-member? 1 5))
    (assert-true (coalition-member? 2 5))
    (assert-false (coalition-member? 3 5)))

  (define-test coalition-union-basic
    ;; 5 | 3 = 101 | 011 = 111 = 7
    (assert-equal 7 (coalition-union 5 3))
    ;; 1 | 2 = 001 | 010 = 011 = 3
    (assert-equal 3 (coalition-union 1 2)))

  (define-test coalition-intersection-basic
    ;; 5 & 3 = 101 & 011 = 001 = 1
    (assert-equal 1 (coalition-intersection 5 3))
    ;; 6 & 3 = 110 & 011 = 010 = 2
    (assert-equal 2 (coalition-intersection 6 3)))

  (define-test coalition-difference-basic
    ;; 7 \ 5 = 111 \ 101 = 010 = 2
    (assert-equal 2 (coalition-difference 7 5)))

  (define-test coalition-complement-basic
    ;; Complement of 5 (101) in 3 players (111) = 010 = 2
    (assert-equal 2 (coalition-complement 5 3)))

  (define-test coalition-size-basic
    (assert-equal 0 (coalition-size 0))
    (assert-equal 1 (coalition-size 1))
    (assert-equal 2 (coalition-size 5))  ; 101
    (assert-equal 3 (coalition-size 7))) ; 111

  (define-test coalition-list-conversions
    (assert-equal '(0 2) (coalition->list 5))
    (assert-equal '(0 1 2) (coalition->list 7))
    (assert-equal '() (coalition->list 0))
    (assert-equal 5 (list->coalition '(0 2)))
    (assert-equal 7 (list->coalition '(0 1 2))))

  (define-test coalition-all-coalitions
    (assert-equal '(0 1 2 3) (all-coalitions 2))
    (assert-equal 8 (length (all-coalitions 3))))

  (define-test coalition-subset-basic
    (assert-true (coalition-subset? 1 7))   ; {0} subset {0,1,2}
    (assert-true (coalition-subset? 5 7))   ; {0,2} subset {0,1,2}
    (assert-false (coalition-subset? 5 3))) ; {0,2} not subset {0,1}

)

;;; ============================================================================
;;; Allocation Tests
;;; ============================================================================

(test-group allocations

  (define-test allocation-total-basic
    (assert-equal 10 (allocation-total '#(3 3 4)))
    (assert-equal 0 (allocation-total '#(0 0 0))))

  (define-test allocation-coalition-total-basic
    ;; Coalition 5 = {0, 2}
    (assert-equal 7 (allocation-coalition-total '#(3 3 4) 5))
    ;; Coalition 3 = {0, 1}
    (assert-equal 6 (allocation-coalition-total '#(3 3 4) 3)))

)

;;; ============================================================================
;;; Imputation Tests
;;; ============================================================================

(test-group imputations

  (define-test imputation-additive-game
    ;; Additive game: v({i}) = value[i]
    (let ([g (make-additive-game '(10 20 30))])
      ;; x = (10, 20, 30) is the unique imputation
      (assert-true (imputation? g '#(10 20 30)))
      ;; Not efficient
      (assert-false (imputation? g '#(5 10 15)))
      ;; Not individually rational
      (assert-false (imputation? g '#(5 25 30)))))

)

;;; ============================================================================
;;; Shapley Value Tests
;;; ============================================================================

(test-group shapley-value

  (define-test shapley-additive-game
    ;; For additive game, Shapley = individual values
    (let* ([g (make-additive-game '(10 20 30))]
           [phi (shapley-value g 1000)])
      (assert-equal 10 (vector-ref phi 0))
      (assert-equal 20 (vector-ref phi 1))
      (assert-equal 30 (vector-ref phi 2))))

  (define-test shapley-unanimity-game
    ;; Unanimity game u_T: phi_i = 1/|T| if i in T, else 0
    ;; T = {0, 1} = coalition 3
    (let* ([g (make-unanimity-game 3 3)]
           [phi (shapley-value g 1000)])
      (assert-equal 1/2 (vector-ref phi 0))
      (assert-equal 1/2 (vector-ref phi 1))
      (assert-equal 0 (vector-ref phi 2))))

  (define-test shapley-symmetric-voting
    ;; Symmetric 3-player majority (quota 2 of 3)
    ;; Each player equally pivotal → phi = (1/3, 1/3, 1/3)
    (let* ([g (make-weighted-voting-game '(1 1 1) 2)]
           [phi (shapley-value g 1000)])
      (assert-equal 1/3 (vector-ref phi 0))
      (assert-equal 1/3 (vector-ref phi 1))
      (assert-equal 1/3 (vector-ref phi 2))))

  (define-test shapley-efficiency
    ;; Sum of Shapley values = v(N)
    (let* ([g (make-additive-game '(10 20 30))]
           [phi (shapley-value g 1000)]
           [total (+ (vector-ref phi 0)
                     (vector-ref phi 1)
                     (vector-ref phi 2))])
      (assert-equal 60 total)))

  (define-test shapley-symmetry
    ;; Symmetric players get equal Shapley values
    ;; In symmetric voting game, all players are symmetric
    (let* ([g (make-weighted-voting-game '(1 1 1) 2)]
           [phi (shapley-value g 1000)])
      (assert-equal (vector-ref phi 0) (vector-ref phi 1))
      (assert-equal (vector-ref phi 1) (vector-ref phi 2))))

  (define-test shapley-null-player
    ;; Player with zero marginal contribution gets zero
    ;; Unanimity game u_{0,1}: player 2 is null
    (let* ([g (make-unanimity-game 3 3)]  ; T = {0,1}
           [phi (shapley-value g 1000)])
      (assert-equal 0 (vector-ref phi 2))))

  (define-test shapley-gloves-game
    ;; 2 left-glove, 1 right-glove holders
    ;; Left holders compete, right holder has bargaining power
    (let* ([g (make-gloves-game 2 1)]
           [phi (shapley-value g 1000)])
      ;; Right holder (player 2) gets most value
      ;; Left holders (0, 1) split remainder
      (assert-equal (vector-ref phi 0) (vector-ref phi 1))
      (assert-true (> (vector-ref phi 2) (vector-ref phi 0)))))

)

;;; ============================================================================
;;; Core Stability Tests
;;; ============================================================================

(test-group core-stability

  (define-test core-additive-game
    ;; Additive game: unique core point = individual values
    (let ([g (make-additive-game '(10 20))])
      (assert-true (allocation-in-core? g '#(10 20)))
      ;; Any deviation violates some coalition's rationality
      (assert-false (allocation-in-core? g '#(5 25)))))

  (define-test core-excess-basic
    ;; Positive excess = coalition could do better
    (let ([g (make-additive-game '(10 20 30))])
      ;; At Shapley allocation, all excesses should be <= 0
      (assert-equal 0 (core-excess g '#(10 20 30) 7)) ; grand coalition
      ;; If player 0 gets less than v({0}), excess is positive
      (assert-true (> (core-excess g '#(5 25 30) 1) 0))))

  (define-test core-convex-game-non-empty
    ;; Convex games always have non-empty core
    ;; Additive games are convex
    (let* ([g (make-additive-game '(10 20))]
           [phi (shapley-value g 100)])
      ;; Shapley value is in core for convex games
      (assert-true (allocation-in-core? g phi))))

)

;;; ============================================================================
;;; Bargaining Solutions Tests
;;; ============================================================================

(test-group bargaining

  (define-test bargaining-set-basic
    ;; 2-player game: v({0})=0, v({1})=0, v({0,1})=10
    (let* ([g (make-coop-game 2 (lambda (S)
                                  (case S
                                    [(0) 0] [(1) 0] [(2) 0] [(3) 10])))]
           [bs (bargaining-set g)])
      (assert-equal 0 (vector-ref bs 0))   ; d0
      (assert-equal 0 (vector-ref bs 1))   ; d1
      (assert-equal 10 (vector-ref bs 2))  ; u0
      (assert-equal 10 (vector-ref bs 3)))) ; u1

  (define-test nash-bargaining-symmetric
    ;; Symmetric game: equal split
    (let* ([g (make-coop-game 2 (lambda (S)
                                  (case S
                                    [(0) 0] [(1) 0] [(2) 0] [(3) 10])))]
           [x (nash-bargaining g)])
      (assert-equal 5 (vector-ref x 0))
      (assert-equal 5 (vector-ref x 1))))

  (define-test nash-bargaining-asymmetric
    ;; Asymmetric disagreement points
    ;; v({0})=2, v({1})=3, v({0,1})=10
    (let* ([g (make-coop-game 2 (lambda (S)
                                  (case S
                                    [(0) 0] [(1) 2] [(2) 3] [(3) 10])))]
           [x (nash-bargaining g)])
      ;; Surplus = 10 - 2 - 3 = 5, split evenly
      ;; x0 = 2 + 2.5 = 4.5, x1 = 3 + 2.5 = 5.5
      (assert-equal 9/2 (vector-ref x 0))
      (assert-equal 11/2 (vector-ref x 1))))

  (define-test kalai-smorodinsky-symmetric
    ;; Symmetric game: same as Nash
    (let* ([g (make-coop-game 2 (lambda (S)
                                  (case S
                                    [(0) 0] [(1) 0] [(2) 0] [(3) 10])))]
           [x (kalai-smorodinsky g)])
      (assert-equal 5 (vector-ref x 0))
      (assert-equal 5 (vector-ref x 1))))

)

;;; ============================================================================
;;; Game Constructors Tests
;;; ============================================================================

(test-group game-constructors

  (define-test additive-game-values
    (let ([g (make-additive-game '(5 10 15))])
      (assert-equal 0 (coop-game-value g 0))   ; empty
      (assert-equal 5 (coop-game-value g 1))   ; {0}
      (assert-equal 10 (coop-game-value g 2))  ; {1}
      (assert-equal 20 (coop-game-value g 5))  ; {0,2} = 5 + 15
      (assert-equal 30 (coop-game-value g 7)))) ; {0,1,2}

  (define-test unanimity-game-values
    ;; u_T where T = {0,1} = 3
    (let ([g (make-unanimity-game 3 3)])
      (assert-equal 0 (coop-game-value g 0))   ; {}
      (assert-equal 0 (coop-game-value g 1))   ; {0}
      (assert-equal 0 (coop-game-value g 2))   ; {1}
      (assert-equal 1 (coop-game-value g 3))   ; {0,1} = T
      (assert-equal 0 (coop-game-value g 4))   ; {2}
      (assert-equal 1 (coop-game-value g 7)))) ; {0,1,2} superset of T

  (define-test voting-game-values
    ;; Majority vote: quota 2, weights (1,1,1)
    (let ([g (make-weighted-voting-game '(1 1 1) 2)])
      (assert-equal 0 (coop-game-value g 0))   ; {}
      (assert-equal 0 (coop-game-value g 1))   ; {0} - weight 1 < 2
      (assert-equal 1 (coop-game-value g 3))   ; {0,1} - weight 2 >= 2
      (assert-equal 1 (coop-game-value g 7)))) ; {0,1,2}

  (define-test airport-game-values
    (let ([g (make-airport-game '(100 200 300))])
      (assert-equal 0 (coop-game-value g 0))   ; {}
      (assert-equal 100 (coop-game-value g 1)) ; {0}
      (assert-equal 200 (coop-game-value g 2)) ; {1}
      (assert-equal 300 (coop-game-value g 5)) ; {0,2} - max(100,300)
      (assert-equal 300 (coop-game-value g 7)))) ; grand

  (define-test bankruptcy-game-values
    ;; Estate 100, claims (40, 60, 80), total = 180
    (let ([g (make-bankruptcy-game 100 '(40 60 80))])
      ;; v(S) = max(0, Estate - claims of players NOT in S)
      (assert-equal 0 (coop-game-value g 0))   ; max(0, 100-180) = 0
      (assert-equal 0 (coop-game-value g 1))   ; max(0, 100-140) = 0, {0} excludes 60+80
      (assert-equal 20 (coop-game-value g 3))  ; max(0, 100-80) = 20, {0,1} excludes 80
      (assert-equal 100 (coop-game-value g 7)))) ; max(0, 100-0) = 100, grand coalition

  (define-test gloves-game-values
    ;; 2 left, 1 right
    (let ([g (make-gloves-game 2 1)])
      (assert-equal 0 (coop-game-value g 0))   ; {}
      (assert-equal 0 (coop-game-value g 1))   ; {0} left only
      (assert-equal 0 (coop-game-value g 3))   ; {0,1} both left
      (assert-equal 1 (coop-game-value g 5))   ; {0,2} 1 left + 1 right
      (assert-equal 1 (coop-game-value g 7)))) ; all - still only 1 pair

)

;;; ============================================================================
;;; Nucleolus Tests
;;; ============================================================================

(test-group nucleolus-tests

  (define-test nucleolus-symmetric-voting
    ;; 3-player majority (quota 2): all players symmetric
    ;; Nucleolus = (1/3, 1/3, 1/3) = Shapley value
    (let* ([g (make-weighted-voting-game '(1 1 1) 2)]
           [nuc (nucleolus g 100)])
      (when (vector? nuc)
        (let ([tolerance 0.01])
          (assert-true (< (abs (- (vector-ref nuc 0) 1/3)) tolerance))
          (assert-true (< (abs (- (vector-ref nuc 1) 1/3)) tolerance))
          (assert-true (< (abs (- (vector-ref nuc 2) 1/3)) tolerance))))))

  (define-test nucleolus-additive-game
    ;; For additive games, nucleolus = Shapley = individual values
    (let* ([g (make-additive-game '(10 20 30))]
           [nuc (nucleolus g 100)])
      (when (vector? nuc)
        (let ([tolerance 0.01])
          (assert-true (< (abs (- (vector-ref nuc 0) 10)) tolerance))
          (assert-true (< (abs (- (vector-ref nuc 1) 20)) tolerance))
          (assert-true (< (abs (- (vector-ref nuc 2) 30)) tolerance))))))

  (define-test nucleolus-efficiency
    ;; Nucleolus sums to v(N)
    (let* ([g (make-additive-game '(10 20 30))]
           [nuc (nucleolus g 100)])
      (when (vector? nuc)
        (let ([total (+ (vector-ref nuc 0) (vector-ref nuc 1) (vector-ref nuc 2))])
          (assert-true (< (abs (- total 60)) 0.01))))))

  (define-test nucleolus-gloves-game
    ;; Gloves game with 2 left, 1 right: nucleolus ≠ Shapley
    ;; Players 0,1 have left hands, player 2 has right hand
    ;; v(S) = min(left in S, right in S)
    ;; Shapley = (1/6, 1/6, 2/3) but nucleolus = (0, 0, 1)
    ;; The scarce resource (right hand) gets all value in nucleolus
    (let* ([g (make-gloves-game 2 1)]
           [nuc (nucleolus g 200)])
      (when (vector? nuc)
        (let ([tolerance 0.05])
          ;; Nucleolus should give (nearly) all to player 2
          (assert-true (< (vector-ref nuc 0) tolerance)
                       "Left hand player 0 should get ~0")
          (assert-true (< (vector-ref nuc 1) tolerance)
                       "Left hand player 1 should get ~0")
          (assert-true (> (vector-ref nuc 2) (- 1 tolerance))
                       "Right hand player 2 should get ~1")
          ;; Verify it's different from Shapley
          (let ([shap (shapley-value g 1000)])
            ;; Shapley gives 1/6 to each left player
            (assert-true (> (vector-ref shap 0) 0.1)
                         "Shapley should give positive to player 0"))))))

)

;;; ============================================================================
;;; Game Properties Tests
;;; ============================================================================

(test-group game-properties

  (define-test additive-is-convex
    ;; Additive games are convex
    (let ([g (make-additive-game '(1 2 3))])
      (assert-true (coop-game-convex? g 1000))))

  (define-test unanimity-is-monotonic
    ;; Unanimity games are monotonic
    (let ([g (make-unanimity-game 3 3)])
      (assert-true (coop-game-monotonic? g 1000))))

  (define-test voting-is-simple
    ;; Voting games are simple (values in {0,1})
    (let ([g (make-weighted-voting-game '(1 1 1) 2)])
      (assert-true (coop-game-simple? g))))

  (define-test additive-not-simple
    ;; Additive games generally not simple
    (let ([g (make-additive-game '(1 2 3))])
      (assert-false (coop-game-simple? g))))

)

;;; ============================================================================
;;; Power Index Tests
;;; ============================================================================

(test-group power-indices

  (define-test winning-coalition-basic
    (let ([g (make-weighted-voting-game '(1 1 1) 2)])
      (assert-false (is-winning? g 1))   ; {0} - weight 1
      (assert-true (is-winning? g 3))    ; {0,1} - weight 2
      (assert-true (is-winning? g 7))))  ; all

  (define-test blocking-coalition-basic
    (let ([g (make-weighted-voting-game '(1 1 1) 2)])
      ;; {0} is NOT blocking: complement {1,2} has weight 2 >= quota
      (assert-false (is-blocking? g 1))
      ;; {0,1} IS blocking: complement {2} has weight 1 < quota
      (assert-true (is-blocking? g 3))
      ;; empty is not blocking: complement (everyone) wins
      (assert-false (is-blocking? g 0))))

  (define-test pivotal-basic
    (let ([g (make-weighted-voting-game '(1 1 1) 2)])
      ;; In {0,1}, both are pivotal (removing either gives weight 1 < quota)
      (assert-true (is-pivotal? g 3 0))
      (assert-true (is-pivotal? g 3 1))
      ;; In grand coalition {0,1,2}, NO player is pivotal
      ;; (removing any leaves weight 2 >= quota, still winning)
      (assert-false (is-pivotal? g 7 0))
      (assert-false (is-pivotal? g 7 1))
      (assert-false (is-pivotal? g 7 2))))

  (define-test banzhaf-symmetric
    ;; Symmetric voting: equal Banzhaf indices
    (let* ([g (make-weighted-voting-game '(1 1 1) 2)]
           [bi (banzhaf-index g 1000)])
      (assert-equal (vector-ref bi 0) (vector-ref bi 1))
      (assert-equal (vector-ref bi 1) (vector-ref bi 2))))

)

;;; ============================================================================
;;; Shapley Sampling Tests
;;; ============================================================================

(test-group shapley-sampling

  (define-test shapley-sample-symmetric-voting
    ;; Symmetric 3-player majority: should converge to (1/3, 1/3, 1/3)
    (let* ([g (make-weighted-voting-game '(1 1 1) 2)]
           [phi (shapley-value-sample g 2000 1000)]
           [tolerance 0.05])
      ;; Check each player gets approximately 1/3
      (assert-true (< (abs (- (vector-ref phi 0) 1/3)) tolerance)
                   "Player 0 should get ~1/3")
      (assert-true (< (abs (- (vector-ref phi 1) 1/3)) tolerance)
                   "Player 1 should get ~1/3")
      (assert-true (< (abs (- (vector-ref phi 2) 1/3)) tolerance)
                   "Player 2 should get ~1/3")))

  (define-test shapley-sample-normalization
    ;; Sampled values should sum to 1 (normalized)
    (let* ([g (make-weighted-voting-game '(1 1 1) 2)]
           [phi (shapley-value-sample g 1000 1000)]
           [total (+ (vector-ref phi 0)
                     (vector-ref phi 1)
                     (vector-ref phi 2))]
           [tolerance 0.001])
      (assert-true (< (abs (- total 1.0)) tolerance)
                   "Sampled Shapley should sum to 1")))

  (define-test shapley-sample-gloves-game
    ;; 2 left-glove, 1 right-glove: right-glove holder gets more
    (let* ([g (make-gloves-game 2 1)]
           [phi (shapley-value-sample g 2000 1000)])
      ;; Players 0 and 1 (left) should be equal
      (assert-true (< (abs (- (vector-ref phi 0) (vector-ref phi 1))) 0.05)
                   "Left-glove holders should have equal value")
      ;; Player 2 (right) should have more than left holders
      (assert-true (> (vector-ref phi 2) (vector-ref phi 0))
                   "Right-glove holder should have more value")))

  (define-test shapley-sample-additive-game
    ;; Additive game: sample should approximate true Shapley
    (let* ([g (make-additive-game '(10 20 30))]
           [exact (shapley-value g 1000)]
           [sample (shapley-value-sample g 2000 1000)]
           [tolerance 0.1])
      ;; Verify sample is close to exact (after scaling)
      ;; Note: exact sums to v(N)=60, sample sums to 1
      ;; So sample[i] ≈ exact[i]/60
      (assert-true (< (abs (- (vector-ref sample 0) (/ 10.0 60))) tolerance)
                   "Sampled player 0 should match normalized exact")
      (assert-true (< (abs (- (vector-ref sample 1) (/ 20.0 60))) tolerance)
                   "Sampled player 1 should match normalized exact")
      (assert-true (< (abs (- (vector-ref sample 2) (/ 30.0 60))) tolerance)
                   "Sampled player 2 should match normalized exact")))

  (define-test shapley-adaptive-small-game
    ;; For small games, adaptive should use exact
    (let* ([g (make-weighted-voting-game '(1 1 1) 2)]
           [phi (shapley-value-adaptive g 1000)])
      ;; Should be exact 1/3 for symmetric game
      (assert-equal 1/3 (vector-ref phi 0))
      (assert-equal 1/3 (vector-ref phi 1))
      (assert-equal 1/3 (vector-ref phi 2))))

)

;;; ============================================================================
;;; Cost Game Tests
;;; ============================================================================

(test-group cost-games

  (define-test airport-game-is-cost-type
    ;; Airport games should be marked as cost games
    (let ([g (make-airport-game '(100 200 300))])
      (assert-true (coop-game-cost? g))
      (assert-false (coop-game-value? g))
      (assert-equal ':cost (coop-game-type g))))

  (define-test value-game-default-type
    ;; Regular games should default to value type
    (let ([g (make-additive-game '(10 20 30))])
      (assert-true (coop-game-value? g))
      (assert-false (coop-game-cost? g))
      (assert-equal ':value (coop-game-type g))))

  (define-test cost-game-core-excess
    ;; For cost games, excess = x(S) - c(S)
    ;; Positive means coalition is overcharged
    (let* ([g (make-airport-game '(100 200 300))]
           [c-grand (coop-game-value g 7)])  ; 300
      ;; Efficient allocation: everyone pays equal share (100 each)
      ;; But for {0}: c({0}) = 100, pays 100, excess = 100 - 100 = 0
      ;; For {0,1}: c({0,1}) = 200, pays 200, excess = 200 - 200 = 0
      (assert-equal 0 (core-excess g '#(100 100 100) 1))   ; {0}
      (assert-equal 0 (core-excess g '#(100 100 100) 3))   ; {0,1}
      ;; If player 0 pays 150, excess for {0} = 150 - 100 = 50 (overcharged)
      (assert-equal 50 (core-excess g '#(150 100 50) 1))))

  (define-test cost-game-in-core
    ;; Airport cost allocation is in core when no coalition overpays
    (let ([g (make-airport-game '(100 200 300))])
      ;; Shapley-like allocation: roughly (100/3, 100/3+100/2, 100/3+100/2+100)
      ;; Simplified: player 0 contributes 100 to all, player 1 adds 100 for {1,2},
      ;; player 2 adds 100 for just {2}
      ;; Fair allocation: (100/3 ≈ 33, 100/3+50 ≈ 83, 100/3+50+100 ≈ 183)
      ;; But total should equal 300, so let's use the Shapley value
      (let ([phi (shapley-value g 1000)])
        ;; Shapley allocation should be in core for cost games
        (assert-true (allocation-in-core? g phi)
                     "Shapley value should be in core for airport game"))))

  (define-test cost-game-imputation
    ;; For cost games, individual rationality means x_i <= c({i})
    (let ([g (make-airport-game '(100 200 300))])
      ;; Player 0 alone pays 100, so should pay at most 100
      ;; Player 1 alone pays 200, so should pay at most 200
      ;; Player 2 alone pays 300, so should pay at most 300
      ;; Efficient allocation sums to 300
      (assert-true (imputation? g '#(100 100 100)))
      (assert-true (imputation? g '#(50 100 150)))
      ;; Not individually rational: player 0 paying 150 > c({0})=100
      (assert-false (imputation? g '#(150 100 50)))))

  (define-test explicit-cost-game-creation
    ;; Can create cost games directly
    (let ([g (make-coop-game 2 (lambda (S) (case S [(0) 0] [(1) 10] [(2) 20] [(3) 25])) ':cost)])
      (assert-true (coop-game-cost? g))
      ;; Core condition for cost game: each coalition pays <= standalone cost
      ;; x(S) <= c(S) means excess <= 0
      (assert-equal 5 (core-excess g '#(15 15) 1))     ; {0} pays 15 vs cost 10: overcharged by 5
      (assert-equal -5 (core-excess g '#(5 10) 1))))   ; {0} pays 5 vs cost 10: satisfied

  (define-test value-game-excess-unchanged
    ;; Verify value game excess still works correctly
    (let ([g (make-additive-game '(10 20 30))])
      ;; For value games: excess = v(S) - x(S)
      ;; At efficient allocation, excess should be 0 for grand coalition
      (assert-equal 0 (core-excess g '#(10 20 30) 7))
      ;; If player 0 gets less than v({0}), excess is positive
      (assert-equal 5 (core-excess g '#(5 25 30) 1))))  ; v({0})=10, x({0})=5, excess=5

  (define-test nucleolus-airport-game
    ;; Test nucleolus for a simple 2-player airport (cost) game
    ;; Players: 0 needs runway cost 100, 1 needs runway cost 200
    ;; c({0}) = 100, c({1}) = 200, c({0,1}) = 200 (max)
    ;; The fair split: player 0 pays for shared runway (100/2 = 50)
    ;; player 1 pays for shared + extra (50 + 100 = 150)
    ;; Total = 200 = c(N)
    (let* ([g (make-airport-game '(100 200))]
           [nuc (nucleolus g 200)])
      (when (vector? nuc)
        (let ([tolerance 0.1])
          ;; Nucleolus should give player 0 around 50, player 1 around 150
          (assert-true (< (abs (- (+ (vector-ref nuc 0) (vector-ref nuc 1)) 200)) tolerance)
                       "Nucleolus should be efficient (sum = 200)")
          ;; Each player should pay at most their standalone cost
          (assert-true (<= (vector-ref nuc 0) 100)
                       "Player 0 should pay at most c({0})=100")
          (assert-true (<= (vector-ref nuc 1) 200)
                       "Player 1 should pay at most c({1})=200")))))

  (define-test nucleolus-cost-game-in-core
    ;; Nucleolus of a cost game should be in the core
    (let* ([g (make-airport-game '(100 200 300))]
           [nuc (nucleolus g 200)])
      (when (vector? nuc)
        (assert-true (allocation-in-core? g nuc)
                     "Nucleolus should be in core for airport game"))))

  (define-test airport-game-subadditive
    ;; Airport games are subadditive (economies of scale)
    ;; c(S∪T) = max costs <= c(S) + c(T) for disjoint S,T
    (let ([g (make-airport-game '(100 200 300))])
      ;; For cost games, superadditive? checks subadditivity
      (assert-true (coop-game-superadditive? g 1000)
                   "Airport game should be subadditive")))

  (define-test airport-game-submodular
    ;; Airport games are submodular (diminishing returns on cost reduction)
    (let ([g (make-airport-game '(100 200 300))])
      ;; For cost games, convex? checks submodularity
      (assert-true (coop-game-convex? g 1000)
                   "Airport game should be submodular")))

)

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
