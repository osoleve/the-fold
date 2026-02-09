(load "core/testing/test-framework.ss")
(load "lattice/game-theory/evolutionary.ss")

(doc 'module 'test-evolutionary)
(doc 'description "Tests for evolutionary game theory")

;;; ============================================================================
;;; Symmetric Game Tests
;;; ============================================================================

(test-group "symmetric-games"
  (define-test "make-symmetric-game creates game"
    (let ([g (make-symmetric-game '(a b) '((1 2) (3 4)))])
      (assert-true (symmetric-game? g))
      (assert-equal 2 (sg-num-strategies g))))

  (define-test "sg-payoff retrieves correct values"
    (let ([g (make-symmetric-game '(a b) '((1 2) (3 4)))])
      (assert-equal 1 (sg-payoff g 0 0))
      (assert-equal 2 (sg-payoff g 0 1))
      (assert-equal 3 (sg-payoff g 1 0))
      (assert-equal 4 (sg-payoff g 1 1))))

  (define-test "sg-strategy-name returns correct name"
    (let ([g (make-symmetric-game '(rock paper scissors) '((0 0 0) (0 0 0) (0 0 0)))])
      (assert-equal 'rock (sg-strategy-name g 0))
      (assert-equal 'paper (sg-strategy-name g 1))
      (assert-equal 'scissors (sg-strategy-name g 2)))))

;;; ============================================================================
;;; Population State Tests
;;; ============================================================================

(test-group "population"
  (define-test "make-population normalizes"
    (let ([pop (make-population '(1 1 1))])
      (assert-true (< (abs (- (pop-freq pop 0) 1/3)) 0.001))
      (assert-true (< (abs (- (pop-freq pop 1) 1/3)) 0.001))
      (assert-true (< (abs (- (pop-freq pop 2) 1/3)) 0.001))))

  (define-test "uniform-population creates equal weights"
    (let ([pop (uniform-population 4)])
      (assert-equal 4 (pop-size pop))
      (assert-true (< (abs (- (pop-freq pop 0) 0.25)) 0.001))
      (assert-true (< (abs (- (pop-freq pop 3) 0.25)) 0.001))))

  (define-test "pure-population puts all mass on one strategy"
    (let ([pop (pure-population 3 1)])
      (assert-equal 0 (pop-freq pop 0))
      (assert-equal 1 (pop-freq pop 1))
      (assert-equal 0 (pop-freq pop 2))))

  (define-test "pop->list converts correctly"
    (let ([pop (make-population '(2 1 1))])
      (assert-equal 3 (length (pop->list pop)))
      (assert-true (< (abs (- (car (pop->list pop)) 0.5)) 0.001)))))

;;; ============================================================================
;;; Fitness Tests
;;; ============================================================================

(test-group "fitness"
  (define-test "strategy-fitness against pure population"
    ;; In Hawk-Dove: hawk vs hawk gets -1, hawk vs dove gets 2
    (let ([pure-hawk (pure-population 2 0)]
          [pure-dove (pure-population 2 1)])
      ;; Hawk fitness vs all hawks = -1
      (assert-equal -1 (strategy-fitness hawk-dove-game pure-hawk 0))
      ;; Hawk fitness vs all doves = 2
      (assert-equal 2 (strategy-fitness hawk-dove-game pure-dove 0))
      ;; Dove fitness vs all hawks = 0
      (assert-equal 0 (strategy-fitness hawk-dove-game pure-hawk 1))
      ;; Dove fitness vs all doves = 1
      (assert-equal 1 (strategy-fitness hawk-dove-game pure-dove 1))))

  (define-test "average-fitness computation"
    (let ([pop (make-population '(1 1))])  ; 50-50 hawk-dove
      ;; Hawk fitness: 0.5*(-1) + 0.5*2 = 0.5
      ;; Dove fitness: 0.5*0 + 0.5*1 = 0.5
      ;; Average: 0.5*0.5 + 0.5*0.5 = 0.5
      (assert-true (< (abs (- (average-fitness hawk-dove-game pop) 0.5)) 0.001))))

  (define-test "all-fitnesses returns list"
    (let ([pop (uniform-population 2)]
          [fits (all-fitnesses hawk-dove-game (uniform-population 2))])
      (assert-equal 2 (length fits)))))

;;; ============================================================================
;;; Replicator Dynamics Tests
;;; ============================================================================

(test-group "replicator-dynamics"
  (define-test "replicator-derivative at equilibrium is near zero"
    ;; At the mixed ESS of Hawk-Dove, derivatives should be near zero
    ;; For standard Hawk-Dove (V=2, C=4), ESS is 50% Hawk
    (let* ([ess-pop (make-population '(1 1))]  ; 50-50
           [derivs (replicator-derivative hawk-dove-game ess-pop)])
      (assert-true (< (abs (car derivs)) 0.001))
      (assert-true (< (abs (cadr derivs)) 0.001))))

  (define-test "replicator-step maintains population validity"
    (let* ([pop (make-population '(0.3 0.7))]
           [next (replicator-step hawk-dove-game pop 0.1)]
           [freqs (pop->list next)]
           [total (apply + freqs)])
      ;; Should still sum to ~1
      (assert-true (< (abs (- total 1.0)) 0.01))
      ;; All non-negative
      (assert-true (>= (car freqs) 0))
      (assert-true (>= (cadr freqs) 0))))

  (define-test "replicator-trajectory returns correct length"
    (let ([traj (replicator-trajectory hawk-dove-game
                                        (make-population '(0.1 0.9))
                                        0.1
                                        10)])
      ;; Initial + 10 steps = 11 populations
      (assert-equal 11 (length traj))))

  (define-test "replicator dynamics moves toward ESS"
    ;; Starting from mostly doves, hawks should increase
    (let* ([pop (make-population '(0.1 0.9))]
           [derivs (replicator-derivative hawk-dove-game pop)])
      ;; Hawk derivative should be positive (hawks growing)
      (assert-true (> (car derivs) 0)))))

;;; ============================================================================
;;; ESS Tests
;;; ============================================================================

(test-group "ess"
  (define-test "Prisoner's Dilemma: Defect is ESS"
    ;; In PD, Defect strictly dominates, so it's the unique ESS
    (assert-true (is-ess? pd-symmetric 1))
    (assert-false (is-ess? pd-symmetric 0)))

  (define-test "Stag Hunt: Both strategies are ESS"
    ;; Stag Hunt has two pure ESS (coordination game)
    (let ([ess-list (find-all-ess stag-hunt-symmetric)])
      (assert-equal 2 (length ess-list))))

  (define-test "Rock-Paper-Scissors: No pure ESS"
    ;; RPS is completely symmetric, no pure strategy is stable
    (let ([ess-list (find-all-ess rps-game)])
      (assert-equal 0 (length ess-list))))

  (define-test "Hawk-Dove: No pure ESS"
    ;; Hawk-Dove has a mixed ESS, no pure ESS
    (let ([ess-list (find-all-ess hawk-dove-game)])
      (assert-equal 0 (length ess-list)))))

;;; ============================================================================
;;; Nash Equilibrium Tests
;;; ============================================================================

(test-group "nash-equilibrium"
  (define-test "is-nash-equilibrium? for PD"
    ;; Defect is the only Nash equilibrium in PD
    (assert-true (is-nash-equilibrium? pd-symmetric 1))
    (assert-false (is-nash-equilibrium? pd-symmetric 0)))

  (define-test "Stag Hunt has two pure Nash equilibria"
    (let ([nash (find-pure-nash stag-hunt-symmetric)])
      (assert-equal 2 (length nash))))

  (define-test "RPS has no pure Nash equilibrium"
    (let ([nash (find-pure-nash rps-game)])
      (assert-equal 0 (length nash)))))

;;; ============================================================================
;;; Invasion Tests
;;; ============================================================================

(test-group "invasion"
  (define-test "can-invade? in PD"
    ;; Defectors can invade cooperators
    (assert-true (can-invade? pd-symmetric 0 1))  ; D invades C
    ;; Cooperators cannot invade defectors
    (assert-false (can-invade? pd-symmetric 1 0)))  ; C cannot invade D

  (define-test "invasion-gradient positive when can invade"
    ;; Small epsilon, mutant should have positive advantage
    (let ([grad (invasion-gradient pd-symmetric 0 1 0.01)])
      (assert-true (> grad 0))))

  (define-test "hawk-dove mutual invasion"
    ;; In Hawk-Dove, each can invade the other
    (assert-true (can-invade? hawk-dove-game 0 1))  ; Dove invades Hawks
    (assert-true (can-invade? hawk-dove-game 1 0)))) ; Hawk invades Doves

;;; ============================================================================
;;; Mixed Strategy Tests
;;; ============================================================================

(test-group "mixed-strategies"
  (define-test "hawk-dove-mixed-ess computes V/C"
    ;; Standard Hawk-Dove: V=2, C=4 → ESS = 0.5
    (assert-true (< (abs (- (hawk-dove-mixed-ess 2 4) 0.5)) 0.001))
    ;; V=3, C=6 → ESS = 0.5
    (assert-true (< (abs (- (hawk-dove-mixed-ess 3 6) 0.5)) 0.001))
    ;; V=1, C=4 → ESS = 0.25
    (assert-true (< (abs (- (hawk-dove-mixed-ess 1 4) 0.25)) 0.001)))

  (define-test "interior equilibrium detection"
    ;; At 50-50 in Hawk-Dove, fitnesses should be equal
    (let ([pop (make-population '(0.5 0.5))])
      (assert-true (is-interior-equilibrium? hawk-dove-game pop 0.01)))))

;;; ============================================================================
;;; Classic Games Verification
;;; ============================================================================

(test-group "classic-games"
  (define-test "hawk-dove payoffs correct"
    ;; Hawk vs Hawk = -1 (= (V-C)/2 = (2-4)/2)
    (assert-equal -1 (sg-payoff hawk-dove-game 0 0))
    ;; Hawk vs Dove = 2 (= V)
    (assert-equal 2 (sg-payoff hawk-dove-game 0 1))
    ;; Dove vs Hawk = 0
    (assert-equal 0 (sg-payoff hawk-dove-game 1 0))
    ;; Dove vs Dove = 1 (= V/2)
    (assert-equal 1 (sg-payoff hawk-dove-game 1 1)))

  (define-test "pd-symmetric payoffs correct"
    ;; Standard PD: T > R > P > S where T=5, R=3, P=1, S=0
    (assert-equal 3 (sg-payoff pd-symmetric 0 0))  ; R: mutual cooperation
    (assert-equal 0 (sg-payoff pd-symmetric 0 1))  ; S: sucker's payoff
    (assert-equal 5 (sg-payoff pd-symmetric 1 0))  ; T: temptation
    (assert-equal 1 (sg-payoff pd-symmetric 1 1))) ; P: mutual defection

  (define-test "rps-game is zero-sum"
    ;; Check diagonal is 0 and matrix is antisymmetric
    (assert-equal 0 (sg-payoff rps-game 0 0))
    (assert-equal 0 (sg-payoff rps-game 1 1))
    (assert-equal 0 (sg-payoff rps-game 2 2))
    (assert-equal (- (sg-payoff rps-game 0 1))
                  (sg-payoff rps-game 1 0))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
