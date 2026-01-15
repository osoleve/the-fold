((name "game")
 (description "Game theory foundations including normal form games,
  cooperative games, matching theory, Nash equilibrium computation,
  Shapley value, and strategic analysis tools.")

 (modules
  ((file "normal-form.ss")
   (purpose "Normal form (strategic form) games and equilibrium")
   (exports
    (make-player "Create a player with name and strategy set")
    (player-name "Get player name")
    (player-strategies "Get available strategies")
    (make-game "Create a game from players and payoff function")
    (game-players "Get list of players")
    (game-payoff "Get payoff for a strategy profile")
    (payoff-matrix "Generate full payoff matrix for 2-player game")
    (best-response "Find best response to opponent strategy")
    (best-responses "Find all best responses (may be multiple)")
    (is-nash-equilibrium? "Check if strategy profile is Nash equilibrium")
    (find-pure-nash "Find all pure strategy Nash equilibria")
    (dominated-strategies "Find dominated strategies for a player")
    (iterated-elimination "Eliminate dominated strategies iteratively")
    (mixed-strategy "Create a probability distribution over strategies")
    (expected-payoff "Compute expected payoff under mixed strategies")
    (support "Get strategies with positive probability in mixed strategy"))
   (example
    ";; Prisoner's Dilemma
     (define p1 (make-player 'alice '(cooperate defect)))
     (define p2 (make-player 'bob '(cooperate defect)))
     (define pd-payoffs
       '(((cooperate cooperate) (3 3))
         ((cooperate defect) (0 5))
         ((defect cooperate) (5 0))
         ((defect defect) (1 1))))
     (define pd (make-game (list p1 p2)
                           (lambda (profile)
                             (assoc-ref pd-payoffs profile))))
     (find-pure-nash pd)
     ;; => ((defect defect))"))

  ((file "coop-games.ss")
   (purpose "Cooperative (coalitional) game theory with TU games")
   (exports
    ;; Coalition operations
    (coalition-empty "Empty coalition (no players)")
    (coalition-singleton "Coalition with single player")
    (coalition-member? "Check if player is in coalition")
    (coalition-union "Union of two coalitions")
    (coalition-intersection "Intersection of coalitions")
    (coalition-complement "Complement w.r.t. grand coalition")
    (coalition-size "Number of players in coalition")
    (coalition->list "Convert coalition to player list")
    (list->coalition "Convert player list to coalition")
    (all-coalitions "Generate all 2^n coalitions")
    ;; Game types
    (make-coop-game "Create cooperative game from n and v")
    (coop-game? "Check if x is a cooperative game")
    (coop-game-players "Get number of players")
    (coop-game-value "Get v(S) for coalition S")
    (coop-game-grand-coalition "Get grand coalition N")
    ;; Allocations and imputations
    (allocation-total "Sum of all payoffs")
    (allocation-coalition-total "Sum of payoffs for coalition")
    (imputation? "Check if allocation is an imputation")
    ;; Solution concepts
    (shapley-value "Compute Shapley value (fair allocation)")
    (allocation-in-core? "Check if allocation is in core")
    (core-excess "Compute excess for coalition")
    (nucleolus "Compute nucleolus via iterated LP")
    (nash-bargaining "Nash bargaining solution (2-player)")
    (kalai-smorodinsky "Kalai-Smorodinsky solution (2-player)")
    ;; Game constructors
    (make-additive-game "Additive game v(S) = sum of values")
    (make-unanimity-game "Unanimity game u_T")
    (make-weighted-voting-game "Weighted voting game")
    (make-airport-game "Airport cost game")
    (make-bankruptcy-game "Bankruptcy game")
    (make-gloves-game "Gloves market game")
    ;; Properties
    (coop-game-superadditive? "Check superadditivity")
    (coop-game-convex? "Check convexity")
    (coop-game-monotonic? "Check monotonicity")
    (coop-game-simple? "Check if simple (voting) game")
    ;; Power indices
    (is-winning? "Check if coalition is winning")
    (is-blocking? "Check if coalition is blocking")
    (is-pivotal? "Check if player is pivotal")
    (banzhaf-index "Compute Banzhaf power index"))
   (example
    ";; Airport game: 3 airlines with runway requirements
     (define airport (make-airport-game '(100 200 300)))
     (shapley-value airport 1000)
     ;; Fair cost allocation based on marginal contributions"))

  ((file "matching.ss")
   (purpose "Two-sided matching, stable matching, and assignment games")
   (exports
    ;; Market construction
    (make-matching-market "Create market from proposers, receivers, preferences")
    (matching-market? "Check if x is a matching market")
    (market-proposers "Get proposer vector")
    (market-receivers "Get receiver vector")
    (market-proposer-prefs "Get preferences for a proposer")
    (market-receiver-prefs "Get preferences for a receiver")
    ;; Stable matching
    (stable-match "Gale-Shapley deferred acceptance (proposer-optimal)")
    (stable-match-receiver-optimal "Receiver-optimal stable matching")
    (matching-stable? "Verify matching has no blocking pairs")
    (same-matched-agents? "Check if two matchings have same matched agents")
    ;; Assignment games
    (make-assignment-game "Create cooperative game from bipartite valuations")
    (optimal-assignment "Find max weight matching via LP")
    ;; Classic market constructors
    (make-medical-residency-market "NRMP-style medical residency market")
    (make-school-choice-market "School choice market"))
   (example
    ";; Classic stable matching problem
     (define market
       (make-matching-market
         '(m1 m2 m3)
         '(w1 w2 w3)
         '((m1 w1 w2 w3) (m2 w2 w1 w3) (m3 w1 w3 w2))
         '((w1 m2 m1 m3) (w2 m1 m2 m3) (w3 m1 m3 m2))))
     (stable-match market 100)
     ;; => proposer-optimal stable matching"))

  ((file "physics-dsl.ss")
   (purpose "Domain-specific language for physics simulations")
   (exports
    (make-body "Create a physical body with mass and position")
    (make-force "Create a force vector")
    (apply-force "Apply force to body, return updated body")
    (simulate-step "Advance simulation by one time step")
    (gravity "Gravitational force between two bodies"))))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("lattice/linalg/vec.ss" "Vector operations")
  ("lattice/linalg/matrix.ss" "Matrix operations")
  ("lattice/optimization/lp.ss" "Linear programming for nucleolus"))

 (notes
  "Game theory in The Fold covers non-cooperative (normal form),
   cooperative (coalitional), and matching theory games.

   Non-cooperative: Players choose simultaneously. Nash equilibrium is
   where no player can unilaterally improve. Focus on finite games
   with pure and mixed strategies.

   Cooperative: Players form binding coalitions. Key concepts are
   the Shapley value (fair division), core stability (no coalition
   wants to deviate), and bargaining solutions (2-player negotiations).
   Constraint: n <= 60 players (bitmask representation).

   Matching: Two-sided markets with preferences. Gale-Shapley (1962)
   produces stable matchings where no pair would prefer each other
   to current partners. Assignment games bridge matching and cooperative
   games - the core equals competitive equilibria (Shapley-Shubik 1971).
   Applications: medical residency (NRMP), school choice, kidney exchange.

   Note on cost games: make-airport-game produces a cost game where
   v(S) represents cost, not value. For cost games, 'fair allocation'
   means cost sharing; the standard core condition x(S) >= v(S) becomes
   'pay at least the standalone cost' which is the opposite intuition.
   Use Shapley value for cost allocation (proven fair for airport games).

   Test suites: 26 tests for normal-form, 45 tests for cooperative,
   34 tests for matching."))
