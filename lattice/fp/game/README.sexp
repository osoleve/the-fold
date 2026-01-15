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
    (gravity "Gravitational force between two bodies")))

  ((file "voting.ss")
   (purpose "Social choice and voting theory")
   (exports
    ;; Preference profiles
    (make-preference-profile "Create profile from list of rankings")
    (profile-voters "Number of voters in profile")
    (profile-num-candidates "Number of candidates")
    (profile-candidates "List of candidates")
    (profile-rankings "Get all voter rankings")
    ;; Positional scoring rules
    (plurality-winner "First-place plurality winner")
    (plurality-scores-all "Plurality scores for all candidates")
    (borda-winner "Borda count winner")
    (borda-scores-all "Borda scores for all candidates")
    (antiplurality-winner "Anti-plurality (veto) winner")
    (antiplurality-scores-all "Anti-plurality scores for all")
    ;; Condorcet methods
    (pairwise-margin "Head-to-head margin between two candidates")
    (pairwise-beats? "Does a beat b pairwise?")
    (condorcet-winner? "Is candidate a Condorcet winner?")
    (condorcet-winner "Find Condorcet winner or #f if none")
    (copeland-score "Copeland score (wins minus losses)")
    (copeland-winner "Copeland method winner")
    ;; Schulze method (beatpath)
    (schulze-strengths "Floyd-Warshall strongest beatpaths")
    (schulze-winner "Schulze method winner")
    (schulze-ranking "Complete Schulze ranking")
    ;; Examples
    (condorcet-cycle-example "Classic a>b>c, b>c>a, c>a>b cycle")
    ;; Manipulation
    (manipulation-possible? "Can voter improve outcome by misreporting?"))
   (example
    ";; Classic Condorcet paradox: cycling majorities
     (define profile (condorcet-cycle-example))
     (plurality-winner profile)   ; => 'a (tie-break to first)
     (condorcet-winner profile)   ; => #f (no Condorcet winner)
     (schulze-winner profile)     ; => deterministic resolution

     ;; Borda vs Plurality disagreement
     (define profile2 (make-preference-profile
                        '((a b c) (a b c) (b c a) (c b a))))
     (plurality-winner profile2)  ; => 'a
     (borda-winner profile2)      ; => 'b"))

  ((file "voting-games.ss")
   (purpose "Bridge between voting theory and cooperative games via power indices")
   (exports
    ;; Profile to game conversion
    (profile->majority-game "Convert profile to simple majority game")
    (profile->weighted-voting-game "Convert with custom voter weights")
    (profile->rule-induced-game "General rule-induced game (expensive)")
    ;; Power indices
    (shapley-shubik-index "Shapley-Shubik power index for voters")
    (shapley-shubik-weighted "Shapley-Shubik with custom weights")
    (banzhaf-voting-power "Normalized Banzhaf power index")
    (banzhaf-weighted "Banzhaf with custom weights")
    ;; Power analysis
    (voter-is-dictator? "Can voter alone determine outcome?")
    (voter-is-dummy? "Is voter never pivotal?")
    (voter-has-veto? "Can voter alone block any outcome?")
    (minimal-winning-coalitions "Find all minimal winning coalitions")
    (is-minimal-winning? "Check if coalition is minimal winning")
    ;; Power distribution metrics
    (power-concentration "Herfindahl-Hirschman Index of power")
    (power-gini "Gini coefficient of power distribution")
    ;; Strategic analysis
    (decisive-for-candidate "Coalitions that can elect a candidate")
    (blocking-for-candidate "Coalitions that can block a candidate")
    ;; Electoral college modeling
    (make-electoral-college-game "Weighted game from state electoral votes")
    (electoral-college-power "Banzhaf power for each state")
    (power-per-capita "Voting power per capita by state")
    ;; Examples
    (us-electoral-college-2020 "Simplified US electoral college data")
    (un-security-council "UN Security Council voting game")
    (simple-3-voter-example "Simple 3-voter example profile"))
   (example
    ";; Compute voter power in a 3-voter majority system
     (define profile (make-preference-profile '((a b) (a b) (b a))))
     (shapley-shubik-index profile)  ; => #(1/3 1/3 1/3) - equal power

     ;; Weighted voting: weights 3,2,1 with quota 4
     (profile->weighted-voting-game profile '(3 2 1))
     ;; Voter 0 has more power than voters 1,2

     ;; UN Security Council: P5 have veto power
     (define unsc (un-security-council))
     (voter-has-veto? ...)"))

  ((file "fair-division.ss")
   (purpose "Fair division algorithms for divisible and indivisible goods")
   (exports
    ;; Cake representation
    (make-cake "Create cake for n players")
    (cake? "Check if x is a cake")
    (cake-players "Get number of players")
    (cake-set-valuation! "Set player's valuation density function")
    (cake-valuation "Compute player's value for interval [a,b]")
    (cake-total-value "Player's value for entire cake")
    ;; Pieces and divisions
    (make-piece "Create piece from interval list")
    (piece-singleton "Single interval piece")
    (piece-value "Player's value for a piece")
    (piece-length "Total length of intervals")
    (piece-merge "Merge two pieces")
    (make-division "Create empty division for n players")
    (division-assign! "Assign piece to player")
    (division-piece "Get player's piece")
    ;; Fairness criteria
    (proportional? "Each player gets >= 1/n of their value")
    (envy-free? "No player prefers another's piece")
    (equitable? "All players receive same share of their value")
    (pareto-optimal? "Cannot improve one without harming another")
    ;; Cake cutting protocols
    (cut-and-choose "2-player proportional + envy-free division")
    (dubins-spanier "n-player proportional moving knife protocol")
    (selfridge-conway "3-player envy-free protocol (simplified)")
    ;; Adjusted winner (2-player, multiple goods)
    (make-adjusted-winner-problem "Create from goods and point allocations")
    (adjusted-winner "Compute envy-free equitable allocation")
    ;; Discrete fair division
    (make-discrete-problem "Create from goods and valuation matrix")
    (round-robin "Simple round-robin allocation")
    (envy-free-up-to-one? "Check if allocation is EF1")
    (maximin-share "Compute player's maximin share guarantee")
    ;; Example cakes
    (uniform-cake "Cake with uniform valuations")
    (simple-cake-2 "2-player cake with opposing preferences")
    (opposing-valuations-cake "3-player cake with opposing preferences"))
   (example
    ";; Cake cutting: 2 players divide a cake
     (define c (uniform-cake 2))
     (define div (cut-and-choose c 100))
     (proportional? c div)    ; => #t
     (envy-free? c div)       ; => #t

     ;; Adjusted winner: divide house, car, boat
     (define prob (make-adjusted-winner-problem
                    '(house car boat)
                    '(40 35 25)    ; player 1's points
                    '(30 50 20)))  ; player 2's points
     (adjusted-winner prob)
     ;; => envy-free + equitable allocation

     ;; Discrete: round-robin for indivisible goods
     (define dp (make-discrete-problem
                  '(A B C D)
                  '((10 20 30 40)    ; player 0's valuations
                    (40 30 20 10)))) ; player 1's valuations
     (define allocs (round-robin dp))
     (envy-free-up-to-one? dp allocs)  ; => #t"))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("lattice/linalg/vec.ss" "Vector operations")
  ("lattice/linalg/matrix.ss" "Matrix operations")
  ("lattice/optimization/lp.ss" "Linear programming for nucleolus"))

 (notes
  "Game theory in The Fold covers non-cooperative (normal form),
   cooperative (coalitional), matching theory, and social choice games.

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

   Voting/Social Choice: Preference aggregation with positional rules
   (plurality, Borda, anti-plurality) and Condorcet methods (pairwise
   comparison, Copeland, Schulze beatpath). Key results: Arrow's theorem
   (no perfect voting rule), Condorcet cycles (majority intransitivity),
   Gibbard-Satterthwaite (strategic manipulation always possible).
   The Schulze method uses Floyd-Warshall to find strongest beatpaths,
   producing total orderings even with cycles.

   Voting-Games Bridge: Connects voting theory with cooperative games.
   Key insight: voting rules induce simple games where coalitions are
   'winning' if they can determine the outcome. Power indices measure
   a voter's ability to influence results:
   - Shapley-Shubik: probability of being pivotal under random orderings
   - Banzhaf: fraction of coalitions where voter is pivotal
   Applications: electoral college analysis (states have different power
   than their vote counts suggest), weighted voting systems, UN Security
   Council (P5 have veto power = 100% Banzhaf).

   Note on cost games: make-airport-game produces a cost game where
   v(S) represents cost, not value. For cost games, 'fair allocation'
   means cost sharing; the standard core condition x(S) >= v(S) becomes
   'pay at least the standalone cost' which is the opposite intuition.
   Use Shapley value for cost allocation (proven fair for airport games).

   Fair Division: Extends cooperative games to non-transferable utility
   settings. Three main problem classes:
   1. Cake cutting (continuous): Resource represented as [0,1] with
      player-specific valuation functions. Protocols:
      - Cut-and-choose (2 players): proportional + envy-free
      - Dubins-Spanier (n players): proportional moving knife
      - Selfridge-Conway (3 players): envy-free (simplified impl)
   2. Adjusted winner (mixed): Point-based allocation where goods may
      be split. Produces envy-free + equitable + Pareto optimal results.
   3. Discrete (indivisible goods): EF1 (envy-free up to one good) is
      achievable when true envy-freeness is impossible. Round-robin
      guarantees EF1. Maximin share provides fairness benchmark.

   Test suites: 26 tests for normal-form, 45 tests for cooperative,
   34 tests for matching, 25 tests for voting, 26 tests for voting-games,
   40 tests for fair-division."))
