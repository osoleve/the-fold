;;; lattice/fp/game/manifest.sexp — Game Theory Skill Manifest

((skill . game)
 (version . "0.2.0")
 (tier . 1)
 (path . "lattice/fp/game")
 (purity . total)
 (stability . stable)
 (fuel-bound . "O(2^n) for n players in cooperative games; O(n*m) for matching")

 (deps . (linalg optimization))

 (description . "Comprehensive game theory library covering non-cooperative games,
cooperative (coalitional) games, two-sided matching, social choice/voting theory,
and fair division. Implements Nash equilibrium, Shapley value, Gale-Shapley stable
matching, Schulze voting method, cake cutting protocols, and adjusted winner.")

 (keywords . (game-theory nash-equilibrium shapley-value cooperative-games
              matching stable-matching gale-shapley voting social-choice
              condorcet schulze fair-division cake-cutting envy-free
              proportional adjusted-winner power-indices banzhaf))

 (aliases . (game games game-theory))

 (exports . (
   ;; normal-form.ss — Strategic Form Games
   make-player player-name player-strategies
   make-game game-players game-payoff
   payoff-matrix best-response best-responses
   is-nash-equilibrium? find-pure-nash
   dominated-strategies iterated-elimination
   mixed-strategy expected-payoff support

   ;; coop-games.ss — Cooperative Games
   coalition-empty coalition-singleton coalition-member?
   coalition-union coalition-intersection coalition-complement
   coalition-size coalition->list list->coalition all-coalitions
   make-coop-game coop-game? coop-game-players coop-game-value
   coop-game-grand-coalition
   allocation-total allocation-coalition-total imputation?
   shapley-value allocation-in-core? core-excess nucleolus
   nash-bargaining kalai-smorodinsky
   make-additive-game make-unanimity-game make-weighted-voting-game
   make-airport-game make-bankruptcy-game make-gloves-game
   coop-game-superadditive? coop-game-convex? coop-game-monotonic?
   coop-game-simple?
   is-winning? is-blocking? is-pivotal? banzhaf-index

   ;; matching.ss — Two-Sided Matching
   make-matching-market matching-market?
   market-proposers market-receivers
   market-proposer-prefs market-receiver-prefs
   stable-match stable-match-receiver-optimal
   matching-stable? same-matched-agents?
   make-assignment-game optimal-assignment
   make-medical-residency-market make-school-choice-market

   ;; voting.ss — Social Choice
   make-preference-profile profile-voters profile-num-candidates
   profile-candidates profile-rankings
   plurality-winner plurality-scores-all
   borda-winner borda-scores-all
   antiplurality-winner antiplurality-scores-all
   pairwise-margin pairwise-beats?
   condorcet-winner? condorcet-winner
   copeland-score copeland-winner
   schulze-strengths schulze-winner schulze-ranking
   condorcet-cycle-example manipulation-possible?

   ;; voting-games.ss — Voting-Games Bridge
   profile->majority-game profile->weighted-voting-game
   profile->rule-induced-game
   shapley-shubik-index shapley-shubik-weighted
   banzhaf-voting-power banzhaf-weighted
   voter-is-dictator? voter-is-dummy? voter-has-veto?
   minimal-winning-coalitions is-minimal-winning?
   power-concentration power-gini
   decisive-for-candidate blocking-for-candidate
   make-electoral-college-game electoral-college-power power-per-capita
   us-electoral-college-2020 un-security-council simple-3-voter-example

   ;; fair-division.ss — Fair Division
   make-cake cake? cake-players cake-set-valuation!
   cake-valuation cake-total-value
   make-piece piece-singleton piece-value piece-length piece-merge piece-empty
   make-division division-assign! division-piece division->list
   proportional? envy-free? equitable? pareto-optimal?
   cut-and-choose dubins-spanier selfridge-conway
   find-cut-point find-cut-point-from
   make-adjusted-winner-problem adjusted-winner
   make-discrete-problem discrete-problem-players discrete-problem-goods-count
   discrete-problem-valuation round-robin discrete-allocation-value
   envy-free-up-to-one? maximin-share
   uniform-cake simple-cake-2 opposing-valuations-cake))

 (modules . (
   (normal-form "normal-form.ss" "Strategic form games, Nash equilibrium, IESDS")
   (coop-games "coop-games.ss" "Coalitional games, Shapley value, core, bargaining")
   (matching "matching.ss" "Two-sided matching, Gale-Shapley, assignment games")
   (voting "voting.ss" "Social choice: plurality, Borda, Condorcet, Schulze")
   (voting-games "voting-games.ss" "Bridge: voting rules to simple games, power indices")
   (fair-division "fair-division.ss" "Cake cutting, adjusted winner, EF1, maximin share"))))
