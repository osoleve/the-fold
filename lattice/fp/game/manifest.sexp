;;; lattice/fp/game/manifest.sexp — Game Theory Skill Manifest

(skill game
  (version "0.2.0")
  (tier 1)
  (path "lattice/fp/game")
  (purity total)
  (stability stable)
  (fuel-bound "O(2^n) for n players in cooperative games; O(n*m) for matching")

  (deps (linalg optimization))

  (description
   "Comprehensive game theory library covering non-cooperative games,
    cooperative (coalitional) games, two-sided matching, social choice/voting theory,
    multi-winner elections, fair division, and mechanism design. Implements Nash equilibrium,
    Shapley value, Gale-Shapley stable matching, Schulze voting, STV, PAV, VCG mechanism,
    first-price/second-price auctions, double auctions, and incentive compatibility analysis.")

  (keywords (game-theory nash-equilibrium shapley-value cooperative-games
             matching stable-matching gale-shapley voting social-choice
             condorcet schulze multi-winner stv approval-voting pav
             monroe chamberlin-courant proportional-representation
             committee-selection fair-division cake-cutting envy-free
             proportional adjusted-winner power-indices banzhaf
             mechanism-design auction vickrey vcg incentive-compatibility
             dsic revelation-principle double-auction pivot-mechanism
             evolutionary replicator-dynamics ess evolutionarily-stable-strategy
             population-dynamics hawk-dove invasion-dynamics
             strategic-voting best-response price-of-anarchy price-of-stability
             gibbard-satterthwaite strategy-proof manipulation
             mcdm multi-criteria decision-making pareto-frontier weighted-borda
             sensitivity-analysis robustness))

  (aliases (game games game-theory))

  (exports
   ;; normal-form.ss — Strategic Form Games
   make-game game-payoff find-pure-nash

   ;; coop-games.ss — Cooperative Games
   coalition-empty coalition-singleton coalition-member?
   coalition-union coalition-intersection coalition-complement
   coalition-size coalition->list list->coalition all-coalitions
   make-coop-game coop-game? coop-game-players coop-game-value
   coop-game-grand-coalition
   allocation-total allocation-coalition-total imputation?
   shapley-value core-excess allocation-in-core? nucleolus
   nash-bargaining kalai-smorodinsky
   make-additive-game make-unanimity-game make-weighted-voting-game
   make-airport-game make-bankruptcy-game make-gloves-game
   coop-game-superadditive? coop-game-convex? coop-game-monotonic?
   coop-game-simple?
   is-winning? is-blocking? is-pivotal? banzhaf-index

   ;; voting.ss — Social Choice
   make-preference-profile profile-voters profile-candidates
   profile-num-candidates profile-rankings
   plurality-winner borda-winner antiplurality-winner
   plurality-scores-all borda-scores-all
   pairwise-margin pairwise-beats?
   condorcet-winner? condorcet-winner
   copeland-score copeland-winner
   schulze-strengths schulze-winner schulze-ranking
   manipulation-possible? condorcet-cycle-example

   ;; strategic-voting.ss — Strategic Voting Equilibrium
   social-welfare find-best-responses best-response-improves?
   is-strategic-equilibrium? find-strategic-equilibrium all-strategic-equilibria
   price-of-anarchy price-of-stability
   count-manipulable-profiles strategy-proofness-ratio compare-strategy-proofness
   find-manipulation-example gibbard-satterthwaite-demo
   strategic-example-1 strategic-example-cycle

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

   ;; multi-winner.ss — Multi-Winner Elections
   droop-quota hare-quota
   stv stv-droop stv-hare
   make-approval-profile approval-profile-voters approval-profile-candidates
   approval-count approval-scores approval-winners
   pav-score pav-winners
   sav-score sav-winners
   monroe-greedy cc-greedy cc-total-satisfaction
   proportionality-score representation-coverage diversity-score
   profile->approval approval->profile
   stv-example approval-example diverse-preferences-example

   ;; mechanism.ss — Mechanism Design
   *tie-break-strategy* with-tie-break
   make-bids make-auction-outcome
   auction-outcome-winner auction-outcome-payment auction-outcome-revenue
   first-price-auction second-price-auction all-pay-auction third-price-auction
   dutch-auction with-reserve bidder-utility
   check-dsic verify-dsic check-ex-post-ir
   vcg-combinatorial pivot-mechanism
   myerson-virtual-value optimal-reserve-uniform
   k-double-auction double-auction-trades double-auction-volume
   double-auction-buyer-surplus double-auction-seller-surplus double-auction-total-surplus
   make-direct-mechanism
   is-budget-balanced? is-weakly-budget-balanced?

   ;; evolutionary.ss — Evolutionary Game Theory
   make-symmetric-game symmetric-game? sg-payoff sg-num-strategies sg-strategy-name
   make-population population? pop-freq pop-size pop->list
   uniform-population pure-population
   strategy-fitness all-fitnesses average-fitness
   replicator-derivative replicator-step replicator-trajectory replicator-converge
   is-ess? find-all-ess is-nash-equilibrium? find-pure-nash
   can-invade? invasion-fitness invasion-gradient
   is-interior-equilibrium? hawk-dove-mixed-ess
   hawk-dove-game stag-hunt-symmetric pd-symmetric rps-game
   analyze-game

   ;; mcdm.ss — Multi-Criteria Decision Making
   make-decision-problem decision-problem? dp-alternatives dp-criteria dp-score
   criterion-ranking dp->profile dp->weighted-profile
   mcdm-borda mcdm-schulze mcdm-copeland mcdm-condorcet mcdm-ranking
   weighted-borda-scores weighted-borda-winner
   dominates? pareto-frontier pareto-dominated is-pareto-optimal?
   sensitivity-to-criterion robustness-profile min-robustness
   has-condorcet-cycle? cycle-participants
   method-agreement unanimous-winner? mcdm-summary
   sorting-algorithm-example tech-choice-example)

  (modules
   (normal-form "normal-form.ss" "Strategic form games, Nash equilibrium, IESDS")
   (coop-games "coop-games.ss" "Coalitional games, Shapley value, core, bargaining")
   (matching "matching.ss" "Two-sided matching, Gale-Shapley, assignment games")
   (voting "voting.ss" "Social choice: plurality, Borda, Condorcet, Schulze")
   (strategic-voting "strategic-voting.ss" "Strategic voting equilibrium, price of anarchy, Gibbard-Satterthwaite")
   (voting-games "voting-games.ss" "Bridge: voting rules to simple games, power indices")
   (multi-winner "multi-winner.ss" "STV, approval voting, PAV, Monroe, Chamberlin-Courant")
   (fair-division "fair-division.ss" "Cake cutting, adjusted winner, EF1, maximin share")
   (mechanism "mechanism.ss" "Auctions, VCG, incentive compatibility, double auctions")
   (evolutionary "evolutionary.ss" "Replicator dynamics, ESS, population games, invasion analysis")
   (mcdm "mcdm.ss" "Multi-criteria decision making via voting aggregation")))
