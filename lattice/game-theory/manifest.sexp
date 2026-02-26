;;; lattice/game-theory/manifest.sexp — Game Theory Skill Manifest

(skill game-theory
  (version "0.2.0")
  (path "lattice/game-theory")
  (purity total)
  (stability stable)
  (fuel-bound "O(2^n) for n players in cooperative games; O(n*m) for matching")

  (deps (linalg optimization))

  (description
   "Comprehensive game theory library covering strategic (normal form) games,
    sequential (extensive form) games, cooperative (coalitional) games, two-sided matching,
    social choice/voting theory, multi-winner elections, fair division, and mechanism design.
    Implements Nash equilibrium, backward induction, subgame perfect equilibrium, Shapley value,
    Gale-Shapley stable matching, Schulze voting, STV, PAV, VCG mechanism,
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
             sensitivity-analysis robustness
             extensive-form game-tree backward-induction subgame-perfect-equilibrium
             sequential-game information-set perfect-information imperfect-information
             ultimatum centipede entry-deterrence stackelberg signaling))

  (aliases (game games game-theory))

  (concepts
    (concept strategic-games
      (description "Normal and extensive form games, Nash equilibrium, backward induction, and subgame perfect equilibrium.")
      (parent game-and-decision-theory)
      (synonyms nash-equilibrium best-response pure-nash mixed-nash))
    (concept cooperative-games
      (description "Coalitional games, Shapley value, core, nucleolus, Banzhaf index, and Nash bargaining solutions.")
      (parent game-and-decision-theory)
      (synonyms shapley-value cooperative-value banzhaf-index power-index))
    (concept social-choice
      (description "Voting systems (plurality, Borda, Schulze, STV, PAV), Condorcet winners, and Gibbard-Satterthwaite impossibility.")
      (parent game-and-decision-theory)
      (synonyms voting-rules plurality borda condorcet schulze))
    (concept mechanism-design
      (description "Auction theory (first-price, VCG, double), incentive compatibility, revelation principle, and Myerson optimal auctions.")
      (parent game-and-decision-theory)
      (synonyms vcg vickrey auction incentive-compatibility stable-matching gale-shapley two-sided-matching))
    (concept evolutionary-game-theory
      (description "Replicator dynamics, evolutionarily stable strategies (ESS), invasion analysis, and population dynamics.")
      (parent game-and-decision-theory)
      (synonyms replicator-dynamics evolutionarily-stable-strategy ess hawk-dove))
    (concept multi-criteria-decision
      (description "Decision making over multiple objectives: Pareto frontiers, weighted Borda, sensitivity analysis, and robustness.")
      (parent game-and-decision-theory)))

  (exports
   ;; normal-form.ss — Strategic Form Games
   (normal-form
    make-game game-payoff find-pure-nash)

   ;; extensive-form.ss — Extensive Form Games
   (extensive-form
    make-terminal terminal? terminal-payoffs terminal-payoff
    make-decision decision? decision-player decision-actions decision-children decision-child
    make-chance chance? chance-outcomes chance-probs chance-children
    make-extensive-game extensive-game? game-num-players game-root game-info-sets
    tree-depth tree-size terminal-nodes
    backward-induction solve-spe spe-result? spe-payoffs spe-strategy
    expected-utility
    make-info-set info-set? info-set-player info-set-label info-set-nodes
    add-info-set perfect-information?
    ultimatum-game make-centipede-game centipede-4 entry-deterrence
    stackelberg-simple signaling-simple
    print-tree)

   ;; coop-games.ss — Cooperative Games
   (coop-games
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
    is-winning? is-blocking? is-pivotal? banzhaf-index)

   ;; matching.ss — Stable Matching
   (matching
    make-matching-market matching-market?
    market-proposers market-receivers market-proposer-prefs market-receiver-prefs
    market-num-proposers market-num-receivers
    stable-match matching-stable?
    stable-match-receiver-optimal
    make-assignment-game assignment-game-value optimal-assignment
    optimal-assignment-subset
    make-medical-residency-market make-school-choice-market
    make-random-preferences preference-rank
    weighted-matching-ilp bottleneck-matching-ilp)

   ;; voting.ss — Social Choice
   (voting
    make-preference-profile profile-voters profile-candidates
    profile-num-candidates profile-rankings
    plurality-winner borda-winner antiplurality-winner
    plurality-scores-all borda-scores-all
    pairwise-margin pairwise-beats?
    condorcet-winner? condorcet-winner
    copeland-score copeland-winner
    schulze-strengths schulze-winner schulze-ranking
    manipulation-possible? condorcet-cycle-example)

   ;; strategic-voting.ss — Strategic Voting Equilibrium
   (strategic-voting
    social-welfare find-best-responses best-response-improves?
    is-strategic-equilibrium? find-strategic-equilibrium all-strategic-equilibria
    price-of-anarchy price-of-stability
    count-manipulable-profiles strategy-proofness-ratio compare-strategy-proofness
    find-manipulation-example gibbard-satterthwaite-demo
    strategic-example-1 strategic-example-cycle)

   ;; voting-games.ss — Voting-Games Bridge
   (voting-games
    profile->majority-game profile->weighted-voting-game
    profile->rule-induced-game
    shapley-shubik-index shapley-shubik-weighted
    banzhaf-voting-power banzhaf-weighted
    voter-is-dictator? voter-is-dummy? voter-has-veto?
    minimal-winning-coalitions is-minimal-winning?
    power-concentration power-gini
    decisive-for-candidate blocking-for-candidate
    make-electoral-college-game electoral-college-power power-per-capita
    us-electoral-college-2020 un-security-council simple-3-voter-example)

   ;; multi-winner.ss — Multi-Winner Elections
   (multi-winner
    droop-quota hare-quota
    stv stv-droop stv-hare
    make-approval-profile approval-profile-voters approval-profile-candidates
    approval-count approval-scores approval-winners
    pav-score pav-winners
    sav-score sav-winners
    monroe-greedy cc-greedy cc-total-satisfaction
    proportionality-score representation-coverage diversity-score
    profile->approval approval->profile
    stv-example approval-example diverse-preferences-example)

   ;; fair-division.ss — Fair Division
   (fair-division
    make-cake cake? cake-players cake-set-valuation! cake-valuation cake-total-value
    make-piece piece-empty piece-singleton piece-value piece-length piece-merge
    make-division division-assign! division-piece division->list
    proportional? envy-free? equitable? pareto-optimal?
    cut-and-choose dubins-spanier selfridge-conway
    make-adjusted-winner-problem adjusted-winner
    make-discrete-problem discrete-problem-players discrete-problem-goods-count
    discrete-problem-valuation round-robin discrete-allocation-value
    envy-free-up-to-one? maximin-share
    uniform-cake simple-cake-2 opposing-valuations-cake)

   ;; mechanism.ss — Mechanism Design
   (mechanism
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
    is-budget-balanced? is-weakly-budget-balanced?)

   ;; evolutionary.ss — Evolutionary Game Theory
   (evolutionary
    make-symmetric-game symmetric-game? sg-payoff sg-num-strategies sg-strategy-name
    make-population population? pop-freq pop-size pop->list
    uniform-population pure-population
    strategy-fitness all-fitnesses average-fitness
    replicator-derivative replicator-step replicator-trajectory replicator-converge
    is-ess? find-all-ess is-nash-equilibrium? find-pure-nash
    can-invade? invasion-fitness invasion-gradient
    is-interior-equilibrium? hawk-dove-mixed-ess
    hawk-dove-game stag-hunt-symmetric pd-symmetric rps-game
    analyze-game)

   ;; mcdm.ss — Multi-Criteria Decision Making
   (mcdm
    make-decision-problem decision-problem? dp-alternatives dp-criteria dp-score
    criterion-ranking dp->profile dp->weighted-profile
    mcdm-borda mcdm-schulze mcdm-copeland mcdm-condorcet mcdm-ranking
    weighted-borda-scores weighted-borda-winner
    dominates? pareto-frontier pareto-dominated is-pareto-optimal?
    sensitivity-to-criterion robustness-profile min-robustness
    has-condorcet-cycle? cycle-participants
    method-agreement unanimous-winner? mcdm-summary
    sorting-algorithm-example tech-choice-example))

  (modules
   (normal-form "normal-form.ss" "Strategic form games, Nash equilibrium, IESDS")
   (extensive-form "extensive-form.ss" "Game trees, backward induction, subgame perfect equilibrium")
   (coop-games "coop-games.ss" "Coalitional games, Shapley value, core, bargaining")
   (matching "matching.ss" "Two-sided matching, Gale-Shapley, assignment games")
   (voting "voting.ss" "Social choice: plurality, Borda, Condorcet, Schulze")
   (strategic-voting "strategic-voting.ss" "Strategic voting equilibrium, price of anarchy, Gibbard-Satterthwaite")
   (voting-games "voting-games.ss" "Bridge: voting rules to simple games, power indices")
   (multi-winner "multi-winner.ss" "STV, approval voting, PAV, Monroe, Chamberlin-Courant")
   (fair-division "fair-division.ss" "Cake cutting, adjusted winner, EF1, maximin share")
   (mechanism "mechanism.ss" "Auctions, VCG, incentive compatibility, double auctions")
   (evolutionary "evolutionary.ss" "Replicator dynamics, ESS, population games, invasion analysis")
   (mcdm "mcdm.ss" "Multi-criteria decision making via voting aggregation")
   (clp-equilibrium "clp-equilibrium.ss" "CLP(FD) bridge for Nash equilibrium via constraint propagation"))

  (bridge-exports
   ;; clp-equilibrium.ss — CLP(FD) ↔ Game Theory Bridge
   (clp-equilibrium
    clp-best-response-p1 clp-best-response-p2 clp-mutual-best-response
    clp-dominance-prune game->clp-nash game->clp-constrained-nash
    clp-find-pure-nash clp-constrained-nash
    nash-solution->names nash-solutions->names)))
