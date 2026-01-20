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
multi-winner elections, and fair division. Implements Nash equilibrium, Shapley value,
Gale-Shapley stable matching, Schulze voting, STV, PAV, Monroe/Chamberlin-Courant,
cake cutting protocols, and adjusted winner.")

 (keywords . (game-theory nash-equilibrium shapley-value cooperative-games
              matching stable-matching gale-shapley voting social-choice
              condorcet schulze multi-winner stv approval-voting pav
              monroe chamberlin-courant proportional-representation
              committee-selection fair-division cake-cutting envy-free
              proportional adjusted-winner power-indices banzhaf))

 (aliases . (game games game-theory))

 (exports . (
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
   stv-example approval-example diverse-preferences-example))

 (modules . (
   (normal-form "normal-form.ss" "Strategic form games, Nash equilibrium, IESDS")
   (coop-games "coop-games.ss" "Coalitional games, Shapley value, core, bargaining")
   (matching "matching.ss" "Two-sided matching, Gale-Shapley, assignment games")
   (voting "voting.ss" "Social choice: plurality, Borda, Condorcet, Schulze")
   (voting-games "voting-games.ss" "Bridge: voting rules to simple games, power indices")
   (multi-winner "multi-winner.ss" "STV, approval voting, PAV, Monroe, Chamberlin-Courant")
   (fair-division "fair-division.ss" "Cake cutting, adjusted winner, EF1, maximin share"))))
