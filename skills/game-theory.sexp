;;; skills/game-theory.sexp — Skill curation file

(skill game-theory
  (description
    "Comprehensive game theory library covering strategic (normal form) games,\n    sequential (extensive form) games, cooperative (coalitional) games, two-sided matching,\n    social choice/voting theory, multi-winner elections, fair division, and mechanism design.\n    Implements Nash equilibrium, backward induction, subgame perfect equilibrium, Shapley value,\n    Gale-Shapley stable matching, Schulze voting, STV, PAV, VCG mechanism,\n    first-price/second-price auctions, double auctions, and incentive compatibility analysis.")

  (keywords (game-theory nash-equilibrium shapley-value cooperative-games matching stable-matching gale-shapley voting social-choice condorcet schulze multi-winner stv approval-voting pav monroe chamberlin-courant proportional-representation committee-selection fair-division cake-cutting envy-free proportional adjusted-winner power-indices banzhaf mechanism-design auction vickrey vcg incentive-compatibility dsic revelation-principle double-auction pivot-mechanism evolutionary replicator-dynamics ess evolutionarily-stable-strategy population-dynamics hawk-dove invasion-dynamics strategic-voting best-response price-of-anarchy price-of-stability gibbard-satterthwaite strategy-proof manipulation mcdm multi-criteria decision-making pareto-frontier weighted-borda sensitivity-analysis robustness extensive-form game-tree backward-induction subgame-perfect-equilibrium sequential-game information-set perfect-information imperfect-information ultimatum centipede entry-deterrence stackelberg signaling))

  (aliases (game games game-theory))

  (concepts (strategic-games cooperative-games social-choice mechanism-design evolutionary-game-theory multi-criteria-decision))

  (modules
    normal-form
    extensive-form
    coop-games
    matching
    voting
    strategic-voting
    voting-games
    multi-winner
    fair-division
    mechanism
    evolutionary
    mcdm
    clp-equilibrium
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
