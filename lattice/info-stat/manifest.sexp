(skill info-stat
  (version "0.1.0")
  (tier 1)
  (path "lattice/info-stat")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n*m) for MI feature selection where m = features, O(n) for model comparison")
  (deps (info statistics))

  (description
   "Bridge between information theory (lattice/info/) and statistics (lattice/statistics/).
    Connects mutual information to feature selection, entropy-based model comparison
    to regression results, and KL divergence to empirical distribution comparison.
    Enables info-theoretic analysis of statistical model outputs and vice versa.")

  (keywords (mutual-information feature-selection aic bic model-comparison
             kl-divergence distribution-comparison info-statistics bridge))
  (aliases (info-stat mi-bridge))

  (exports
   (info-stat/mi-bridge
    mi-select-features mi-rank-features
    model-aic-info model-bic-info model-aicc-info
    compare-models-info
    kl-compare-distributions kl-compare-symmetric
    empirical-to-probs))

  (modules
   (info-stat/mi-bridge "mi-bridge.ss"
    "MI-based feature selection, entropy-based model comparison for regression
     results, and KL divergence for empirical distribution comparison.")))
