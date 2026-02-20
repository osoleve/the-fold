((name "info-stat")
(purpose "Bridge between information theory and statistics")
(description "Connects the info skill (entropy, divergence, channel capacity)
to the statistics skill (regression, GLM, time series). Enables MI-based
feature selection, entropy-grounded model comparison for regression results,
and KL divergence comparison of empirical distributions.")
(modules
  ((mi-bridge.ss "MI feature selection, AIC/BIC/AICc for regression models
   via info-theoretic log-likelihood, KL divergence for empirical samples")))
(dependencies (info statistics))
(key-concepts
  ((mi-feature-selection "Rank/select features by I(feature; target)")
  (info-model-comparison "AIC/BIC/AICc grounded in Gaussian log-likelihood")
  (kl-empirical "KL divergence between raw sample vectors via shared binning")))
(test-coverage "21 tests in test-mi-bridge.ss"))
