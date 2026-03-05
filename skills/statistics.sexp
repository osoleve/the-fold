;;; skills/statistics.sexp — Skill curation file

(skill statistics
  (description
    "Comprehensive statistical modeling library including linear and generalized\n    linear models, regularized regression, time series analysis, and hypothesis\n    testing. GLM uses IRLS for fitting with automatic SE computation. Time series\n    covers AR, MA, and exponential smoothing methods.")

  (keywords (statistics regression linear-model glm logistic regularized ridge lasso elastic-net time-series ar ma exponential-smoothing hypothesis-testing t-test f-test chi-squared anova forecast diagnostics cook-distance leverage residuals orthogonal-polynomials legendre chebyshev hermite laguerre))

  (aliases (stats stat regression models))

  (concepts (statistical-modeling))

  (modules
    result-types
    model-protocol
    summary-stats
    design-matrix
    diagnostics
    linear
    regularized
    glm
    families
    link-functions
    distributions
    t-test
    chi-squared
    anova
    f-test
    differencing
    acf-pacf
    ar
    ar-poly
    ma
    exponential
    forecast
    info-stat/mi-bridge
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
