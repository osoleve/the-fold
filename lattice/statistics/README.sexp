;;; lattice/statistics/README.sexp — Statistics Skill Documentation

(readme statistics
  (summary "Comprehensive statistical modeling library for The Fold")

  (overview
   "The statistics skill provides a complete suite of statistical tools including:
    - Linear regression (OLS, WLS) with full diagnostics
    - Generalized Linear Models (GLM) via IRLS
    - Regularized regression (Ridge, Lasso, Elastic Net)
    - Time series analysis (AR, MA, exponential smoothing)
    - Hypothesis testing (t-tests, chi-squared, ANOVA, F-tests)

    All regression models provide standard errors, t-statistics, and p-values.
    GLM implementation uses Iteratively Reweighted Least Squares (IRLS) which
    computes SEs directly from the inverse Hessian.

    Logistic regression is implemented as a GLM wrapper:
    (logistic-fit X y criteria) = (glm-fit binomial-family logit-link X y criteria)")

  (usage
   ((linear-regression
     "Fit linear model with OLS:"
     "(define model (linear-model-fit X y))"
     "(define predictions (linear-model-predict model X-new))"
     "(define r-squared (lm-r-squared model))"
     "(define coefficients (lm-coefficients model))")

    (logistic-regression
     "Fit binary classifier via GLM:"
     "(define model (logistic-fit X y '((max-iter 100) (tol 1e-6))))"
     "(define probs (glm-predict-proba model X-new))"
     "(define classes (glm-predict model X-new))")

    (regularized-regression
     "Fit with L1/L2 regularization:"
     "(define ridge-model (ridge-fit X y lambda))"
     "(define lasso-model (lasso-fit X y lambda criteria))"
     "(define elastic (elastic-net-fit X y lambda alpha criteria))"
     "(define best-lambda (cross-validate-lambda X y 'ridge k-folds lambdas))")

    (time-series
     "AR and exponential smoothing:"
     "(define ar-model (ar-fit xs p))"
     "(define forecasts (ar-forecast ar-model xs h))"
     "(define hw-model (holt-winters xs 0.3 0.1 0.2 12 'additive))"
     "(define hw-forecasts (hw-forecast hw-model 12))")

    (hypothesis-tests
     "Statistical hypothesis testing:"
     "(define t-result (t-test-two-sample xs ys))"
     "(define anova-result (anova-one-way groups))"
     "(define chi-result (chi-squared-test-independence contingency-table))")))

  (architecture
   "Three-layer structure following Lattice conventions:

    core/
    ├── result-types.ss    # Extensible result structures
    ├── summary-stats.ss   # Descriptive statistics
    ├── design-matrix.ss   # Design matrix utilities
    └── diagnostics.ss     # Leverage, Cook's D, VIF

    regression/
    ├── linear.ss          # OLS, WLS
    ├── regularized.ss     # Ridge, Lasso, Elastic Net
    ├── glm.ss             # GLM via IRLS
    ├── families.ss        # Gaussian, Binomial, Poisson, Gamma
    └── link-functions.ss  # Identity, Logit, Log, Probit

    hypothesis/
    ├── distributions.ss   # t, chi-sq, F using special-functions.ss
    ├── t-test.ss          # t-tests
    ├── chi-squared.ss     # Chi-squared tests
    ├── anova.ss           # ANOVA
    └── f-test.ss          # F-tests

    timeseries/
    ├── differencing.ss    # Differencing, Box-Cox
    ├── acf-pacf.ss        # ACF, PACF, Durbin-Levinson
    ├── ar.ss              # AR(p) via Yule-Walker
    ├── ma.ss              # MA(q) via innovations
    ├── exponential.ss     # Holt-Winters
    └── forecast.ss        # Accuracy metrics")

  (dependencies
   ((linalg "Matrix operations, least squares")
    (fp/numeric "Special functions (betainc, gammainc-reg, erf)")))

  (key-decisions
   ((glm-over-separate-logistic
     "Logistic regression is a thin wrapper around GLM (binomial + logit).
      This avoids code duplication and ensures consistent SE computation.")
    (irls-over-gradient-descent
     "GLM uses IRLS instead of gradient descent because:
      1. IRLS provides SEs directly from (X'WX)^-1
      2. Faster convergence for well-conditioned problems
      3. No learning rate tuning required")
    (ar-over-arima
     "AR + exponential smoothing instead of full ARIMA because:
      1. More numerically stable
      2. Yule-Walker has closed-form solution
      3. ARIMA can be added later as extension")
    (coordinate-descent-for-lasso
     "Lasso uses coordinate descent with soft-thresholding because
      L1 penalty has undefined gradient at 0.")))

  (testing
   "(load \"lattice/statistics/test-statistics.ss\")
    (run-all-tests)"))

