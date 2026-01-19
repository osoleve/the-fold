;;; lattice/statistics/manifest.sexp — Statistics Skill Manifest

(skill statistics
  (version "0.1.0")
  (tier 1)
  (path "lattice/statistics")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n³) for regression, O(n²) for time series, O(n) for descriptive stats")
  (deps (linalg fp algebra))

  (description
   "Comprehensive statistical modeling library including linear and generalized
    linear models, regularized regression, time series analysis, and hypothesis
    testing. GLM uses IRLS for fitting with automatic SE computation. Time series
    covers AR, MA, and exponential smoothing methods.")

  (keywords (statistics regression linear-model glm logistic regularized
             ridge lasso elastic-net time-series ar ma exponential-smoothing
             hypothesis-testing t-test f-test chi-squared anova forecast
             diagnostics cook-distance leverage residuals
             orthogonal-polynomials legendre chebyshev hermite laguerre))

  (aliases (stats stat regression models))

  (exports
   ;; Core
   (result-types LinearModelResult GLMResult TestResult ARResult MAResult
                 ANOVAResult ForecastResult)
   (summary-stats vec-mean vec-variance vec-std-dev vec-median vec-quantile
                  vec-quantiles quantiles vec-covariance vec-correlation)
   (design-matrix add-intercept standardize-columns dummy-encode polynomial-features
                  legendre-p chebyshev-t hermite-h laguerre-l
                  legendre-features chebyshev-features hermite-features laguerre-features
                  orthogonal-features)
   (diagnostics hat-matrix-diagonal cooks-distance residuals-studentized vif)

   ;; Regression
   (linear linear-model-fit linear-model-predict weighted-linear-fit)
   (regularized ridge-fit lasso-fit elastic-net-fit cross-validate-lambda)
   (glm glm-fit glm-predict logistic-fit poisson-fit)
   (families gaussian-family binomial-family poisson-family gamma-family)
   (link-functions identity-link logit-link log-link probit-link inverse-link)

   ;; Hypothesis Testing
   (distributions t-cdf t-quantile chi-squared-cdf chi-squared-quantile
                  f-cdf f-quantile t-pvalue chi-squared-pvalue f-pvalue)
   (t-test t-test-one-sample t-test-two-sample t-test-paired)
   (chi-squared chi-squared-test-goodness chi-squared-test-independence)
   (anova anova-one-way anova-effect-size)
   (f-test f-test-variance f-test-regression f-test-nested-models
           levene-test bartlett-test)

   ;; Time Series
   (differencing vec-diff difference lag-diff seasonal-difference integrate
                 log-transform box-cox)
   (acf-pacf acf pacf ljung-box-test box-pierce-test ccf)
   (ar ar-fit ar-forecast ar-select-order ar-aic ar-bic)
   (ar-poly ar-char-poly-vec ma-char-poly ar-companion-poly
            arma-common-factors arma-reducible?
            ar-stable-simple? ma-invertible-simple? ar-forecast-weights)
   (ma ma-fit ma-forecast moving-average weighted-moving-average
       exponential-moving-average arma-fit-simple)
   (exponential simple-exponential-smooth holt-smooth holt-winters
                holt-damped ses-forecast holt-forecast hw-forecast
                optimize-ses-alpha)
   (forecast mae mse rmse mape smape mase theil-u tracking-signal
             naive-forecast drift-forecast seasonal-naive-forecast
             mean-forecast forecast-intervals forecast-accuracy
             time-series-cv combine-forecasts-mean combine-forecasts-weighted))

  (modules
   ;; Core modules
   (result-types "core/result-types.ss" "Statistical model result structures")
   (summary-stats "core/summary-stats.ss" "Descriptive statistics")
   (design-matrix "core/design-matrix.ss" "Design matrix, orthogonal polynomial bases")
   (diagnostics "core/diagnostics.ss" "Model diagnostics (leverage, Cook's D)")

   ;; Regression modules
   (linear "regression/linear.ss" "OLS and WLS linear regression")
   (regularized "regression/regularized.ss" "Ridge, Lasso, Elastic Net")
   (glm "regression/glm.ss" "Generalized Linear Models via IRLS")
   (families "regression/families.ss" "GLM families (Gaussian, Binomial, Poisson, Gamma)")
   (link-functions "regression/link-functions.ss" "Link functions (identity, logit, log, probit)")

   ;; Hypothesis testing modules
   (distributions "hypothesis/distributions.ss" "t, chi-squared, F distributions")
   (t-test "hypothesis/t-test.ss" "One-sample, two-sample, paired t-tests")
   (chi-squared "hypothesis/chi-squared.ss" "Goodness-of-fit, independence tests")
   (anova "hypothesis/anova.ss" "One-way ANOVA with effect sizes")
   (f-test "hypothesis/f-test.ss" "F-tests for variance and regression")

   ;; Time series modules
   (differencing "timeseries/differencing.ss" "Differencing and integration")
   (acf-pacf "timeseries/acf-pacf.ss" "ACF, PACF, Ljung-Box test")
   (ar "timeseries/ar.ss" "Autoregressive models via Yule-Walker")
   (ar-poly "timeseries/ar-poly.ss" "Polynomial analysis for AR/MA models")
   (ma "timeseries/ma.ss" "Moving average models via innovations")
   (exponential "timeseries/exponential.ss" "Holt-Winters exponential smoothing")
   (forecast "timeseries/forecast.ss" "Forecast accuracy and utilities")))

