(load "core/base/prelude.ss")

(doc 'module 'result-types)
(doc 'description "Statistical Model Result Types — Extensible result structures for statistical models and tests")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'linear-model-results)

(doc 'description "Linear model results (coefficients, SE, t-values, p-values, R²)")
(doc 'description "GLM results (deviance, AIC, family info)")
(doc 'description "Test results (statistic, df, p-value, CI)")
(doc 'description "Time series results (AR coefficients, forecasts)")
(define (make-linear-model-result coefficients se t-values p-values
                                  r-squared adj-r-squared
                                  residuals fitted sse mse n p)
  (doc 'type '(-> Vec Vec Vec Vec Num Num Vec Vec Num Num Nat Nat LinearModelResult))
  (doc 'description "Create a linear model result with full diagnostics")
  (doc 'param 'coefficients "fitted coefficients (including intercept if present)")
  (doc 'param 'se "standard errors of coefficients")
  (doc 'param 't-values "t-statistics (coefficients / se)")
  (doc 'param 'p-values "two-tailed p-values from t-distribution")
  (doc 'param 'r-squared "coefficient of determination")
  (doc 'param 'adj-r-squared "adjusted R²")
  (doc 'param 'residuals "raw residuals (y - fitted)")
  (doc 'param 'fitted "fitted values (X * beta)")
  (doc 'param 'sse "sum of squared errors")
  (doc 'param 'mse "mean squared error (sse / df-residual)")
  (doc 'param 'n "number of observations")
  (doc 'param 'p "number of parameters (including intercept)")
  (list 'linear-model
        coefficients se t-values p-values
        r-squared adj-r-squared
        residuals fitted sse mse n p))
(define (linear-model? x)
  (and (pair? x) (eq? (car x) 'linear-model)))

;;; Accessors
(define (linear-model-coefficients m) (list-ref m 1))
(define (linear-model-se m) (list-ref m 2))
(define (linear-model-t-values m) (list-ref m 3))
(define (linear-model-p-values m) (list-ref m 4))
(define (linear-model-r-squared m) (list-ref m 5))
(define (linear-model-adj-r-squared m) (list-ref m 6))
(define (linear-model-residuals m) (list-ref m 7))
(define (linear-model-fitted m) (list-ref m 8))
(define (linear-model-sse m) (list-ref m 9))
(define (linear-model-mse m) (list-ref m 10))
(define (linear-model-n m) (list-ref m 11))
(define (linear-model-p m) (list-ref m 12))

(define (linear-model-df-residual m)
  (doc 'type '(-> LinearModelResult Nat))
  (doc 'description "Degrees of freedom for residuals = n - p")
  (- (linear-model-n m) (linear-model-p m)))

(define (linear-model-sigma m)
  (doc 'type '(-> LinearModelResult Num))
  (doc 'description "Residual standard error = sqrt(mse)")
  (sqrt (linear-model-mse m)))

(doc 'section 'glm-results)
(define (make-glm-result family link coefficients se z-values p-values
                         deviance null-deviance aic
                         residuals fitted n iterations converged?)
  (doc 'type '(-> Symbol Symbol Vec Vec Vec Vec Num Num Num Vec Vec Nat Nat Bool GLMResult))
  (doc 'description "Create a GLM result")
  (doc 'param 'family "family name (gaussian, binomial, poisson, gamma)")
  (doc 'param 'link "link function name (identity, logit, log, probit)")
  (doc 'param 'coefficients "fitted coefficients")
  (doc 'param 'se "standard errors")
  (doc 'param 'z-values "z-statistics (coefficients / se)")
  (doc 'param 'p-values "two-tailed p-values from normal distribution")
  (doc 'param 'deviance "residual deviance")
  (doc 'param 'null-deviance "null model deviance")
  (doc 'param 'aic "Akaike Information Criterion")
  (doc 'param 'residuals "deviance residuals")
  (doc 'param 'fitted "fitted values on response scale")
  (doc 'param 'n "number of observations")
  (doc 'param 'iterations "IRLS iterations used")
  (doc 'param 'converged? "whether IRLS converged")
  (list 'glm-model
        family link coefficients se z-values p-values
        deviance null-deviance aic
        residuals fitted n iterations converged?))

;;; glm-result? : Any → Bool
(define (glm-result? x)
  (and (pair? x) (eq? (car x) 'glm-model)))

;;; Accessors
(define (glm-family m) (list-ref m 1))
(define (glm-link m) (list-ref m 2))
(define (glm-coefficients m) (list-ref m 3))
(define (glm-se m) (list-ref m 4))
(define (glm-z-values m) (list-ref m 5))
(define (glm-p-values m) (list-ref m 6))
(define (glm-deviance m) (list-ref m 7))
(define (glm-null-deviance m) (list-ref m 8))
(define (glm-aic m) (list-ref m 9))
(define (glm-residuals m) (list-ref m 10))
(define (glm-fitted m) (list-ref m 11))
(define (glm-n m) (list-ref m 12))
(define (glm-iterations m) (list-ref m 13))
(define (glm-converged? m) (list-ref m 14))

(define (glm-df-residual m)
  (doc 'type '(-> GLMResult Nat))
  (- (glm-n m) (vector-length (glm-coefficients m))))

(doc 'section 'hypothesis-test-results)
(define (make-test-result test-name statistic df p-value ci effect-size)
  (doc 'type '(-> Symbol Num Any Num (or (Pair Num Num) False) (or Num False) TestResult))
  (doc 'description "Create a hypothesis test result")
  (doc 'param 'test-name "name of the test (t-test-one-sample, chi-squared, etc.)")
  (doc 'param 'statistic "test statistic value")
  (doc 'param 'df "degrees of freedom (number or pair for F-test)")
  (doc 'param 'p-value "p-value")
  (doc 'param 'ci "confidence interval (pair) or #f if not applicable")
  (doc 'param 'effect-size "effect size or #f if not computed")
  (list 'test-result test-name statistic df p-value ci effect-size))
(define (test-result? x)
  (and (pair? x) (eq? (car x) 'test-result)))

;;; Accessors
(define (test-name r) (list-ref r 1))
(define (test-statistic r) (list-ref r 2))
(define (test-df r) (list-ref r 3))
(define (test-p-value r) (list-ref r 4))
(define (test-ci r) (list-ref r 5))
(define (test-effect-size r) (list-ref r 6))

(define (test-significant? r alpha)
  (doc 'type '(-> TestResult Num Bool))
  (doc 'description "Check if test is significant at given alpha level")
  (< (test-p-value r) alpha))

(doc 'section 'anova-results)
(define (make-anova-result f-statistic df-between df-within p-value
                           ss-between ss-within ms-between ms-within)
  (doc 'type '(-> Num Nat Nat Num Num Num Num Num ANOVAResult))
  (doc 'description "Create an ANOVA result")
  (doc 'param 'f-statistic "F statistic")
  (doc 'param 'df-between "degrees of freedom between groups")
  (doc 'param 'df-within "degrees of freedom within groups")
  (doc 'param 'p-value "p-value")
  (doc 'param 'ss-between "sum of squares between groups")
  (doc 'param 'ss-within "sum of squares within groups")
  (doc 'param 'ms-between "mean square between")
  (doc 'param 'ms-within "mean square within")
  (list 'anova-result f-statistic df-between df-within p-value
        ss-between ss-within ms-between ms-within))

;;; anova-result? : Any → Bool
(define (anova-result? x)
  (and (pair? x) (eq? (car x) 'anova-result)))

;;; Accessors
(define (anova-f-statistic r) (list-ref r 1))
(define (anova-df-between r) (list-ref r 2))
(define (anova-df-within r) (list-ref r 3))
(define (anova-p-value r) (list-ref r 4))
(define (anova-ss-between r) (list-ref r 5))
(define (anova-ss-within r) (list-ref r 6))
(define (anova-ms-between r) (list-ref r 7))
(define (anova-ms-within r) (list-ref r 8))

(doc 'section 'ar-model-results)
(define (make-ar-result coefficients intercept residuals fitted sigma aic order)
  (doc 'type '(-> Vec Num Vec Vec Num Num Nat ARResult))
  (doc 'description "Create an autoregressive model result")
  (doc 'param 'coefficients "AR coefficients (phi_1, ..., phi_p)")
  (doc 'param 'intercept "intercept term (mean for centered series)")
  (doc 'param 'residuals "model residuals")
  (doc 'param 'fitted "fitted values")
  (doc 'param 'sigma "residual standard deviation")
  (doc 'param 'aic "Akaike Information Criterion")
  (doc 'param 'order "AR order (p)")
  (list 'ar-result coefficients intercept residuals fitted sigma aic order))

;;; ar-result? : Any → Bool
(define (ar-result? x)
  (and (pair? x) (eq? (car x) 'ar-result)))

;;; Accessors
(define (ar-coefficients r) (list-ref r 1))
(define (ar-intercept r) (list-ref r 2))
(define (ar-residuals r) (list-ref r 3))
(define (ar-fitted r) (list-ref r 4))
(define (ar-sigma r) (list-ref r 5))
(define (ar-aic r) (list-ref r 6))
(doc 'export #t)
(define (ar-order r) (list-ref r 7))

(doc 'section 'forecast-results)
(define (make-forecast-result point lower upper level)
  (doc 'type '(-> Vec Vec Vec Num ForecastResult))
  (doc 'description "Create a forecast result")
  (doc 'param 'point "point forecasts")
  (doc 'param 'lower "lower confidence bounds")
  (doc 'param 'upper "upper confidence bounds")
  (doc 'param 'level "confidence level (e.g., 0.95)")
  (list 'forecast-result point lower upper level))

;;; forecast-result? : Any → Bool
(define (forecast-result? x)
  (and (pair? x) (eq? (car x) 'forecast-result)))

;;; Accessors
(define (forecast-point r) (list-ref r 1))
(define (forecast-lower r) (list-ref r 2))
(define (forecast-upper r) (list-ref r 3))
(define (forecast-level r) (list-ref r 4))
