;;; lattice/statistics/core/result-types.ss — Statistical Model Result Types
;;;
;;; Extensible result structures for statistical models and tests.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - Linear model results (coefficients, SE, t-values, p-values, R²)
;;;   - GLM results (deviance, AIC, family info)
;;;   - Test results (statistic, df, p-value, CI)
;;;   - Time series results (AR coefficients, forecasts)
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Linear Model Results
;;; ====

;;; make-linear-model-result : Vec × Vec × Vec × Vec × Num × Num × Vec × Vec × Num × Num × Nat × Nat → LinearModelResult
;;; Create a linear model result with full diagnostics.
;;;
;;; Fields:
;;;   coefficients - fitted coefficients (including intercept if present)
;;;   se           - standard errors of coefficients
;;;   t-values     - t-statistics (coefficients / se)
;;;   p-values     - two-tailed p-values from t-distribution
;;;   r-squared    - coefficient of determination
;;;   adj-r-squared - adjusted R²
;;;   residuals    - raw residuals (y - fitted)
;;;   fitted       - fitted values (X * beta)
;;;   sse          - sum of squared errors
;;;   mse          - mean squared error (sse / df-residual)
;;;   n            - number of observations
;;;   p            - number of parameters (including intercept)
(define (make-linear-model-result coefficients se t-values p-values
                                  r-squared adj-r-squared
                                  residuals fitted sse mse n p)
  (list 'linear-model
        coefficients se t-values p-values
        r-squared adj-r-squared
        residuals fitted sse mse n p))

;;; linear-model? : Any → Bool
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

;;; linear-model-df-residual : LinearModelResult → Nat
;;; Degrees of freedom for residuals = n - p
(define (linear-model-df-residual m)
  (- (linear-model-n m) (linear-model-p m)))

;;; linear-model-sigma : LinearModelResult → Num
;;; Residual standard error = sqrt(mse)
(define (linear-model-sigma m)
  (sqrt (linear-model-mse m)))

;;; ====
;;; GLM Results
;;; ====

;;; make-glm-result : Symbol × Symbol × Vec × Vec × Vec × Vec × Num × Num × Num × Vec × Vec × Nat × Nat × Bool → GLMResult
;;; Create a GLM result.
;;;
;;; Fields:
;;;   family       - family name (gaussian, binomial, poisson, gamma)
;;;   link         - link function name (identity, logit, log, probit)
;;;   coefficients - fitted coefficients
;;;   se           - standard errors
;;;   z-values     - z-statistics (coefficients / se)
;;;   p-values     - two-tailed p-values from normal distribution
;;;   deviance     - residual deviance
;;;   null-deviance - null model deviance
;;;   aic          - Akaike Information Criterion
;;;   residuals    - deviance residuals
;;;   fitted       - fitted values on response scale
;;;   n            - number of observations
;;;   iterations   - IRLS iterations used
;;;   converged?   - whether IRLS converged
(define (make-glm-result family link coefficients se z-values p-values
                         deviance null-deviance aic
                         residuals fitted n iterations converged?)
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

;;; glm-df-residual : GLMResult → Nat
(define (glm-df-residual m)
  (- (glm-n m) (vector-length (glm-coefficients m))))

;;; ====
;;; Hypothesis Test Results
;;; ====

;;; make-test-result : Symbol × Num × Any × Num × (Num × Num) | #f × Num | #f → TestResult
;;; Create a hypothesis test result.
;;;
;;; Fields:
;;;   test-name  - name of the test (t-test-one-sample, chi-squared, etc.)
;;;   statistic  - test statistic value
;;;   df         - degrees of freedom (number or pair for F-test)
;;;   p-value    - p-value
;;;   ci         - confidence interval (pair) or #f if not applicable
;;;   effect-size - effect size or #f if not computed
(define (make-test-result test-name statistic df p-value ci effect-size)
  (list 'test-result test-name statistic df p-value ci effect-size))

;;; test-result? : Any → Bool
(define (test-result? x)
  (and (pair? x) (eq? (car x) 'test-result)))

;;; Accessors
(define (test-name r) (list-ref r 1))
(define (test-statistic r) (list-ref r 2))
(define (test-df r) (list-ref r 3))
(define (test-p-value r) (list-ref r 4))
(define (test-ci r) (list-ref r 5))
(define (test-effect-size r) (list-ref r 6))

;;; test-significant? : TestResult × Num → Bool
;;; Check if test is significant at given alpha level.
(define (test-significant? r alpha)
  (< (test-p-value r) alpha))

;;; ====
;;; ANOVA Results
;;; ====

;;; make-anova-result : Num × Nat × Nat × Num × Num × Num × Num × Num → ANOVAResult
;;; Create an ANOVA result.
;;;
;;; Fields:
;;;   f-statistic   - F statistic
;;;   df-between    - degrees of freedom between groups
;;;   df-within     - degrees of freedom within groups
;;;   p-value       - p-value
;;;   ss-between    - sum of squares between groups
;;;   ss-within     - sum of squares within groups
;;;   ms-between    - mean square between
;;;   ms-within     - mean square within
(define (make-anova-result f-statistic df-between df-within p-value
                           ss-between ss-within ms-between ms-within)
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

;;; ====
;;; AR Model Results
;;; ====

;;; make-ar-result : Vec × Num × Vec × Vec × Num × Num × Nat → ARResult
;;; Create an autoregressive model result.
;;;
;;; Fields:
;;;   coefficients - AR coefficients (phi_1, ..., phi_p)
;;;   intercept    - intercept term (mean for centered series)
;;;   residuals    - model residuals
;;;   fitted       - fitted values
;;;   sigma        - residual standard deviation
;;;   aic          - Akaike Information Criterion
;;;   order        - AR order (p)
(define (make-ar-result coefficients intercept residuals fitted sigma aic order)
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
(define (ar-order r) (list-ref r 7))

;;; ====
;;; Forecast Results
;;; ====

;;; make-forecast-result : Vec × Vec × Vec × Num → ForecastResult
;;; Create a forecast result.
;;;
;;; Fields:
;;;   point      - point forecasts
;;;   lower      - lower confidence bounds
;;;   upper      - upper confidence bounds
;;;   level      - confidence level (e.g., 0.95)
(define (make-forecast-result point lower upper level)
  (list 'forecast-result point lower upper level))

;;; forecast-result? : Any → Bool
(define (forecast-result? x)
  (and (pair? x) (eq? (car x) 'forecast-result)))

;;; Accessors
(define (forecast-point r) (list-ref r 1))
(define (forecast-lower r) (list-ref r 2))
(define (forecast-upper r) (list-ref r 3))
(define (forecast-level r) (list-ref r 4))
