;;; lattice/statistics/test-statistics.ss — Statistics Skill Tests
;;;
;;; Comprehensive tests for the statistics skill.

(load "core/testing/test-framework.ss")

;;; ====
;;; Core Module Tests
;;; ====

(load "lattice/statistics/core/summary-stats.ss")

(test-group "summary-stats"
            
            (define-test "vec-mean computes correct mean"
              (let ([xs (vector 1 2 3 4 5)])
                   (assert-equal 3 (vec-mean xs))))
            
            (define-test "vec-variance computes sample variance"
              (let ([xs (vector 2 4 4 4 5 5 7 9)])
                   ;; Mean = 5, sum of squared deviations = 32
                   ;; Sample variance = 32/7 ≈ 4.571 (uses n-1 denominator)
                   (assert-true (< (abs (- (vec-variance xs) (/ 32 7))) 0.01))))

            (define-test "vec-std-dev is sqrt of variance"
              (let ([xs (vector 2 4 4 4 5 5 7 9)])
                   ;; Sample std-dev = sqrt(32/7) ≈ 2.138
                   (assert-true (< (abs (- (vec-std-dev xs) (sqrt (/ 32 7)))) 0.01))))
            
            (define-test "vec-median finds middle value"
              (let ([xs (vector 1 3 5 7 9)])
                   (assert-equal 5 (vec-median xs))))
            
            (define-test "vec-quantile computes percentiles"
              (let ([xs (vector 1 2 3 4 5 6 7 8 9 10)])
                   (assert-true (< (abs (- (vec-quantile xs 0.5) 5.5)) 0.01))))

            (define-test "quantiles computes multiple quantiles at once"
              (let* ([xs '(1 2 3 4 5 6 7 8 9 10)]
                     [qs (quantiles xs '(0.25 0.5 0.75))])
                (assert-true (< (abs (- (car qs) 3.25)) 0.01))    ; Q1
                (assert-true (< (abs (- (cadr qs) 5.5)) 0.01))    ; median
                (assert-true (< (abs (- (caddr qs) 7.75)) 0.01)))) ; Q3

            (define-test "quantiles matches individual quantile calls"
              (let* ([xs '(5 2 8 1 9 3 7 4 6 10)]  ; unsorted
                     [qs (quantiles xs '(0.1 0.5 0.9))]
                     [q1 (quantile xs 0.1)]
                     [q2 (quantile xs 0.5)]
                     [q3 (quantile xs 0.9)])
                (assert-true (< (abs (- (car qs) q1)) 1e-10))
                (assert-true (< (abs (- (cadr qs) q2)) 1e-10))
                (assert-true (< (abs (- (caddr qs) q3)) 1e-10))))

            (define-test "vec-quantiles works on vectors"
              (let* ([v (vector 1 2 3 4 5 6 7 8 9 10)]
                     [qs (vec-quantiles v '(0.25 0.75))])
                (assert-true (< (abs (- (car qs) 3.25)) 0.01))
                (assert-true (< (abs (- (cadr qs) 7.75)) 0.01))))

            (define-test "iqr uses batch quantiles"
              ;; Verify iqr gives correct result (it now uses quantiles internally)
              (let ([xs '(1 2 3 4 5 6 7 8 9 10)])
                (assert-true (< (abs (- (iqr xs) 4.5)) 0.01)))))

;;; ====
;;; Distribution Tests
;;; ====

(load "lattice/statistics/hypothesis/distributions.ss")

(test-group "distributions"
            
            (define-test "t-cdf at 0 returns 0.5"
              (assert-true (< (abs (- (t-cdf 0 10) 0.5)) 0.01)))
            
            (define-test "chi-squared-cdf is non-decreasing"
              (let ([p1 (chi-squared-cdf 1 5)]
                    [p2 (chi-squared-cdf 2 5)]
                    [p3 (chi-squared-cdf 5 5)])
                   (assert-true (< p1 p2))
                   (assert-true (< p2 p3))))
            
            (define-test "f-cdf returns values in [0, 1]"
              (let ([p (f-cdf 2.5 5 10)])
                   (assert-true (>= p 0))
                   (assert-true (<= p 1))))
            
            (define-test "t-quantile inverts t-cdf approximately"
              (let* ([t-val (t-quantile 0.975 30)]
                     [p-back (t-cdf t-val 30)])
                    (assert-true (< (abs (- p-back 0.975)) 0.01))))
            )

;;; ====
;;; Linear Regression Tests
;;; ====

(load "lattice/statistics/regression/linear.ss")

(test-group "linear-regression"
            
            (define-test "linear-model-fit recovers coefficients"
              ;; y = 2 + 3*x + noise
              (let* ([n 20]
                     [X (let ([m (make-vector (* n 2))])
                             (do ([i 0 (+ i 1)])
                                 [(= i n)]
                                 (vector-set! m (* i 2) 1)           ; intercept
                                 (vector-set! m (+ (* i 2) 1) i))    ; x
                             (list 'matrix n 2 m))]
                     [y (let ([v (make-vector n)])
                             (do ([i 0 (+ i 1)])
                                 [(= i n) v]
                                 (vector-set! v i (+ 2 (* 3 i)))))]
                     [model (linear-model-fit X y)]
                     [coeffs (linear-model-coefficients model)])
                    ;; Should recover approximately 2 and 3
                    (assert-true (< (abs (- (vector-ref coeffs 0) 2)) 0.1))
                    (assert-true (< (abs (- (vector-ref coeffs 1) 3)) 0.1))))
            
            (define-test "R-squared is 1 for perfect fit"
              (let* ([n 10]
                     [X (let ([m (make-vector (* n 2))])
                             (do ([i 0 (+ i 1)])
                                 [(= i n)]
                                 (vector-set! m (* i 2) 1)
                                 (vector-set! m (+ (* i 2) 1) i))
                             (list 'matrix n 2 m))]
                     [y (let ([v (make-vector n)])
                             (do ([i 0 (+ i 1)])
                                 [(= i n) v]
                                 (vector-set! v i (+ 5 (* 2 i)))))]
                     [model (linear-model-fit X y)])
                    (assert-true (> (linear-model-r-squared model) 0.99)))))

;;; ====
;;; GLM Tests
;;; ====

(load "lattice/statistics/regression/glm.ss")

(test-group "glm"
            
            (define-test "glm-fit with gaussian family matches linear"
              (let* ([n 20]
                     [X (let ([m (make-vector (* n 2))])
                             (do ([i 0 (+ i 1)])
                                 [(= i n)]
                                 (vector-set! m (* i 2) 1)
                                 (vector-set! m (+ (* i 2) 1) i))
                             (list 'matrix n 2 m))]
                     [y (let ([v (make-vector n)])
                             (do ([i 0 (+ i 1)])
                                 [(= i n) v]
                                 (vector-set! v i (+ 1 (* 0.5 i)))))]
                     [model (glm-fit gaussian-family identity-link X y 50 1e-6)]
                     [coeffs (glm-coefficients model)])
                    (assert-true (< (abs (- (vector-ref coeffs 0) 1)) 0.5))
                    (assert-true (< (abs (- (vector-ref coeffs 1) 0.5)) 0.2)))))

;;; ====
;;; Time Series Tests
;;; ====

(load "lattice/statistics/timeseries/acf-pacf.ss")
(load "lattice/statistics/timeseries/ar.ss")
(load "lattice/statistics/timeseries/exponential.ss")
(load "lattice/statistics/timeseries/forecast.ss")

(test-group "timeseries"
            
            (define-test "acf at lag 0 is 1"
              (let* ([xs (vector 1 2 3 4 5 6 7 8 9 10)]
                     [acf-vals (acf xs 3)])
                    (assert-equal 1 (vector-ref acf-vals 0))))
            
            (define-test "pacf at lag 0 is 1"
              (let* ([xs (vector 1 2 3 4 5 6 7 8 9 10)]
                     [pacf-vals (pacf xs 3)])
                    (assert-equal 1 (vector-ref pacf-vals 0))))
            
            (define-test "simple-exponential-smooth produces same length"
              (let* ([xs (vector 1 2 3 4 5)]
                     [result (simple-exponential-smooth xs 0.3)]
                     [smoothed (caddr result)])
                    (assert-equal 5 (vector-length smoothed))))
            
            (define-test "mae is 0 for perfect forecast"
              (let* ([actual (vector 1 2 3 4 5)]
                     [forecast (vector 1 2 3 4 5)])
                    (assert-equal 0 (mae actual forecast))))
            
            (define-test "rmse equals mae for constant error"
              (let* ([actual (vector 0 0 0 0)]
                     [forecast (vector 1 1 1 1)])
                    (assert-equal (mae actual forecast) (rmse actual forecast)))))

;;; ====
;;; Hypothesis Tests
;;; ====

(load "lattice/statistics/hypothesis/t-test.ss")
(load "lattice/statistics/hypothesis/chi-squared.ss")
(load "lattice/statistics/hypothesis/anova.ss")

(test-group "hypothesis-tests"
            
            (define-test "t-test-one-sample returns test result"
              (let* ([xs (vector 5.1 4.9 5.0 5.2 4.8 5.1 5.0)]
                     [result (t-test-one-sample xs 5.0)])
                    (assert-true (test-result? result))
                    (assert-equal 't-test-one-sample (test-name result))))

            (define-test "chi-squared-test-goodness returns result"
              (let* ([observed (vector 10 20 30)]
                     [expected (vector 15 20 25)]
                     [result (chi-squared-test-goodness observed expected)])
                    (assert-true (test-result? result))
                    (assert-equal 'chi-squared-goodness (test-name result))))
            
            (define-test "anova-one-way with identical groups gives high p-value"
              (let* ([groups (list (vector 5 5 5 5)
                                   (vector 5 5 5 5)
                                   (vector 5 5 5 5))]
                     [result (anova-one-way groups)]
                     [p-value (anova-p-value result)])
                    (assert-true (> p-value 0.05)))))

;;; ====
;;; Design Matrix Tests (Orthogonal Polynomials)
;;; ====

(load "lattice/statistics/core/design-matrix.ss")

(test-group "orthogonal-polynomials"

            ;; Legendre polynomial tests
            (define-test "legendre-p base cases"
              (assert-equal 1 (legendre-p 0 0.5))
              (assert-equal 0.5 (legendre-p 1 0.5)))

            (define-test "legendre-p P_2(x) = (3x² - 1)/2"
              (let* ([x 0.6]
                     [expected (/ (- (* 3 x x) 1) 2)]
                     [actual (legendre-p 2 x)])
                    (assert-true (< (abs (- actual expected)) 1e-10))))

            (define-test "legendre-p P_3(x) = (5x³ - 3x)/2"
              (let* ([x 0.4]
                     [expected (/ (- (* 5 x x x) (* 3 x)) 2)]
                     [actual (legendre-p 3 x)])
                    (assert-true (< (abs (- actual expected)) 1e-10))))

            ;; Chebyshev polynomial tests
            (define-test "chebyshev-t base cases"
              (assert-equal 1 (chebyshev-t 0 0.5))
              (assert-equal 0.5 (chebyshev-t 1 0.5)))

            (define-test "chebyshev-t T_2(x) = 2x² - 1"
              (let* ([x 0.7]
                     [expected (- (* 2 x x) 1)]
                     [actual (chebyshev-t 2 x)])
                    (assert-true (< (abs (- actual expected)) 1e-10))))

            (define-test "chebyshev-t T_3(x) = 4x³ - 3x"
              (let* ([x 0.3]
                     [expected (- (* 4 x x x) (* 3 x))]
                     [actual (chebyshev-t 3 x)])
                    (assert-true (< (abs (- actual expected)) 1e-10))))

            ;; Hermite polynomial tests (probabilist's)
            (define-test "hermite-h base cases"
              (assert-equal 1 (hermite-h 0 1.5))
              (assert-equal 1.5 (hermite-h 1 1.5)))

            (define-test "hermite-h He_2(x) = x² - 1"
              (let* ([x 2.0]
                     [expected (- (* x x) 1)]
                     [actual (hermite-h 2 x)])
                    (assert-true (< (abs (- actual expected)) 1e-10))))

            (define-test "hermite-h He_3(x) = x³ - 3x"
              (let* ([x 1.5]
                     [expected (- (* x x x) (* 3 x))]
                     [actual (hermite-h 3 x)])
                    (assert-true (< (abs (- actual expected)) 1e-10))))

            ;; Laguerre polynomial tests
            (define-test "laguerre-l base cases"
              (assert-equal 1 (laguerre-l 0 2.0))
              (assert-equal -1.0 (laguerre-l 1 2.0)))  ; 1 - 2 = -1

            (define-test "laguerre-l L_2(x) = (x² - 4x + 2)/2"
              (let* ([x 3.0]
                     [expected (/ (+ (- (* x x) (* 4 x)) 2) 2)]
                     [actual (laguerre-l 2 x)])
                    (assert-true (< (abs (- actual expected)) 1e-10))))

            (define-test "laguerre-l L_3(x) = (-x³ + 9x² - 18x + 6)/6"
              (let* ([x 2.0]
                     [expected (/ (+ (- (- (* x x x)) (* 9 x x)) (* 18 x) (- 6)) 6)]
                     [actual (laguerre-l 3 x)])
                    ;; L_3(2) = (-8 + 36 - 36 + 6)/6 = -2/6 = -1/3
                    (assert-true (< (abs (- actual (/ -1 3))) 1e-10)))))

(test-group "orthogonal-features"

            (define-test "legendre-features creates correct matrix dimensions"
              (let* ([xs (vector 0.0 1.0 2.0 3.0)]
                     [result (legendre-features xs 3)])
                    (assert-equal 4 (matrix-rows result))
                    (assert-equal 4 (matrix-cols result))))  ; degree 3 = 4 columns

            (define-test "legendre-features first column is all 1s (P_0)"
              (let* ([xs (vector 1.0 2.0 3.0 4.0 5.0)]
                     [result (legendre-features xs 2)])
                    (assert-equal 1 (matrix-ref result 0 0))
                    (assert-equal 1 (matrix-ref result 2 0))
                    (assert-equal 1 (matrix-ref result 4 0))))

            (define-test "chebyshev-features creates correct matrix dimensions"
              (let* ([xs (vector 0.0 0.5 1.0)]
                     [result (chebyshev-features xs 4)])
                    (assert-equal 3 (matrix-rows result))
                    (assert-equal 5 (matrix-cols result))))  ; degree 4 = 5 columns

            (define-test "hermite-features creates correct matrix dimensions"
              (let* ([xs (vector -1.0 0.0 1.0 2.0)]
                     [result (hermite-features xs 2)])
                    (assert-equal 4 (matrix-rows result))
                    (assert-equal 3 (matrix-cols result))))

            (define-test "laguerre-features creates correct matrix dimensions"
              (let* ([xs (vector 1.0 2.0 3.0)]
                     [result (laguerre-features xs 3)])
                    (assert-equal 3 (matrix-rows result))
                    (assert-equal 4 (matrix-cols result))))

            (define-test "orthogonal-features dispatches to correct basis"
              (let* ([xs (vector 0.0 1.0 2.0)]
                     [leg (orthogonal-features xs 2 'legendre)]
                     [cheb (orthogonal-features xs 2 'chebyshev)])
                    ;; Both should have same dimensions
                    (assert-equal 3 (matrix-rows leg))
                    (assert-equal 3 (matrix-cols leg))
                    (assert-equal 3 (matrix-rows cheb))
                    (assert-equal 3 (matrix-cols cheb))))

            (define-test "orthogonal-features with constant input handles edge case"
              ;; All same values => zero range
              (let* ([xs (vector 5.0 5.0 5.0)]
                     [result (legendre-features xs 2)])
                    ;; Should not crash, first column still 1s
                    (assert-equal 3 (matrix-rows result))
                    (assert-equal 1 (matrix-ref result 0 0)))))

;;; ====
;;; Regularized Regression Tests
;;; ====

(load "lattice/statistics/regression/regularized.ss")

(test-group "regularized-regression"
            
            (define-test "ridge-fit with small lambda returns valid coefficients"
              ;; Test that ridge-fit runs without error and returns coefficients
              ;; Note: For proper ridge regression, predictors should be standardized
              (let* ([n 10]
                     [X (let ([m (make-vector (* n 2))])
                             (do ([i 0 (+ i 1)])
                                 [(= i n)]
                                 (vector-set! m (* i 2) 1.0)
                                 (vector-set! m (+ (* i 2) 1) (/ i 10.0)))
                             (list 'matrix n 2 m))]
                     [y (let ([v (make-vector n)])
                             (do ([i 0 (+ i 1)])
                                 [(= i n) v]
                                 (vector-set! v i (+ 0.5 (* 0.3 (/ i 10.0))))))]
                     [model (ridge-fit X y 0.1)]
                     [coeffs (linear-model-coefficients model)])
                    ;; Should return valid coefficients vector
                    (assert-true (vector? coeffs))
                    (assert-equal 2 (vector-length coeffs)))))

;;; ====
;;; F-Test Tests
;;; ====

(load "lattice/statistics/hypothesis/f-test.ss")

(test-group "f-test"

            (define-test "f-test-variance returns test result"
              (let* ([xs (vector 4.0 5.0 6.0 7.0 8.0)]
                     [ys (vector 2.0 8.0 1.0 9.0 3.0)]
                     [result (f-test-variance xs ys)])
                    (assert-true (test-result? result))
                    (assert-equal 'f-test-variance (test-name result))))

            (define-test "f-test-variance: equal variances gives high p-value"
              (let* ([xs (vector 10.0 11.0 12.0 13.0 14.0 15.0)]
                     [ys (vector 20.0 21.0 22.0 23.0 24.0 25.0)]
                     [result (f-test-variance xs ys)])
                    ;; Same spread, different means — p should be high
                    (assert-true (> (test-p-value result) 0.5))))

            (define-test "f-test-variance: very different variances gives low p-value"
              (let* ([xs (vector 10.0 10.01 10.02 9.99 9.98 10.0)]
                     [ys (vector 0.0 20.0 -10.0 30.0 5.0 -5.0)]
                     [result (f-test-variance xs ys)])
                    (assert-true (< (test-p-value result) 0.05))))

            (define-test "f-test-regression: high R² gives low p-value"
              (let* ([result (f-test-regression 0.95 100 3)])
                    (assert-true (test-result? result))
                    (assert-true (< (test-p-value result) 0.01))))

            (define-test "f-test-regression: zero R² gives p-value of 1"
              (let* ([result (f-test-regression 0 50 5)])
                    (assert-true (>= (test-p-value result) 0.99))))

            (define-test "f-test-nested: extra predictors with no improvement"
              (let* ([result (f-test-nested-models 0.60 0.60 100 5 3)])
                    ;; Same R² means extra predictors add nothing
                    (assert-true (>= (test-p-value result) 0.99))))

            (define-test "f-test-nested: significant improvement"
              (let* ([result (f-test-nested-models 0.90 0.50 100 5 3)])
                    (assert-true (< (test-p-value result) 0.01))))

            (define-test "levene-test with equal-variance groups"
              (let* ([g1 (vector 10.0 11.0 12.0 13.0 14.0)]
                     [g2 (vector 20.0 21.0 22.0 23.0 24.0)]
                     [g3 (vector 30.0 31.0 32.0 33.0 34.0)]
                     [result (levene-test (list g1 g2 g3))])
                    (assert-true (test-result? result))
                    (assert-equal 'levene-test (test-name result))
                    ;; Same spread => high p-value
                    (assert-true (> (test-p-value result) 0.05))))

            (define-test "bartlett-test with equal-variance groups"
              (let* ([g1 (vector 10.0 11.0 12.0 13.0 14.0)]
                     [g2 (vector 20.0 21.0 22.0 23.0 24.0)]
                     [result (bartlett-test (list g1 g2))])
                    (assert-true (test-result? result))
                    (assert-equal 'bartlett-test (test-name result))
                    (assert-true (> (test-p-value result) 0.05)))))

;;; ====
;;; Expanded T-Test Tests
;;; ====

(test-group "t-test-expanded"

            (define-test "t-test-two-sample equal variance"
              (let* ([xs (vector 5.0 6.0 7.0 8.0 9.0)]
                     [ys (vector 15.0 16.0 17.0 18.0 19.0)]
                     [result (t-test-two-sample xs ys #t)])
                    (assert-true (test-result? result))
                    (assert-equal 't-test-two-sample-equal (test-name result))
                    ;; Means are clearly different
                    (assert-true (< (test-p-value result) 0.001))))

            (define-test "t-test-two-sample Welch's"
              (let* ([xs (vector 5.0 6.0 7.0 8.0 9.0)]
                     [ys (vector 15.0 16.0 17.0 18.0 19.0)]
                     [result (t-test-two-sample xs ys #f)])
                    (assert-true (test-result? result))
                    (assert-equal 't-test-two-sample-welch (test-name result))
                    (assert-true (< (test-p-value result) 0.001))))

            (define-test "t-test-two-sample: identical groups gives high p-value"
              (let* ([xs (vector 5.0 6.0 7.0 8.0 9.0)]
                     [ys (vector 5.0 6.0 7.0 8.0 9.0)]
                     [result (t-test-two-sample xs ys)])
                    (assert-true (> (test-p-value result) 0.99))))

            (define-test "t-test-paired detects difference"
              (let* ([before (vector 10.0 12.0 14.0 16.0 18.0)]
                     [after  (vector 15.0 17.0 19.0 21.0 23.0)]
                     [result (t-test-paired before after)])
                    (assert-true (test-result? result))
                    (assert-equal 't-test-paired (test-name result))
                    ;; Consistent 5-unit increase
                    (assert-true (< (test-p-value result) 0.001))))

            (define-test "t-test-paired: no difference gives high p-value"
              (let* ([xs (vector 10.0 12.0 14.0 16.0 18.0)]
                     [ys (vector 10.1 11.9 14.1 15.9 18.0)]
                     [result (t-test-paired xs ys)])
                    ;; Tiny random differences => high p-value
                    (assert-true (> (test-p-value result) 0.5))))

            (define-test "t-test-one-sample-greater: sample above mu0"
              (let* ([xs (vector 10.0 11.0 12.0 13.0 14.0 15.0)]
                     [result (t-test-one-sample-greater xs 5.0)])
                    (assert-true (test-result? result))
                    ;; Mean ~12.5, testing > 5 — should be highly significant
                    (assert-true (< (test-p-value result) 0.001))))

            (define-test "t-test-one-sample-less: sample above mu0 gives high p-value"
              (let* ([xs (vector 10.0 11.0 12.0 13.0 14.0 15.0)]
                     [result (t-test-one-sample-less xs 5.0)])
                    ;; Mean ~12.5, testing < 5 — not significant
                    (assert-true (> (test-p-value result) 0.99))))

            (define-test "t-test-one-sample effect size is Cohen's d"
              (let* ([xs (vector 5.1 4.9 5.0 5.2 4.8 5.1 5.0)]
                     [result (t-test-one-sample xs 5.0)]
                     [d (test-effect-size result)])
                    ;; Effect size should be small (mean ≈ mu0)
                    (assert-true (< (abs d) 0.5))))

            (define-test "t-test-one-sample CI contains true mean"
              (let* ([xs (vector 5.1 4.9 5.0 5.2 4.8 5.1 5.0)]
                     [result (t-test-one-sample xs 5.0)]
                     [ci (test-ci result)])
                    ;; 95% CI should contain 5.0
                    (assert-true (<= (car ci) 5.0))
                    (assert-true (>= (cdr ci) 5.0)))))

;;; ====
;;; Expanded Chi-Squared Tests
;;; ====

(test-group "chi-squared-expanded"

            (define-test "chi-squared-test-goodness-uniform"
              (let* ([observed (vector 25 25 25 25)]
                     [result (chi-squared-test-goodness-uniform observed)])
                    (assert-true (test-result? result))
                    ;; Perfect uniform => chi-sq = 0
                    (assert-true (< (test-statistic result) 0.01))
                    (assert-true (> (test-p-value result) 0.99))))

            (define-test "chi-squared-test-goodness: bad fit"
              (let* ([observed (vector 90 5 5)]
                     [expected (vector 33 34 33)]
                     [result (chi-squared-test-goodness observed expected)])
                    ;; Very bad fit => low p-value
                    (assert-true (< (test-p-value result) 0.001))))

            (define-test "chi-squared-test-independence: independent vars"
              ;; Construct a table where rows and columns are independent
              ;; Expected: each cell = row_total * col_total / grand_total
              (let* ([table (list 'matrix 2 2 (vector 25 25 25 25))]
                     [result (chi-squared-test-independence table)])
                    (assert-true (test-result? result))
                    (assert-equal 'chi-squared-independence (test-name result))
                    ;; Perfect independence => chi-sq ≈ 0
                    (assert-true (< (test-statistic result) 0.01))))

            (define-test "chi-squared-test-independence: strong association"
              ;; Diagonal-heavy table = strong association
              (let* ([table (list 'matrix 2 2 (vector 90 10 10 90))]
                     [result (chi-squared-test-independence table)])
                    (assert-true (< (test-p-value result) 0.001))
                    ;; Cramér's V should be high
                    (assert-true (> (test-effect-size result) 0.5))))

            (define-test "chi-squared-test-goodness-proportions"
              (let* ([observed (vector 30 20 50)]
                     [proportions (vector 0.3 0.2 0.5)]
                     [result (chi-squared-test-goodness-proportions observed proportions)])
                    ;; Observed matches expected proportions perfectly
                    (assert-true (> (test-p-value result) 0.99)))))

;;; ====
;;; Expanded ANOVA Tests
;;; ====

(test-group "anova-expanded"

            (define-test "anova-one-way: different groups gives low p-value"
              (let* ([g1 (vector 1.0 2.0 3.0 4.0 5.0)]
                     [g2 (vector 11.0 12.0 13.0 14.0 15.0)]
                     [g3 (vector 21.0 22.0 23.0 24.0 25.0)]
                     [result (anova-one-way (list g1 g2 g3))])
                    (assert-true (< (anova-p-value result) 0.001))))

            (define-test "anova-one-way: degrees of freedom"
              (let* ([g1 (vector 1.0 2.0 3.0)]
                     [g2 (vector 4.0 5.0 6.0)]
                     [g3 (vector 7.0 8.0 9.0)]
                     [result (anova-one-way (list g1 g2 g3))])
                    ;; k=3 groups, n=9 total
                    (assert-equal 2 (anova-df-between result))  ; k-1
                    (assert-equal 6 (anova-df-within result)))) ; n-k

            (define-test "anova eta-squared for identical groups is 0"
              (let* ([g1 (vector 5.0 5.0 5.0 5.0)]
                     [g2 (vector 5.0 5.0 5.0 5.0)]
                     [result (anova-one-way (list g1 g2))]
                     [eta2 (anova-eta-squared result)])
                    (assert-true (< eta2 0.01))))

            (define-test "anova eta-squared: well-separated groups"
              (let* ([g1 (vector 1.0 1.0 1.0)]
                     [g2 (vector 100.0 100.0 100.0)]
                     [result (anova-one-way (list g1 g2))]
                     [eta2 (anova-eta-squared result)])
                    ;; Nearly all variance explained by group membership
                    (assert-true (> eta2 0.99))))

            (define-test "anova omega-squared <= eta-squared"
              (let* ([g1 (vector 1.0 2.0 3.0 4.0)]
                     [g2 (vector 5.0 6.0 7.0 8.0)]
                     [result (anova-one-way (list g1 g2))]
                     [eta2 (anova-eta-squared result)]
                     [omega2 (anova-omega-squared result)])
                    ;; Omega-squared is less biased, always <= eta-squared
                    (assert-true (<= omega2 eta2))))

            (define-test "tukey-hsd-bonferroni returns pairwise comparisons"
              (let* ([g1 (vector 1.0 2.0 3.0)]
                     [g2 (vector 10.0 11.0 12.0)]
                     [g3 (vector 5.0 6.0 7.0)]
                     [results (tukey-hsd-bonferroni (list g1 g2 g3) 0.05)])
                    ;; 3 groups => 3 pairwise comparisons
                    (assert-equal 3 (length results))
                    ;; g1 vs g2 should be significant (diff ≈ 9)
                    (let ([comparison-0-1 (car results)])
                         (assert-true (list-ref comparison-0-1 4))))))

;;; ====
;;; Diagnostics Tests
;;; ====

(load "lattice/statistics/core/diagnostics.ss")

(test-group "diagnostics"

            (define-test "hat-matrix-diagonal: leverages sum to p"
              ;; Sum of leverages = trace(H) = p (number of predictors)
              (let* ([n 10]
                     [X (let ([m (make-vector (* n 2))])
                             (do ([i 0 (+ i 1)])
                                 [(= i n)]
                                 (vector-set! m (* i 2) 1.0)
                                 (vector-set! m (+ (* i 2) 1) (+ 1.0 i)))
                             (list 'matrix n 2 m))]
                     [h (hat-matrix-diagonal X)]
                     [h-sum (let loop ([i 0] [s 0])
                                 (if (= i n) s
                                     (loop (+ i 1) (+ s (vector-ref h i)))))])
                    ;; Sum of leverages should equal 2 (number of predictors)
                    (assert-true (< (abs (- h-sum 2)) 0.01))))

            (define-test "hat-matrix-diagonal: values in [0, 1]"
              (let* ([n 10]
                     [X (let ([m (make-vector (* n 2))])
                             (do ([i 0 (+ i 1)])
                                 [(= i n)]
                                 (vector-set! m (* i 2) 1.0)
                                 (vector-set! m (+ (* i 2) 1) (+ 1.0 i)))
                             (list 'matrix n 2 m))]
                     [h (hat-matrix-diagonal X)])
                    (let loop ([i 0])
                         (when (< i n)
                               (assert-true (>= (vector-ref h i) 0))
                               (assert-true (<= (vector-ref h i) 1))
                               (loop (+ i 1))))))

            (define-test "cooks-distance: low for well-fit data"
              (let* ([n 10]
                     ;; Perfect linear: y = 1 + 2x, residuals = 0
                     [residuals (make-vector n 0.0)]
                     [leverage (make-vector n 0.2)]
                     [D (cooks-distance residuals leverage 2 0.01)])
                    ;; All Cook's D should be 0
                    (let loop ([i 0])
                         (when (< i n)
                               (assert-true (< (vector-ref D i) 0.001))
                               (loop (+ i 1))))))

            (define-test "cooks-distance-threshold scales inversely with n"
              (assert-true (< (cooks-distance-threshold 100)
                              (cooks-distance-threshold 10))))

            (define-test "residuals-studentized preserves sign"
              (let* ([raw (vector -2.0 1.0 -0.5 3.0)]
                     [leverage (vector 0.1 0.2 0.15 0.1)]
                     [sigma 1.0]
                     [stud (residuals-studentized raw leverage sigma)])
                    ;; Signs should match
                    (assert-true (< (vector-ref stud 0) 0))
                    (assert-true (> (vector-ref stud 1) 0))
                    (assert-true (< (vector-ref stud 2) 0))
                    (assert-true (> (vector-ref stud 3) 0))))

            (define-test "vif: uncorrelated predictors give VIF ≈ 1"
              ;; Two uncorrelated columns (zero mean-centered correlation)
              ;; Col1: [1,2,3,4,5,6], Col2: [3,1,4,6,2,5] — near-zero correlation
              (let* ([X (list 'matrix 6 2
                              (vector 1.0 3.0  2.0 1.0  3.0 4.0
                                      4.0 6.0  5.0 2.0  6.0 5.0))]
                     [vifs (vif X)])
                    ;; Low correlation => VIF near 1
                    (assert-true (< (vector-ref vifs 0) 2.0))
                    (assert-true (< (vector-ref vifs 1) 2.0)))))

;;; ====
;;; Link Functions Tests
;;; ====

(load "lattice/statistics/regression/link-functions.ss")

(test-group "link-functions"

            (define-test "identity-link roundtrip"
              (let* ([g (link-g identity-link)]
                     [g-inv (link-inverse identity-link)])
                    (assert-equal 5.0 (g-inv (g 5.0)))
                    (assert-equal -3.0 (g-inv (g -3.0)))))

            (define-test "logit-link roundtrip"
              (let* ([g (link-g logit-link)]
                     [g-inv (link-inverse logit-link)])
                    ;; g-inv(g(0.3)) ≈ 0.3
                    (assert-true (< (abs (- (g-inv (g 0.3)) 0.3)) 1e-6))
                    (assert-true (< (abs (- (g-inv (g 0.8)) 0.8)) 1e-6))))

            (define-test "logit-link: g(0.5) = 0"
              (let ([g (link-g logit-link)])
                    (assert-true (< (abs (g 0.5)) 1e-6))))

            (define-test "logit-link inverse is sigmoid"
              (let ([g-inv (link-inverse logit-link)])
                    ;; sigmoid(0) = 0.5
                    (assert-true (< (abs (- (g-inv 0) 0.5)) 1e-6))))

            (define-test "log-link roundtrip"
              (let* ([g (link-g log-link)]
                     [g-inv (link-inverse log-link)])
                    (assert-true (< (abs (- (g-inv (g 2.0)) 2.0)) 1e-6))
                    (assert-true (< (abs (- (g-inv (g 0.1)) 0.1)) 1e-6))))

            (define-test "log-link: g(1) = 0"
              (let ([g (link-g log-link)])
                    (assert-true (< (abs (g 1)) 1e-6))))

            (define-test "inverse-link roundtrip"
              (let* ([g (link-g inverse-link)]
                     [g-inv (link-inverse inverse-link)])
                    ;; g-inv(g(2)) = g-inv(0.5) = 2
                    (assert-true (< (abs (- (g-inv (g 2.0)) 2.0)) 1e-6))))

            (define-test "probit-link roundtrip"
              (let* ([g (link-g probit-link)]
                     [g-inv (link-inverse probit-link)])
                    ;; probit(0.5) = 0, so g-inv(g(0.5)) ≈ 0.5
                    (assert-true (< (abs (- (g-inv (g 0.5)) 0.5)) 0.01))
                    (assert-true (< (abs (- (g-inv (g 0.9)) 0.9)) 0.01))))

            (define-test "canonical-link dispatches correctly"
              (assert-equal 'identity (link-name (canonical-link 'gaussian)))
              (assert-equal 'logit (link-name (canonical-link 'binomial)))
              (assert-equal 'log (link-name (canonical-link 'poisson)))
              (assert-equal 'inverse (link-name (canonical-link 'gamma))))

            (define-test "link-derivative: identity is always 1"
              (let ([g-prime (link-derivative identity-link)])
                    (assert-equal 1 (g-prime 0.5))
                    (assert-equal 1 (g-prime 100)))))

;;; ====
;;; Differencing Tests
;;; ====

(load "lattice/statistics/timeseries/differencing.ss")

(test-group "differencing"

            (define-test "vec-diff: simple first difference"
              (let* ([xs (vector 1 3 6 10 15)]
                     [d (vec-diff xs)])
                    ;; Diffs: 2, 3, 4, 5
                    (assert-equal 4 (vector-length d))
                    (assert-equal 2 (vector-ref d 0))
                    (assert-equal 3 (vector-ref d 1))
                    (assert-equal 4 (vector-ref d 2))
                    (assert-equal 5 (vector-ref d 3))))

            (define-test "difference order 0 returns original"
              (let* ([xs (vector 1 2 3 4 5)]
                     [d (difference xs 0)])
                    (assert-equal xs d)))

            (define-test "difference order 2: second differences"
              (let* ([xs (vector 1 3 6 10 15)]
                     [d2 (difference xs 2)])
                    ;; First diff: 2,3,4,5. Second diff: 1,1,1
                    (assert-equal 3 (vector-length d2))
                    (assert-equal 1 (vector-ref d2 0))
                    (assert-equal 1 (vector-ref d2 1))
                    (assert-equal 1 (vector-ref d2 2))))

            (define-test "lag-diff with lag 2"
              (let* ([xs (vector 1 2 4 7 11)]
                     [d (lag-diff xs 2)])
                    ;; x[2]-x[0]=3, x[3]-x[1]=5, x[4]-x[2]=7
                    (assert-equal 3 (vector-length d))
                    (assert-equal 3 (vector-ref d 0))
                    (assert-equal 5 (vector-ref d 1))
                    (assert-equal 7 (vector-ref d 2))))

            (define-test "integrate inverts vec-diff"
              (let* ([xs (vector 1 3 6 10 15)]
                     [d (vec-diff xs)]
                     [recovered (integrate d 1)])
                    ;; Should recover original
                    (assert-equal 5 (vector-length recovered))
                    (assert-equal 1 (vector-ref recovered 0))
                    (assert-equal 3 (vector-ref recovered 1))
                    (assert-equal 6 (vector-ref recovered 2))
                    (assert-equal 10 (vector-ref recovered 3))
                    (assert-equal 15 (vector-ref recovered 4))))

            (define-test "seasonal-difference with period 4"
              (let* ([xs (vector 1 2 3 4 5 6 7 8)]
                     [d (seasonal-difference xs 4 1)])
                    ;; x[4]-x[0]=4, x[5]-x[1]=4, x[6]-x[2]=4, x[7]-x[3]=4
                    (assert-equal 4 (vector-length d))
                    (assert-equal 4 (vector-ref d 0))
                    (assert-equal 4 (vector-ref d 3))))

            (define-test "log-transform and exp are inverses"
              (let* ([xs (vector 1.0 2.0 3.0 4.0 5.0)]
                     [logged (log-transform xs)]
                     [recovered (exp-transform logged)])
                    (let loop ([i 0])
                         (when (< i 5)
                               (assert-true (< (abs (- (vector-ref recovered i)
                                                       (vector-ref xs i)))
                                               1e-10))
                               (loop (+ i 1))))))

            (define-test "box-cox lambda=1 is linear"
              (let* ([xs (vector 1.0 2.0 3.0 4.0)]
                     [result (box-cox xs 1)])
                    ;; (x^1 - 1)/1 = x - 1
                    (assert-true (< (abs (- (vector-ref result 0) 0.0)) 1e-10))
                    (assert-true (< (abs (- (vector-ref result 1) 1.0)) 1e-10))
                    (assert-true (< (abs (- (vector-ref result 2) 2.0)) 1e-10))))

            (define-test "box-cox lambda≈0 is log"
              (let* ([xs (vector 1.0 2.0 3.0)]
                     [bc (box-cox xs 0)]
                     [lt (log-transform xs)])
                    ;; Lambda = 0 => Box-Cox = log
                    (let loop ([i 0])
                         (when (< i 3)
                               (assert-true (< (abs (- (vector-ref bc i)
                                                       (vector-ref lt i)))
                                               1e-10))
                               (loop (+ i 1))))))

            (define-test "is-stationary? on constant series"
              (let ([xs (vector 5 5 5 5 5 5 5 5 5 5)])
                    (assert-true (is-stationary? xs)))))

;;; ====
;;; Moving Average Tests
;;; ====

(load "lattice/statistics/timeseries/ma.ss")

(test-group "moving-average"

            (define-test "moving-average length is n - window + 1"
              (let* ([xs (vector 1 2 3 4 5 6 7)]
                     [result (moving-average xs 3)])
                    (assert-equal 5 (vector-length result))))

            (define-test "moving-average window=1 is identity"
              (let* ([xs (vector 1.0 2.0 3.0)]
                     [result (moving-average xs 1)])
                    (assert-equal 3 (vector-length result))
                    (assert-equal 1.0 (vector-ref result 0))
                    (assert-equal 2.0 (vector-ref result 1))
                    (assert-equal 3.0 (vector-ref result 2))))

            (define-test "moving-average values are correct"
              (let* ([xs (vector 1.0 2.0 3.0 4.0 5.0)]
                     [result (moving-average xs 3)])
                    ;; (1+2+3)/3=2, (2+3+4)/3=3, (3+4+5)/3=4
                    (assert-equal 2.0 (vector-ref result 0))
                    (assert-equal 3.0 (vector-ref result 1))
                    (assert-equal 4.0 (vector-ref result 2))))

            (define-test "exponential-moving-average same length as input"
              (let* ([xs (vector 1.0 2.0 3.0 4.0 5.0)]
                     [result (exponential-moving-average xs 0.3)])
                    (assert-equal 5 (vector-length result))))

            (define-test "exponential-moving-average alpha=1 is identity"
              (let* ([xs (vector 3.0 1.0 4.0 1.0 5.0)]
                     [result (exponential-moving-average xs 1.0)])
                    (assert-equal 3.0 (vector-ref result 0))
                    (assert-equal 1.0 (vector-ref result 1))
                    (assert-equal 4.0 (vector-ref result 2))))

            (define-test "weighted-moving-average with equal weights = simple MA"
              (let* ([xs (vector 1.0 2.0 3.0 4.0 5.0)]
                     [weights (vector 1.0 1.0 1.0)]
                     [result (weighted-moving-average xs weights)])
                    ;; Equal weights with window 3 = simple moving average
                    (assert-true (< (abs (- (vector-ref result 0) 2.0)) 1e-10))
                    (assert-true (< (abs (- (vector-ref result 1) 3.0)) 1e-10))
                    (assert-true (< (abs (- (vector-ref result 2) 4.0)) 1e-10)))))

;;; ====
;;; Result Types Tests
;;; ====

(test-group "result-types"

            (define-test "test-result roundtrip"
              (let ([r (make-test-result 'my-test 2.5 10 0.03 (cons 1.0 4.0) 0.8)])
                    (assert-true (test-result? r))
                    (assert-equal 'my-test (test-name r))
                    (assert-equal 2.5 (test-statistic r))
                    (assert-equal 10 (test-df r))
                    (assert-equal 0.03 (test-p-value r))
                    (assert-equal 1.0 (car (test-ci r)))
                    (assert-equal 4.0 (cdr (test-ci r)))
                    (assert-equal 0.8 (test-effect-size r))))

            (define-test "test-significant? at various alpha levels"
              (let ([r (make-test-result 'test 2.0 10 0.04 #f #f)])
                    (assert-true (test-significant? r 0.05))
                    (assert-false (test-significant? r 0.01))))

            (define-test "anova-result roundtrip"
              (let ([r (make-anova-result 5.0 2 12 0.02 100 200 50 16.67)])
                    (assert-true (anova-result? r))
                    (assert-equal 5.0 (anova-f-statistic r))
                    (assert-equal 2 (anova-df-between r))
                    (assert-equal 12 (anova-df-within r))
                    (assert-equal 0.02 (anova-p-value r))
                    (assert-equal 100 (anova-ss-between r))
                    (assert-equal 200 (anova-ss-within r))))

            (define-test "linear-model-result accessors"
              (let* ([coeffs (vector 1.0 2.0)]
                     [se (vector 0.1 0.2)]
                     [tvals (vector 10.0 10.0)]
                     [pvals (vector 0.001 0.001)]
                     [resid (vector 0.1 -0.1)]
                     [fitted (vector 1.9 4.1)]
                     [model (make-linear-model-result coeffs se tvals pvals
                                                       0.99 0.98 resid fitted 0.02 0.01 2 2)])
                    (assert-true (linear-model? model))
                    (assert-equal coeffs (linear-model-coefficients model))
                    (assert-equal 0.99 (linear-model-r-squared model))
                    (assert-equal 0 (linear-model-df-residual model))))

            (define-test "ar-result roundtrip"
              (let* ([coeffs (vector 0.5 -0.3)]
                     [r (make-ar-result coeffs 2.0 (vector 0 0) (vector 0 0) 1.0 100 2)])
                    (assert-true (ar-result? r))
                    (assert-equal coeffs (ar-coefficients r))
                    (assert-equal 2.0 (ar-intercept r))
                    (assert-equal 2 (ar-order r))))

            (define-test "forecast-result roundtrip"
              (let* ([point (vector 5.0 6.0 7.0)]
                     [lower (vector 3.0 4.0 5.0)]
                     [upper (vector 7.0 8.0 9.0)]
                     [r (make-forecast-result point lower upper 0.95)])
                    (assert-true (forecast-result? r))
                    (assert-equal point (forecast-point r))
                    (assert-equal 0.95 (forecast-level r)))))

;;; ====
;;; Expanded Distribution Tests
;;; ====

(test-group "distributions-expanded"

            (define-test "chi-squared-quantile inverts chi-squared-cdf"
              (let* ([q (chi-squared-quantile 0.95 5)]
                     [p-back (chi-squared-cdf q 5)])
                    (assert-true (< (abs (- p-back 0.95)) 0.01))))

            (define-test "f-quantile inverts f-cdf"
              (let* ([q (f-quantile 0.95 5 10)]
                     [p-back (f-cdf q 5 10)])
                    (assert-true (< (abs (- p-back 0.95)) 0.01))))

            (define-test "t-pdf is positive"
              (assert-true (> (t-pdf 0 10) 0))
              (assert-true (> (t-pdf 2 10) 0))
              (assert-true (> (t-pdf -3 10) 0)))

            (define-test "t-pdf is symmetric"
              (assert-true (< (abs (- (t-pdf 2 10) (t-pdf -2 10))) 1e-10)))

            (define-test "chi-squared-cdf at 0 is 0"
              (assert-equal 0 (chi-squared-cdf 0 5)))

            (define-test "f-cdf at 0 is 0"
              (assert-equal 0 (f-cdf 0 5 10)))

            (define-test "t-pvalue-two-tailed: large t gives small p"
              (assert-true (< (t-pvalue-two-tailed 10 30) 0.001)))

            (define-test "t-pvalue-two-tailed: t=0 gives p=1"
              (assert-true (< (abs (- (t-pvalue-two-tailed 0 30) 1.0)) 0.01)))

            (define-test "f-pvalue: large F gives small p"
              (assert-true (< (f-pvalue 50 5 10) 0.001)))

            (define-test "chi-squared-pvalue: large chi-sq gives small p"
              (assert-true (< (chi-squared-pvalue 50 5) 0.001))))

;;; ====
;;; Model Protocol Tests
;;; ====

(test-group "model-protocol-via-linear"

            (define-test "linear model has valid residuals"
              (let* ([n 10]
                     [X (let ([m (make-vector (* n 2))])
                             (do ([i 0 (+ i 1)])
                                 [(= i n)]
                                 (vector-set! m (* i 2) 1.0)
                                 (vector-set! m (+ (* i 2) 1) (+ 0.0 i)))
                             (list 'matrix n 2 m))]
                     [y (let ([v (make-vector n)])
                             (do ([i 0 (+ i 1)])
                                 [(= i n) v]
                                 (vector-set! v i (+ 2.0 (* 3.0 i)))))]
                     [model (linear-model-fit X y)]
                     [resid (linear-model-residuals model)]
                     [fitted (linear-model-fitted model)])
                    ;; Perfect fit: all residuals should be ~0
                    (let loop ([i 0])
                         (when (< i n)
                               (assert-true (< (abs (vector-ref resid i)) 0.01))
                               (loop (+ i 1))))
                    ;; Fitted + residual = y
                    (let loop ([i 0])
                         (when (< i n)
                               (assert-true (< (abs (- (+ (vector-ref fitted i)
                                                          (vector-ref resid i))
                                                       (vector-ref y i)))
                                               0.01))
                               (loop (+ i 1))))))

            (define-test "linear model SE and t-values"
              (let* ([n 20]
                     [X (let ([m (make-vector (* n 2))])
                             (do ([i 0 (+ i 1)])
                                 [(= i n)]
                                 (vector-set! m (* i 2) 1.0)
                                 (vector-set! m (+ (* i 2) 1) (+ 0.0 i)))
                             (list 'matrix n 2 m))]
                     [y (let ([v (make-vector n)])
                             (do ([i 0 (+ i 1)])
                                 [(= i n) v]
                                 (vector-set! v i (+ 2.0 (* 3.0 i)))))]
                     [model (linear-model-fit X y)]
                     [se (linear-model-se model)]
                     [tvals (linear-model-t-values model)])
                    ;; SE should be finite and positive
                    (assert-true (> (vector-length se) 0))
                    ;; t-values should be large for perfect fit
                    (assert-true (> (abs (vector-ref tvals 1)) 100)))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests-and-exit)

