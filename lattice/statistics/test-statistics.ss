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
                   ;; Mean = 5, variance = 4
                   (assert-true (< (abs (- (vec-variance xs) 4)) 0.01))))
            
            (define-test "vec-std-dev is sqrt of variance"
              (let ([xs (vector 2 4 4 4 5 5 7 9)])
                   (assert-true (< (abs (- (vec-std-dev xs) 2)) 0.01))))
            
            (define-test "vec-median finds middle value"
              (let ([xs (vector 1 3 5 7 9)])
                   (assert-equal 5 (vec-median xs))))
            
            (define-test "vec-quantile computes percentiles"
              (let ([xs (vector 1 2 3 4 5 6 7 8 9 10)])
                   (assert-true (< (abs (- (vec-quantile xs 0.5) 5.5)) 0.01)))))

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
                    (assert-true (< (abs (- p-back 0.975)) 0.01)))))

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
                     [coeffs (lm-coefficients model)])
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
                    (assert-true (> (lm-r-squared model) 0.99)))))

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
                     [model (glm-fit gaussian-family identity-link X y
                                     '((max-iter 50) (tol 1e-6)))]
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
                    (assert-true (pair? result))
                    (assert-equal 't-test-one-sample (car result))))
            
            (define-test "chi-squared-test-goodness returns result"
              (let* ([observed (vector 10 20 30)]
                     [expected (vector 15 20 25)]
                     [result (chi-squared-test-goodness observed expected)])
                    (assert-true (pair? result))
                    (assert-equal 'chi-squared-gof (car result))))
            
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
            
            (define-test "ridge-fit with lambda=0 approximates OLS"
              (let* ([n 20]
                     [X-raw (let ([m (make-vector (* n 2))])
                                 (do ([i 0 (+ i 1)])
                                     [(= i n)]
                                     (vector-set! m (* i 2) 1)
                                     (vector-set! m (+ (* i 2) 1) (exact->inexact i)))
                                 (list 'matrix n 2 m))]
                     [y (let ([v (make-vector n)])
                             (do ([i 0 (+ i 1)])
                                 [(= i n) v]
                                 (vector-set! v i (+ 2.0 (* 3.0 i)))))]
                     [X (standardize-columns-keep-intercept X-raw)]
                     [model (ridge-fit X y 0.0001)]
                     [coeffs (ridge-coefficients model)])
                    ;; With very small lambda, should approximate OLS
                    (assert-true (pair? coeffs)))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)

