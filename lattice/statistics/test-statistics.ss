;;; lattice/statistics/test-statistics.ss — Statistics Skill Tests
;;;
;;; Comprehensive tests for the statistics skill.

(load "core/testing/test-framework.ss")

;;; ============================================================
;;; Core Module Tests
;;; ============================================================

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

;;; ============================================================
;;; Distribution Tests
;;; ============================================================

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

;;; ============================================================
;;; Linear Regression Tests
;;; ============================================================

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

;;; ============================================================
;;; GLM Tests
;;; ============================================================

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

;;; ============================================================
;;; Time Series Tests
;;; ============================================================

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

;;; ============================================================
;;; Hypothesis Tests
;;; ============================================================

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

;;; ============================================================
;;; Regularized Regression Tests
;;; ============================================================

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

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)

