;;; lattice/statistics/regression/linear.ss — Linear Regression
;;;
;;; Ordinary Least Squares (OLS) and Weighted Least Squares (WLS).
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - linear-model-fit: OLS regression with full diagnostics
;;;   - weighted-linear-fit: WLS regression
;;;   - linear-model-predict: Prediction from fitted model
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/matrix.ss
;;;   - linalg/matrix-solvers.ss
;;;   - statistics/core/result-types.ss
;;;   - statistics/core/summary-stats.ss
;;;   - statistics/core/diagnostics.ss
;;;   - statistics/hypothesis/distributions.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/statistics/core/result-types.ss")
(load "lattice/statistics/core/summary-stats.ss")
(load "lattice/statistics/core/diagnostics.ss")
(load "lattice/statistics/hypothesis/distributions.ss")

;;; ============================================================
;;; Ordinary Least Squares
;;; ============================================================

;;; linear-model-fit : Matrix × Vec → LinearModelResult | Error
;;; Fit linear model y = X*beta + epsilon using OLS.
;;; X should include intercept column if desired.
;;; Returns full diagnostic information.
(define (linear-model-fit X y)
  (let* ([n (matrix-rows X)]
         [p (matrix-cols X)])
        ;; Check dimensions
        (if (not (= n (vector-length y)))
            (list 'error 'dimension-mismatch n (vector-length y))
            ;; Fit using QR-based least squares
            (let ([beta (matrix-least-squares X y)])
                 (if (and (pair? beta) (eq? (car beta) 'error))
                     beta
                     (linear-model-compute-diagnostics X y beta n p))))))

;;; linear-model-compute-diagnostics : Matrix × Vec × Vec × Nat × Nat → LinearModelResult
;;; Compute all diagnostic statistics for a fitted linear model.
(define (linear-model-compute-diagnostics X y beta n p)
  (let* (;; Fitted values: y-hat = X * beta
         [fitted (matrix-vec-mul X beta)]
         ;; Residuals: e = y - y-hat
         [residuals (residuals-raw y fitted)]
         ;; SSE = sum(e^2)
         [sse (let loop ([i 0] [s 0])
                   (if (= i n)
                       s
                       (loop (+ i 1)
                             (+ s (expt (vector-ref residuals i) 2)))))]
         ;; Degrees of freedom
         [df-residual (- n p)]
         ;; Mean squared error
         [mse (if (> df-residual 0) (/ sse df-residual) 0)]
         ;; Residual standard error (sigma)
         [sigma (sqrt mse)]
         ;; Standard errors of coefficients
         [se (compute-coefficient-se X mse)]
         ;; t-values and p-values
         [t-values (if (or (not se) (and (pair? se) (eq? (car se) 'error)))
                       (make-vector p 0)
                       (compute-t-values beta se))]
         [p-values (compute-p-values t-values df-residual)]
         ;; R-squared
         [r-squared (compute-r-squared y fitted)]
         ;; Adjusted R-squared
         [adj-r-squared (compute-adj-r-squared r-squared n p)])
        (make-linear-model-result
         beta se t-values p-values
         r-squared adj-r-squared
         residuals fitted sse mse n p)))

;;; compute-coefficient-se : Matrix × Num → Vec | Error
;;; Compute standard errors: SE = sqrt(diag((X'X)^(-1)) * mse)
(define (compute-coefficient-se X mse)
  (let* ([p (matrix-cols X)]
         [XtX (matrix-mul (matrix-transpose X) X)]
         [XtX-inv (matrix-inverse XtX)])
        (if (and (pair? XtX-inv) (eq? (car XtX-inv) 'error))
            XtX-inv
            (let ([se (make-vector p)])
                 (do ([j 0 (+ j 1)])
                     [(= j p) se]
                     (let ([var-j (* (matrix-ref XtX-inv j j) mse)])
                          (vector-set! se j (sqrt (max var-j 0)))))))))

;;; compute-t-values : Vec × Vec → Vec
;;; t-values = beta / se
(define (compute-t-values beta se)
  (let* ([p (vector-length beta)]
         [t (make-vector p)])
        (do ([j 0 (+ j 1)])
            [(= j p) t]
            (let ([sej (vector-ref se j)])
                 (vector-set! t j
                              (if (= sej 0)
                                  0
                                  (/ (vector-ref beta j) sej)))))))

;;; compute-p-values : Vec × Nat → Vec
;;; Two-tailed p-values from t-distribution.
(define (compute-p-values t-values df)
  (let* ([p (vector-length t-values)]
         [pv (make-vector p)])
        (do ([j 0 (+ j 1)])
            [(= j p) pv]
            (vector-set! pv j (t-pvalue-two-tailed (vector-ref t-values j) df)))))

;;; compute-r-squared : Vec × Vec → Num
;;; R² = 1 - SSE/SST
(define (compute-r-squared y fitted)
  (let* ([n (vector-length y)]
         [y-mean (vec-mean y)]
         [sst (let loop ([i 0] [s 0])
                   (if (= i n)
                       s
                       (loop (+ i 1)
                             (+ s (expt (- (vector-ref y i) y-mean) 2)))))]
         [sse (let loop ([i 0] [s 0])
                   (if (= i n)
                       s
                       (loop (+ i 1)
                             (+ s (expt (- (vector-ref y i)
                                           (vector-ref fitted i))
                                        2)))))])
        (if (= sst 0)
            1  ; Perfect fit or no variance
            (- 1 (/ sse sst)))))

;;; compute-adj-r-squared : Num × Nat × Nat → Num
;;; Adjusted R² = 1 - (1 - R²) * (n - 1) / (n - p)
(define (compute-adj-r-squared r-squared n p)
  (if (<= (- n p) 0)
      r-squared
      (- 1 (* (- 1 r-squared) (/ (- n 1) (- n p))))))

;;; ============================================================
;;; Weighted Least Squares
;;; ============================================================

;;; weighted-linear-fit : Matrix × Vec × Vec → LinearModelResult | Error
;;; Fit weighted linear model: minimize sum(w_i * (y_i - X_i*beta)^2)
;;; Equivalent to OLS on (sqrt(W)*X, sqrt(W)*y).
(define (weighted-linear-fit X y weights)
  (let* ([n (matrix-rows X)]
         [p (matrix-cols X)])
        (if (or (not (= n (vector-length y)))
                (not (= n (vector-length weights))))
            (list 'error 'dimension-mismatch n (vector-length y))
            ;; Transform: X* = sqrt(W) * X, y* = sqrt(W) * y
            (let* ([X-weighted (weight-matrix X weights)]
                   [y-weighted (weight-vector y weights)]
                   ;; Fit on weighted data
                   [beta (matrix-least-squares X-weighted y-weighted)])
                  (if (and (pair? beta) (eq? (car beta) 'error))
                      beta
                      ;; Compute diagnostics on original scale
                      (weighted-diagnostics X y beta weights n p))))))

;;; weight-matrix : Matrix × Vec → Matrix
;;; Multiply each row by sqrt(weight).
(define (weight-matrix X weights)
  (let* ([m (matrix-rows X)]
         [n (matrix-cols X)]
         [data (make-vector (* m n))])
        (do ([i 0 (+ i 1)])
            [(= i m)]
            (let ([w (sqrt (vector-ref weights i))])
                 (do ([j 0 (+ j 1)])
                     [(= j n)]
                     (vector-set! data (+ (* i n) j)
                                  (* w (matrix-ref X i j))))))
        (list 'matrix m n data)))

;;; weight-vector : Vec × Vec → Vec
;;; Multiply each element by sqrt(weight).
(define (weight-vector y weights)
  (let* ([n (vector-length y)]
         [y-w (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) y-w]
            (vector-set! y-w i (* (sqrt (vector-ref weights i))
                                  (vector-ref y i))))))

;;; weighted-diagnostics : Matrix × Vec × Vec × Vec × Nat × Nat → LinearModelResult
;;; Compute diagnostics for WLS.
(define (weighted-diagnostics X y beta weights n p)
  (let* ([fitted (matrix-vec-mul X beta)]
         [residuals (residuals-raw y fitted)]
         ;; Weighted SSE = sum(w * e²)
         [sse (let loop ([i 0] [s 0])
                   (if (= i n)
                       s
                       (loop (+ i 1)
                             (+ s (* (vector-ref weights i)
                                     (expt (vector-ref residuals i) 2))))))]
         [df-residual (- n p)]
         [mse (if (> df-residual 0) (/ sse df-residual) 0)]
         [sigma (sqrt mse)]
         ;; SE from weighted (X'WX)^(-1)
         [se (compute-weighted-se X weights mse)]
         [t-values (if (or (not se) (and (pair? se) (eq? (car se) 'error)))
                       (make-vector p 0)
                       (compute-t-values beta se))]
         [p-values (compute-p-values t-values df-residual)]
         ;; Weighted R²: R²_w = 1 - SSE_w / SST_w
         ;; where SST_w = sum(w * (y - y_bar_w)²) and y_bar_w = weighted mean
         [y-bar-w (/ (let loop ([i 0] [s 0])
                          (if (= i n)
                              s
                              (loop (+ i 1)
                                    (+ s (* (vector-ref weights i)
                                            (vector-ref y i))))))
                     (let loop ([i 0] [s 0])
                          (if (= i n)
                              s
                              (loop (+ i 1)
                                    (+ s (vector-ref weights i))))))]
         [sst-w (let loop ([i 0] [s 0])
                     (if (= i n)
                         s
                         (loop (+ i 1)
                               (+ s (* (vector-ref weights i)
                                       (expt (- (vector-ref y i) y-bar-w) 2))))))]
         [r-squared (if (< sst-w 1e-10) 0 (- 1 (/ sse sst-w)))]
         [adj-r-squared (compute-adj-r-squared r-squared n p)])
        (make-linear-model-result
         beta se t-values p-values
         r-squared adj-r-squared
         residuals fitted sse mse n p)))

;;; compute-weighted-se : Matrix × Vec × Num → Vec | Error
;;; SE from (X'WX)^(-1) * mse
(define (compute-weighted-se X weights mse)
  (let* ([m (matrix-rows X)]
         [p (matrix-cols X)]
         ;; Compute X'WX
         [XtWX-data (make-vector (* p p) 0)]
         [XtWX (list 'matrix p p XtWX-data)])
        ;; XtWX[j,k] = sum_i(w_i * X[i,j] * X[i,k])
        (do ([j 0 (+ j 1)])
            [(= j p)]
            (do ([k 0 (+ k 1)])
                [(= k p)]
                (let ([sum (let loop ([i 0] [s 0])
                                (if (= i m)
                                    s
                                    (loop (+ i 1)
                                          (+ s (* (vector-ref weights i)
                                                  (matrix-ref X i j)
                                                  (matrix-ref X i k))))))])
                     (vector-set! XtWX-data (+ (* j p) k) sum))))
        (let ([XtWX-inv (matrix-inverse XtWX)])
             (if (and (pair? XtWX-inv) (eq? (car XtWX-inv) 'error))
                 XtWX-inv
                 (let ([se (make-vector p)])
                      (do ([j 0 (+ j 1)])
                          [(= j p) se]
                          (vector-set! se j (sqrt (max (* (matrix-ref XtWX-inv j j) mse) 0)))))))))

;;; ============================================================
;;; Prediction
;;; ============================================================

;;; linear-model-predict : LinearModelResult × Matrix → Vec
;;; Predict y values for new X.
(define (linear-model-predict model X-new)
  (matrix-vec-mul X-new (linear-model-coefficients model)))

;;; linear-model-predict-interval : LinearModelResult × Matrix × Num → (List Vec Vec Vec)
;;; Predict with prediction intervals.
;;; Returns list of (point-predictions lower-bounds upper-bounds).
;;; Uses prediction interval formula: y_hat +/- t * sigma * sqrt(1 + x'(X'X)^-1 x)
;;; Approximation: uses average leverage when (X'X)^-1 is unavailable.
(define (linear-model-predict-interval model X-new confidence)
  (let* ([n-new (matrix-rows X-new)]
         [p (matrix-cols X-new)]
         [beta (linear-model-coefficients model)]
         [mse (linear-model-mse model)]
         [sigma (sqrt mse)]
         [n-orig (linear-model-n model)]
         [df (- n-orig p)]
         [t-crit (t-quantile (+ 0.5 (/ confidence 2)) (max df 1))]
         [predictions (matrix-vec-mul X-new beta)]
         [lower (make-vector n-new)]
         [upper (make-vector n-new)]
         ;; Compute X'X from new data for leverage approximation
         [XtX-inv (compute-XtX-inverse X-new)])
        (if (and (pair? XtX-inv) (eq? (car XtX-inv) 'error))
            ;; Fallback: use constant SE based on average leverage
            (let* ([avg-leverage (/ p n-orig)]
                   [se-pred (* sigma (sqrt (+ 1 avg-leverage)))]
                   [margin (* t-crit se-pred)])
                  (do ([i 0 (+ i 1)])
                      [(= i n-new)]
                      (let ([pred (vector-ref predictions i)])
                           (vector-set! lower i (- pred margin))
                           (vector-set! upper i (+ pred margin))))
                  (list predictions lower upper))
            ;; Full calculation with leverage
            (begin
             (do ([i 0 (+ i 1)])
                 [(= i n-new)]
                 (let* ([xi (extract-row X-new i)]
                        [leverage (compute-leverage-single xi XtX-inv p)]
                        [se-pred (* sigma (sqrt (+ 1 leverage)))]
                        [margin (* t-crit se-pred)]
                        [pred (vector-ref predictions i)])
                       (vector-set! lower i (- pred margin))
                       (vector-set! upper i (+ pred margin))))
             (list predictions lower upper)))))

;;; compute-XtX-inverse : Matrix → Matrix | Error
;;; Compute (X'X)^-1 for prediction intervals.
(define (compute-XtX-inverse X)
  (let* ([n (matrix-rows X)]
         [p (matrix-cols X)]
         [XtX-data (make-vector (* p p) 0)])
        ;; XtX[j,k] = sum_i X[i,j] * X[i,k]
        (do ([j 0 (+ j 1)])
            [(= j p)]
            (do ([k 0 (+ k 1)])
                [(= k p)]
                (let ([sum (let loop ([i 0] [s 0])
                                (if (= i n)
                                    s
                                    (loop (+ i 1)
                                          (+ s (* (matrix-ref X i j)
                                                  (matrix-ref X i k))))))])
                     (vector-set! XtX-data (+ (* j p) k) sum))))
        (matrix-inverse (list 'matrix p p XtX-data))))

;;; extract-row : Matrix × Nat → Vec
;;; Extract row i from matrix as a vector.
(define (extract-row M i)
  (let* ([p (matrix-cols M)]
         [row (make-vector p)])
        (do ([j 0 (+ j 1)])
            [(= j p) row]
            (vector-set! row j (matrix-ref M i j)))))

;;; compute-leverage-single : Vec × Matrix × Nat → Num
;;; Compute leverage h_ii = x' (X'X)^-1 x for a single observation.
(define (compute-leverage-single x XtX-inv p)
  ;; h = x' * XtX-inv * x
  (let loop-i ([i 0] [sum 0])
       (if (= i p)
           sum
           (let ([inner (let loop-j ([j 0] [s 0])
                             (if (= j p)
                                 s
                                 (loop-j (+ j 1)
                                         (+ s (* (matrix-ref XtX-inv i j)
                                                 (vector-ref x j))))))])
                (loop-i (+ i 1)
                        (+ sum (* (vector-ref x i) inner)))))))

;;; ============================================================
;;; Model Summary
;;; ============================================================

;;; linear-model-f-statistic : LinearModelResult → Num
;;; Overall F-statistic for regression.
;;; F = ((SST - SSE) / (p - 1)) / (SSE / (n - p))
(define (linear-model-f-statistic model)
  (let* ([r2 (linear-model-r-squared model)]
         [n (linear-model-n model)]
         [p (linear-model-p model)]
         [df1 (- p 1)]
         [df2 (- n p)])
        (if (or (<= df1 0) (<= df2 0) (= r2 1))
            0
            (/ (* r2 df2) (* (- 1 r2) df1)))))

;;; linear-model-f-pvalue : LinearModelResult → Num
;;; P-value for overall F-test.
(define (linear-model-f-pvalue model)
  (let* ([f-stat (linear-model-f-statistic model)]
         [p (linear-model-p model)]
         [n (linear-model-n model)]
         [df1 (- p 1)]
         [df2 (- n p)])
        (if (or (<= df1 0) (<= df2 0))
            1
            (f-pvalue f-stat df1 df2))))
