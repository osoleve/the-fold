;;; lattice/statistics/regression/regularized.ss — Regularized Regression
;;;
;;; Ridge, Lasso, and Elastic Net regression.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - ridge-fit: L2 regularized regression
;;;   - lasso-fit: L1 regularized regression (coordinate descent)
;;;   - elastic-net-fit: Combined L1 + L2 regularization
;;;   - cross-validate-lambda: K-fold CV for hyperparameter selection
;;;
;;; Note: Regularized methods require standardized predictors!
;;; Use design-matrix.ss standardize-columns before fitting.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/matrix.ss
;;;   - linalg/matrix-solvers.ss
;;;   - statistics/core/result-types.ss
;;;   - statistics/core/summary-stats.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/statistics/core/result-types.ss")
(load "lattice/statistics/core/summary-stats.ss")

;;; ============================================================
;;; Ridge Regression
;;; ============================================================

;;; Ridge regression minimizes: ||y - X*beta||² + lambda * ||beta||²
;;; Closed-form solution: beta = (X'X + lambda*I)^(-1) X'y

;;; ridge-fit : Matrix × Vec × Num → LinearModelResult | Error
;;; Fit ridge regression with given lambda.
;;; X should be standardized (except intercept column if present).
;;; lambda: regularization parameter (> 0)
(define (ridge-fit X y lambda)
  (let* ([n (matrix-rows X)]
         [p (matrix-cols X)])
        (if (not (= n (vector-length y)))
            (list 'error 'dimension-mismatch n (vector-length y))
            (if (<= lambda 0)
                (list 'error 'invalid-lambda lambda)
                (let* ([XtX (matrix-mul (matrix-transpose X) X)]
                       [I (identity-matrix p)]
                       [regularized (matrix-add XtX (matrix-scale lambda I))]
                       [Xty (matrix-vec-mul (matrix-transpose X) y)]
                       [beta (matrix-solve regularized Xty)])
                      (if (and (pair? beta) (eq? (car beta) 'error))
                          beta
                          ;; Compute diagnostics (no SE for ridge)
                          (ridge-diagnostics X y beta lambda n p)))))))

;;; ridge-diagnostics : Matrix × Vec × Vec × Num × Nat × Nat → LinearModelResult
(define (ridge-diagnostics X y beta lambda n p)
  (let* ([fitted (matrix-vec-mul X beta)]
         [residuals (make-vector n)]
         [sse 0])
        ;; Compute residuals and SSE
        (do ([i 0 (+ i 1)])
            [(= i n)]
            (let ([r (- (vector-ref y i) (vector-ref fitted i))])
                 (vector-set! residuals i r)
                 (set! sse (+ sse (* r r)))))
        (let* ([df-residual (- n p)]  ; Approximate
               [mse (if (> df-residual 0) (/ sse df-residual) 0)]
               [r-squared (compute-r-squared-ridge y fitted)]
               ;; No SE, t-values, p-values for ridge
               [se (make-vector p 0)]
               [t-values (make-vector p 0)]
               [p-values (make-vector p 1)])
              (make-linear-model-result
               beta se t-values p-values
               r-squared r-squared  ; adj-r² same as r² for ridge
               residuals fitted sse mse n p))))

;;; compute-r-squared-ridge : Vec × Vec → Num
(define (compute-r-squared-ridge y fitted)
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
        (if (= sst 0) 1 (- 1 (/ sse sst)))))

;;; ============================================================
;;; Lasso Regression
;;; ============================================================

;;; Lasso minimizes: (1/2n)||y - X*beta||² + lambda * ||beta||_1
;;; Uses coordinate descent with soft-thresholding.

;;; lasso-fit : Matrix × Vec × Num × Nat × Num → LinearModelResult | Error
;;; Fit lasso regression via coordinate descent.
;;; X should be standardized.
;;;   lambda: regularization parameter
;;;   max-iter: maximum iterations (default 1000)
;;;   tol: convergence tolerance (default 1e-6)
(define (lasso-fit X y lambda . opts)
  (let* ([max-iter (if (>= (length opts) 1) (car opts) 1000)]
         [tol (if (>= (length opts) 2) (cadr opts) 1e-6)]
         [n (matrix-rows X)]
         [p (matrix-cols X)])
        (if (not (= n (vector-length y)))
            (list 'error 'dimension-mismatch n (vector-length y))
            (if (<= lambda 0)
                (list 'error 'invalid-lambda lambda)
                ;; Initialize beta to zeros
                (let ([beta (make-vector p 0)])
                     (lasso-coordinate-descent X y beta lambda n p max-iter tol))))))

;;; lasso-coordinate-descent : Matrix × Vec × Vec × Num × Nat × Nat × Nat × Num → LinearModelResult
(define (lasso-coordinate-descent X y beta lambda n p max-iter tol)
  (let* (;; Precompute column norms squared
         [col-norms (make-vector p)])
        (do ([j 0 (+ j 1)])
            [(= j p)]
            (let ([norm-sq (let loop ([i 0] [s 0])
                                (if (= i n)
                                    s
                                    (loop (+ i 1)
                                          (+ s (expt (matrix-ref X i j) 2)))))])
                 (vector-set! col-norms j norm-sq)))
        ;; Coordinate descent loop
        (let loop ([iter 0])
             (if (>= iter max-iter)
                 (lasso-finalize X y beta n p)
                 (let* ([max-change 0])
                       ;; Update each coefficient
                       (do ([j 0 (+ j 1)])
                           [(= j p)]
                           (let* ([old-beta-j (vector-ref beta j)]
                                  ;; Compute partial residual
                                  [rho (lasso-partial-residual X y beta j n)]
                                  [norm-sq (vector-ref col-norms j)]
                                  ;; Soft-thresholding
                                  [new-beta-j (soft-threshold rho (* lambda n) norm-sq)])
                                 (vector-set! beta j new-beta-j)
                                 (set! max-change (max max-change (abs (- new-beta-j old-beta-j))))))
                       ;; Check convergence
                       (if (< max-change tol)
                           (lasso-finalize X y beta n p)
                           (loop (+ iter 1))))))))

;;; lasso-partial-residual : Matrix × Vec × Vec × Nat × Nat → Num
;;; Compute rho_j = X_j' (y - X_{-j} * beta_{-j})
(define (lasso-partial-residual X y beta j n)
  (let loop ([i 0] [s 0])
       (if (= i n)
           s
           (let* ([yi (vector-ref y i)]
                  [xij (matrix-ref X i j)]
                  [pred-minus-j (let inner ([k 0] [p 0])
                                     (if (= k (vector-length beta))
                                         p
                                         (if (= k j)
                                             (inner (+ k 1) p)
                                             (inner (+ k 1)
                                                    (+ p (* (vector-ref beta k)
                                                            (matrix-ref X i k)))))))])
                 (loop (+ i 1)
                       (+ s (* xij (- yi pred-minus-j))))))))

;;; soft-threshold : Num × Num × Num → Num
;;; S(z, gamma) = sign(z) * max(|z| - gamma, 0) / norm-sq
(define (soft-threshold z gamma norm-sq)
  (let ([abs-z (abs z)])
       (if (<= abs-z gamma)
           0
           (/ (* (if (>= z 0) 1 -1) (- abs-z gamma))
              (max norm-sq 1e-10)))))

;;; lasso-finalize : Matrix × Vec × Vec × Nat × Nat → LinearModelResult
(define (lasso-finalize X y beta n p)
  (let* ([fitted (matrix-vec-mul X beta)]
         [residuals (make-vector n)]
         [sse 0])
        (do ([i 0 (+ i 1)])
            [(= i n)]
            (let ([r (- (vector-ref y i) (vector-ref fitted i))])
                 (vector-set! residuals i r)
                 (set! sse (+ sse (* r r)))))
        (let* ([df-residual (- n (count-nonzero beta))]
               [mse (if (> df-residual 0) (/ sse df-residual) 0)]
               [r-squared (compute-r-squared-ridge y fitted)]
               [se (make-vector p 0)]
               [t-values (make-vector p 0)]
               [p-values (make-vector p 1)])
              (make-linear-model-result
               beta se t-values p-values
               r-squared r-squared
               residuals fitted sse mse n p))))

;;; count-nonzero : Vec → Nat
(define (count-nonzero v)
  (let ([n (vector-length v)])
       (let loop ([i 0] [c 0])
            (if (= i n)
                c
                (loop (+ i 1)
                      (if (= (vector-ref v i) 0) c (+ c 1)))))))

;;; ============================================================
;;; Elastic Net
;;; ============================================================

;;; Elastic Net minimizes: (1/2n)||y - X*beta||² + lambda * (alpha*||beta||_1 + (1-alpha)*||beta||²/2)
;;; alpha=1 is lasso, alpha=0 is ridge.

;;; elastic-net-fit : Matrix × Vec × Num × Num × Nat × Num → LinearModelResult | Error
;;; Fit elastic net via coordinate descent.
;;;   lambda: regularization strength
;;;   alpha: mixing parameter in [0,1] (1=lasso, 0=ridge)
(define (elastic-net-fit X y lambda alpha . opts)
  (let* ([max-iter (if (>= (length opts) 1) (car opts) 1000)]
         [tol (if (>= (length opts) 2) (cadr opts) 1e-6)]
         [n (matrix-rows X)]
         [p (matrix-cols X)])
        (if (not (= n (vector-length y)))
            (list 'error 'dimension-mismatch n (vector-length y))
            (if (or (<= lambda 0) (< alpha 0) (> alpha 1))
                (list 'error 'invalid-parameters lambda alpha)
                (let ([beta (make-vector p 0)])
                     (elastic-net-coordinate-descent X y beta lambda alpha n p max-iter tol))))))

;;; elastic-net-coordinate-descent : Matrix × Vec × Vec × Num × Num × Nat × Nat × Nat × Num → LinearModelResult
(define (elastic-net-coordinate-descent X y beta lambda alpha n p max-iter tol)
  (let* ([col-norms (make-vector p)])
        (do ([j 0 (+ j 1)])
            [(= j p)]
            (let ([norm-sq (let loop ([i 0] [s 0])
                                (if (= i n)
                                    s
                                    (loop (+ i 1)
                                          (+ s (expt (matrix-ref X i j) 2)))))])
                 (vector-set! col-norms j norm-sq)))
        (let loop ([iter 0])
             (if (>= iter max-iter)
                 (lasso-finalize X y beta n p)
                 (let* ([max-change 0])
                       (do ([j 0 (+ j 1)])
                           [(= j p)]
                           (let* ([old-beta-j (vector-ref beta j)]
                                  [rho (lasso-partial-residual X y beta j n)]
                                  [norm-sq (vector-ref col-norms j)]
                                  ;; Elastic net update
                                  [l1-penalty (* lambda alpha n)]
                                  [l2-penalty (* lambda (- 1 alpha) n)]
                                  [new-beta-j (elastic-net-update rho l1-penalty l2-penalty norm-sq)])
                                 (vector-set! beta j new-beta-j)
                                 (set! max-change (max max-change (abs (- new-beta-j old-beta-j))))))
                       (if (< max-change tol)
                           (lasso-finalize X y beta n p)
                           (loop (+ iter 1))))))))

;;; elastic-net-update : Num × Num × Num × Num → Num
;;; Update rule: S(rho, l1) / (norm-sq + l2)
(define (elastic-net-update rho l1-penalty l2-penalty norm-sq)
  (let ([abs-rho (abs rho)])
       (if (<= abs-rho l1-penalty)
           0
           (/ (* (if (>= rho 0) 1 -1) (- abs-rho l1-penalty))
              (max (+ norm-sq l2-penalty) 1e-10)))))

;;; ============================================================
;;; Cross-Validation
;;; ============================================================

;;; cross-validate-lambda : Matrix × Vec × (List Num) × Nat × Symbol → (Num × (List Num))
;;; K-fold cross-validation for lambda selection.
;;; Returns (best-lambda, cv-errors).
;;;   X: design matrix
;;;   y: response
;;;   lambdas: list of lambda values to try
;;;   k: number of folds (default 5)
;;;   method: 'ridge, 'lasso, or 'elastic-net
(define (cross-validate-lambda X y lambdas k method . opts)
  (let* ([alpha (if (>= (length opts) 1) (car opts) 1)]  ; For elastic net
         [n (matrix-rows X)]
         [fold-size (quotient n k)]
         [cv-errors (map (lambda (lam)
                                 (cv-error-for-lambda X y lam k method alpha n))
                         lambdas)]
         ;; Find lambda with minimum CV error
         [best-idx (find-min-index cv-errors)]
         [best-lambda (list-ref lambdas best-idx)])
        (cons best-lambda cv-errors)))

;;; cv-error-for-lambda : Matrix × Vec × Num × Nat × Symbol × Num × Nat → Num
;;; Compute CV error for a single lambda.
(define (cv-error-for-lambda X y lambda k method alpha n)
  (let* ([fold-size (quotient n k)]
         [total-error 0])
        (do ([fold 0 (+ fold 1)])
            [(= fold k) (/ total-error n)]
            (let* ([start (* fold fold-size)]
                   [end (if (= fold (- k 1)) n (+ start fold-size))]
                   ;; Split into train and test
                   [train-test (split-fold X y start end n)]
                   [X-train (car train-test)]
                   [y-train (cadr train-test)]
                   [X-test (caddr train-test)]
                   [y-test (cadddr train-test)]
                   ;; Fit model on training data
                   [model (case method
                                [(ridge) (ridge-fit X-train y-train lambda)]
                                [(lasso) (lasso-fit X-train y-train lambda)]
                                [(elastic-net) (elastic-net-fit X-train y-train lambda alpha)]
                                [else (ridge-fit X-train y-train lambda)])]
                   ;; Predict on test data
                   [fold-error (if (and (pair? model) (eq? (car model) 'error))
                                   1e10
                                   (compute-mse-test X-test y-test
                                                     (linear-model-coefficients model)))])
                  (set! total-error (+ total-error (* fold-error (- end start))))))))

;;; split-fold : Matrix × Vec × Nat × Nat × Nat → (List Matrix Vec Matrix Vec)
;;; Split data into train (excluding fold) and test (fold only).
(define (split-fold X y start end n)
  (let* ([p (matrix-cols X)]
         [n-test (- end start)]
         [n-train (- n n-test)]
         [X-train-data (make-vector (* n-train p))]
         [X-test-data (make-vector (* n-test p))]
         [y-train (make-vector n-train)]
         [y-test (make-vector n-test)])
        (let ([train-idx 0] [test-idx 0])
             (do ([i 0 (+ i 1)])
                 [(= i n)]
                 (if (and (>= i start) (< i end))
                     ;; Test set
                     (begin
                      (vector-set! y-test test-idx (vector-ref y i))
                      (do ([j 0 (+ j 1)])
                          [(= j p)]
                          (vector-set! X-test-data (+ (* test-idx p) j)
                                       (matrix-ref X i j)))
                      (set! test-idx (+ test-idx 1)))
                     ;; Train set
                     (begin
                      (vector-set! y-train train-idx (vector-ref y i))
                      (do ([j 0 (+ j 1)])
                          [(= j p)]
                          (vector-set! X-train-data (+ (* train-idx p) j)
                                       (matrix-ref X i j)))
                      (set! train-idx (+ train-idx 1))))))
        (list (list 'matrix n-train p X-train-data)
              y-train
              (list 'matrix n-test p X-test-data)
              y-test)))

;;; compute-mse-test : Matrix × Vec × Vec → Num
(define (compute-mse-test X y beta)
  (let* ([n (matrix-rows X)]
         [fitted (matrix-vec-mul X beta)])
        (let loop ([i 0] [sum 0])
             (if (= i n)
                 (/ sum n)
                 (loop (+ i 1)
                       (+ sum (expt (- (vector-ref y i)
                                       (vector-ref fitted i))
                                    2)))))))

;;; find-min-index : (List Num) → Nat
(define (find-min-index xs)
  (let loop ([xs (cdr xs)] [idx 1] [min-val (car xs)] [min-idx 0])
       (if (null? xs)
           min-idx
           (if (< (car xs) min-val)
               (loop (cdr xs) (+ idx 1) (car xs) idx)
               (loop (cdr xs) (+ idx 1) min-val min-idx)))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; identity-matrix : Nat → Matrix
(define (identity-matrix n)
  (let ([data (make-vector (* n n) 0)])
       (do ([i 0 (+ i 1)])
           [(= i n)]
           (vector-set! data (+ (* i n) i) 1))
       (list 'matrix n n data)))

;;; matrix-scale : Num × Matrix → Matrix
(define (matrix-scale s M)
  (let* ([m (matrix-rows M)]
         [n (matrix-cols M)]
         [data (make-vector (* m n))])
        (do ([i 0 (+ i 1)])
            [(= i m)]
            (do ([j 0 (+ j 1)])
                [(= j n)]
                (vector-set! data (+ (* i n) j)
                             (* s (matrix-ref M i j)))))
        (list 'matrix m n data)))

;;; matrix-add : Matrix × Matrix → Matrix
(define (matrix-add A B)
  (let* ([m (matrix-rows A)]
         [n (matrix-cols A)]
         [data (make-vector (* m n))])
        (do ([i 0 (+ i 1)])
            [(= i m)]
            (do ([j 0 (+ j 1)])
                [(= j n)]
                (vector-set! data (+ (* i n) j)
                             (+ (matrix-ref A i j)
                                (matrix-ref B i j)))))
        (list 'matrix m n data)))
