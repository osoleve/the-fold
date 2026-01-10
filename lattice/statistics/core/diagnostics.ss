;;; lattice/statistics/core/diagnostics.ss — Model Diagnostics
;;;
;;; Diagnostic measures for regression models.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - Residual types (raw, standardized, studentized)
;;;   - Leverage (hat matrix diagonal)
;;;   - Cook's distance
;;;   - DFBetas
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/matrix.ss
;;;   - linalg/matrix-solvers.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-solvers.ss")

;;; ============================================================
;;; Hat Matrix and Leverage
;;; ============================================================

;;; The hat matrix H = X(X'X)^(-1)X' projects y onto the column space of X.
;;; Leverage h_ii = H[i,i] measures influence of observation i.
;;; High leverage (h_ii > 2p/n) indicates potentially influential point.

;;; hat-matrix : Matrix → Matrix
;;; Compute the full hat matrix H = X(X'X)^(-1)X'.
;;; Note: This is O(n²p) in memory, use hat-matrix-diagonal for large n.
(define (hat-matrix X)
  (let* ([XtX (matrix-mul (matrix-transpose X) X)]
         [XtX-inv (matrix-inverse XtX)])
        (if (and (pair? XtX-inv) (eq? (car XtX-inv) 'error))
            XtX-inv
            (matrix-mul (matrix-mul X XtX-inv) (matrix-transpose X)))))

;;; hat-matrix-diagonal : Matrix → Vec
;;; Compute only the diagonal of the hat matrix (leverage values).
;;; More efficient than computing full H for large n.
;;; h_ii = x_i' (X'X)^(-1) x_i
(define (hat-matrix-diagonal X)
  (let* ([m (matrix-rows X)]
         [n (matrix-cols X)]
         [XtX (matrix-mul (matrix-transpose X) X)]
         [XtX-inv (matrix-inverse XtX)]
         [h (make-vector m)])
        (if (and (pair? XtX-inv) (eq? (car XtX-inv) 'error))
            XtX-inv
            (begin
             (do ([i 0 (+ i 1)])
                 [(= i m)]
                 ;; Extract row i as vector
                 (let ([xi (make-vector n)])
                      (do ([j 0 (+ j 1)])
                          [(= j n)]
                          (vector-set! xi j (matrix-ref X i j)))
                      ;; h_ii = xi' * XtX-inv * xi
                      (let* ([temp (make-vector n 0)])
                            ;; temp = XtX-inv * xi
                            (do ([r 0 (+ r 1)])
                                [(= r n)]
                                (let ([sum (let loop ([c 0] [s 0])
                                                (if (= c n)
                                                    s
                                                    (loop (+ c 1)
                                                          (+ s (* (matrix-ref XtX-inv r c)
                                                                  (vector-ref xi c))))))])
                                     (vector-set! temp r sum)))
                            ;; h_ii = xi' * temp
                            (let ([hii (let loop ([j 0] [s 0])
                                            (if (= j n)
                                                s
                                                (loop (+ j 1)
                                                      (+ s (* (vector-ref xi j)
                                                              (vector-ref temp j))))))])
                                 (vector-set! h i hii)))))
             h))))

;;; ============================================================
;;; Residual Types
;;; ============================================================

;;; residuals-raw : Vec × Vec → Vec
;;; Raw residuals: e_i = y_i - y-hat_i
(define (residuals-raw y fitted)
  (let* ([n (vector-length y)]
         [e (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) e]
            (vector-set! e i (- (vector-ref y i) (vector-ref fitted i))))))

;;; residuals-standardized : Vec × Num → Vec
;;; Standardized residuals: e_i / sigma
;;; where sigma = residual standard error
(define (residuals-standardized raw-residuals sigma)
  (let* ([n (vector-length raw-residuals)]
         [r (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) r]
            (vector-set! r i (/ (vector-ref raw-residuals i) sigma)))))

;;; residuals-studentized : Vec × Vec × Num → Vec
;;; Internally studentized residuals: r_i = e_i / (sigma * sqrt(1 - h_ii))
;;; These have approximately unit variance even with non-constant variance.
(define (residuals-studentized raw-residuals leverage sigma)
  (let* ([n (vector-length raw-residuals)]
         [r (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) r]
            (let* ([ei (vector-ref raw-residuals i)]
                   [hi (vector-ref leverage i)]
                   [denom (* sigma (sqrt (max (- 1 hi) 1e-10)))])
                  (vector-set! r i (/ ei denom))))
        r))

;;; residuals-studentized-external : Matrix × Vec × Vec × Vec × Num × Nat → Vec
;;; Externally studentized (deleted) residuals: t_i = e_i / (s_{-i} * sqrt(1 - h_ii))
;;; where s_{-i} is the residual SE computed without observation i.
;;; Follows a t-distribution with (n - p - 1) df under normality.
(define (residuals-studentized-external raw-residuals leverage sse n p)
  (let* ([df (- n p)]
         [mse (/ sse df)]
         [sigma (sqrt mse)]
         [r (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) r]
            (let* ([ei (vector-ref raw-residuals i)]
                   [hi (vector-ref leverage i)]
                   [ei2 (* ei ei)]
                   ;; s_{-i}^2 = ((n-p)*mse - e_i^2/(1-h_i)) / (n-p-1)
                   [s2-i (/ (- (* df mse) (/ ei2 (max (- 1 hi) 1e-10)))
                            (- df 1))]
                   [s-i (sqrt (max s2-i 1e-10))]
                   [denom (* s-i (sqrt (max (- 1 hi) 1e-10)))])
                  (vector-set! r i (/ ei denom))))
        r))

;;; ============================================================
;;; Influence Measures
;;; ============================================================

;;; cooks-distance : Vec × Vec × Nat × Num → Vec
;;; Cook's distance measures the influence of each observation on all fitted values.
;;; D_i = (1/p) * (e_i^2 / mse) * (h_ii / (1 - h_ii)^2)
;;; Points with D_i > 4/n or D_i > 1 are potentially influential.
(define (cooks-distance raw-residuals leverage p mse)
  (let* ([n (vector-length raw-residuals)]
         [D (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) D]
            (let* ([ei (vector-ref raw-residuals i)]
                   [hi (vector-ref leverage i)]
                   [ei2 (* ei ei)]
                   [denom (max (- 1 hi) 1e-10)]
                   [Di (/ (* (/ ei2 mse) (/ hi (* denom denom))) p)])
                  (vector-set! D i Di)))
        D))

;;; cooks-distance-threshold : Nat → Num
;;; Common threshold for influential points: 4/n
(define (cooks-distance-threshold n)
  (/ 4 n))

;;; dffits : Vec × Vec × Num → Vec
;;; DFFITS measures the influence of observation i on its own fitted value.
;;; DFFITS_i = t_i * sqrt(h_ii / (1 - h_ii))
;;; where t_i is the externally studentized residual.
;;; Points with |DFFITS_i| > 2*sqrt(p/n) are potentially influential.
(define (dffits studentized-external leverage)
  (let* ([n (vector-length studentized-external)]
         [df (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) df]
            (let* ([ti (vector-ref studentized-external i)]
                   [hi (vector-ref leverage i)]
                   [denom (max (- 1 hi) 1e-10)])
                  (vector-set! df i (* ti (sqrt (/ hi denom))))))
        df))

;;; dffits-threshold : Nat × Nat → Num
;;; Common threshold for DFFITS: 2*sqrt(p/n)
(define (dffits-threshold p n)
  (* 2 (sqrt (/ p n))))

;;; ============================================================
;;; Collinearity Diagnostics
;;; ============================================================

;;; vif : Matrix → Vec
;;; Variance Inflation Factor for each predictor.
;;; VIF_j = 1 / (1 - R²_j) where R²_j is the R² from regressing x_j on other predictors.
;;; VIF > 10 indicates serious multicollinearity.
;;; Note: Assumes intercept is NOT in X (or is first column to be skipped).
(define (vif X)
  (let* ([n (matrix-cols X)]
         [vifs (make-vector n)])
        (do ([j 0 (+ j 1)])
            [(= j n) vifs]
            (let* ([r2j (r-squared-predictor X j)])
                  (vector-set! vifs j (/ 1 (max (- 1 r2j) 1e-10)))))
        vifs))

;;; r-squared-predictor : Matrix × Nat → Num
;;; Compute R² from regressing column j on all other columns.
(define (r-squared-predictor X j)
  (let* ([m (matrix-rows X)]
         [n (matrix-cols X)]
         ;; Extract column j as y
         [y (make-vector m)]
         ;; Build matrix of other columns
         [X-other-data (make-vector (* m (- n 1)))]
         [X-other (list 'matrix m (- n 1) X-other-data)])
        ;; Fill y and X-other
        (do ([i 0 (+ i 1)])
            [(= i m)]
            (vector-set! y i (matrix-ref X i j))
            (let ([col-idx 0])
                 (do ([k 0 (+ k 1)])
                     [(= k n)]
                     (when (not (= k j))
                           (vector-set! X-other-data (+ (* i (- n 1)) col-idx)
                                        (matrix-ref X i k))
                           (set! col-idx (+ col-idx 1))))))
        ;; Compute R² = 1 - SSE/SST
        (let* ([y-mean (let loop ([i 0] [s 0])
                            (if (= i m)
                                (/ s m)
                                (loop (+ i 1) (+ s (vector-ref y i)))))]
               [sst (let loop ([i 0] [s 0])
                         (if (= i m)
                             s
                             (loop (+ i 1)
                                   (+ s (expt (- (vector-ref y i) y-mean) 2)))))]
               [beta (matrix-least-squares X-other y)])
              (if (and (pair? beta) (eq? (car beta) 'error))
                  0  ; Singular, assume no multicollinearity
                  (let* ([fitted (matrix-vec-mul X-other beta)]
                         [sse (let loop ([i 0] [s 0])
                                   (if (= i m)
                                       s
                                       (loop (+ i 1)
                                             (+ s (expt (- (vector-ref y i)
                                                           (vector-ref fitted i))
                                                        2)))))])
                        (- 1 (/ sse (max sst 1e-10))))))))
