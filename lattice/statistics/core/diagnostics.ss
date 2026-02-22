(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module diagnostics
;;; @requires prelude matrix matrix-solvers
(require 'prelude)
(require 'matrix)
(require 'matrix-solvers)

(doc 'module 'diagnostics)
(doc 'description "Model Diagnostics — Diagnostic measures for regression models")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'description "Residual types (raw, standardized, studentized)")
(doc 'description "Leverage (hat matrix diagonal)")
(doc 'description "Cook's distance")
(doc 'description "DFBetas")

(doc 'section 'hat-matrix-and-leverage)

(doc 'note "The hat matrix H = X(X'X)^(-1)X' projects y onto the column space of X")
(doc 'note "Leverage h_ii = H[i,i] measures influence of observation i")
(doc 'note "High leverage (h_ii > 2p/n) indicates potentially influential point")
(define (hat-matrix X)
  (doc 'type '(-> Matrix Matrix))
  (doc 'description "Compute the full hat matrix H = X(X'X)^(-1)X'")
  (doc 'note "This is O(n²p) in memory, use hat-matrix-diagonal for large n")
  (let* ([XtX (matrix-mul (matrix-transpose X) X)]
         [XtX-inv (matrix-inverse XtX)])
        (if (and (pair? XtX-inv) (eq? (car XtX-inv) 'error))
            XtX-inv
            (matrix-mul (matrix-mul X XtX-inv) (matrix-transpose X)))))
(define (hat-matrix-diagonal X)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec))
  (doc 'description "Compute only the diagonal of the hat matrix (leverage values)")
  (doc 'note "More efficient than computing full H for large n")
  (doc 'note "h_ii = x_i' (X'X)^(-1) x_i")
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

(doc 'section 'residual-types)

(define (residuals-raw y fitted)
  (doc 'type '(-> Vec Vec Vec))
  (doc 'description "Raw residuals: e_i = y_i - y-hat_i")
  (let* ([n (vector-length y)]
         [e (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) e]
            (vector-set! e i (- (vector-ref y i) (vector-ref fitted i))))))

(define (residuals-standardized raw-residuals sigma)
  (doc 'type '(-> Vec Num Vec))
  (doc 'description "Standardized residuals: e_i / sigma where sigma = residual standard error")
  (let* ([n (vector-length raw-residuals)]
         [r (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) r]
            (vector-set! r i (/ (vector-ref raw-residuals i) sigma)))))

(define (residuals-studentized raw-residuals leverage sigma)
  (doc 'export #t)
  (doc 'type '(-> Vec Vec Num Vec))
  (doc 'description "Internally studentized residuals: r_i = e_i / (sigma * sqrt(1 - h_ii))")
  (doc 'note "These have approximately unit variance even with non-constant variance")
  (let* ([n (vector-length raw-residuals)]
         [r (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) r]
            (let* ([ei (vector-ref raw-residuals i)]
                   [hi (vector-ref leverage i)]
                   [denom (* sigma (sqrt (max (- 1 hi) 1e-10)))])
                  (vector-set! r i (/ ei denom))))
        r))

(define (residuals-studentized-external raw-residuals leverage sse n p)
  (doc 'type '(-> Vec Vec Num Nat Nat Vec))
  (doc 'description "Externally studentized (deleted) residuals: t_i = e_i / (s_{-i} * sqrt(1 - h_ii))")
  (doc 'note "s_{-i} is the residual SE computed without observation i")
  (doc 'note "Follows a t-distribution with (n - p - 1) df under normality")
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

(doc 'section 'influence-measures)

(define (cooks-distance raw-residuals leverage p mse)
  (doc 'export #t)
  (doc 'type '(-> Vec Vec Nat Num Vec))
  (doc 'description "Cook's distance measures the influence of each observation on all fitted values")
  (doc 'note "D_i = (1/p) * (e_i^2 / mse) * (h_ii / (1 - h_ii)^2)")
  (doc 'note "Points with D_i > 4/n or D_i > 1 are potentially influential")
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

(define (cooks-distance-threshold n)
  (doc 'type '(-> Nat Num))
  (doc 'description "Common threshold for influential points: 4/n")
  (/ 4 n))

(define (dffits studentized-external leverage)
  (doc 'type '(-> Vec Vec Vec))
  (doc 'description "DFFITS measures the influence of observation i on its own fitted value")
  (doc 'note "DFFITS_i = t_i * sqrt(h_ii / (1 - h_ii)) where t_i is the externally studentized residual")
  (doc 'note "Points with |DFFITS_i| > 2*sqrt(p/n) are potentially influential")
  (let* ([n (vector-length studentized-external)]
         [df (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) df]
            (let* ([ti (vector-ref studentized-external i)]
                   [hi (vector-ref leverage i)]
                   [denom (max (- 1 hi) 1e-10)])
                  (vector-set! df i (* ti (sqrt (/ hi denom))))))
        df))

(define (dffits-threshold p n)
  (doc 'type '(-> Nat Nat Num))
  (doc 'description "Common threshold for DFFITS: 2*sqrt(p/n)")
  (* 2 (sqrt (/ p n))))

(doc 'section 'collinearity-diagnostics)

(define (vif X)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec))
  (doc 'description "Variance Inflation Factor for each predictor")
  (doc 'note "VIF_j = 1 / (1 - R²_j) where R²_j is the R² from regressing x_j on other predictors")
  (doc 'note "VIF > 10 indicates serious multicollinearity")
  (doc 'note "Assumes intercept is NOT in X (or is first column to be skipped)")
  (let* ([n (matrix-cols X)]
         [vifs (make-vector n)])
        (do ([j 0 (+ j 1)])
            [(= j n) vifs]
            (let* ([r2j (r-squared-predictor X j)])
                  (vector-set! vifs j (/ 1 (max (- 1 r2j) 1e-10)))))
        vifs))

(define (r-squared-predictor X j)
  (doc 'type '(-> Matrix Nat Num))
  (doc 'description "Compute R² from regressing column j on all other columns")
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
