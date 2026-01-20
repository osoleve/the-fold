(load "core/base/prelude.ss")
(load "lattice/statistics/core/result-types.ss")
(load "lattice/statistics/core/summary-stats.ss")
(load "lattice/statistics/hypothesis/distributions.ss")

(doc 'module 'f-test)
(doc 'description "F-Tests — F-tests for variance comparison and regression")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'description "f-test-variance: Compare variances of two samples")
(doc 'description "f-test-regression: Overall significance of regression")

(doc 'section 'variance-ratio-test)
(define (f-test-variance xs ys)
  (doc 'export #t)
  (doc 'type '(-> Vec Vec TestResult))
  (doc 'description "Test H0: var(x) = var(y) vs H1: var(x) != var(y)")
  (doc 'note "F = var(x) / var(y) follows F(n1-1, n2-1) under H0")
  (let* ([n1 (vector-length xs)]
         [n2 (vector-length ys)]
         [var1 (vec-variance xs)]
         [var2 (vec-variance ys)]
         ;; Always put larger variance in numerator for consistency
         [ordered (if (>= var1 var2)
                      (list var1 var2 (- n1 1) (- n2 1))
                      (list var2 var1 (- n2 1) (- n1 1)))]
         [var-num (car ordered)]
         [var-den (cadr ordered)]
         [df1 (caddr ordered)]
         [df2 (cadddr ordered)]
         [f-stat (/ var-num (max var-den 1e-10))]
         ;; Two-tailed p-value
         [p-upper (- 1 (f-cdf f-stat df1 df2))]
         [p-value (* 2 (min p-upper (- 1 p-upper)))]
         ;; Confidence interval for ratio
         [ci (variance-ratio-ci var1 var2 (- n1 1) (- n2 1) 0.95)])
        (make-test-result 'f-test-variance f-stat (cons df1 df2) p-value ci #f)))

;;; variance-ratio-ci : Num × Num × Num × Num × Num → (Num . Num)
;;; Confidence interval for variance ratio.
(define (variance-ratio-ci var1 var2 df1 df2 confidence)
  (let* ([alpha (- 1 confidence)]
         [ratio (/ var1 (max var2 1e-10))]
         [f-lower (f-quantile (/ alpha 2) df1 df2)]
         [f-upper (f-quantile (- 1 (/ alpha 2)) df1 df2)]
         ;; CI for ratio
         [ci-lower (/ ratio f-upper)]
         [ci-upper (/ ratio f-lower)])
        (cons ci-lower ci-upper)))

;;; ====
;;; Levene's Test
;;; ====

;;; levene-test : (List Vec) → TestResult
;;; Levene's test for homogeneity of variances.
;;; More robust than F-test to non-normality.
;;; Tests H0: all group variances are equal.
(define (levene-test groups)
  (doc 'export #t)
  (let* ([k (length groups)]
         ;; Transform: Z_ij = |X_ij - median(X_i)|
         [z-groups (map (lambda (g)
                                (let* ([med (vec-median g)]
                                       [n (vector-length g)]
                                       [z (make-vector n)])
                                      (do ([i 0 (+ i 1)])
                                          [(= i n) z]
                                          (vector-set! z i
                                                       (abs (- (vector-ref g i) med))))))
                        groups)]
         ;; Perform one-way ANOVA on transformed data
         [anova-result (anova-one-way z-groups)])
        (make-test-result 'levene-test
                          (anova-f-statistic anova-result)
                          (cons (anova-df-between anova-result)
                                (anova-df-within anova-result))
                          (anova-p-value anova-result)
                          #f #f)))

;;; ====
;;; Bartlett's Test
;;; ====

;;; bartlett-test : (List Vec) → TestResult
;;; Bartlett's test for homogeneity of variances.
;;; Assumes normal distributions; sensitive to non-normality.
(define (bartlett-test groups)
  (doc 'export #t)
  (let* ([k (length groups)]
         [ns (map vector-length groups)]
         [n (apply + ns)]
         [vars (map vec-variance groups)]
         ;; Pooled variance (weighted)
         [sp2 (/ (apply + (map * (map (lambda (ni) (- ni 1)) ns) vars))
                 (- n k))]
         ;; Bartlett statistic
         [sum-log (apply + (map (lambda (ni vi)
                                        (* (- ni 1) (log (max vi 1e-10))))
                                ns vars))]
         [num (* (- n k) (log sp2))]
         [chi-sq-num (- num sum-log)]
         ;; Correction factor
         [c (+ 1 (/ (- (apply + (map (lambda (ni) (/ 1 (- ni 1))) ns))
                       (/ 1 (- n k)))
                    (* 3 (- k 1))))]
         [chi-sq (/ chi-sq-num c)]
         [df (- k 1)]
         [p-value (chi-squared-pvalue chi-sq df)])
        (make-test-result 'bartlett-test chi-sq df p-value #f #f)))

;;; ====
;;; Regression F-Test
;;; ====

;;; f-test-regression : Num × Num × Nat × Nat → TestResult
;;; Test overall significance of regression model.
;;; F = (R² / (p-1)) / ((1-R²) / (n-p))
;;; where p includes intercept.
(define (f-test-regression r-squared n p)
  (doc 'export #t)
  (let* ([df1 (- p 1)]          ; Regression df (excluding intercept)
         [df2 (- n p)]          ; Residual df
         [f-stat (if (or (<= df1 0) (<= df2 0) (>= r-squared 1))
                     0
                     (/ (* r-squared df2)
                        (* (- 1 r-squared) df1)))]
         [p-value (if (= f-stat 0)
                      1
                      (f-pvalue f-stat df1 df2))])
        (make-test-result 'f-test-regression f-stat (cons df1 df2) p-value #f #f)))

;;; f-test-nested-models : Num × Num × Nat × Nat × Nat → TestResult
;;; Compare nested regression models.
;;; Tests whether additional predictors significantly improve fit.
;;; r2-full: R² of full model
;;; r2-reduced: R² of reduced (nested) model
;;; n: sample size
;;; p-full: number of parameters in full model
;;; p-reduced: number of parameters in reduced model
(define (f-test-nested-models r2-full r2-reduced n p-full p-reduced)
  (doc 'export #t)
  (let* ([df1 (- p-full p-reduced)]      ; Additional parameters
         [df2 (- n p-full)]               ; Residual df
         [r2-diff (- r2-full r2-reduced)]
         [f-stat (if (or (<= df1 0) (<= df2 0) (>= r2-full 1))
                     0
                     (/ (* r2-diff df2)
                        (* (- 1 r2-full) df1)))]
         [p-value (if (= f-stat 0)
                      1
                      (f-pvalue f-stat df1 df2))])
        (make-test-result 'f-test-nested f-stat (cons df1 df2) p-value #f #f)))
