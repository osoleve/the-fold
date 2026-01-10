;;; lattice/statistics/hypothesis/t-test.ss — T-Tests
;;;
;;; One-sample, two-sample, and paired t-tests.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - t-test-one-sample: Test mean = mu0
;;;   - t-test-two-sample: Compare two group means
;;;   - t-test-paired: Paired samples test
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - statistics/core/result-types.ss
;;;   - statistics/core/summary-stats.ss
;;;   - statistics/hypothesis/distributions.ss

(load "core/base/prelude.ss")
(load "lattice/statistics/core/result-types.ss")
(load "lattice/statistics/core/summary-stats.ss")
(load "lattice/statistics/hypothesis/distributions.ss")

;;; ============================================================
;;; One-Sample T-Test
;;; ============================================================

;;; t-test-one-sample : Vec × Num × Num → TestResult
;;; Test H0: mean = mu0 vs H1: mean != mu0 (two-tailed).
;;;   xs: sample data
;;;   mu0: hypothesized mean (default 0)
;;;   confidence: confidence level for CI (default 0.95)
(define (t-test-one-sample xs . opts)
  (let* ([mu0 (if (>= (length opts) 1) (car opts) 0)]
         [confidence (if (>= (length opts) 2) (cadr opts) 0.95)]
         [n (vector-length xs)]
         [x-bar (vec-mean xs)]
         [s (vec-std-dev xs)]
         [se (/ s (sqrt n))]
         [t-stat (/ (- x-bar mu0) se)]
         [df (- n 1)]
         [p-value (t-pvalue-two-tailed t-stat df)]
         [t-crit (t-quantile (+ 0.5 (/ confidence 2)) df)]
         [ci-lower (- x-bar (* t-crit se))]
         [ci-upper (+ x-bar (* t-crit se))]
         [effect-size (/ (- x-bar mu0) s)])  ; Cohen's d
        (make-test-result 't-test-one-sample t-stat df p-value
                          (cons ci-lower ci-upper) effect-size)))

;;; t-test-one-sample-list : (List Num) × Num × Num → TestResult
;;; List version.
(define (t-test-one-sample-list xs . opts)
  (apply t-test-one-sample (list->vector xs) opts))

;;; ============================================================
;;; Two-Sample T-Test
;;; ============================================================

;;; t-test-two-sample : Vec × Vec × Bool × Num → TestResult
;;; Test H0: mean(x) = mean(y) vs H1: mean(x) != mean(y).
;;;   xs: first sample
;;;   ys: second sample
;;;   equal-variance?: assume equal variances (default #t for Student's t)
;;;   confidence: confidence level for CI (default 0.95)
(define (t-test-two-sample xs ys . opts)
  (let* ([equal-variance? (if (>= (length opts) 1) (car opts) #t)]
         [confidence (if (>= (length opts) 2) (cadr opts) 0.95)])
        (if equal-variance?
            (t-test-two-sample-equal xs ys confidence)
            (t-test-two-sample-welch xs ys confidence))))

;;; t-test-two-sample-equal : Vec × Vec × Num → TestResult
;;; Student's t-test (assumes equal variances).
(define (t-test-two-sample-equal xs ys confidence)
  (let* ([n1 (vector-length xs)]
         [n2 (vector-length ys)]
         [x-bar (vec-mean xs)]
         [y-bar (vec-mean ys)]
         [s1-sq (vec-variance xs)]
         [s2-sq (vec-variance ys)]
         ;; Pooled variance
         [s-pooled-sq (/ (+ (* (- n1 1) s1-sq)
                            (* (- n2 1) s2-sq))
                         (+ n1 n2 -2))]
         [s-pooled (sqrt s-pooled-sq)]
         ;; Standard error of difference
         [se (* s-pooled (sqrt (+ (/ 1 n1) (/ 1 n2))))]
         [diff (- x-bar y-bar)]
         [t-stat (/ diff se)]
         [df (+ n1 n2 -2)]
         [p-value (t-pvalue-two-tailed t-stat df)]
         [t-crit (t-quantile (+ 0.5 (/ confidence 2)) df)]
         [ci-lower (- diff (* t-crit se))]
         [ci-upper (+ diff (* t-crit se))]
         ;; Effect size: Cohen's d
         [effect-size (/ diff s-pooled)])
        (make-test-result 't-test-two-sample-equal t-stat df p-value
                          (cons ci-lower ci-upper) effect-size)))

;;; t-test-two-sample-welch : Vec × Vec × Num → TestResult
;;; Welch's t-test (does not assume equal variances).
(define (t-test-two-sample-welch xs ys confidence)
  (let* ([n1 (vector-length xs)]
         [n2 (vector-length ys)]
         [x-bar (vec-mean xs)]
         [y-bar (vec-mean ys)]
         [s1-sq (vec-variance xs)]
         [s2-sq (vec-variance ys)]
         ;; Standard error of difference
         [se (sqrt (+ (/ s1-sq n1) (/ s2-sq n2)))]
         [diff (- x-bar y-bar)]
         [t-stat (/ diff se)]
         ;; Welch-Satterthwaite degrees of freedom
         [v1 (/ s1-sq n1)]
         [v2 (/ s2-sq n2)]
         [df-num (expt (+ v1 v2) 2)]
         [df-den (+ (/ (expt v1 2) (- n1 1))
                    (/ (expt v2 2) (- n2 1)))]
         [df (/ df-num df-den)]
         [p-value (t-pvalue-two-tailed t-stat df)]
         [t-crit (t-quantile (+ 0.5 (/ confidence 2)) df)]
         [ci-lower (- diff (* t-crit se))]
         [ci-upper (+ diff (* t-crit se))]
         ;; Effect size: Cohen's d using pooled std
         [s-pooled (sqrt (/ (+ (* (- n1 1) s1-sq) (* (- n2 1) s2-sq))
                            (+ n1 n2 -2)))]
         [effect-size (/ diff s-pooled)])
        (make-test-result 't-test-two-sample-welch t-stat df p-value
                          (cons ci-lower ci-upper) effect-size)))

;;; ============================================================
;;; Paired T-Test
;;; ============================================================

;;; t-test-paired : Vec × Vec × Num → TestResult
;;; Test H0: mean(x - y) = 0 vs H1: mean(x - y) != 0.
;;; Equivalent to one-sample t-test on differences.
(define (t-test-paired xs ys . opts)
  (let* ([confidence (if (>= (length opts) 1) (car opts) 0.95)]
         [n (vector-length xs)])
        (if (not (= n (vector-length ys)))
            (error 't-test-paired "samples must have equal length")
            (let* ([diffs (make-vector n)]
                   [_ (do ([i 0 (+ i 1)])
                          [(= i n)]
                          (vector-set! diffs i (- (vector-ref xs i)
                                                  (vector-ref ys i))))]
                   [result (t-test-one-sample diffs 0 confidence)])
                  ;; Re-label as paired test
                  (make-test-result 't-test-paired
                                    (test-statistic result)
                                    (test-df result)
                                    (test-p-value result)
                                    (test-ci result)
                                    (test-effect-size result))))))

;;; t-test-paired-list : List × List × Num → TestResult
;;; List wrapper for t-test-paired.
(define (t-test-paired-list xs ys . opts)
  (let ([confidence (if (>= (length opts) 1) (car opts) 0.95)])
       (t-test-paired (list->vector xs) (list->vector ys) confidence)))

;;; ============================================================
;;; One-Sided Tests
;;; ============================================================

;;; t-test-one-sample-greater : Vec × Num → TestResult
;;; Test H0: mean <= mu0 vs H1: mean > mu0.
(define (t-test-one-sample-greater xs mu0)
  (let* ([n (vector-length xs)]
         [x-bar (vec-mean xs)]
         [s (vec-std-dev xs)]
         [se (/ s (sqrt n))]
         [t-stat (/ (- x-bar mu0) se)]
         [df (- n 1)]
         ;; One-tailed p-value
         [p-value (- 1 (t-cdf t-stat df))]
         [effect-size (/ (- x-bar mu0) s)])
        (make-test-result 't-test-one-sample-greater t-stat df p-value
                          #f effect-size)))

;;; t-test-one-sample-less : Vec × Num → TestResult
;;; Test H0: mean >= mu0 vs H1: mean < mu0.
(define (t-test-one-sample-less xs mu0)
  (let* ([n (vector-length xs)]
         [x-bar (vec-mean xs)]
         [s (vec-std-dev xs)]
         [se (/ s (sqrt n))]
         [t-stat (/ (- x-bar mu0) se)]
         [df (- n 1)]
         ;; One-tailed p-value
         [p-value (t-cdf t-stat df)]
         [effect-size (/ (- x-bar mu0) s)])
        (make-test-result 't-test-one-sample-less t-stat df p-value
                          #f effect-size)))
