(load "core/base/prelude.ss")
(load "lattice/statistics/core/result-types.ss")
(load "lattice/statistics/core/summary-stats.ss")
(load "lattice/statistics/hypothesis/distributions.ss")

(doc 'module 'anova)
(doc 'description "Analysis of Variance — One-way and two-way ANOVA")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'description "anova-one-way: Compare means across multiple groups")
(doc 'description "anova-two-way: Two-factor ANOVA")

(doc 'section 'one-way-anova)
(define (anova-one-way groups)
  (doc 'export #t)
  (doc 'type '(-> (List Vec) ANOVAResult))
  (doc 'description "Test H0: all group means are equal")
  (doc 'note "F = (SS_between / df_between) / (SS_within / df_within)")
  (let* ([k (length groups)]
         [ns (map vector-length groups)]        ; Group sizes
         [n (apply + ns)]                       ; Total sample size
         ;; Group means
         [means (map vec-mean groups)]
         ;; Grand mean
         [grand-mean (compute-grand-mean groups)]
         ;; SS between groups
         [ss-between (compute-ss-between groups means grand-mean ns)]
         ;; SS within groups
         [ss-within (compute-ss-within groups means)]
         ;; Degrees of freedom
         [df-between (- k 1)]
         [df-within (- n k)]
         ;; Mean squares
         [ms-between (/ ss-between (max df-between 1))]
         [ms-within (/ ss-within (max df-within 1))]
         ;; F statistic
         [f-stat (/ ms-between (max ms-within 1e-10))]
         ;; P-value
         [p-value (f-pvalue f-stat df-between df-within)])
        (make-anova-result f-stat df-between df-within p-value
                           ss-between ss-within ms-between ms-within)))

;;; compute-grand-mean : (List Vec) → Num
;;; Compute overall mean across all groups.
(define (compute-grand-mean groups)
  (let loop ([gs groups] [sum 0] [count 0])
       (if (null? gs)
           (/ sum count)
           (let* ([g (car gs)]
                  [n (vector-length g)])
                 (loop (cdr gs)
                       (+ sum (let inner ([i 0] [s 0])
                                   (if (= i n)
                                       s
                                       (inner (+ i 1) (+ s (vector-ref g i))))))
                       (+ count n))))))

;;; compute-ss-between : (List Vec) × (List Num) × Num × (List Num) → Num
;;; SS_between = sum(n_i * (mean_i - grand_mean)²)
(define (compute-ss-between groups means grand-mean ns)
  (let loop ([ms means] [sizes ns] [sum 0])
       (if (null? ms)
           sum
           (loop (cdr ms)
                 (cdr sizes)
                 (+ sum (* (car sizes)
                           (expt (- (car ms) grand-mean) 2)))))))

;;; compute-ss-within : (List Vec) × (List Num) → Num
;;; SS_within = sum_i sum_j (x_ij - mean_i)²
(define (compute-ss-within groups means)
  (let loop ([gs groups] [ms means] [sum 0])
       (if (null? gs)
           sum
           (let* ([g (car gs)]
                  [m (car ms)]
                  [n (vector-length g)]
                  [ss (let inner ([i 0] [s 0])
                           (if (= i n)
                               s
                               (inner (+ i 1)
                                      (+ s (expt (- (vector-ref g i) m) 2)))))])
                 (loop (cdr gs) (cdr ms) (+ sum ss))))))

;;; ====
;;; Effect Size Measures
;;; ====

;;; anova-eta-squared : ANOVAResult → Num
;;; Eta-squared = SS_between / SS_total
;;; Proportion of variance explained by group membership.
(define (anova-eta-squared result)
  (let* ([ss-between (anova-ss-between result)]
         [ss-within (anova-ss-within result)]
         [ss-total (+ ss-between ss-within)])
        (/ ss-between (max ss-total 1e-10))))

;;; anova-omega-squared : ANOVAResult → Num
;;; Omega-squared (less biased than eta-squared).
;;; omega² = (SS_between - df_between * MS_within) / (SS_total + MS_within)
(define (anova-omega-squared result)
  (let* ([ss-between (anova-ss-between result)]
         [ss-within (anova-ss-within result)]
         [df-between (anova-df-between result)]
         [ms-within (anova-ms-within result)]
         [ss-total (+ ss-between ss-within)]
         [num (- ss-between (* df-between ms-within))]
         [den (+ ss-total ms-within)])
        (/ num (max den 1e-10))))

;;; ====
;;; Post-Hoc Tests
;;; ====

;;; tukey-hsd : (List Vec) → (List (List Symbol Num Num))
;;; Tukey's Honest Significant Difference test.
;;; Returns list of (group1-idx, group2-idx, diff, p-value) for each pair.
;;; Note: Full implementation requires studentized range distribution.
;;; This is a simplified version using Bonferroni correction.
(define (tukey-hsd-bonferroni groups alpha)
  (let* ([k (length groups)]
         [n (apply + (map vector-length groups))]
         [means (map vec-mean groups)]
         [ns (map vector-length groups)]
         ;; Pooled variance
         [ss-within (compute-ss-within groups means)]
         [df-within (- n k)]
         [ms-within (/ ss-within df-within)]
         ;; Number of comparisons
         [num-comparisons (/ (* k (- k 1)) 2)]
         [alpha-adj (/ alpha num-comparisons)]  ; Bonferroni
         [results '()])
        ;; Compare all pairs
        (let loop-i ([i 0] [res '()])
             (if (>= i (- k 1))
                 (reverse res)
                 (loop-i
                  (+ i 1)
                  (let loop-j ([j (+ i 1)] [r res])
                       (if (>= j k)
                           r
                           (let* ([mean-i (list-ref means i)]
                                  [mean-j (list-ref means j)]
                                  [ni (list-ref ns i)]
                                  [nj (list-ref ns j)]
                                  [se (sqrt (* ms-within (+ (/ 1 ni) (/ 1 nj))))]
                                  [diff (- mean-i mean-j)]
                                  [t-stat (/ (abs diff) se)]
                                  [p-value (t-pvalue-two-tailed t-stat df-within)]
                                  [significant? (< p-value alpha-adj)])
                                 (loop-j (+ j 1)
                                         (cons (list i j diff p-value significant?) r))))))))))

;;; ====
;;; Two-Way ANOVA (Simplified)
;;; ====

;;; Two-way ANOVA requires a more complex data structure.
;;; This is a simplified implementation for balanced designs.

;;; anova-two-way : Matrix × Vec × Vec → TwoWayANOVAResult
;;; Two-way ANOVA for balanced designs.
;;; data: matrix where rows are observations
;;; factor-a: factor A levels for each observation
;;; factor-b: factor B levels for each observation
;;; Note: This is a placeholder; full implementation is more complex.
(define (anova-two-way data factor-a factor-b)
  ;; For now, just return a message indicating this needs more work
  (list 'not-implemented
        "Two-way ANOVA requires more complex implementation"))
