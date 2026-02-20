(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module info-stat/mi-bridge
;;; @requires prelude entropy empirical-info model-selection-info result-types summary-stats sort

(require 'prelude)
(require 'entropy)
(require 'empirical-info)
(require 'model-selection-info)
(require 'result-types)
(require 'summary-stats)
(require 'sort)

(doc 'module 'info-stat/mi-bridge)
(doc 'bridges '(info statistics))
(doc 'description "Bridge connecting information theory to statistics:
  MI-based feature selection, entropy-based model comparison for regression
  results, and KL divergence for empirical distribution comparison.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Section 1: Mutual Information Feature Selection
;;; ============================================================================

(doc 'section 'mi-feature-selection)

(doc 'note "Feature selection via mutual information: for each candidate feature
  column, estimate I(feature; target) from empirical data, then rank or select
  the top-k features. Uses discretization from empirical-info.")

(define (mi-rank-features columns target . opts)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number)) (List Number) [Nat] (List (Pair Nat Number))))
  (doc 'description "Rank features by mutual information with a target variable.
   columns is a list of feature columns (each a list of numbers).
   target is the target variable (list of numbers).
   Optional argument specifies number of bins for discretization.
   Returns list of (feature-index . MI-score) pairs sorted descending by MI.")
  (let* ([num-bins (if (and (pair? opts) (integer? (car opts)))
                       (car opts)
                       (default-num-bins (length target)))]
         [indexed-scores
          (let loop ([cols columns] [i 0] [acc '()])
            (if (null? cols)
                acc
                (let ([mi (mutual-information-empirical (car cols) target num-bins)])
                  (loop (cdr cols) (+ i 1) (cons (cons i mi) acc)))))])
    (sort-by (lambda (a b) (> (cdr a) (cdr b))) indexed-scores)))

(define (mi-select-features columns target k . opts)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number)) (List Number) Nat [Nat] (List Nat)))
  (doc 'description "Select top-k features by mutual information with target.
   columns is a list of feature columns (each a list of numbers).
   target is the target variable (list of numbers).
   k is the number of features to select.
   Optional argument specifies number of bins for discretization.
   Returns list of feature indices (0-based) for the top-k features.")
  (let* ([ranked (apply mi-rank-features columns target opts)]
         [top-k (take-up-to k ranked)])
    (map car top-k)))

(define (take-up-to k lst)
  (doc 'export #f)
  (doc 'type '(-> Nat (List a) (List a)))
  (doc 'description "Take up to k elements from a list")
  (cond
    [(<= k 0) '()]
    [(null? lst) '()]
    [else (cons (car lst) (take-up-to (- k 1) (cdr lst)))]))

;;; ============================================================================
;;; Section 2: Entropy-Based Model Comparison
;;; ============================================================================

(doc 'section 'entropy-model-comparison)

(doc 'note "Wire the information-theoretic model selection criteria (AIC, BIC, AICc)
  from model-selection-info to work with linear-model and GLM result types from
  statistics. The key bridge: extract residuals and parameter counts from a
  regression result, compute Gaussian log-likelihood, then apply IC formulas.")

(define (model-log-likelihood-gaussian model)
  (doc 'export #f)
  (doc 'type '(-> (Union LinearModelResult GLMResult) Number))
  (doc 'description "Extract Gaussian log-likelihood from a fitted model result.
   Uses the residual vector to compute log-likelihood assuming Gaussian errors.")
  (cond
    [(linear-model? model)
     (log-likelihood-gaussian-vec (linear-model-residuals model))]
    [(glm-result? model)
     (log-likelihood-gaussian-vec (glm-residuals model))]
    [else (error 'model-log-likelihood-gaussian
                 "unsupported model type" (if (pair? model) (car model) model))]))

(define (model-param-count model)
  (doc 'export #f)
  (doc 'type '(-> (Union LinearModelResult GLMResult) Nat))
  (doc 'description "Extract parameter count from a fitted model result.")
  (cond
    [(linear-model? model)
     (linear-model-p model)]
    [(glm-result? model)
     (vector-length (glm-coefficients model))]
    [else (error 'model-param-count
                 "unsupported model type" (if (pair? model) (car model) model))]))

(define (model-obs-count model)
  (doc 'export #f)
  (doc 'type '(-> (Union LinearModelResult GLMResult) Nat))
  (doc 'description "Extract observation count from a fitted model result.")
  (cond
    [(linear-model? model)
     (linear-model-n model)]
    [(glm-result? model)
     (glm-n model)]
    [else (error 'model-obs-count
                 "unsupported model type" (if (pair? model) (car model) model))]))

(define (model-aic-info model)
  (doc 'export #t)
  (doc 'type '(-> (Union LinearModelResult GLMResult) Number))
  (doc 'description "Compute AIC for a regression model using info-theoretic AIC.
   AIC = -2*logL + 2*k where logL is Gaussian log-likelihood and k is parameter count.
   Lower is better. Grounded in KL divergence from model-selection-info.")
  (let ([ll (model-log-likelihood-gaussian model)]
        [k (model-param-count model)])
    (aic ll k)))

(define (model-bic-info model)
  (doc 'export #t)
  (doc 'type '(-> (Union LinearModelResult GLMResult) Number))
  (doc 'description "Compute BIC for a regression model using info-theoretic BIC.
   BIC = -2*logL + k*log(n). Penalizes complexity more than AIC for n >= 8.")
  (let ([ll (model-log-likelihood-gaussian model)]
        [k (model-param-count model)]
        [n (model-obs-count model)])
    (bic ll k n)))

(define (model-aicc-info model)
  (doc 'export #t)
  (doc 'type '(-> (Union LinearModelResult GLMResult) Number))
  (doc 'description "Compute corrected AIC (AICc) for small-sample regression models.
   AICc = AIC + 2k(k+1)/(n-k-1). Use when n/k < 40.")
  (let ([ll (model-log-likelihood-gaussian model)]
        [k (model-param-count model)]
        [n (model-obs-count model)])
    (aicc ll k n)))

(define (compare-models-info models)
  (doc 'export #t)
  (doc 'type '(-> (List (Union LinearModelResult GLMResult))
                  (List (Pair Number Number Number))))
  (doc 'description "Compare multiple regression models using info-theoretic criteria.
   Returns list of (AIC BIC AICc) triples, one per model, in the same order.
   Use with aic-weights on the AIC column for relative model probabilities.")
  (map (lambda (m)
         (list (model-aic-info m)
               (model-bic-info m)
               (model-aicc-info m)))
       models))

;;; ============================================================================
;;; Section 3: KL-Divergence Distribution Comparison
;;; ============================================================================

(doc 'section 'kl-distribution-comparison)

(doc 'note "Compare two empirical distributions using KL divergence.
  The key step is converting raw sample vectors to probability mass functions
  over a shared bin grid, then feeding to kl-divergence from entropy.ss.
  Additive smoothing prevents infinite divergence from zero-probability bins.")

(define (empirical-to-probs xs num-bins)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Nat (List Number)))
  (doc 'description "Convert a sample vector to a probability distribution via binning.
   Returns a list of probabilities (one per bin) that sums to 1.
   Uses uniform binning with the given number of bins.")
  (if (or (null? xs) (<= num-bins 0))
      '()
      (let* ([bins (discretize-uniform xs num-bins)]
             [counts (make-vector num-bins 0)])
        ;; Count occurrences per bin
        (let loop ([remaining bins])
          (if (null? remaining)
              ;; Convert counts to probabilities
              (let ([n (length xs)])
                (let build ([i 0] [acc '()])
                  (if (= i num-bins)
                      (reverse acc)
                      (build (+ i 1) (cons (/ (vector-ref counts i) n) acc)))))
              (begin
                (vector-set! counts (car remaining)
                             (+ 1 (vector-ref counts (car remaining))))
                (loop (cdr remaining))))))))

(define (smooth-probs probs alpha)
  (doc 'export #f)
  (doc 'type '(-> (List Number) Number (List Number)))
  (doc 'description "Apply additive (Laplace) smoothing to a probability distribution.
   Prevents zero probabilities which cause infinite KL divergence.
   alpha is the smoothing parameter (typically a small value like 1e-10).")
  (let* ([n (length probs)]
         [total-alpha (* n alpha)]
         [total (+ 1 total-alpha)])
    (map (lambda (p) (/ (+ p alpha) total)) probs)))

(define (kl-compare-distributions xs ys . opts)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (List Number) [Nat] Number))
  (doc 'description "Compute KL divergence D_KL(P||Q) between two empirical distributions.
   xs and ys are raw sample vectors. They are discretized into a shared bin grid
   (using the global min/max across both samples), then KL divergence is computed.
   Applies additive smoothing (1e-10) to prevent infinite divergence from empty bins.
   Optional third argument specifies number of bins (default: Sturges' rule on xs).")
  (if (or (null? xs) (null? ys))
      0
      (let* ([num-bins (if (and (pair? opts) (integer? (car opts)))
                           (car opts)
                           (default-num-bins (length xs)))]
             ;; Use shared binning range across both samples
             [all-vals (append xs ys)]
             [lo (apply min all-vals)]
             [hi (apply max all-vals)]
             [range (- hi lo)])
        (if (<= range 0)
            ;; All values identical in both samples -- zero divergence
            0
            (let* ([bin-width (/ range num-bins)]
                   [assign (lambda (v)
                             (min (inexact->exact (floor (/ (- v lo) bin-width)))
                                  (- num-bins 1)))]
                   ;; Bin both samples on the shared grid
                   [bins-x (map assign xs)]
                   [bins-y (map assign ys)]
                   [probs-x (bins-to-probs bins-x num-bins (length xs))]
                   [probs-y (bins-to-probs bins-y num-bins (length ys))]
                   ;; Apply additive smoothing to prevent infinite KL
                   [smoothed-x (smooth-probs probs-x 1e-10)]
                   [smoothed-y (smooth-probs probs-y 1e-10)])
              (kl-divergence smoothed-x smoothed-y))))))

(define (kl-compare-symmetric xs ys . opts)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (List Number) [Nat] Number))
  (doc 'description "Symmetric KL divergence between two empirical distributions.
   (D_KL(P||Q) + D_KL(Q||P)) / 2. Useful when neither distribution is 'the reference'.")
  (let ([fwd (apply kl-compare-distributions xs ys opts)]
        [rev (apply kl-compare-distributions ys xs opts)])
    (/ (+ fwd rev) 2)))

(define (bins-to-probs bins num-bins n)
  (doc 'export #f)
  (doc 'type '(-> (List Nat) Nat Nat (List Number)))
  (doc 'description "Convert bin assignments to probability distribution")
  (let ([counts (make-vector num-bins 0)])
    (let loop ([remaining bins])
      (if (null? remaining)
          (let build ([i 0] [acc '()])
            (if (= i num-bins)
                (reverse acc)
                (build (+ i 1) (cons (/ (vector-ref counts i) n) acc))))
          (begin
            (vector-set! counts (car remaining)
                         (+ 1 (vector-ref counts (car remaining))))
            (loop (cdr remaining)))))))
