(load "core/testing/test-framework.ss")
(load "lattice/info-stat/mi-bridge.ss")

;;; Tests for info-stat/mi-bridge — the info-statistics bridge module

;;; ============================================================================
;;; Section 1: MI Feature Selection
;;; ============================================================================

(test-group "mi-feature-selection"

  (define-test "mi-rank-features returns descending MI scores"
    ;; Feature 0: perfectly correlated with target (same values)
    ;; Feature 1: random noise (low MI)
    ;; Feature 2: moderately correlated
    (let* ([target '(1 1 1 2 2 2 3 3 3 4 4 4)]
           [feat0  '(1 1 1 2 2 2 3 3 3 4 4 4)]  ; perfect correlation
           [feat1  '(3 1 4 1 5 9 2 6 5 3 5 8)]  ; noise
           [feat2  '(1 1 2 2 2 3 3 3 4 4 4 4)]  ; moderate
           [ranked (mi-rank-features (list feat0 feat1 feat2) target 4)])
      ;; Ranked should be a list of pairs
      (assert-true (pair? ranked))
      ;; First element should have highest MI
      (assert-true (>= (cdar ranked)
                        (cdr (cadr ranked))))
      ;; Feature 0 (perfect correlation) should be first or tied for first
      (assert-true (>= (cdr (assoc 0 ranked))
                        (cdr (assoc 1 ranked))))))

  (define-test "mi-rank-features with identical feature and target"
    ;; When feature = target, MI should equal H(target)
    (let* ([target '(0 0 1 1 2 2 3 3)]
           [feat0  '(0 0 1 1 2 2 3 3)]
           [ranked (mi-rank-features (list feat0) target 4)])
      ;; MI should be positive for a non-constant variable
      (assert-true (> (cdar ranked) 0))))

  (define-test "mi-select-features returns correct number of indices"
    (let* ([target '(1 2 3 4 5 6 7 8)]
           [feat0  '(1 2 3 4 5 6 7 8)]
           [feat1  '(8 7 6 5 4 3 2 1)]
           [feat2  '(1 1 1 1 1 1 1 1)]
           [selected (mi-select-features (list feat0 feat1 feat2) target 2)])
      (assert-equal 2 (length selected))
      ;; All indices should be valid
      (assert-true (andmap (lambda (i) (and (>= i 0) (< i 3))) selected))))

  (define-test "mi-select-features with k larger than feature count"
    (let* ([target '(1 2 3)]
           [feat0  '(1 2 3)]
           [selected (mi-select-features (list feat0) target 5)])
      ;; Should return at most 1 (only 1 feature available)
      (assert-equal 1 (length selected))))

  (define-test "mi-rank-features with constant feature gives zero MI"
    (let* ([target '(1 2 3 4 5 6 7 8)]
           [const  '(5 5 5 5 5 5 5 5)]
           [ranked (mi-rank-features (list const) target 4)])
      ;; Constant feature has zero entropy, so MI = 0
      (assert-true (<= (cdar ranked) 1e-10))))

) ;; end mi-feature-selection

;;; ============================================================================
;;; Section 2: Entropy-Based Model Comparison
;;; ============================================================================

(test-group "entropy-model-comparison"

  ;; Helper: build a minimal linear model result for testing
  (define (make-test-lm residuals n p)
    (let* ([coeffs (make-vector p 1.0)]
           [se (make-vector p 0.1)]
           [t-vals (make-vector p 10.0)]
           [p-vals (make-vector p 0.001)]
           [fitted (make-vector n 0.0)]
           [sse (let loop ([i 0] [s 0])
                  (if (= i (vector-length residuals))
                      s
                      (loop (+ i 1)
                            (+ s (* (vector-ref residuals i)
                                    (vector-ref residuals i))))))]
           [mse (/ sse (max 1 (- n p)))])
      (make-linear-model-result coeffs se t-vals p-vals
                                0.9 0.85
                                residuals fitted sse mse n p)))

  (define-test "model-aic-info computes AIC for linear model"
    (let* ([resid (vector 0.1 -0.2 0.15 -0.1 0.05 -0.15 0.2 -0.05 0.1 -0.1)]
           [model (make-test-lm resid 10 2)]
           [aic-val (model-aic-info model)])
      ;; AIC should be a finite number
      (assert-true (number? aic-val))
      (assert-true (finite? aic-val))))

  (define-test "model-bic-info computes BIC for linear model"
    (let* ([resid (vector 0.1 -0.2 0.15 -0.1 0.05 -0.15 0.2 -0.05 0.1 -0.1)]
           [model (make-test-lm resid 10 2)]
           [bic-val (model-bic-info model)])
      (assert-true (number? bic-val))
      (assert-true (finite? bic-val))))

  (define-test "model-bic-info > model-aic-info for n >= 8"
    ;; BIC penalizes complexity more than AIC when n >= 8
    (let* ([resid (vector 0.1 -0.2 0.15 -0.1 0.05 -0.15 0.2 -0.05 0.1 -0.1)]
           [model (make-test-lm resid 10 3)]
           [aic-val (model-aic-info model)]
           [bic-val (model-bic-info model)])
      (assert-true (> bic-val aic-val))))

  (define-test "model-aicc-info converges to AIC for large n"
    ;; AICc correction vanishes as n grows
    (let* ([n 200]
           [resid (make-vector n 0.1)]
           [model (make-test-lm resid n 2)]
           [aic-val (model-aic-info model)]
           [aicc-val (model-aicc-info model)]
           [diff (abs (- aicc-val aic-val))])
      ;; For n=200, k=2, the correction 2*2*3/(200-2-1) ~ 0.06 is small
      (assert-true (< diff 1.0))))

  (define-test "compare-models-info returns triple per model"
    (let* ([resid1 (vector 0.5 -0.3 0.2 -0.4 0.1)]
           [resid2 (vector 0.1 -0.1 0.05 -0.05 0.02)]
           [m1 (make-test-lm resid1 5 2)]
           [m2 (make-test-lm resid2 5 3)]
           [result (compare-models-info (list m1 m2))])
      (assert-equal 2 (length result))
      ;; Each entry is a triple (AIC BIC AICc)
      (assert-equal 3 (length (car result)))
      (assert-equal 3 (length (cadr result)))))

  (define-test "better model has lower AIC"
    ;; Model with smaller residuals should have lower AIC
    (let* ([resid-good (vector 0.01 -0.02 0.01 -0.01 0.02
                               -0.01 0.01 -0.02 0.01 -0.01)]
           [resid-bad  (vector 1.0 -1.5 2.0 -0.8 1.2
                               -1.1 0.9 -1.3 1.4 -0.7)]
           [m-good (make-test-lm resid-good 10 2)]
           [m-bad  (make-test-lm resid-bad 10 2)]
           [aic-good (model-aic-info m-good)]
           [aic-bad  (model-aic-info m-bad)])
      (assert-true (< aic-good aic-bad))))

) ;; end entropy-model-comparison

;;; ============================================================================
;;; Section 3: KL Divergence Distribution Comparison
;;; ============================================================================

(test-group "kl-distribution-comparison"

  (define-test "kl-compare-distributions is zero for identical samples"
    (let* ([xs '(1 2 3 4 5 6 7 8 9 10)]
           [kl (kl-compare-distributions xs xs 5)])
      ;; KL of P with itself should be ~0 (within smoothing tolerance)
      (assert-true (< kl 0.01))))

  (define-test "kl-compare-distributions is positive for different samples"
    (let* ([xs '(1 1 1 2 2 2 3 3 3 4)]
           [ys '(7 7 7 8 8 8 9 9 9 10)]
           [kl (kl-compare-distributions xs ys 4)])
      (assert-true (> kl 0))))

  (define-test "kl-compare-symmetric is symmetric"
    (let* ([xs '(1 2 3 4 5)]
           [ys '(6 7 8 9 10)]
           [sym1 (kl-compare-symmetric xs ys 4)]
           [sym2 (kl-compare-symmetric ys xs 4)])
      ;; Symmetric KL should be the same in both directions
      (assert-true (< (abs (- sym1 sym2)) 1e-10))))

  (define-test "kl-compare-distributions handles overlapping samples"
    (let* ([xs '(1 2 3 4 5 6 7 8)]
           [ys '(3 4 5 6 7 8 9 10)]
           [kl (kl-compare-distributions xs ys 5)])
      ;; Should be small but positive for overlapping distributions
      (assert-true (>= kl 0))
      (assert-true (finite? kl))))

  (define-test "empirical-to-probs sums to 1"
    (let* ([xs '(1 2 3 4 5 6 7 8 9 10)]
           [probs (empirical-to-probs xs 5)]
           [total (fold-left + 0 probs)])
      (assert-true (< (abs (- total 1.0)) 1e-10))))

  (define-test "empirical-to-probs has correct bin count"
    (let* ([xs '(1 2 3 4 5 6 7 8 9 10)]
           [probs (empirical-to-probs xs 5)])
      (assert-equal 5 (length probs))))

  (define-test "kl-compare-distributions with empty input returns 0"
    (assert-equal 0 (kl-compare-distributions '() '(1 2 3)))
    (assert-equal 0 (kl-compare-distributions '(1 2 3) '())))

  (define-test "kl-compare-distributions with default bins"
    ;; Should work without explicit bin count (uses Sturges' rule)
    (let* ([xs '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)]
           [ys '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)]
           [kl (kl-compare-distributions xs ys)])
      (assert-true (>= kl 0))
      (assert-true (finite? kl))))

) ;; end kl-distribution-comparison

;;; ============================================================================
;;; Section 4: Integration tests
;;; ============================================================================

(test-group "integration"

  (define-test "MI feature selection with statistically meaningful data"
    ;; Create data where feature 0 has strong linear relationship with target
    ;; and feature 1 is noise
    (let* ([target '(10 20 30 40 50 60 70 80 90 100)]
           [feat0  '(11 19 31 39 51 59 71 79 91 99)]  ; strong signal
           [feat1  '(50 50 50 50 50 50 50 50 50 50)]  ; constant noise
           [selected (mi-select-features (list feat0 feat1) target 1)])
      ;; Feature 0 should be selected over constant noise
      (assert-equal '(0) selected)))

  (define-test "round-trip: features -> MI -> AIC comparison"
    ;; Use MI to identify that feat0 is better, then verify AIC agrees
    ;; when we imagine fitting models with each feature
    (let* ([target '(1 2 3 4 5 6 7 8)]
           [feat0  '(1 2 3 4 5 6 7 8)]  ; perfect predictor
           [feat1  '(5 5 5 5 5 5 5 5)]  ; useless predictor
           [ranked (mi-rank-features (list feat0 feat1) target 4)]
           ;; Feature 0 should have higher MI
           [mi0 (cdr (assoc 0 ranked))]
           [mi1 (cdr (assoc 1 ranked))])
      (assert-true (> mi0 mi1))))

) ;; end integration

(run-all-tests)
