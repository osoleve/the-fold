;;; lattice/statistics/core/test-model-protocol.ss — Tests for Statistical Model Protocols
;;;
;;; Verifies that the protocol system correctly dispatches to implementations
;;; for linear-model, glm-model, and ar-result types.

(load "core/testing/test-framework.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/statistics/core/model-protocol.ss")
(load "lattice/statistics/regression/linear.ss")
(load "lattice/statistics/regression/regularized.ss")

(test-group "model-protocol"

  (define-test "protocol-dispatch-linear-model-coefficients"
    ;; Create a simple linear model result
    (let* ([coeffs '#(1.5 2.0)]
           [se '#(0.1 0.2)]
           [t-vals '#(15.0 10.0)]
           [p-vals '#(0.001 0.001)]
           [residuals '#(0.1 -0.1 0.05)]
           [fitted '#(2.9 3.1 4.95)]
           [model (make-linear-model-result
                   coeffs se t-vals p-vals
                   0.95 0.94
                   residuals fitted
                   0.0125 0.00625 3 2)])
      ;; Protocol should extract coefficients
      (assert-equal coeffs (model-coefficients model))))

  (define-test "protocol-dispatch-linear-model-residuals"
    (let* ([residuals '#(0.1 -0.1 0.05)]
           [model (make-linear-model-result
                   '#(1.0) '#(0.1) '#(10.0) '#(0.001)
                   0.9 0.89
                   residuals '#(1.0 2.0 3.0)
                   0.01 0.01 3 1)])
      (assert-equal residuals (model-residuals model))))

  (define-test "protocol-dispatch-linear-model-n"
    (let ([model (make-linear-model-result
                  '#(1.0) '#(0.1) '#(10.0) '#(0.001)
                  0.9 0.89
                  '#(0.1) '#(1.0)
                  0.01 0.01 42 1)])
      (assert-equal 42 (model-n model))))

  (define-test "protocol-dispatch-linear-model-p"
    (let ([model (make-linear-model-result
                  '#(1.0 2.0 3.0) '#(0.1 0.1 0.1) '#(10.0 20.0 30.0) '#(0.001 0.001 0.001)
                  0.9 0.89
                  '#(0.1) '#(1.0)
                  0.01 0.01 10 3)])
      (assert-equal 3 (model-p model))))

  (define-test "model-mse-computation"
    ;; MSE should be sum(residuals²)/n
    (let* ([residuals '#(1.0 -1.0 2.0 -2.0)]  ; sum of squares = 10
           [model (make-linear-model-result
                   '#(1.0) '#(0.1) '#(10.0) '#(0.001)
                   0.9 0.89
                   residuals '#(1.0 2.0 3.0 4.0)
                   10.0 2.5 4 1)])
      ;; MSE = 10/4 = 2.5
      (assert-equal 2.5 (model-mse model))))

  (define-test "model-rmse-computation"
    (let* ([residuals '#(1.0 -1.0 2.0 -2.0)]
           [model (make-linear-model-result
                   '#(1.0) '#(0.1) '#(10.0) '#(0.001)
                   0.9 0.89
                   residuals '#(1.0 2.0 3.0 4.0)
                   10.0 2.5 4 1)])
      ;; RMSE = sqrt(2.5) ≈ 1.581
      (let ([rmse (model-rmse model)])
        (assert-true (< (abs (- rmse (sqrt 2.5))) 0.001)))))

  (define-test "model-mae-computation"
    (let* ([residuals '#(1.0 -1.0 2.0 -2.0)]  ; MAE = (1+1+2+2)/4 = 1.5
           [model (make-linear-model-result
                   '#(1.0) '#(0.1) '#(10.0) '#(0.001)
                   0.9 0.89
                   residuals '#(1.0 2.0 3.0 4.0)
                   10.0 2.5 4 1)])
      (assert-equal 1.5 (model-mae model))))

  (define-test "model-summary-returns-assoc"
    (let* ([model (make-linear-model-result
                   '#(1.0 2.0) '#(0.1 0.2) '#(10.0 10.0) '#(0.001 0.001)
                   0.95 0.94
                   '#(0.1 -0.1) '#(1.0 2.0)
                   0.02 0.01 2 2)]
           [summary (model-summary model)])
      ;; Should have expected keys (assq returns pair, so use pair? to check)
      (assert-true (pair? (assq 'type summary)))
      (assert-true (pair? (assq 'n summary)))
      (assert-true (pair? (assq 'p summary)))
      (assert-true (pair? (assq 'sigma summary)))
      (assert-true (pair? (assq 'mse summary)))
      (assert-true (pair? (assq 'aic summary)))
      ;; Linear model should include r-squared
      (assert-true (pair? (assq 'r-squared summary)))))

  (define-test "compare-models-aic-sorts-correctly"
    ;; Create models with known AICs
    (let* ([model1 (make-linear-model-result
                    '#(1.0) '#(0.1) '#(10.0) '#(0.001)
                    0.9 0.89 '#(0.5 0.5) '#(1.0 2.0)
                    0.5 0.25 2 1)]  ; Lower SSE → lower AIC
           [model2 (make-linear-model-result
                    '#(1.0) '#(0.1) '#(10.0) '#(0.001)
                    0.8 0.79 '#(1.0 1.0) '#(1.0 2.0)
                    2.0 1.0 2 1)]   ; Higher SSE → higher AIC
           [comparison (compare-models-aic (list model2 model1))])
      ;; First model should have lower AIC (delta = 0.0)
      (assert-equal 0.0 (cdr (car comparison)))
      ;; The better model (model1) should be first
      (assert-equal model1 (car (car comparison)))))

  (define-test "glm-protocol-dispatch"
    ;; Create a GLM result and verify protocol dispatch
    (let* ([model (make-glm-result
                   'binomial 'logit
                   '#(0.5 1.2) '#(0.1 0.2) '#(5.0 6.0) '#(0.001 0.001)
                   10.5 25.0 15.0
                   '#(0.1 -0.2) '#(0.4 0.8)
                   2 5 #t)])
      (assert-equal '#(0.5 1.2) (model-coefficients model))
      (assert-equal 2 (model-n model))
      (assert-equal 10.5 (model-deviance model))
      (assert-equal 15.0 (model-aic model))))

  (define-test "ar-protocol-dispatch"
    ;; Create an AR result and verify protocol dispatch
    (let* ([model (make-ar-result
                   '#(0.5 0.3)  ; AR coefficients
                   2.0          ; intercept/mean
                   '#(0.1 -0.1 0.05)  ; residuals
                   '#(2.0 2.1 1.95)   ; fitted
                   0.1          ; sigma
                   15.5         ; AIC
                   2)])         ; order
      (assert-equal '#(0.5 0.3) (model-coefficients model))
      (assert-equal 2.0 (model-intercept model))
      (assert-equal 3 (model-n model))
      (assert-equal 15.5 (model-aic model))))

  (define-test "protocol-handles-missing-optional"
    ;; AR model should return #f for r-squared (not meaningful)
    (let* ([model (make-ar-result
                   '#(0.5) 1.0 '#(0.1) '#(1.0) 0.1 10.0 1)])
      (assert-false (model-r-squared model))))

  (define-test "linear-model-predict-via-protocol"
    ;; Test that prediction works through protocol
    (let* ([coeffs '#(1.0 2.0)]  ; y = 1 + 2*x
           [model (make-linear-model-result
                   coeffs '#(0.1 0.1) '#(10.0 20.0) '#(0.001 0.001)
                   0.99 0.98
                   '#(0.0 0.0) '#(3.0 5.0)
                   0.0 0.0 2 2)]
           ;; New data: X = [[1, 1], [1, 2], [1, 3]] (with intercept column)
           [X-new (list 'matrix 3 2 '#(1 1 1 2 1 3))]
           [predictions (model-predict model X-new)])
      ;; Expected: [1+2*1, 1+2*2, 1+2*3] = [3, 5, 7]
      (assert-equal 3 (vector-length predictions))
      (assert-equal 3.0 (vector-ref predictions 0))
      (assert-equal 5.0 (vector-ref predictions 1))
      (assert-equal 7.0 (vector-ref predictions 2)))))

  ;; Integration test: cross-validation with actual linear model fitting
  (define-test "cv-score-with-linear-model-fit"
    ;; Create synthetic data: y = 2 + 3*x (exact linear relationship)
    ;; X has intercept column
    (let* ([n 20]
           [X-data (make-vector (* n 2))]
           [y (make-vector n)])
      ;; Fill X and y with exact linear relationship
      (do ([i 0 (+ i 1)])
          [(= i n)]
        (let* ([x-val (+ 1.0 (* i 0.5))]
               [y-val (+ 2.0 (* 3.0 x-val))])
          (vector-set! X-data (* i 2) 1.0)      ; intercept
          (vector-set! X-data (+ (* i 2) 1) x-val)
          (vector-set! y i y-val)))
      (let* ([X (list 'matrix n 2 X-data)]
             ;; Use cv-score with linear-model-fit
             [cv-mse (cv-score linear-model-fit X y 4)])
        ;; For exact linear data, CV MSE should be very small
        (assert-true (< cv-mse 0.001))
        ;; CV MSE should be non-negative
        (assert-true (>= cv-mse 0.0)))))

  ;; Test that same cv-score function works with different model types
  (define-test "cv-score-polymorphic-interface"
    ;; The key insight: cv-score doesn't care about the model type!
    ;; It just needs a fit function that returns something implementing protocols
    (let* ([n 12]
           [X-data (make-vector (* n 2))]
           [y (make-vector n)])
      (do ([i 0 (+ i 1)])
          [(= i n)]
        (let* ([x-val (+ 1.0 i)]
               [y-val (+ 1.0 (* 2.0 x-val))])
          (vector-set! X-data (* i 2) 1.0)
          (vector-set! X-data (+ (* i 2) 1) x-val)
          (vector-set! y i y-val)))
      (let* ([X (list 'matrix n 2 X-data)]
             ;; Both use the same cv-score interface
             [cv-ols (cv-score linear-model-fit X y 3)]
             [cv-ridge (cv-score (lambda (X y) (ridge-fit X y 0.1)) X y 3)])
        ;; Both should produce valid results
        (assert-true (number? cv-ols))
        (assert-true (number? cv-ridge))
        ;; For this simple data, OLS should do slightly better than ridge
        ;; (or very similar since regularization is small)
        (assert-true (< (abs (- cv-ols cv-ridge)) 1.0)))))

(run-all-tests)
