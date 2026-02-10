;;; lattice/statistics/regression/test-traced-regression.ss — Tests for Traced Regression
;;; @module test-traced-regression

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/statistics/regression/traced-regression.ss")

(test-group "traced-regression"

  ;;; --- Parameter Structure ---

  (define-test "make-regression-params"
    (let ([params (make-regression-params '#(1.0 2.0 3.0))])
      (assert-true (regression-params? params))
      (assert-equal '#(1.0 2.0 3.0) (regression-params-coefficients params))))

  ;;; --- Parameter Optics ---

  (define-test "params-coef-lens view"
    (let ([params (make-regression-params '#(1.0 2.0))])
      (assert-equal '#(1.0 2.0) (^. params params-coef-lens))))

  (define-test "params-coef-lens set"
    (let* ([params (make-regression-params '#(1.0 2.0))]
           [params2 (& params (.~ params-coef-lens '#(10.0 20.0)))])
      (assert-equal '#(10.0 20.0) (^. params2 params-coef-lens))))

  (define-test "params-coef-i-lens"
    (let ([params (make-regression-params '#(1.0 2.0 3.0))])
      (assert-equal 2.0 (^. params (params-coef-i-lens 1)))
      (let ([params2 (& params (.~ (params-coef-i-lens 1) 99.0))])
        (assert-equal 99.0 (^. params2 (params-coef-i-lens 1)))
        (assert-equal 1.0 (^. params2 (params-coef-i-lens 0))))))

  ;;; --- Untraced Loss Functions ---

  (define-test "compute-sse simple"
    ;; y = [3, 5, 7], X = [[1,1], [1,2], [1,3]], beta = [1, 2]
    ;; predictions: 1*1+1*2=3, 1*1+2*2=5, 1*1+3*2=7
    ;; residuals: 0, 0, 0 -> SSE = 0
    (let ([X (matrix-from-lists '((1 1) (1 2) (1 3)))]
          [y '#(3 5 7)]
          [coef '#(1.0 2.0)])
      (assert-true (< (compute-sse X y coef) 0.001))))

  (define-test "compute-sse with error"
    ;; y = [4, 5, 7], X = [[1,1], [1,2], [1,3]], beta = [1, 2]
    ;; predictions: 3, 5, 7
    ;; residuals: 1, 0, 0 -> SSE = 1
    (let ([X (matrix-from-lists '((1 1) (1 2) (1 3)))]
          [y '#(4 5 7)]
          [coef '#(1.0 2.0)])
      (assert-true (< (abs (- (compute-sse X y coef) 1.0)) 0.001))))

  (define-test "compute-ridge-loss"
    (let ([X (matrix-from-lists '((1 1) (1 2) (1 3)))]
          [y '#(3 5 7)]
          [coef '#(1.0 2.0)]
          [lambda 0.1])
      ;; SSE = 0, penalty = 0.1 * (1 + 4) = 0.5
      (assert-true (< (abs (- (compute-ridge-loss X y coef lambda) 0.5)) 0.001))))

  ;;; --- Gradient Computation ---

  (define-test "mse-gradient direction"
    ;; Starting from wrong coefficients, gradient should point toward correct ones
    (let* ([X (matrix-from-lists '((1 1) (1 2) (1 3)))]
           [y '#(3 5 7)]  ; True beta = [1, 2]
           [params (make-regression-params '#(0.0 0.0))]  ; Start at zero
           [grad (mse-gradient X y params)])
      ;; Gradient should be negative (pointing toward positive coefficients)
      (assert-true (< (vector-ref grad 0) 0))
      (assert-true (< (vector-ref grad 1) 0))))

  (define-test "ridge-gradient includes penalty"
    (let* ([X (matrix-from-lists '((1 1) (1 2)))]
           [y '#(3 5)]
           [params (make-regression-params '#(2.0 2.0))]
           [grad-mse (mse-gradient X y params)]
           [grad-ridge (ridge-gradient X y params 1.0)])
      ;; Ridge gradient = MSE gradient + 2*lambda*beta
      ;; So ridge gradient should be larger in magnitude when beta > 0
      (assert-true (> (vector-ref grad-ridge 0) (vector-ref grad-mse 0)))
      (assert-true (> (vector-ref grad-ridge 1) (vector-ref grad-mse 1)))))

  ;;; --- Gradient Descent ---

  (define-test "mse-gd converges"
    ;; Simple regression: y = 1 + 2*x
    (let* ([X (matrix-from-lists '((1 1) (1 2) (1 3) (1 4)))]
           [y '#(3.0 5.0 7.0 9.0)]  ; y = 1 + 2*x
           [params (init-params 2)]
           [final (mse-gd X y params 0.1 100)]
           [coef (regression-params-coefficients final)])
      ;; Should converge close to [1, 2]
      (assert-true (< (abs (- (vector-ref coef 0) 1.0)) 0.5))
      (assert-true (< (abs (- (vector-ref coef 1) 2.0)) 0.5))))

  (define-test "ridge-gd-step reduces loss"
    ;; Verify that a single step in the gradient direction reduces loss
    (let* ([X (matrix-from-lists '((1 1) (1 2) (1 3) (1 4)))]
           [y '#(3.0 5.0 7.0 9.0)]
           [params (make-regression-params '#(0.5 1.0))]  ; Start near solution
           [loss-before (compute-ridge-loss X y (regression-params-coefficients params) 0.1)]
           [params-after (ridge-gd-step X y params 0.1 0.01)]  ; Small step
           [loss-after (compute-ridge-loss X y (regression-params-coefficients params-after) 0.1)])
      ;; Loss should decrease (or stay same if at minimum)
      (assert-true (<= loss-after (+ loss-before 0.01)))))  ; Allow small tolerance

  (define-test "ridge-gd makes progress"
    ;; Multiple steps should reduce loss
    (let* ([X (matrix-from-lists '((1 1) (1 2) (1 3) (1 4)))]
           [y '#(3.0 5.0 7.0 9.0)]
           [params (init-params 2)]
           [loss-before (compute-ridge-loss X y (regression-params-coefficients params) 0.01)]
           [params-after (ridge-gd X y params 0.01 0.01 10)]  ; 10 small steps
           [loss-after (compute-ridge-loss X y (regression-params-coefficients params-after) 0.01)])
      ;; Loss should decrease
      (assert-true (< loss-after loss-before))))

  ;;; --- Coefficient Sensitivity ---

  (define-test "coefficient-sensitivity"
    (let* ([X (matrix-from-lists '((1 0) (1 0) (1 0)))]
           [y '#(1.0 1.0 1.0)]  ; y = 1, only intercept matters
           [params (make-regression-params '#(0.0 5.0))])
      ;; Gradient w.r.t. coefficient 1 should be 0 (it's not used)
      ;; Gradient w.r.t. coefficient 0 should be non-zero
      (let ([sens0 (coefficient-sensitivity X y params 0)]
            [sens1 (coefficient-sensitivity X y params 1)])
        (assert-true (> (abs sens0) 0.001))
        (assert-true (< (abs sens1) 0.001)))))

  ;;; --- Initialization ---

  (define-test "init-params creates zeros"
    (let ([params (init-params 3)])
      (assert-equal 0.0 (vector-ref (regression-params-coefficients params) 0))
      (assert-equal 0.0 (vector-ref (regression-params-coefficients params) 1))
      (assert-equal 0.0 (vector-ref (regression-params-coefficients params) 2))))

) ; end test-group

(run-all-tests)
