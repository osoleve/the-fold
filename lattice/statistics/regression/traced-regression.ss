;;; lattice/statistics/regression/traced-regression.ss — Traced Optics for Regression
;;;
;;; Integrates traced-optics with regression models for:
;;;   - Computing gradients of loss functions w.r.t. parameters
;;;   - Gradient-based optimization (ridge, lasso-like penalties)
;;;   - Model component access via optics
;;;
;;; The key insight: use optics to specify WHICH parameters to differentiate,
;;; and let the autodiff system compute gradients THROUGH that path.
;;;
;;; Example:
;;;   ;; Gradient of ridge loss w.r.t. coefficients
;;;   (optic-gradient
;;;     (lambda (params) (traced-ridge-loss X y params lambda))
;;;     params-coef-lens
;;;     my-params)

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module traced-regression
;;; @requires prelude matrix optics traced-optics result-types
(require 'prelude)
(require 'matrix)
(require 'optics)
(require 'traced-optics)
(require 'result-types)

(doc 'module 'traced-regression)
(doc 'bridges '(statistics optics autodiff))
(doc 'description "Traced optics integration for regression - gradients through parameter paths")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Section: Parameter Structure
;;; ============================================================

(doc 'section 'params)

;;; Regression parameters are structured as:
;;;   (regression-params coefficients)
;;; where coefficients is a Scheme vector.

(define (make-regression-params coefficients)
  (doc 'export #t)
  (doc 'type '(-> Vec RegressionParams))
  (doc 'description "Create regression parameters from coefficient vector")
  (list 'regression-params coefficients))

(define (regression-params? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'regression-params)))

(define (regression-params-coefficients params)
  (doc 'export #t)
  (cadr params))

;;; ============================================================
;;; Section: Vector Optics (needed for parameter optics)
;;; ============================================================

;;; vec-element-lens : Nat -> Lens Vec a
;;; (Defined here for standalone use, also available in matrix-optics)
(define (vec-element-lens n)
  (make-lens
   (lambda (v) (vector-ref v n))
   (lambda (val v)
     (let* ([len (vector-length v)]
            [new-v (make-vector len)])
       (do ([i 0 (+ i 1)])
           ((= i len) new-v)
         (vector-set! new-v i (if (= i n) val (vector-ref v i))))))))

;;; vec-elements-each : Traversal Vec a
(define vec-elements-each
  (make-traversal
   (lambda (f v)
     (let* ([len (vector-length v)]
            [new-v (make-vector len)])
       (do ([i 0 (+ i 1)])
           ((= i len) new-v)
         (vector-set! new-v i (f (vector-ref v i))))))
   vector->list))

;;; ============================================================
;;; Section: Parameter Optics
;;; ============================================================

(doc 'section 'parameter-optics)

;;; params-coef-lens : Lens RegressionParams Vec
;;; Focus on the coefficient vector.
(doc params-coef-lens 'export #t)
(doc params-coef-lens 'type '(Lens RegressionParams Vec))
(doc params-coef-lens 'description "Lens focusing on coefficient vector")
(define params-coef-lens
  (make-lens
   regression-params-coefficients
   (lambda (new-coef params)
     (make-regression-params new-coef))))

;;; params-coef-i-lens : Nat -> Lens RegressionParams Num
;;; Focus on coefficient i.
(define (params-coef-i-lens i)
  (doc 'export #t)
  (doc 'type '(-> Nat (Lens RegressionParams Num)))
  (>>> params-coef-lens (vec-element-lens i)))

;;; params-coef-each : Traversal RegressionParams Num
;;; Traverse each coefficient.
(doc params-coef-each 'export #t)
(doc params-coef-each 'type '(Traversal RegressionParams Num))
(define params-coef-each
  (>>> params-coef-lens vec-elements-each))

;;; ============================================================
;;; Section: Traced Loss Functions
;;; ============================================================

(doc 'section 'traced-loss)

;;; traced-sse-list : Matrix x Vec x (List Traced) -> Traced
;;; Sum of squared errors: sum((y - X*beta)^2)
;;; The coefficients are a LIST of traced values; X and y are constants.
(define (traced-sse-list X y traced-coef-list)
  (doc 'export #t)
  (doc 'type '(-> Matrix (List Traced) Traced))
  (doc 'description "Traced SSE loss - coefficients are list of traced values")
  (let* ([n (vector-length y)]
         [p (length traced-coef-list)]
         [coef-vec (list->vector traced-coef-list)])  ; Convert for indexing
    ;; Compute residuals: y_i - sum_j(X_ij * beta_j)
    (let loop ([i 0] [total-sq 0])
      (if (= i n)
          total-sq
          (let* ([yi (vector-ref y i)]
                 ;; Compute prediction: sum_j X_ij * beta_j
                 [pred (let inner ([j 0] [sum 0])
                         (if (= j p)
                             sum
                             (inner (+ j 1)
                                    (traced-add sum
                                                (traced-mul (matrix-ref X i j)
                                                            (vector-ref coef-vec j))))))]
                 [resid (traced-sub yi pred)]
                 [sq (traced-sq resid)])
            (loop (+ i 1) (traced-add total-sq sq)))))))

;;; traced-sse : Matrix x Vec x Vec-traced -> Traced
;;; Wrapper that converts vector to list for tracing.
(define (traced-sse X y traced-coef)
  (doc 'export #t)
  (cond
    [(list? traced-coef) (traced-sse-list X y traced-coef)]
    [(vector? traced-coef) (traced-sse-list X y (vector->list traced-coef))]
    [else (error 'traced-sse "Expected list or vector of traced coefficients")]))

;;; traced-ridge-penalty : (List Traced) x Num -> Traced
;;; Ridge penalty: reg-lambda * sum(beta^2)
(define (traced-ridge-penalty traced-coef-list reg-lambda)
  (doc 'export #t)
  (doc 'type '(-> (List Traced) Num Traced))
  (doc 'description "Ridge (L2) penalty on coefficients")
  (traced-mul reg-lambda
              (fold-left (lambda (sum coef-i)
                           (traced-add sum (traced-sq coef-i)))
                         0
                         traced-coef-list)))

;;; traced-ridge-loss : Matrix x Vec x (List Traced) x Num -> Traced
;;; Ridge regression loss: SSE + reg-lambda * ||beta||^2
(define (traced-ridge-loss X y traced-coef reg-lambda)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec (List Traced) Num Traced))
  (doc 'description "Ridge regression loss = SSE + reg-lambda * L2 penalty")
  (let ([coef-list (if (list? traced-coef) traced-coef (vector->list traced-coef))])
    (traced-add (traced-sse-list X y coef-list)
                (traced-ridge-penalty coef-list reg-lambda))))

;;; traced-mse : Matrix x Vec x (Vec|List) Traced -> Traced
;;; Mean squared error: SSE / n
(define (traced-mse X y traced-coef)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec (List Traced) Traced))
  (doc 'description "Traced MSE loss")
  (let ([coef-list (if (list? traced-coef) traced-coef (vector->list traced-coef))])
    (traced-div (traced-sse-list X y coef-list)
                (vector-length y))))

;;; ============================================================
;;; Section: Gradient Computation
;;; ============================================================

(doc 'section 'gradient-computation)

;;; ridge-gradient : Matrix x Vec x RegressionParams x Num -> Vec
;;; Compute gradient of ridge loss w.r.t. all coefficients.
;;; Uses traversal to trace each coefficient individually.
(define (ridge-gradient X y params reg-lambda)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec RegressionParams Num Vec))
  (doc 'description "Gradient of ridge loss w.r.t. coefficient vector")
  (let ([grad-list (optic-gradient-list
                    (lambda (p)
                      ;; p has traced coefficients (as list through traversal)
                      (let ([coef-list (^.. p params-coef-each)])
                        (traced-ridge-loss X y coef-list reg-lambda)))
                    params-coef-each
                    params)])
    (list->vector grad-list)))

;;; mse-gradient : Matrix x Vec x RegressionParams -> Vec
;;; Gradient of MSE w.r.t. all coefficients.
(define (mse-gradient X y params)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec RegressionParams Vec))
  (doc 'description "Gradient of MSE w.r.t. coefficient vector")
  (let ([grad-list (optic-gradient-list
                    (lambda (p)
                      (let ([coef-list (^.. p params-coef-each)])
                        (traced-mse X y coef-list)))
                    params-coef-each
                    params)])
    (list->vector grad-list)))

;;; coefficient-sensitivity : Matrix x Vec x RegressionParams x Nat -> Num
;;; Gradient of MSE w.r.t. coefficient i.
;;; High magnitude = coefficient i strongly affects loss.
(define (coefficient-sensitivity X y params i)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec RegressionParams Nat Num))
  (doc 'description "Sensitivity of loss to coefficient i")
  (optic-gradient
   (lambda (p)
     (let ([coef (regression-params-coefficients p)])
       (traced-mse X y coef)))
   (params-coef-i-lens i)
   params))

;;; ============================================================
;;; Section: Gradient Descent Optimization
;;; ============================================================

(doc 'section 'optimization)

;;; ridge-gd-step : Matrix x Vec x RegressionParams x Num x Num -> RegressionParams
;;; Single gradient descent step for ridge regression.
(define (ridge-gd-step X y params reg-lambda rate)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec RegressionParams Num Num RegressionParams))
  (doc 'description "One gradient descent step for ridge regression")
  (let* ([coef (regression-params-coefficients params)]
         [grad (ridge-gradient X y params reg-lambda)]
         [p (vector-length coef)]
         [new-coef (make-vector p)])
    (do ([i 0 (+ i 1)])
        ((= i p) (make-regression-params new-coef))
      (vector-set! new-coef i
                   (- (vector-ref coef i)
                      (* rate (vector-ref grad i)))))))

;;; ridge-gd : Matrix x Vec x RegressionParams x Num x Num x Nat -> RegressionParams
;;; Multiple gradient descent steps.
(define (ridge-gd X y params reg-lambda rate n-steps)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec RegressionParams Num Num Nat RegressionParams))
  (doc 'description "Ridge regression via gradient descent")
  (if (<= n-steps 0)
      params
      (ridge-gd X y
                (ridge-gd-step X y params reg-lambda rate)
                reg-lambda rate (- n-steps 1))))

;;; mse-gd-step : Matrix x Vec x RegressionParams x Num -> RegressionParams
;;; Single gradient descent step for OLS (MSE minimization).
(define (mse-gd-step X y params rate)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec RegressionParams Num RegressionParams))
  (doc 'description "One gradient descent step for MSE")
  (let* ([coef (regression-params-coefficients params)]
         [grad (mse-gradient X y params)]
         [p (vector-length coef)]
         [new-coef (make-vector p)])
    (do ([i 0 (+ i 1)])
        ((= i p) (make-regression-params new-coef))
      (vector-set! new-coef i
                   (- (vector-ref coef i)
                      (* rate (vector-ref grad i)))))))

;;; mse-gd : Matrix x Vec x RegressionParams x Num x Nat -> RegressionParams
;;; OLS via gradient descent.
(define (mse-gd X y params rate n-steps)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec RegressionParams Num Nat RegressionParams))
  (doc 'description "OLS regression via gradient descent")
  (if (<= n-steps 0)
      params
      (mse-gd X y
              (mse-gd-step X y params rate)
              rate (- n-steps 1))))

;;; ============================================================
;;; Section: Utilities
;;; ============================================================

(doc 'section 'utilities)

;;; compute-sse : Matrix x Vec x Vec -> Num
;;; Untraced SSE computation.
(define (compute-sse X y coef)
  (doc 'export #t)
  (let* ([n (vector-length y)]
         [p (vector-length coef)])
    (let loop ([i 0] [total 0])
      (if (= i n)
          total
          (let* ([yi (vector-ref y i)]
                 [pred (let inner ([j 0] [sum 0])
                         (if (= j p)
                             sum
                             (inner (+ j 1)
                                    (+ sum (* (matrix-ref X i j)
                                              (vector-ref coef j))))))]
                 [resid (- yi pred)])
            (loop (+ i 1) (+ total (* resid resid))))))))

;;; compute-ridge-loss : Matrix x Vec x Vec x Num -> Num
;;; Untraced ridge loss computation.
(define (compute-ridge-loss X y coef reg-lambda)
  (doc 'export #t)
  (let ([penalty (let loop ([j 0] [sum 0])
                   (if (= j (vector-length coef))
                       sum
                       (loop (+ j 1)
                             (+ sum (expt (vector-ref coef j) 2)))))])
    (+ (compute-sse X y coef)
       (* reg-lambda penalty))))

;;; init-params : Nat -> RegressionParams
;;; Initialize parameters with zeros.
(define (init-params p)
  (doc 'export #t)
  (doc 'type '(-> Nat RegressionParams))
  (doc 'description "Initialize regression parameters with zeros")
  (make-regression-params (make-vector p 0.0)))

;;; init-params-random : Nat x Num -> RegressionParams
;;; Initialize parameters with small random values.
(define (init-params-random p scale)
  (doc 'export #t)
  (doc 'type '(-> Nat Num RegressionParams))
  (doc 'description "Initialize regression parameters with random values")
  (let ([coef (make-vector p)])
    (do ([i 0 (+ i 1)])
        ((= i p) (make-regression-params coef))
      (vector-set! coef i (* scale (- (random 1000) 500) 0.001)))))

;;; ============================================================
;;; Section: Model Result Optics
;;; ============================================================

(doc 'section 'result-optics)

;;; Optics for accessing components of LinearModelResult

;;; linear-model-coef-lens : Lens LinearModelResult Vec
(doc linear-model-coef-lens 'export #t)
(define linear-model-coef-lens
  (make-lens
   linear-model-coefficients
   (lambda (new-coef model)
     ;; Rebuild model with new coefficients (other fields unchanged)
     (make-linear-model-result
      new-coef
      (linear-model-se model)
      (linear-model-t-values model)
      (linear-model-p-values model)
      (linear-model-r-squared model)
      (linear-model-adj-r-squared model)
      (linear-model-residuals model)
      (linear-model-fitted model)
      (linear-model-sse model)
      (linear-model-mse model)
      (linear-model-n model)
      (linear-model-p model)))))

;;; linear-model-coef-i-lens : Nat -> Lens LinearModelResult Num
(define (linear-model-coef-i-lens i)
  (doc 'export #t)
  (>>> linear-model-coef-lens (vec-element-lens i)))

;;; linear-model-residuals-lens : Lens LinearModelResult Vec
(doc linear-model-residuals-lens 'export #t)
(define linear-model-residuals-lens
  (make-getter linear-model-residuals))

;;; linear-model-r2-getter : Getter LinearModelResult Num
(doc linear-model-r2-getter 'export #t)
(define linear-model-r2-getter
  (make-getter linear-model-r-squared))

;;; ============================================================
;;; Exports Summary
;;; ============================================================
;;;
;;; Parameter structure:
;;;   make-regression-params, regression-params?, regression-params-coefficients
;;;
;;; Parameter optics:
;;;   params-coef-lens, params-coef-i-lens, params-coef-each
;;;
;;; Traced loss functions:
;;;   traced-sse, traced-ridge-penalty, traced-ridge-loss, traced-mse
;;;
;;; Gradient computation:
;;;   ridge-gradient, mse-gradient, coefficient-sensitivity
;;;
;;; Optimization:
;;;   ridge-gd-step, ridge-gd, mse-gd-step, mse-gd
;;;
;;; Utilities:
;;;   compute-sse, compute-ridge-loss, init-params, init-params-random
;;;
;;; Model result optics:
;;;   linear-model-coef-lens, linear-model-coef-i-lens
;;;   linear-model-residuals-lens, linear-model-r2-getter
