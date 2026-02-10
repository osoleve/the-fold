;;; lattice/statistics/core/model-protocol.ss — Unified Statistical Model Protocols
;;;
;;; Defines a protocol-based interface for statistical models, enabling:
;;; - Polymorphic prediction across model types
;;; - Unified diagnostics and summary statistics
;;; - Cross-validation and model comparison pipelines

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module model-protocol
;;; @requires prelude protocol result-types sort
(require 'prelude)
(require 'protocol)
(require 'result-types)
(require 'sort)

(doc 'module 'model-protocol)
(doc 'description "Unified protocol interface for statistical models")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================================
;;; Core Model Protocols
;;; ============================================================================
;;;
;;; Every statistical model implements these protocols for a unified interface.

(doc 'section 'coefficients-protocol)

;;; model-coefficients : Model → Vec
;;; Extract fitted coefficients from any model type.
(define-protocol (model-coefficients model)
  "Extract fitted coefficients from a statistical model")

;;; model-intercept : Model → Num | #f
;;; Get intercept term if the model has one, #f otherwise.
(define-protocol/default (model-intercept model)
  (lambda (m) #f))

(doc 'section 'prediction-protocol)

;;; model-predict : Model × Matrix → Vec
;;; Generate predictions from fitted model.
;;; For regression models: X-new design matrix → predicted y
;;; For time series: X-new is the original series, returns next-step predictions
(define-protocol (model-predict model X-new)
  "Generate predictions from a fitted model")

(doc 'section 'residuals-protocol)

;;; model-residuals : Model → Vec
;;; Get model residuals (y - fitted).
(define-protocol (model-residuals model)
  "Get model residuals")

;;; model-fitted : Model → Vec
;;; Get fitted values from model.
(define-protocol (model-fitted model)
  "Get fitted values from model")

(doc 'section 'diagnostics-protocol)

;;; model-n : Model → Nat
;;; Number of observations used in fitting.
(define-protocol (model-n model)
  "Get number of observations")

;;; model-p : Model → Nat
;;; Number of parameters (including intercept if present).
(define-protocol (model-p model)
  "Get number of parameters")

;;; model-df-residual : Model → Nat
;;; Residual degrees of freedom (n - p).
(define-protocol (model-df-residual model)
  "Get residual degrees of freedom")

;;; model-sigma : Model → Num
;;; Residual standard error.
(define-protocol (model-sigma model)
  "Get residual standard error")

(doc 'section 'information-criteria)

;;; model-aic : Model → Num
;;; Akaike Information Criterion (lower is better).
;;; AIC = -2*log(L) + 2*k where k = number of parameters
(define-protocol/default (model-aic model)
  (lambda (m)
    ;; Default: compute from sigma if available
    (let* ([n (model-n m)]
           [p (model-p m)]
           [sigma (model-sigma m)])
      (+ (* n (log (* 2 3.14159265 sigma sigma)))
         n
         (* 2 (+ p 1))))))

;;; model-bic : Model → Num
;;; Bayesian Information Criterion (lower is better).
;;; BIC = -2*log(L) + log(n)*k
(define-protocol/default (model-bic model)
  (lambda (m)
    (let* ([n (model-n m)]
           [p (model-p m)]
           [sigma (model-sigma m)])
      (+ (* n (log (* 2 3.14159265 sigma sigma)))
         n
         (* (log n) (+ p 1))))))

(doc 'section 'goodness-of-fit)

;;; model-r-squared : Model → Num | #f
;;; Coefficient of determination (R²). Returns #f for models where R² is not meaningful.
(define-protocol/default (model-r-squared model)
  (lambda (m) #f))

;;; model-deviance : Model → Num | #f
;;; Residual deviance (for GLMs). Returns #f for non-GLM models.
(define-protocol/default (model-deviance model)
  (lambda (m) #f))

(doc 'section 'standard-errors)

;;; model-se : Model → Vec | #f
;;; Standard errors of coefficients. Returns #f if not available (e.g., lasso).
(define-protocol/default (model-se model)
  (lambda (m) #f))

;;; ============================================================================
;;; Implementations for Linear Models
;;; ============================================================================

(doc 'section 'linear-model-implementation)

(implement-protocol! 'model-coefficients 'linear-model
  (lambda (m) (linear-model-coefficients m)))

(implement-protocol! 'model-predict 'linear-model
  (lambda (m X-new)
    (matrix-vec-mul X-new (linear-model-coefficients m))))

(implement-protocol! 'model-residuals 'linear-model
  (lambda (m) (linear-model-residuals m)))

(implement-protocol! 'model-fitted 'linear-model
  (lambda (m) (linear-model-fitted m)))

(implement-protocol! 'model-n 'linear-model
  (lambda (m) (linear-model-n m)))

(implement-protocol! 'model-p 'linear-model
  (lambda (m) (linear-model-p m)))

(implement-protocol! 'model-df-residual 'linear-model
  (lambda (m) (linear-model-df-residual m)))

(implement-protocol! 'model-sigma 'linear-model
  (lambda (m) (linear-model-sigma m)))

(implement-protocol! 'model-r-squared 'linear-model
  (lambda (m) (linear-model-r-squared m)))

(implement-protocol! 'model-se 'linear-model
  (lambda (m) (linear-model-se m)))

(implement-protocol! 'model-aic 'linear-model
  (lambda (m)
    (let* ([n (linear-model-n m)]
           [p (linear-model-p m)]
           [sse (linear-model-sse m)]
           [sigma-sq (/ sse n)])
      ;; AIC = n*log(sigma²) + 2*(p+1)
      (+ (* n (log sigma-sq))
         (* 2 (+ p 1))))))

(implement-protocol! 'model-bic 'linear-model
  (lambda (m)
    (let* ([n (linear-model-n m)]
           [p (linear-model-p m)]
           [sse (linear-model-sse m)]
           [sigma-sq (/ sse n)])
      ;; BIC = n*log(sigma²) + log(n)*(p+1)
      (+ (* n (log sigma-sq))
         (* (log n) (+ p 1))))))

;;; ============================================================================
;;; Implementations for GLM Models
;;; ============================================================================

(doc 'section 'glm-model-implementation)

(implement-protocol! 'model-coefficients 'glm-model
  (lambda (m) (glm-coefficients m)))

(implement-protocol! 'model-predict 'glm-model
  (lambda (m X-new)
    ;; GLM prediction requires link function inverse
    ;; We store the link name, so we need to reconstruct
    (let* ([beta (glm-coefficients m)]
           [eta (matrix-vec-mul X-new beta)]
           [link-name (glm-link m)]
           [n (vector-length eta)]
           [mu (make-vector n)])
      ;; Apply inverse link
      (do ([i 0 (+ i 1)])
          [(= i n) mu]
        (vector-set! mu i
          (case link-name
            [(identity) (vector-ref eta i)]
            [(logit) (/ 1 (+ 1 (exp (- (vector-ref eta i)))))]
            [(log) (exp (vector-ref eta i))]
            [(probit) (probit-cdf (vector-ref eta i))]
            [else (vector-ref eta i)]))))))

(implement-protocol! 'model-residuals 'glm-model
  (lambda (m) (glm-residuals m)))

(implement-protocol! 'model-fitted 'glm-model
  (lambda (m) (glm-fitted m)))

(implement-protocol! 'model-n 'glm-model
  (lambda (m) (glm-n m)))

(implement-protocol! 'model-p 'glm-model
  (lambda (m) (vector-length (glm-coefficients m))))

(implement-protocol! 'model-df-residual 'glm-model
  (lambda (m) (glm-df-residual m)))

(implement-protocol! 'model-sigma 'glm-model
  (lambda (m)
    ;; For GLMs, sigma is typically 1 for binomial/poisson, estimated for gaussian/gamma
    ;; Approximate from deviance
    (sqrt (/ (glm-deviance m) (glm-df-residual m)))))

(implement-protocol! 'model-deviance 'glm-model
  (lambda (m) (glm-deviance m)))

(implement-protocol! 'model-se 'glm-model
  (lambda (m) (glm-se m)))

(implement-protocol! 'model-aic 'glm-model
  (lambda (m) (glm-aic m)))

(implement-protocol! 'model-bic 'glm-model
  (lambda (m)
    (let* ([aic (glm-aic m)]
           [n (glm-n m)]
           [p (vector-length (glm-coefficients m))])
      ;; BIC = AIC - 2*p + log(n)*p
      (+ aic
         (* -2 p)
         (* (log n) p)))))

;;; ============================================================================
;;; Implementations for AR Models
;;; ============================================================================

(doc 'section 'ar-model-implementation)

(implement-protocol! 'model-coefficients 'ar-result
  (lambda (m) (ar-coefficients m)))

(implement-protocol! 'model-intercept 'ar-result
  (lambda (m) (ar-intercept m)))

(implement-protocol! 'model-predict 'ar-result
  (lambda (m xs)
    ;; For AR models, "prediction" is one-step-ahead forecast
    ;; xs should be the original time series
    (let* ([phi (ar-coefficients m)]
           [mean (ar-intercept m)]
           [p (ar-order m)]
           [n (vector-length xs)]
           [predictions (make-vector n 0)])
      ;; First p values are just the original data
      (do ([t 0 (+ t 1)])
          [(= t p)]
        (vector-set! predictions t (vector-ref xs t)))
      ;; Remaining values are AR predictions
      (do ([t p (+ t 1)])
          [(= t n) predictions]
        (let ([pred (+ mean
                       (let loop ([k 0] [s 0])
                         (if (= k p)
                             s
                             (loop (+ k 1)
                                   (+ s (* (vector-ref phi k)
                                           (- (vector-ref xs (- t k 1)) mean)))))))])
          (vector-set! predictions t pred))))))

(implement-protocol! 'model-residuals 'ar-result
  (lambda (m) (ar-residuals m)))

(implement-protocol! 'model-fitted 'ar-result
  (lambda (m) (ar-fitted m)))

(implement-protocol! 'model-n 'ar-result
  (lambda (m) (vector-length (ar-fitted m))))

(implement-protocol! 'model-p 'ar-result
  (lambda (m) (+ (ar-order m) 1)))  ; AR coefficients + intercept

(implement-protocol! 'model-df-residual 'ar-result
  (lambda (m)
    (- (vector-length (ar-fitted m))
       (+ (ar-order m) 1))))

(implement-protocol! 'model-sigma 'ar-result
  (lambda (m) (ar-sigma m)))

(implement-protocol! 'model-aic 'ar-result
  (lambda (m) (ar-aic m)))

;;; ============================================================================
;;; Derived Operations (work with any model via protocols)
;;; ============================================================================

(doc 'section 'derived-operations)

;;; model-mse : Model → Num
;;; Mean squared error of residuals.
(define (model-mse model)
  (doc 'export #t)
  (doc 'type '(-> Model Num))
  (doc 'description "Compute mean squared error from model residuals")
  (let* ([resid (model-residuals model)]
         [n (vector-length resid)])
    (/ (let loop ([i 0] [s 0])
         (if (= i n)
             s
             (loop (+ i 1)
                   (+ s (expt (vector-ref resid i) 2)))))
       n)))

;;; model-rmse : Model → Num
;;; Root mean squared error.
(define (model-rmse model)
  (doc 'export #t)
  (doc 'type '(-> Model Num))
  (doc 'description "Compute root mean squared error")
  (sqrt (model-mse model)))

;;; model-mae : Model → Num
;;; Mean absolute error of residuals.
(define (model-mae model)
  (doc 'export #t)
  (doc 'type '(-> Model Num))
  (doc 'description "Compute mean absolute error from model residuals")
  (let* ([resid (model-residuals model)]
         [n (vector-length resid)])
    (/ (let loop ([i 0] [s 0])
         (if (= i n)
             s
             (loop (+ i 1)
                   (+ s (abs (vector-ref resid i))))))
       n)))

;;; compare-models-aic : (List Model) → (List (Pair Model Num))
;;; Compare models by AIC, returning sorted list with delta-AIC.
(define (compare-models-aic models)
  (doc 'export #t)
  (doc 'type '(-> (List Model) (List (Pair Model Num))))
  (doc 'description "Compare models by AIC, return sorted by delta-AIC from best")
  (let* ([aics (map (lambda (m) (cons m (model-aic m))) models)]
         [sorted (sort-by (lambda (a b) (< (cdr a) (cdr b))) aics)]
         [min-aic (cdr (car sorted))])
    (map (lambda (pair)
           (cons (car pair) (- (cdr pair) min-aic)))
         sorted)))

;;; compare-models-bic : (List Model) → (List (Pair Model Num))
;;; Compare models by BIC.
(define (compare-models-bic models)
  (doc 'export #t)
  (doc 'type '(-> (List Model) (List (Pair Model Num))))
  (doc 'description "Compare models by BIC, return sorted by delta-BIC from best")
  (let* ([bics (map (lambda (m) (cons m (model-bic m))) models)]
         [sorted (sort-by (lambda (a b) (< (cdr a) (cdr b))) bics)]
         [min-bic (cdr (car sorted))])
    (map (lambda (pair)
           (cons (car pair) (- (cdr pair) min-bic)))
         sorted)))

;;; model-summary : Model → Assoc
;;; Generate a summary association list for any model type.
(define (model-summary model)
  (doc 'export #t)
  (doc 'type '(-> Model Assoc))
  (doc 'description "Generate unified summary for any statistical model")
  (let* ([type-tag (car model)]
         [base-summary
          `((type . ,type-tag)
            (n . ,(model-n model))
            (p . ,(model-p model))
            (df-residual . ,(model-df-residual model))
            (sigma . ,(model-sigma model))
            (mse . ,(model-mse model))
            (rmse . ,(model-rmse model))
            (aic . ,(model-aic model))
            (bic . ,(model-bic model)))])
    ;; Add type-specific metrics
    (let ([r2 (model-r-squared model)]
          [dev (model-deviance model)])
      (append base-summary
              (if r2 `((r-squared . ,r2)) '())
              (if dev `((deviance . ,dev)) '())))))

;;; ============================================================================
;;; Cross-Validation (Polymorphic)
;;; ============================================================================

(doc 'section 'cross-validation)

;;; The key insight: with protocols, we can write generic cross-validation
;;; that works with ANY model type, as long as we have a fit function.

;;; cv-score : FitFn × Matrix × Vec × Nat → Num
;;; K-fold cross-validation for any model type.
;;; fit-fn takes (X y) and returns a model.
(define (cv-score fit-fn X y k)
  (doc 'export #t)
  (doc 'type '(-> (-> Matrix Vec Model) Matrix Vec Nat Num))
  (doc 'description "K-fold cross-validation returning mean MSE")
  (doc 'param 'fit-fn "Function (X y) → Model that fits the model")
  (doc 'param 'X "Design matrix")
  (doc 'param 'y "Response vector")
  (doc 'param 'k "Number of folds")
  (let* ([n (matrix-rows X)]
         [fold-size (quotient n k)]
         [total-error 0])
    (do ([fold 0 (+ fold 1)])
        [(= fold k) (/ total-error n)]
      (let* ([start (* fold fold-size)]
             [end (if (= fold (- k 1)) n (+ start fold-size))]
             [split (cv-split X y start end n)]
             [X-train (car split)]
             [y-train (cadr split)]
             [X-test (caddr split)]
             [y-test (cadddr split)]
             [model (fit-fn X-train y-train)])
        (if (and (pair? model) (eq? (car model) 'error))
            (set! total-error (+ total-error (* 1e10 (- end start))))
            (let* ([predictions (model-predict model X-test)]
                   [fold-mse (compute-mse predictions y-test)])
              (set! total-error (+ total-error (* fold-mse (- end start))))))))))

;;; cv-split : Matrix × Vec × Nat × Nat × Nat → (List Matrix Vec Matrix Vec)
;;; Split data into train (excluding fold) and test (fold only).
(define (cv-split X y start end n)
  (let* ([p (matrix-cols X)]
         [n-test (- end start)]
         [n-train (- n n-test)]
         [X-train-data (make-vector (* n-train p))]
         [X-test-data (make-vector (* n-test p))]
         [y-train (make-vector n-train)]
         [y-test (make-vector n-test)]
         [train-idx 0]
         [test-idx 0])
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (if (and (>= i start) (< i end))
          ;; Test set
          (begin
            (vector-set! y-test test-idx (vector-ref y i))
            (do ([j 0 (+ j 1)])
                [(= j p)]
              (vector-set! X-test-data (+ (* test-idx p) j)
                           (matrix-ref X i j)))
            (set! test-idx (+ test-idx 1)))
          ;; Train set
          (begin
            (vector-set! y-train train-idx (vector-ref y i))
            (do ([j 0 (+ j 1)])
                [(= j p)]
              (vector-set! X-train-data (+ (* train-idx p) j)
                           (matrix-ref X i j)))
            (set! train-idx (+ train-idx 1)))))
    (list (list 'matrix n-train p X-train-data)
          y-train
          (list 'matrix n-test p X-test-data)
          y-test)))

;;; compute-mse : Vec × Vec → Num
(define (compute-mse predictions actual)
  (let ([n (vector-length predictions)])
    (/ (let loop ([i 0] [s 0])
         (if (= i n)
             s
             (loop (+ i 1)
                   (+ s (expt (- (vector-ref predictions i)
                                 (vector-ref actual i))
                              2)))))
       n)))

;;; ============================================================================
;;; Helpers
;;; ============================================================================

;;; matrix-vec-mul : Matrix × Vec → Vec
;;; Matrix-vector multiplication (y = X * beta).
;;; Defined here to avoid circular dependencies.
(define (matrix-vec-mul M v)
  (let* ([m (cadr M)]
         [n (caddr M)]
         [data (cadddr M)]
         [result (make-vector m)])
    (do ([i 0 (+ i 1)])
        [(= i m) result]
      (let ([sum (let loop ([j 0] [s 0])
                   (if (= j n)
                       s
                       (loop (+ j 1)
                             (+ s (* (vector-ref data (+ (* i n) j))
                                     (vector-ref v j))))))])
        (vector-set! result i sum)))))

;;; matrix-ref : Matrix × Nat × Nat → Num
(define (matrix-ref M i j)
  (let* ([n (caddr M)]
         [data (cadddr M)])
    (vector-ref data (+ (* i n) j))))

;;; matrix-rows : Matrix → Nat
(define (matrix-rows M) (cadr M))

;;; matrix-cols : Matrix → Nat
(define (matrix-cols M) (caddr M))

;;; probit-cdf : Num → Num
;;; Standard normal CDF approximation.
(define (probit-cdf x)
  (/ (+ 1 (erf (/ x (sqrt 2)))) 2))

;;; erf : Num → Num
;;; Error function approximation.
(define (erf x)
  (let* ([a1 0.254829592]
         [a2 -0.284496736]
         [a3 1.421413741]
         [a4 -1.453152027]
         [a5 1.061405429]
         [p 0.3275911]
         [sign (if (< x 0) -1 1)]
         [x (abs x)]
         [t (/ 1 (+ 1 (* p x)))]
         [y (- 1 (* (+ (* (+ (* (+ (* (+ (* a5 t) a4) t) a3) t) a2) t) a1)
                    t (exp (- (* x x)))))])
    (* sign y)))

;;; ============================================================================
;;; Exports Summary
;;; ============================================================================

(doc 'section 'exports-summary)
(doc 'exports "
Core Protocols (work with any model type):
  model-coefficients    Get fitted coefficients
  model-intercept       Get intercept (if any)
  model-predict         Generate predictions
  model-residuals       Get residuals
  model-fitted          Get fitted values
  model-n               Number of observations
  model-p               Number of parameters
  model-df-residual     Residual degrees of freedom
  model-sigma           Residual standard error
  model-aic             Akaike Information Criterion
  model-bic             Bayesian Information Criterion
  model-r-squared       R² (if meaningful)
  model-deviance        Deviance (for GLMs)
  model-se              Standard errors (if available)

Derived Operations:
  model-mse             Mean squared error
  model-rmse            Root mean squared error
  model-mae             Mean absolute error
  model-summary         Unified summary association list
  compare-models-aic    Compare models by AIC
  compare-models-bic    Compare models by BIC

Cross-Validation:
  cv-score              K-fold CV for any model type
")

(display "  Use (model-predict m X) for any model type\n")
(display "  Use (model-summary m) for unified diagnostics\n")
(display "  Use (cv-score fit-fn X y k) for cross-validation\n")
