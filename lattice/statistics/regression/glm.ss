;;; lattice/statistics/regression/glm.ss — Generalized Linear Models
;;;
;;; GLM fitting via Iteratively Reweighted Least Squares (IRLS).
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - glm-fit: Fit GLM with specified family and link
;;;   - logistic-fit: Binary logistic regression (wrapper)
;;;   - poisson-fit: Poisson regression (wrapper)
;;;   - glm-predict: Predictions from fitted model
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/matrix.ss
;;;   - linalg/matrix-solvers.ss
;;;   - statistics/core/result-types.ss
;;;   - statistics/regression/families.ss
;;;   - statistics/regression/link-functions.ss
;;;   - statistics/hypothesis/distributions.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/statistics/core/result-types.ss")
(load "lattice/statistics/regression/families.ss")
(load "lattice/statistics/regression/link-functions.ss")
(load "lattice/statistics/hypothesis/distributions.ss")

;;; ============================================================
;;; IRLS Algorithm
;;; ============================================================

;;; The IRLS algorithm iteratively solves weighted least squares:
;;;
;;; 1. Initialize: eta = g(mu_0) where mu_0 = initialize(y)
;;; 2. Iterate until convergence:
;;;    a. mu = g^(-1)(eta)
;;;    b. V = variance(mu) from family
;;;    c. W = 1 / (V * g'(mu)²) (working weights)
;;;    d. z = eta + (y - mu) * g'(mu) (working response)
;;;    e. Solve WLS: beta = (X'WX)^(-1) X'Wz
;;;    f. eta = X * beta
;;; 3. Compute standard errors from (X'WX)^(-1)

;;; glm-fit : GLMFamily × LinkFunction × Matrix × Vec × Nat × Num → GLMResult | Error
;;; Fit a GLM using IRLS.
;;;   family: GLM family (gaussian, binomial, poisson, gamma)
;;;   link: link function
;;;   X: design matrix (should include intercept if desired)
;;;   y: response vector
;;;   max-iter: maximum IRLS iterations (default 25)
;;;   tol: convergence tolerance (default 1e-8)
(define (glm-fit family link X y . opts)
  (let* ([max-iter (if (>= (length opts) 1) (car opts) 25)]
         [tol (if (>= (length opts) 2) (cadr opts) 1e-8)]
         [n (matrix-rows X)]
         [p (matrix-cols X)]
         [init-fn (family-initialize family)]
         [var-fn (family-variance family)]
         [g-inv (link-inverse link)]
         [g-deriv (link-derivative link)]
         [g (link-g link)])
        ;; Check dimensions
        (if (not (= n (vector-length y)))
            (list 'error 'dimension-mismatch n (vector-length y))
            ;; Initialize mu and eta
            (let* ([mu-init (make-vector n)]
                   [eta-init (make-vector n)])
                  ;; Initialize mu from y
                  (do ([i 0 (+ i 1)])
                      [(= i n)]
                      (let ([mu-i (init-fn (vector-ref y i))])
                           (vector-set! mu-init i mu-i)
                           (vector-set! eta-init i (g mu-i))))
                  ;; Run IRLS
                  (irls-loop family link X y mu-init eta-init
                             0 max-iter tol n p)))))

;;; irls-loop : ... → GLMResult | Error
;;; Main IRLS iteration loop.
(define (irls-loop family link X y mu eta iter max-iter tol n p)
  (if (>= iter max-iter)
      ;; Max iterations reached
      (irls-finalize family link X y mu eta iter #f n p)
      ;; Perform one IRLS step
      (let* ([result (irls-step family link X y mu eta n p)])
            (if (and (pair? result) (eq? (car result) 'error))
                result
                (let* ([beta-new (car result)]
                       [eta-new (cadr result)]
                       [mu-new (caddr result)]
                       ;; Check convergence: max|eta_new - eta| < tol
                       [converged? (irls-converged? eta eta-new tol)])
                      (if converged?
                          (irls-finalize family link X y mu-new eta-new iter #t n p)
                          (irls-loop family link X y mu-new eta-new
                                     (+ iter 1) max-iter tol n p)))))))

;;; irls-step : GLMFamily × LinkFunction × Matrix × Vec × Vec × Vec × Nat × Nat → (List Vec Vec Vec) | Error
;;; Perform one IRLS step.
(define (irls-step family link X y mu eta n p)
  (let* ([var-fn (family-variance family)]
         [g-inv (link-inverse link)]
         [g-deriv (link-derivative link)]
         ;; Compute working weights and response
         [W (make-vector n)]
         [z (make-vector n)])
        ;; W_i = 1 / (V(mu_i) * g'(mu_i)²)
        ;; z_i = eta_i + (y_i - mu_i) * g'(mu_i)
        (do ([i 0 (+ i 1)])
            [(= i n)]
            (let* ([mu-i (vector-ref mu i)]
                   [eta-i (vector-ref eta i)]
                   [y-i (vector-ref y i)]
                   [V-i (var-fn mu-i)]
                   [g-prime-i (g-deriv mu-i)]
                   [g-prime-sq (* g-prime-i g-prime-i)]
                   [w-i (/ 1 (max (* V-i g-prime-sq) 1e-10))]
                   [z-i (+ eta-i (* (- y-i mu-i) g-prime-i))])
                  (vector-set! W i w-i)
                  (vector-set! z i z-i)))
        ;; Solve weighted least squares: beta = (X'WX)^(-1) X'Wz
        (let ([beta (weighted-least-squares X z W n p)])
             (if (and (pair? beta) (eq? (car beta) 'error))
                 beta
                 ;; Compute new eta and mu
                 (let* ([eta-new (matrix-vec-mul X beta)]
                        [mu-new (make-vector n)])
                       (do ([i 0 (+ i 1)])
                           [(= i n)]
                           (vector-set! mu-new i (g-inv (vector-ref eta-new i))))
                       (list beta eta-new mu-new))))))

;;; weighted-least-squares : Matrix × Vec × Vec × Nat × Nat → Vec | Error
;;; Solve (X'WX) beta = X'Wz.
(define (weighted-least-squares X z W n p)
  (let* ([XtWX-data (make-vector (* p p) 0)]
         [XtWX (list 'matrix p p XtWX-data)]
         [XtWz (make-vector p 0)])
        ;; Compute X'WX and X'Wz
        (do ([j 0 (+ j 1)])
            [(= j p)]
            ;; X'Wz[j] = sum_i(W_i * X[i,j] * z_i)
            (let ([sum-z (let loop ([i 0] [s 0])
                              (if (= i n)
                                  s
                                  (loop (+ i 1)
                                        (+ s (* (vector-ref W i)
                                                (matrix-ref X i j)
                                                (vector-ref z i))))))])
                 (vector-set! XtWz j sum-z))
            ;; X'WX[j,k] = sum_i(W_i * X[i,j] * X[i,k])
            (do ([k 0 (+ k 1)])
                [(= k p)]
                (let ([sum-xx (let loop ([i 0] [s 0])
                                   (if (= i n)
                                       s
                                       (loop (+ i 1)
                                             (+ s (* (vector-ref W i)
                                                     (matrix-ref X i j)
                                                     (matrix-ref X i k))))))])
                     (vector-set! XtWX-data (+ (* j p) k) sum-xx))))
        ;; Solve XtWX * beta = XtWz
        (matrix-solve XtWX XtWz)))

;;; irls-converged? : Vec × Vec × Num → Bool
;;; Check if IRLS has converged.
(define (irls-converged? eta-old eta-new tol)
  (let ([n (vector-length eta-old)])
       (let loop ([i 0] [max-diff 0])
            (if (= i n)
                (< max-diff tol)
                (loop (+ i 1)
                      (max max-diff
                           (abs (- (vector-ref eta-new i)
                                   (vector-ref eta-old i)))))))))

;;; irls-finalize : ... → GLMResult
;;; Compute final statistics after IRLS convergence.
(define (irls-finalize family link X y mu eta iter converged? n p)
  (let* ([var-fn (family-variance family)]
         [g-deriv (link-derivative link)]
         ;; Compute beta from final eta
         [beta (matrix-least-squares X eta)]
         ;; Compute final working weights
         [W (make-vector n)]
         [_ (do ([i 0 (+ i 1)])
                [(= i n)]
                (let* ([mu-i (vector-ref mu i)]
                       [V-i (var-fn mu-i)]
                       [g-prime-i (g-deriv mu-i)]
                       [g-prime-sq (* g-prime-i g-prime-i)]
                       [w-i (/ 1 (max (* V-i g-prime-sq) 1e-10))])
                      (vector-set! W i w-i)))]
         ;; Standard errors from (X'WX)^(-1)
         [se (compute-glm-se X W p)]
         ;; Z-values and p-values
         [z-values (if (or (not se) (and (pair? se) (eq? (car se) 'error)))
                       (make-vector p 0)
                       (compute-z-values beta se))]
         [p-values (compute-z-pvalues z-values)]
         ;; Deviance
         [deviance (compute-deviance family y mu)]
         [null-deviance (compute-null-deviance family y)]
         ;; AIC
         [aic (compute-aic deviance n p)]
         ;; Deviance residuals
         [residuals (compute-deviance-residuals family y mu)])
        (make-glm-result
         (family-name family) (link-name link)
         beta se z-values p-values
         deviance null-deviance aic
         residuals mu n iter converged?)))

;;; compute-glm-se : Matrix × Vec × Nat → Vec | Error
;;; Standard errors from (X'WX)^(-1).
(define (compute-glm-se X W p)
  (let* ([n (matrix-rows X)]
         [XtWX-data (make-vector (* p p) 0)]
         [XtWX (list 'matrix p p XtWX-data)])
        ;; Compute X'WX
        (do ([j 0 (+ j 1)])
            [(= j p)]
            (do ([k 0 (+ k 1)])
                [(= k p)]
                (let ([sum (let loop ([i 0] [s 0])
                                (if (= i n)
                                    s
                                    (loop (+ i 1)
                                          (+ s (* (vector-ref W i)
                                                  (matrix-ref X i j)
                                                  (matrix-ref X i k))))))])
                     (vector-set! XtWX-data (+ (* j p) k) sum))))
        (let ([XtWX-inv (matrix-inverse XtWX)])
             (if (and (pair? XtWX-inv) (eq? (car XtWX-inv) 'error))
                 XtWX-inv
                 (let ([se (make-vector p)])
                      (do ([j 0 (+ j 1)])
                          [(= j p) se]
                          (vector-set! se j (sqrt (max (matrix-ref XtWX-inv j j) 0)))))))))

;;; compute-z-values : Vec × Vec → Vec
(define (compute-z-values beta se)
  (let* ([p (vector-length beta)]
         [z (make-vector p)])
        (do ([j 0 (+ j 1)])
            [(= j p) z]
            (let ([sej (vector-ref se j)])
                 (vector-set! z j (if (= sej 0) 0 (/ (vector-ref beta j) sej)))))))

;;; compute-z-pvalues : Vec → Vec
;;; Two-tailed p-values from standard normal.
(define (compute-z-pvalues z-values)
  (let* ([p (vector-length z-values)]
         [pv (make-vector p)])
        (do ([j 0 (+ j 1)])
            [(= j p) pv]
            (let* ([z (abs (vector-ref z-values j))]
                   [pval (* 2 (- 1 (probit-cdf z)))])
                  (vector-set! pv j pval)))))

;;; compute-deviance-residuals : GLMFamily × Vec × Vec → Vec
;;; Signed deviance residuals.
(define (compute-deviance-residuals family y mu)
  (let* ([n (vector-length y)]
         [dev-fn (family-deviance-contrib family)]
         [r (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) r]
            (let* ([yi (vector-ref y i)]
                   [mui (vector-ref mu i)]
                   [d (dev-fn yi mui)]
                   [sign (if (>= (- yi mui) 0) 1 -1)])
                  (vector-set! r i (* sign (sqrt (max d 0))))))))

;;; ============================================================
;;; Convenience Wrappers
;;; ============================================================

;;; logistic-fit : Matrix × Vec → GLMResult | Error
;;; Fit binary logistic regression.
;;; y should be 0/1 binary response.
(define (logistic-fit X y . opts)
  (apply glm-fit binomial-family logit-link X y opts))

;;; poisson-fit : Matrix × Vec → GLMResult | Error
;;; Fit Poisson regression.
;;; y should be non-negative counts.
(define (poisson-fit X y . opts)
  (apply glm-fit poisson-family log-link X y opts))

;;; gamma-fit : Matrix × Vec → GLMResult | Error
;;; Fit Gamma regression with log link.
;;; y should be positive continuous.
(define (gamma-fit X y . opts)
  (apply glm-fit gamma-family log-link X y opts))

;;; probit-fit : Matrix × Vec → GLMResult | Error
;;; Fit probit regression (binomial with probit link).
(define (probit-fit X y . opts)
  (apply glm-fit binomial-family probit-link X y opts))

;;; ============================================================
;;; Prediction
;;; ============================================================

;;; glm-predict : GLMResult × Matrix → Vec
;;; Predict on response scale.
(define (glm-predict model X-new)
  (let* ([beta (glm-coefficients model)]
         [link-name (glm-link model)]
         [link (case link-name
                     [(identity) identity-link]
                     [(logit) logit-link]
                     [(log) log-link]
                     [(probit) probit-link]
                     [else identity-link])]
         [g-inv (link-inverse link)]
         [eta (matrix-vec-mul X-new beta)]
         [n (vector-length eta)]
         [mu (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) mu]
            (vector-set! mu i (g-inv (vector-ref eta i))))))

;;; glm-predict-linear : GLMResult × Matrix → Vec
;;; Predict on linear predictor scale (eta).
(define (glm-predict-linear model X-new)
  (matrix-vec-mul X-new (glm-coefficients model)))

;;; logistic-classify : GLMResult × Matrix × Num → Vec
;;; Classify binary outcomes using threshold.
(define (logistic-classify model X-new threshold)
  (let* ([probs (glm-predict model X-new)]
         [n (vector-length probs)]
         [classes (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n) classes]
            (vector-set! classes i (if (>= (vector-ref probs i) threshold) 1 0)))))

;;; ============================================================
;;; Model Comparison
;;; ============================================================

;;; glm-likelihood-ratio-test : GLMResult × GLMResult → TestResult
;;; Likelihood ratio test comparing two nested models.
;;; null-model should be nested within alt-model.
(define (glm-likelihood-ratio-test null-model alt-model)
  (let* ([dev-null (glm-deviance null-model)]
         [dev-alt (glm-deviance alt-model)]
         [df-null (- (glm-n null-model) (vector-length (glm-coefficients null-model)))]
         [df-alt (- (glm-n alt-model) (vector-length (glm-coefficients alt-model)))]
         [chi-sq (- dev-null dev-alt)]
         [df (- df-null df-alt)]
         [p-value (chi-squared-pvalue chi-sq df)])
        (make-test-result 'likelihood-ratio chi-sq df p-value #f #f)))
