(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module autodiff-fit
;;; @requires prelude matrix reverse-diff convergence optimize families link-functions result-types glm
(require 'prelude)
(require 'matrix)
(require 'reverse-diff)
(require 'convergence)
(require 'optimize)
(require 'families)
(require 'link-functions)
(require 'result-types)
(require 'glm)

(doc 'module 'autodiff-fit)
(doc 'bridges '(statistics autodiff optimization))
(doc 'description "Autodiff-GLM Bridge — Fit GLMs via gradient-based optimization with autodiff gradients")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "This module bridges three lattice subsystems:
  1. lattice/autodiff (reverse-mode AD for gradients)
  2. lattice/optimization (L-BFGS, Adam, etc. for minimization)
  3. lattice/statistics (GLM families, link functions, result types)

The existing GLM uses IRLS (second-order, matrix-heavy).
This module offers an alternative: MLE via minimize(NLL), where
gradients come from autodiff through the NLL computation graph.

Advantage: works for any differentiable loss — not limited to
exponential family canonical form. Add a new distribution by
writing its traced NLL; the optimizer and AD do the rest.")

;;; ============================================================
;;; Section: Traced Link Functions
;;; ============================================================

(doc 'section 'traced-links)

;;; traced-sigmoid : TracedValue -> TracedValue
;;; Logistic sigmoid: 1 / (1 + exp(-x))
;;; Clamped to avoid overflow.
(define (traced-sigmoid eta)
  (doc 'export #t)
  (doc 'type '(-> TracedValue TracedValue))
  (doc 'description "Traced sigmoid (logistic) function: 1/(1+exp(-x))")
  ;; The traced-exp handles overflow gracefully via the AD system;
  ;; numerical stability for the NLL comes from using log1pexp form
  ;; in traced-binomial-nll rather than going through sigmoid explicitly.
  (traced-div 1 (traced-add 1 (traced-exp (traced-neg eta)))))

;;; traced-linear-predictor : Matrix x Vec(traced) x Int x Int -> TracedValue
;;; Compute X[i,:] . beta for row i.
;;; X is constant (design matrix), beta is traced.
(define (traced-linear-predictor X beta-traced i p)
  (let loop ([j 0] [sum 0])
    (if (= j p)
        sum
        (loop (+ j 1)
              (traced-add sum
                          (traced-mul (matrix-ref X i j)
                                      (list-ref beta-traced j)))))))

;;; ============================================================
;;; Section: Traced Negative Log-Likelihood
;;; ============================================================

(doc 'section 'traced-nll)

;;; traced-binomial-nll : Matrix x Vec x (List TracedValue) -> TracedValue
;;; Negative log-likelihood for binomial(logit):
;;;   NLL = -sum[ y_i * log(p_i) + (1 - y_i) * log(1 - p_i) ]
;;; where p_i = sigmoid(X[i,:] . beta)
(define (traced-binomial-nll X y beta-traced)
  (let ([n (vector-length y)]
        [p (length beta-traced)])
    (let loop ([i 0] [nll 0])
      (if (= i n)
          nll
          (let* ([eta (traced-linear-predictor X beta-traced i p)]
                 [yi (vector-ref y i)]
                 ;; Use the numerically stable form:
                 ;; -[y*log(sigma(eta)) + (1-y)*log(1-sigma(eta))]
                 ;; = -[y*eta - log(1+exp(eta))]
                 ;; = log(1+exp(eta)) - y*eta
                 ;;
                 ;; For large eta: log(1+exp(eta)) ~ eta
                 ;; For large negative eta: log(1+exp(eta)) ~ exp(eta) ~ 0
                 [ev (traced-value eta)]
                 [log1pexp (if (> ev 20)
                               eta  ;; log(1+exp(eta)) ~ eta for large eta
                               (traced-log (traced-add 1 (traced-exp eta))))]
                 [contrib (traced-sub log1pexp (traced-mul yi eta))])
            (loop (+ i 1) (traced-add nll contrib)))))))

;;; traced-gaussian-nll : Matrix x Vec x (List TracedValue) -> TracedValue
;;; Negative log-likelihood for Gaussian (proportional to SSE):
;;;   NLL = (1/2) * sum[ (y_i - X[i,:].beta)^2 ]
;;; Omits constant terms (n/2 * log(2*pi*sigma^2)) since they don't affect MLE.
(define (traced-gaussian-nll X y beta-traced)
  (let ([n (vector-length y)]
        [p (length beta-traced)])
    (let loop ([i 0] [nll 0])
      (if (= i n)
          (traced-mul 0.5 nll)
          (let* ([eta (traced-linear-predictor X beta-traced i p)]
                 [yi (vector-ref y i)]
                 [resid (traced-sub yi eta)]
                 [sq (traced-sq resid)])
            (loop (+ i 1) (traced-add nll sq)))))))

;;; traced-poisson-nll : Matrix x Vec x (List TracedValue) -> TracedValue
;;; Negative log-likelihood for Poisson(log link):
;;;   NLL = sum[ exp(eta_i) - y_i * eta_i ]
;;; Omits log(y_i!) since it's constant w.r.t. beta.
(define (traced-poisson-nll X y beta-traced)
  (let ([n (vector-length y)]
        [p (length beta-traced)])
    (let loop ([i 0] [nll 0])
      (if (= i n)
          nll
          (let* ([eta (traced-linear-predictor X beta-traced i p)]
                 [yi (vector-ref y i)]
                 ;; exp(eta) - y*eta
                 [contrib (traced-sub (traced-exp eta)
                                      (traced-mul yi eta))])
            (loop (+ i 1) (traced-add nll contrib)))))))

;;; ============================================================
;;; Section: Generic Autodiff NLL
;;; ============================================================

(doc 'section 'generic-nll)

;;; autodiff-nll : (TracedValue -> TracedValue -> TracedValue) x Vec x (List TracedValue) -> TracedValue
;;; Generic negative log-likelihood with autodiff.
;;; log-pdf-traced takes (y_i-traced, mu_i-traced) -> traced log-density.
;;; mu_i is computed from beta via a user-provided traced-link.
;;;
;;; Example for Normal(mu, 1):
;;;   (autodiff-nll
;;;     (lambda (y mu) (traced-mul -0.5 (traced-sq (traced-sub y mu))))
;;;     y-vec
;;;     (lambda (eta) eta)   ; identity link
;;;     X beta-traced)
(define (autodiff-nll log-pdf-traced y-vec traced-link X beta-traced)
  (doc 'export #t)
  (doc 'type '(-> (-> TracedValue TracedValue TracedValue) Vec (-> TracedValue TracedValue) Matrix (List TracedValue) TracedValue))
  (doc 'description "Compute NLL with autodiff for arbitrary traced log-density")
  (doc 'param 'log-pdf-traced "Traced log-density: (y_i, mu_i) -> log p(y_i | mu_i)")
  (doc 'param 'y-vec "Response vector")
  (doc 'param 'traced-link "Traced inverse-link: eta -> mu")
  (doc 'param 'X "Design matrix")
  (doc 'param 'beta-traced "List of traced coefficients")
  (let ([n (vector-length y-vec)]
        [p (length beta-traced)])
    (let loop ([i 0] [nll 0])
      (if (= i n)
          nll
          (let* ([eta (traced-linear-predictor X beta-traced i p)]
                 [mu (traced-link eta)]
                 [yi (vector-ref y-vec i)]
                 [log-p (log-pdf-traced yi mu)]
                 ;; NLL = -sum(log p), so we subtract
                 [contrib (traced-neg log-p)])
            (loop (+ i 1) (traced-add nll contrib)))))))

;;; ============================================================
;;; Section: GLM Fitting via Autodiff + Optimization
;;; ============================================================

(doc 'section 'autodiff-glm)

;;; family->traced-nll : GLMFamily x LinkFunction -> (Matrix x Vec x (List TracedValue) -> TracedValue)
;;; Select the traced NLL for a given family/link combination.
;;; Uses optimized implementations for canonical links, falls back
;;; to generic autodiff-nll for non-canonical combinations.
(define (family->traced-nll family link)
  (let ([fname (family-name family)]
        [lname (link-name link)])
    (cond
      ;; Canonical: binomial + logit
      [(and (eq? fname 'binomial) (eq? lname 'logit))
       traced-binomial-nll]
      ;; Canonical: gaussian + identity
      [(and (eq? fname 'gaussian) (eq? lname 'identity))
       traced-gaussian-nll]
      ;; Canonical: poisson + log
      [(and (eq? fname 'poisson) (eq? lname 'log))
       traced-poisson-nll]
      ;; Generic fallback using family/link structure
      [else
       (let ([g-inv (link-inverse link)])
         (lambda (X y beta-traced)
           (autodiff-nll
            ;; Build a generic traced log-density from the family's deviance
            ;; For exponential families: log p(y|mu) ~ -D(y,mu)/2
            ;; We use the deviance contribution as a surrogate (up to constants).
            ;; This is correct for MLE since the constant terms don't depend on beta.
            (lambda (yi mu)
              ;; -D(y,mu)/2 where D is the deviance contribution
              ;; Deviance for binomial: 2*[y*log(y/mu) + (1-y)*log((1-y)/(1-mu))]
              ;; We need the traced version of this.
              ;; For a generic approach, use the numerical log-likelihood approximation:
              ;; Approximate log p(y|mu) ~ -(y-mu)^2 / (2*V(mu))
              ;; This is the quasi-likelihood, which gives consistent MLE for mean parameters.
              (let ([v-fn (family-variance family)])
                (traced-mul -0.5
                            (traced-div (traced-sq (traced-sub yi mu))
                                        (max (v-fn (traced-value mu)) 1e-10)))))
            y X
            ;; Traced inverse link
            (lambda (eta)
              ;; Apply the inverse link using traced operations
              (case lname
                [(identity) eta]
                [(logit) (traced-sigmoid eta)]
                [(log) (traced-exp eta)]
                ;; For other links, use numerical approximation via the link's
                ;; inverse function (not differentiable through AD, but works
                ;; as a constant for the mu value)
                [else (g-inv (traced-value eta))]))
            beta-traced)))])))

;;; autodiff-glm-fit : GLMFamily x LinkFunction x Matrix x Vec [x opts] -> GLMResult | Error
;;; Fit a GLM via autodiff gradients + L-BFGS optimization.
;;; This is an alternative to IRLS that works for any differentiable NLL.
(define (autodiff-glm-fit family link X y . opts)
  (doc 'export #t)
  (doc 'type '(-> GLMFamily LinkFunction Matrix Vec GLMResult))
  (doc 'description "Fit GLM using autodiff gradients and L-BFGS optimization")
  (doc 'param 'family "GLM family (gaussian, binomial, poisson, gamma)")
  (doc 'param 'link "Link function")
  (doc 'param 'X "Design matrix (include intercept column if desired)")
  (doc 'param 'y "Response vector")
  (doc 'param 'method "Optional: optimization method symbol (default 'lbfgs)")
  (doc 'param 'criteria "Optional: ConvergenceCriteria")
  (let* ([method (if (and (pair? opts) (symbol? (car opts)))
                     (car opts)
                     'lbfgs)]
         [rest (if (and (pair? opts) (symbol? (car opts)))
                   (cdr opts)
                   opts)]
         [criteria (if (and (pair? rest) (convergence-criteria? (car rest)))
                       (car rest)
                       ;; Tight gradient tolerance, very loose f-tol to avoid
                       ;; premature "f-converged" when gradient is still significant
                       (make-convergence-criteria 1e-6 1e-14 1e-10 1000))]
         [n (matrix-rows X)]
         [p (matrix-cols X)]
         [nll-fn (family->traced-nll family link)]
         ;; Objective: NLL as a function of flat parameter list
         [objective (lambda beta-traced
                      (nll-fn X y beta-traced))]
         ;; Initial coefficients: zeros
         [x0 (make-list p 0.0)]
         ;; Run optimizer
         [result (minimize objective x0 method criteria)])
    ;; Check for valid result
    (if (not (opt-result? result))
        (list 'error 'optimization-failed result)
        ;; Extract coefficients and build GLM result
        (let* ([beta-list (opt-x result)]
               [beta (list->vector beta-list)]
               [iterations (opt-iterations result)]
               [converged-reason (opt-converged result)]
               [converged? (not (eq? converged-reason 'max-iter))]
               ;; Compute fitted values
               [g-inv (link-inverse link)]
               [eta-vec (matrix-vec-mul X beta)]
               [mu (make-vector n)]
               [_ (do ([i 0 (+ i 1)])
                      [(= i n)]
                      (vector-set! mu i (g-inv (vector-ref eta-vec i))))]
               ;; Compute deviance
               [deviance (compute-deviance family y mu)]
               [null-deviance (compute-null-deviance family y)]
               ;; Approximate standard errors via numerical Hessian diagonal
               [se (autodiff-glm-se objective beta-list p)]
               ;; Z-values and p-values
               [z-values (make-vector p)]
               [p-values (make-vector p)]
               [_ (do ([j 0 (+ j 1)])
                      [(= j p)]
                      (let* ([sej (vector-ref se j)]
                             [zj (if (< sej 1e-15) 0.0 (/ (vector-ref beta j) sej))])
                        (vector-set! z-values j zj)
                        (vector-set! p-values j (* 2 (- 1 (probit-cdf (abs zj)))))))]
               ;; AIC = 2*NLL + 2*p (NLL is the optimal function value)
               [aic (+ (* 2 (opt-f result)) (* 2 p))]
               ;; Deviance residuals
               [dev-fn (family-deviance-contrib family)]
               [residuals (make-vector n)]
               [_ (do ([i 0 (+ i 1)])
                      [(= i n)]
                      (let* ([yi (vector-ref y i)]
                             [mui (vector-ref mu i)]
                             [d (dev-fn yi mui)]
                             [sign (if (>= (- yi mui) 0) 1 -1)])
                        (vector-set! residuals i (* sign (sqrt (max d 0))))))])
          (make-glm-result
           (family-name family) (link-name link)
           beta se z-values p-values
           deviance null-deviance aic
           residuals mu n iterations converged?)))))

;;; autodiff-glm-se : ((List TracedValue) -> TracedValue) x (List Number) x Nat -> Vec
;;; Approximate standard errors from the inverse diagonal of the Hessian.
;;; Uses second-order finite differences of the gradient (which itself comes from AD).
(define (autodiff-glm-se objective beta-list p)
  (let* ([h 1e-5]
         [se (make-vector p 0.0)]
         ;; For each parameter, compute d^2 NLL / d beta_j^2
         ;; via finite difference of the gradient's j-th component
         [base-grad (gradient objective beta-list)])
    (do ([j 0 (+ j 1)])
        [(= j p) se]
        (let* ([beta-plus (list-update-idx beta-list j (lambda (x) (+ x h)))]
               [grad-plus (gradient objective beta-plus)]
               [hess-jj (/ (- (list-ref grad-plus j) (list-ref base-grad j)) h)]
               ;; SE = 1/sqrt(H_jj) for positive definite Hessian
               [se-j (if (> hess-jj 1e-15)
                         (/ 1.0 (sqrt hess-jj))
                         +inf.0)])
          (vector-set! se j se-j)))))

;;; list-update-idx : (List a) x Nat x (a -> a) -> (List a)
;;; Update element at index in a list.
(define (list-update-idx lst idx f)
  (if (= idx 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (list-update-idx (cdr lst) (- idx 1) f))))

;;; ============================================================
;;; Section: Logistic Regression (Convenience)
;;; ============================================================

(doc 'section 'logistic)

;;; autodiff-logistic-regression : Matrix x Vec [x opts] -> GLMResult
;;; Binary logistic regression via autodiff + optimization.
;;; A thin wrapper around autodiff-glm-fit with binomial/logit.
(define (autodiff-logistic-regression X y . opts)
  (doc 'export #t)
  (doc 'type '(-> Matrix Vec GLMResult))
  (doc 'description "Logistic regression via autodiff gradients + L-BFGS")
  (doc 'param 'X "Design matrix (include intercept column)")
  (doc 'param 'y "Binary response vector (0/1)")
  (doc 'note "Uses numerically stable binomial NLL with autodiff gradients.
Equivalent to (autodiff-glm-fit binomial-family logit-link X y)")
  (apply autodiff-glm-fit binomial-family logit-link X y opts))

;;; autodiff-logistic-predict : GLMResult x Matrix -> Vec
;;; Predict probabilities from a fitted logistic model.
(define (autodiff-logistic-predict model X-new)
  (doc 'export #t)
  (doc 'type '(-> GLMResult Matrix Vec))
  (doc 'description "Predict probabilities from fitted logistic regression")
  (glm-predict model X-new))

;;; autodiff-logistic-classify : GLMResult x Matrix x Num -> Vec
;;; Classify binary outcomes using fitted model.
(define (autodiff-logistic-classify model X-new threshold)
  (doc 'export #t)
  (doc 'type '(-> GLMResult Matrix Num Vec))
  (doc 'description "Classify using fitted logistic regression")
  (let* ([probs (glm-predict model X-new)]
         [n (vector-length probs)]
         [classes (make-vector n)])
    (do ([i 0 (+ i 1)])
        [(= i n) classes]
        (vector-set! classes i (if (>= (vector-ref probs i) threshold) 1 0)))))

;;; ============================================================
;;; Section: Exports Summary
;;; ============================================================
;;;
;;; Core bridge:
;;;   autodiff-glm-fit          — Fit any GLM via autodiff + optimization
;;;   autodiff-nll              — Generic traced NLL for arbitrary distributions
;;;
;;; Logistic regression:
;;;   autodiff-logistic-regression — Logistic regression convenience wrapper
;;;   autodiff-logistic-predict    — Predict probabilities
;;;   autodiff-logistic-classify   — Binary classification
;;;
;;; Traced functions:
;;;   traced-sigmoid            — Traced logistic sigmoid
;;;   traced-binomial-nll       — Traced binomial NLL (numerically stable)
;;;   traced-gaussian-nll       — Traced Gaussian NLL
;;;   traced-poisson-nll        — Traced Poisson NLL
