(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module model-selection-info
;;; @requires prelude entropy transcendental

(require 'prelude)
(require 'entropy)
(require 'transcendental)

(doc 'module 'model-selection-info)
(doc 'description "Information-theoretic model selection criteria (AIC, BIC, AICc)")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Bridges lattice/info/ to lattice/statistics/ by providing
  standalone information criteria grounded in Shannon entropy.
  These can be used with any log-likelihood value, not just
  those from the model-protocol system.")

(doc 'section 'log-likelihoods)

(define (log-likelihood-gaussian residuals)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Gaussian log-likelihood from residuals: -n/2 * (1 + log(2*pi*sigma^2))
   where sigma^2 = (1/n) * sum(r_i^2). This is the maximized log-likelihood.")
  (let* ([n (length residuals)]
         [ss (fold-left + 0 (map (lambda (r) (* r r)) residuals))]
         [sigma2 (/ ss n)])
    (if (<= sigma2 0)
        0
        (* -0.5 n (+ 1 (log-num (* 2 (pi-value) sigma2)))))))

(define (log-likelihood-gaussian-vec residuals-vec)
  (doc 'export #t)
  (doc 'type '(-> Vec Number))
  (doc 'description "Gaussian log-likelihood from a vector of residuals")
  (let* ([n (vector-length residuals-vec)]
         [ss (let loop ([i 0] [s 0])
               (if (= i n)
                   s
                   (let ([r (vector-ref residuals-vec i)])
                     (loop (+ i 1) (+ s (* r r))))))]
         [sigma2 (/ ss n)])
    (if (<= sigma2 0)
        0
        (* -0.5 n (+ 1 (log-num (* 2 (pi-value) sigma2)))))))

(doc 'section 'information-criteria)

(define (aic log-lik k)
  (doc 'export #t)
  (doc 'type '(-> Number Nat Number))
  (doc 'description "Akaike Information Criterion: AIC = -2*logL + 2*k
   where k is the number of estimated parameters. Lower is better.
   Grounded in KL divergence: AIC estimates 2 * E[D_KL(truth || model)].")
  (+ (* -2 log-lik) (* 2 k)))

(define (bic log-lik k n)
  (doc 'export #t)
  (doc 'type '(-> Number Nat Nat Number))
  (doc 'description "Bayesian Information Criterion: BIC = -2*logL + k*log(n)
   where k is parameter count and n is sample size. Lower is better.
   Penalizes complexity more heavily than AIC for n >= 8.")
  (+ (* -2 log-lik) (* k (log-num n))))

(define (aicc log-lik k n)
  (doc 'export #t)
  (doc 'type '(-> Number Nat Nat Number))
  (doc 'description "Corrected AIC for small samples: AICc = AIC + 2k(k+1)/(n-k-1).
   Reduces to AIC as n -> infinity. Use when n/k < 40.")
  (let ([base (aic log-lik k)])
    (if (<= (- n k 1) 0)
        +inf.0  ; Undefined when n <= k+1
        (+ base (/ (* 2 k (+ k 1)) (- n k 1))))))

(doc 'section 'model-comparison)

(define (aic-weights aic-values)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (List Number)))
  (doc 'description "Akaike weights from a list of AIC values.
   w_i = exp(-0.5 * delta_i) / sum(exp(-0.5 * delta_j))
   where delta_i = AIC_i - AIC_min. Weights sum to 1.")
  (if (null? aic-values)
      '()
      (let* ([aic-min (apply min aic-values)]
             [deltas (map (lambda (a) (- a aic-min)) aic-values)]
             [raw-weights (map (lambda (d) (exp-num (* -0.5 d))) deltas)]
             [total (fold-left + 0 raw-weights)])
        (if (<= total 0)
            (map (lambda (_) (/ 1.0 (length aic-values))) aic-values)
            (map (lambda (w) (/ w total)) raw-weights)))))

(define (evidence-ratio w-i w-j)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number))
  (doc 'description "Evidence ratio between two models: w_i / w_j.
   An evidence ratio of 10 means model i is 10x more likely than model j.")
  (if (<= w-j 0)
      +inf.0
      (/ w-i w-j)))

(doc 'section 'entropy-rate-connection)

(define (residual-entropy-bits residuals)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Differential entropy of residuals assuming Gaussian distribution.
   h = 0.5 * log2(2*pi*e*sigma^2) bits. Connects model diagnostics to
   info-theoretic channel capacity: a model with lower residual entropy
   captures more information from the data.")
  (let* ([n (length residuals)]
         [ss (fold-left + 0 (map (lambda (r) (* r r)) residuals))]
         [sigma2 (/ ss n)])
    (if (<= sigma2 0)
        -inf.0
        (gaussian-entropy (sqrt sigma2)))))
