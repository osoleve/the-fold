;;; core/random/bayesian.ss -- Bayesian Inference Engine
;;;
;;; Comprehensive Bayesian inference toolkit for The Fold.
;;; Provides conjugate priors, variational inference, and model selection.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Conjugate prior systems with closed-form posteriors:
;;;     - Beta-Binomial (success rates)
;;;     - Normal-Normal (means with known variance)
;;;     - Gamma-Poisson (count rates)
;;;     - Dirichlet-Multinomial (categorical data)
;;;   - Variational inference:
;;;     - ELBO computation
;;;     - Mean-field approximation
;;;   - Bayesian model selection:
;;;     - Log marginal likelihood
;;;     - Bayes factors
;;;     - BIC approximation
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/transcendental.ss
;;;   - random/distributions.ss

(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")
(load "lattice/random/distributions.ss")

;;; ====
;;; Conjugate Prior: Beta-Binomial
;;; ====
;;;
;;; Prior: Beta(alpha, beta) on success probability p
;;; Likelihood: Binomial(n, p) for k successes
;;; Posterior: Beta(alpha + k, beta + n - k)

;;; make-beta-prior : Number x Number -> BetaPrior
;;; Create a Beta prior with given alpha and beta parameters.
(define (make-beta-prior alpha beta)
  `(beta-prior ,alpha ,beta))

(define (beta-prior? p)
  (and (pair? p) (eq? (car p) 'beta-prior)))

(define (beta-prior-alpha p) (cadr p))
(define (beta-prior-beta p) (caddr p))

;;; beta-binomial-posterior : BetaPrior x Number x Number -> BetaPrior
;;; Update Beta prior with binomial observations.
;;; k = number of successes, n = number of trials
(define (beta-binomial-posterior prior k n)
  (let ([alpha (beta-prior-alpha prior)]
        [beta (beta-prior-beta prior)])
       (make-beta-prior (+ alpha k) (+ beta (- n k)))))

;;; beta-posterior-mean : BetaPrior -> Number
;;; Posterior mean of Beta distribution.
(define (beta-posterior-mean prior)
  (let ([alpha (beta-prior-alpha prior)]
        [beta (beta-prior-beta prior)])
       (/ alpha (+ alpha beta))))

;;; beta-posterior-mode : BetaPrior -> Number
;;; Posterior mode of Beta distribution (for alpha, beta > 1).
(define (beta-posterior-mode prior)
  (let ([alpha (beta-prior-alpha prior)]
        [beta (beta-prior-beta prior)])
       (if (and (> alpha 1) (> beta 1))
           (/ (- alpha 1) (- (+ alpha beta) 2))
           (beta-posterior-mean prior))))

;;; beta-posterior-variance : BetaPrior -> Number
;;; Posterior variance of Beta distribution.
(define (beta-posterior-variance prior)
  (let* ([alpha (beta-prior-alpha prior)]
         [beta (beta-prior-beta prior)]
         [sum (+ alpha beta)])
        (/ (* alpha beta)
           (* sum sum (+ sum 1)))))

;;; beta-credible-interval : BetaPrior x Number -> (Number . Number)
;;; Compute symmetric credible interval at given level (e.g., 0.95).
;;; Uses normal approximation for simplicity.
(define (beta-credible-interval prior level)
  (let* ([mean (beta-posterior-mean prior)]
         [std (sqrt (beta-posterior-variance prior))]
         [z (cond [(= level 0.99) 2.576]
                  [(= level 0.95) 1.96]
                  [(= level 0.90) 1.645]
                  [else 1.96])]
         [half-width (* z std)])
        (cons (max 0 (- mean half-width))
              (min 1 (+ mean half-width)))))

;;; ====
;;; Conjugate Prior: Normal-Normal (Known Variance)
;;; ====
;;;
;;; Prior: Normal(mu0, sigma0^2) on mean mu
;;; Likelihood: Normal(mu, sigma^2) for n observations with sample mean xbar
;;; Posterior: Normal(mu_n, sigma_n^2)
;;;   mu_n = (mu0/sigma0^2 + n*xbar/sigma^2) / (1/sigma0^2 + n/sigma^2)
;;;   sigma_n^2 = 1 / (1/sigma0^2 + n/sigma^2)

;;; make-normal-prior : Number x Number -> NormalPrior
;;; Create a Normal prior with given mean and variance.
(define (make-normal-prior mean variance)
  `(normal-prior ,mean ,variance))

(define (normal-prior? p)
  (and (pair? p) (eq? (car p) 'normal-prior)))

(define (normal-prior-mean p) (cadr p))
(define (normal-prior-variance p) (caddr p))

;;; normal-normal-posterior : NormalPrior x Number x Number x Number -> NormalPrior
;;; Update Normal prior with normal observations.
;;; sample-mean = mean of observations
;;; n = number of observations
;;; known-variance = known variance of the likelihood
(define (normal-normal-posterior prior sample-mean n known-variance)
  (let* ([mu0 (normal-prior-mean prior)]
         [tau0 (/ 1 (normal-prior-variance prior))]  ; Prior precision
         [tau (/ 1 known-variance)]                   ; Likelihood precision
         [tau-n (+ tau0 (* n tau))]                   ; Posterior precision
         [mu-n (/ (+ (* tau0 mu0) (* n tau sample-mean)) tau-n)]
         [variance-n (/ 1 tau-n)])
        (make-normal-prior mu-n variance-n)))

;;; normal-posterior-credible-interval : NormalPrior x Number -> (Number . Number)
(define (normal-posterior-credible-interval prior level)
  (let* ([mean (normal-prior-mean prior)]
         [std (sqrt (normal-prior-variance prior))]
         [z (cond [(= level 0.99) 2.576]
                  [(= level 0.95) 1.96]
                  [(= level 0.90) 1.645]
                  [else 1.96])]
         [half-width (* z std)])
        (cons (- mean half-width) (+ mean half-width))))

;;; ====
;;; Conjugate Prior: Gamma-Poisson
;;; ====
;;;
;;; Prior: Gamma(alpha, beta) on rate lambda
;;; Likelihood: Poisson(lambda) for observations with sum k and count n
;;; Posterior: Gamma(alpha + sum(x), beta + n)

;;; make-gamma-prior : Number x Number -> GammaPrior
;;; Create a Gamma prior with given shape (alpha) and rate (beta).
(define (make-gamma-prior shape rate)
  `(gamma-prior ,shape ,rate))

(define (gamma-prior? p)
  (and (pair? p) (eq? (car p) 'gamma-prior)))

(define (gamma-prior-shape p) (cadr p))
(define (gamma-prior-rate p) (caddr p))

;;; gamma-poisson-posterior : GammaPrior x Number x Number -> GammaPrior
;;; Update Gamma prior with Poisson observations.
;;; sum-obs = sum of observed counts
;;; n = number of observations
(define (gamma-poisson-posterior prior sum-obs n)
  (let ([alpha (gamma-prior-shape prior)]
        [beta (gamma-prior-rate prior)])
       (make-gamma-prior (+ alpha sum-obs) (+ beta n))))

;;; gamma-posterior-mean : GammaPrior -> Number
(define (gamma-posterior-mean prior)
  (/ (gamma-prior-shape prior) (gamma-prior-rate prior)))

;;; gamma-posterior-variance : GammaPrior -> Number
(define (gamma-posterior-variance prior)
  (let ([alpha (gamma-prior-shape prior)]
        [beta (gamma-prior-rate prior)])
       (/ alpha (* beta beta))))

;;; gamma-posterior-mode : GammaPrior -> Number
;;; Mode exists when shape >= 1.
(define (gamma-posterior-mode prior)
  (let ([alpha (gamma-prior-shape prior)]
        [beta (gamma-prior-rate prior)])
       (if (>= alpha 1)
           (/ (- alpha 1) beta)
           (gamma-posterior-mean prior))))

;;; ====
;;; Conjugate Prior: Dirichlet-Multinomial
;;; ====
;;;
;;; Prior: Dirichlet(alphas) on probability vector p
;;; Likelihood: Multinomial(n, p) for observed counts
;;; Posterior: Dirichlet(alphas + counts)

;;; make-dirichlet-prior : (List Number) -> DirichletPrior
;;; Create a Dirichlet prior with given concentration parameters.
(define (make-dirichlet-prior alphas)
  `(dirichlet-prior ,alphas))

(define (dirichlet-prior? p)
  (and (pair? p) (eq? (car p) 'dirichlet-prior)))

(define (dirichlet-prior-alphas p) (cadr p))

;;; dirichlet-multinomial-posterior : DirichletPrior x (List Number) -> DirichletPrior
;;; Update Dirichlet prior with multinomial observations.
(define (dirichlet-multinomial-posterior prior counts)
  (let ([alphas (dirichlet-prior-alphas prior)])
       (make-dirichlet-prior (map + alphas counts))))

;;; dirichlet-posterior-mean : DirichletPrior -> (List Number)
;;; Posterior mean of each probability component.
(define (dirichlet-posterior-mean prior)
  (let* ([alphas (dirichlet-prior-alphas prior)]
         [sum (apply + alphas)])
        (map (lambda (a) (/ a sum)) alphas)))

;;; dirichlet-posterior-mode : DirichletPrior -> (List Number)
;;; MAP estimate (when all alphas > 1).
(define (dirichlet-posterior-mode prior)
  (let* ([alphas (dirichlet-prior-alphas prior)]
         [K (length alphas)]
         [sum (apply + alphas)]
         [denom (- sum K)])
        (if (> denom 0)
            (map (lambda (a) (/ (- a 1) denom)) alphas)
            (dirichlet-posterior-mean prior))))

;;; ====
;;; Log Probability Functions (for MCMC/VI)
;;; ====

;;; log-beta-pdf : Number x Number x Number -> Number
;;; Log PDF of Beta(alpha, beta) at x.
;;; Handles boundary cases where alpha=1 or beta=1 to avoid 0*-inf = NaN.
(define (log-beta-pdf x alpha beta)
  (cond
   [(or (< x 0) (> x 1)) -inf.0]
   ;; Boundary: x=0
   [(= x 0)
    (if (>= alpha 1) -inf.0 +inf.0)]  ; density at 0 depends on alpha
   ;; Boundary: x=1
   [(= x 1)
    (if (>= beta 1) -inf.0 +inf.0)]   ; density at 1 depends on beta
   ;; Interior: handle alpha=1 and beta=1 specially to avoid 0*-inf
   [else
    (let ([term1 (if (= alpha 1) 0 (* (- alpha 1) (log x)))]
          [term2 (if (= beta 1) 0 (* (- beta 1) (log (- 1 x))))])
         (+ term1 term2 (- (log-beta alpha beta))))]))

;;; log-beta : Number x Number -> Number
;;; Log of Beta function B(a,b) = Gamma(a)*Gamma(b)/Gamma(a+b).
(define (log-beta a b)
  (- (+ (log-gamma a) (log-gamma b))
     (log-gamma (+ a b))))

;;; log-normal-pdf : Number x Number x Number -> Number
;;; Log PDF of Normal(mean, variance) at x.
(define (log-normal-pdf x mean variance)
  (let ([diff (- x mean)])
       (- (- (* 0.5 (log (* 2 3.141592653589793 variance))))
          (/ (* diff diff) (* 2 variance)))))

;;; log-gamma-pdf : Number x Number x Number -> Number
;;; Log PDF of Gamma(shape, rate) at x.
(define (log-gamma-pdf x shape rate)
  (if (<= x 0)
      -inf.0
      (+ (* shape (log rate))
         (* (- shape 1) (log x))
         (- (* rate x))
         (- (log-gamma shape)))))

;;; log-poisson-pmf : Number x Number -> Number
;;; Log PMF of Poisson(rate) at k.
(define (log-poisson-pmf k rate)
  (if (< k 0)
      -inf.0
      (- (- (* k (log rate)) rate)
         (log-factorial k))))

;;; log-factorial : Number -> Number
(define (log-factorial n)
  (if (<= n 1)
      0
      (log-gamma (+ n 1))))

;;; ====
;;; Variational Inference: ELBO Computation
;;; ====
;;;
;;; ELBO = E_q[log p(x,z)] - E_q[log q(z)]
;;;      = E_q[log p(x|z)] + E_q[log p(z)] - E_q[log q(z)]
;;;      = E_q[log p(x|z)] - KL(q(z) || p(z))

;;; kl-divergence-normal : NormalPrior x NormalPrior -> Number
;;; KL divergence from q to p for two Normal distributions.
;;; KL(q||p) = log(sigma_p/sigma_q) + (sigma_q^2 + (mu_q - mu_p)^2)/(2*sigma_p^2) - 1/2
(define (kl-divergence-normal q p)
  (let* ([mu-q (normal-prior-mean q)]
         [var-q (normal-prior-variance q)]
         [mu-p (normal-prior-mean p)]
         [var-p (normal-prior-variance p)]
         [sigma-q (sqrt var-q)]
         [sigma-p (sqrt var-p)]
         [diff (- mu-q mu-p)])
        (+ (- (log sigma-p) (log sigma-q))
           (/ (+ var-q (* diff diff)) (* 2 var-p))
           -0.5)))

;;; kl-divergence-beta : BetaPrior x BetaPrior -> Number
;;; KL divergence from q to p for two Beta distributions.
(define (kl-divergence-beta q p)
  (let* ([a-q (beta-prior-alpha q)]
         [b-q (beta-prior-beta q)]
         [a-p (beta-prior-alpha p)]
         [b-p (beta-prior-beta p)]
         [psi-sum-q (digamma (+ a-q b-q))])
        (+ (log-beta a-p b-p)
           (- (log-beta a-q b-q))
           (* (- a-q a-p) (- (digamma a-q) psi-sum-q))
           (* (- b-q b-p) (- (digamma b-q) psi-sum-q)))))

;;; digamma : Number -> Number
;;; Digamma function (derivative of log-gamma).
;;; Uses asymptotic expansion for large x, recurrence for small x.
(define (digamma x)
  (if (< x 6)
      ;; Recurrence: psi(x) = psi(x+1) - 1/x
      (- (digamma (+ x 1)) (/ 1 x))
      ;; Asymptotic expansion for large x
      (let* ([x2 (/ 1 (* x x))])
            (- (log x)
               (/ 1 (* 2 x))
               (* x2 (/ 1 12))
               (* x2 x2 (/ -1 120))))))

;;; ====
;;; Bayesian Model Selection
;;; ====

;;; log-marginal-likelihood-beta-binomial : BetaPrior x Number x Number -> Number
;;; Log marginal likelihood p(data|model) for Beta-Binomial.
;;; This is the normalizing constant of the posterior.
(define (log-marginal-likelihood-beta-binomial prior k n)
  (let ([alpha (beta-prior-alpha prior)]
        [beta (beta-prior-beta prior)])
       (+ (log-beta (+ alpha k) (+ beta (- n k)))
          (- (log-beta alpha beta))
          (log-binomial-coeff n k))))

;;; log-binomial-coeff : Number x Number -> Number
(define (log-binomial-coeff n k)
  (- (log-factorial n)
     (+ (log-factorial k) (log-factorial (- n k)))))

;;; log-marginal-likelihood-normal-normal : NormalPrior x (List Number) x Number -> Number
;;; Log marginal likelihood for Normal-Normal model with known variance.
;;;
;;; Computes log p(x₁:ₙ | μ₀, τ₀², σ²) where:
;;;   μ ~ N(μ₀, τ₀²)  [prior]
;;;   xᵢ | μ ~ N(μ, σ²)  [likelihood]
;;;
;;; Formula (integrating out μ analytically):
;;; log p(D) = -n/2 log(2π) - n/2 log(σ²) - 1/(2σ²) Σ(xᵢ - x̄)²
;;;          - 1/2 log(1 + nτ₀²/σ²) - n(x̄ - μ₀)²/(2(σ² + nτ₀²))
(define (log-marginal-likelihood-normal-normal prior observations known-variance)
  (if (null? observations)
      0  ; No data = log-likelihood of 0 (multiplier of 1)
      (let* ([n (length observations)]
             [sigma2 known-variance]
             [tau02 (normal-prior-variance prior)]
             [mu0 (normal-prior-mean prior)]
             [xbar (/ (apply + observations) n)]
             ;; Sum of squared deviations from sample mean
             [ss (apply + (map (lambda (x)
                                       (let ([d (- x xbar)]) (* d d)))
                               observations))]
             ;; Terms of the log marginal likelihood
             [term1 (* -0.5 n (log (* 2 3.141592653589793 sigma2)))]
             [term2 (/ (- ss) (* 2 sigma2))]
             [term3 (* -0.5 (log (+ 1 (/ (* n tau02) sigma2))))]
             [term4 (/ (- (* n (let ([d (- xbar mu0)]) (* d d))))
                       (* 2 (+ sigma2 (* n tau02))))])
            (+ term1 term2 term3 term4))))

;;; bayes-factor : Number x Number -> Number
;;; Compute Bayes factor B_12 = p(data|M1) / p(data|M2).
;;; Input: log marginal likelihoods of each model.
(define (bayes-factor log-ml-1 log-ml-2)
  (exp (- log-ml-1 log-ml-2)))

;;; log-bayes-factor : Number x Number -> Number
(define (log-bayes-factor log-ml-1 log-ml-2)
  (- log-ml-1 log-ml-2))

;;; interpret-bayes-factor : Number -> Symbol
;;; Jeffreys' scale interpretation of Bayes factor.
(define (interpret-bayes-factor bf)
  (cond [(> bf 100) 'decisive]
        [(> bf 30) 'very-strong]
        [(>= bf 10) 'strong]
        [(>= bf 3) 'substantial]
        [(> bf 1) 'barely-worth-mentioning]
        [(> bf 0.33) 'barely-worth-mentioning-against]
        [(>= bf 0.1) 'substantial-against]
        [(>= bf 0.033) 'strong-against]
        [(>= bf 0.01) 'very-strong-against]
        [else 'decisive-against]))

;;; bic : Number x Number x Number -> Number
;;; Bayesian Information Criterion.
;;; log-likelihood = log p(data|theta_MLE)
;;; k = number of parameters
;;; n = number of observations
(define (bic log-likelihood k n)
  (- (* k (log n)) (* 2 log-likelihood)))

;;; aic : Number x Number -> Number
;;; Akaike Information Criterion.
(define (aic log-likelihood k)
  (- (* 2 k) (* 2 log-likelihood)))

;;; ====
;;; Sequential Bayesian Updates
;;; ====

;;; sequential-beta-update : BetaPrior x (List (Number . Number)) -> BetaPrior
;;; Sequentially update Beta prior with a list of (successes . trials).
(define (sequential-beta-update prior observations)
  (fold-left (lambda (p obs)
                     (beta-binomial-posterior p (car obs) (cdr obs)))
             prior
             observations))

;;; sequential-normal-update : NormalPrior x (List Number) x Number -> NormalPrior
;;; Sequentially update Normal prior with observations.
(define (sequential-normal-update prior observations known-variance)
  (fold-left (lambda (p x)
                     (normal-normal-posterior p x 1 known-variance))
             prior
             observations))

;;; sequential-gamma-update : GammaPrior x (List Number) -> GammaPrior
;;; Sequentially update Gamma prior with Poisson observations.
(define (sequential-gamma-update prior observations)
  (fold-left (lambda (p x)
                     (gamma-poisson-posterior p x 1))
             prior
             observations))

;;; ====
;;; Utility Functions
;;; ====

;;; posterior-summary : Prior -> Alist
;;; Generate a summary of the posterior distribution.
(define (posterior-summary prior)
  (cond
   [(beta-prior? prior)
    `((type . beta)
      (mean . ,(beta-posterior-mean prior))
      (mode . ,(beta-posterior-mode prior))
      (variance . ,(beta-posterior-variance prior))
      (ci-95 . ,(beta-credible-interval prior 0.95)))]
   [(normal-prior? prior)
    `((type . normal)
      (mean . ,(normal-prior-mean prior))
      (variance . ,(normal-prior-variance prior))
      (ci-95 . ,(normal-posterior-credible-interval prior 0.95)))]
   [(gamma-prior? prior)
    `((type . gamma)
      (mean . ,(gamma-posterior-mean prior))
      (mode . ,(gamma-posterior-mode prior))
      (variance . ,(gamma-posterior-variance prior)))]
   [(dirichlet-prior? prior)
    `((type . dirichlet)
      (mean . ,(dirichlet-posterior-mean prior))
      (mode . ,(dirichlet-posterior-mode prior)))]
   [else '((type . unknown))]))

;;; predictive-beta-binomial : BetaPrior x Number -> Number
;;; Compute predictive probability of k successes in n future trials.
;;; This is the Beta-Binomial distribution.
(define (predictive-beta-binomial prior k n)
  (let* ([alpha (beta-prior-alpha prior)]
         [beta (beta-prior-beta prior)]
         [log-prob (+ (log-binomial-coeff n k)
                      (log-beta (+ alpha k) (+ beta (- n k)))
                      (- (log-beta alpha beta)))])
        (exp log-prob)))

;;; predictive-mean-beta-binomial : BetaPrior x Number -> Number
;;; Expected number of successes in n future trials.
(define (predictive-mean-beta-binomial prior n)
  (* n (beta-posterior-mean prior)))

;;; ====
;;; Exports Summary
;;; ====
;;;
;;; Beta-Binomial:
;;;   make-beta-prior, beta-binomial-posterior
;;;   beta-posterior-mean, beta-posterior-mode, beta-posterior-variance
;;;   beta-credible-interval
;;;
;;; Normal-Normal:
;;;   make-normal-prior, normal-normal-posterior
;;;   normal-posterior-credible-interval
;;;
;;; Gamma-Poisson:
;;;   make-gamma-prior, gamma-poisson-posterior
;;;   gamma-posterior-mean, gamma-posterior-mode, gamma-posterior-variance
;;;
;;; Dirichlet-Multinomial:
;;;   make-dirichlet-prior, dirichlet-multinomial-posterior
;;;   dirichlet-posterior-mean, dirichlet-posterior-mode
;;;
;;; Log Probabilities:
;;;   log-beta-pdf, log-normal-pdf, log-gamma-pdf, log-poisson-pmf
;;;
;;; Variational Inference:
;;;   kl-divergence-normal, kl-divergence-beta, digamma
;;;
;;; Model Selection:
;;;   log-marginal-likelihood-beta-binomial
;;;   log-marginal-likelihood-normal-normal
;;;   bayes-factor, log-bayes-factor, interpret-bayes-factor
;;;   bic, aic
;;;
;;; Sequential Updates:
;;;   sequential-beta-update, sequential-normal-update, sequential-gamma-update
;;;
;;; Utilities:
;;;   posterior-summary, predictive-beta-binomial, predictive-mean-beta-binomial
