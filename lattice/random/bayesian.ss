(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")
(load "lattice/random/distributions.ss")



(doc 'module 'bayesian)
(doc 'description "Comprehensive Bayesian inference toolkit for The Fold.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'conjugate-prior-beta-binomial)

(doc "Conjugate Prior: Beta-Binomial")







(define (make-beta-prior alpha beta)
  `(beta-prior ,alpha ,beta))

(define (beta-prior? p)
  (and (pair? p) (eq? (car p) 'beta-prior)))

(define (beta-prior-alpha p) (cadr p))
(define (beta-prior-beta p) (caddr p))

(define (beta-binomial-posterior prior k n)
  (let ([alpha (beta-prior-alpha prior)]
        [beta (beta-prior-beta prior)])
       (make-beta-prior (+ alpha k) (+ beta (- n k)))))

(define (beta-posterior-mean prior)
  (let ([alpha (beta-prior-alpha prior)]
        [beta (beta-prior-beta prior)])
       (/ alpha (+ alpha beta))))

(define (beta-posterior-mode prior)
  (let ([alpha (beta-prior-alpha prior)]
        [beta (beta-prior-beta prior)])
       (if (and (> alpha 1) (> beta 1))
           (/ (- alpha 1) (- (+ alpha beta) 2))
           (beta-posterior-mean prior))))

(define (beta-posterior-variance prior)
  (let* ([alpha (beta-prior-alpha prior)]
         [beta (beta-prior-beta prior)]
         [sum (+ alpha beta)])
        (/ (* alpha beta)
           (* sum sum (+ sum 1)))))

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


(doc 'section 'conjugate-prior-normal-normal-known-variance)

(doc "Conjugate Prior: Normal-Normal (Known Variance)")



(define (make-normal-prior mean variance)
  `(normal-prior ,mean ,variance))

(define (normal-prior? p)
  (and (pair? p) (eq? (car p) 'normal-prior)))

(define (normal-prior-mean p) (cadr p))
(define (normal-prior-variance p) (caddr p))

(define (normal-normal-posterior prior sample-mean n known-variance)
  (let* ([mu0 (normal-prior-mean prior)]
         [tau0 (/ 1 (normal-prior-variance prior))]  ; Prior precision
         [tau (/ 1 known-variance)]                   ; Likelihood precision
         [tau-n (+ tau0 (* n tau))]                   ; Posterior precision
         [mu-n (/ (+ (* tau0 mu0) (* n tau sample-mean)) tau-n)]
         [variance-n (/ 1 tau-n)])
        (make-normal-prior mu-n variance-n)))

(define (normal-posterior-credible-interval prior level)
  (let* ([mean (normal-prior-mean prior)]
         [std (sqrt (normal-prior-variance prior))]
         [z (cond [(= level 0.99) 2.576]
                  [(= level 0.95) 1.96]
                  [(= level 0.90) 1.645]
                  [else 1.96])]
         [half-width (* z std)])
        (cons (- mean half-width) (+ mean half-width))))


(doc 'section 'conjugate-prior-gamma-poisson)

(doc "Conjugate Prior: Gamma-Poisson")



(define (make-gamma-prior shape rate)
  `(gamma-prior ,shape ,rate))

(define (gamma-prior? p)
  (and (pair? p) (eq? (car p) 'gamma-prior)))

(define (gamma-prior-shape p) (cadr p))
(define (gamma-prior-rate p) (caddr p))

(define (gamma-poisson-posterior prior sum-obs n)
  (let ([alpha (gamma-prior-shape prior)]
        [beta (gamma-prior-rate prior)])
       (make-gamma-prior (+ alpha sum-obs) (+ beta n))))

(define (gamma-posterior-mean prior)
  (/ (gamma-prior-shape prior) (gamma-prior-rate prior)))

(define (gamma-posterior-variance prior)
  (let ([alpha (gamma-prior-shape prior)]
        [beta (gamma-prior-rate prior)])
       (/ alpha (* beta beta))))

(define (gamma-posterior-mode prior)
  (let ([alpha (gamma-prior-shape prior)]
        [beta (gamma-prior-rate prior)])
       (if (>= alpha 1)
           (/ (- alpha 1) beta)
           (gamma-posterior-mean prior))))


(doc 'section 'conjugate-prior-dirichlet-multinomial)

(doc "Conjugate Prior: Dirichlet-Multinomial")



(define (make-dirichlet-prior alphas)
  `(dirichlet-prior ,alphas))

(define (dirichlet-prior? p)
  (and (pair? p) (eq? (car p) 'dirichlet-prior)))

(define (dirichlet-prior-alphas p) (cadr p))

(define (dirichlet-multinomial-posterior prior counts)
  (let ([alphas (dirichlet-prior-alphas prior)])
       (make-dirichlet-prior (map + alphas counts))))

(define (dirichlet-posterior-mean prior)
  (let* ([alphas (dirichlet-prior-alphas prior)]
         [sum (apply + alphas)])
        (map (lambda (a) (/ a sum)) alphas)))

(define (dirichlet-posterior-mode prior)
  (let* ([alphas (dirichlet-prior-alphas prior)]
         [K (length alphas)]
         [sum (apply + alphas)]
         [denom (- sum K)])
        (if (> denom 0)
            (map (lambda (a) (/ (- a 1) denom)) alphas)
            (dirichlet-posterior-mean prior))))


(doc 'section 'log-probability-functions-for-mcmc/vi)

(doc "Log Probability Functions (for MCMC/VI)")



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

(define (log-beta a b)
  (- (+ (log-gamma a) (log-gamma b))
     (log-gamma (+ a b))))

(define (log-normal-pdf x mean variance)
  (let ([diff (- x mean)])
       (- (- (* 0.5 (log (* 2 3.141592653589793 variance))))
          (/ (* diff diff) (* 2 variance)))))

(define (log-gamma-pdf x shape rate)
  (if (<= x 0)
      -inf.0
      (+ (* shape (log rate))
         (* (- shape 1) (log x))
         (- (* rate x))
         (- (log-gamma shape)))))

(define (log-poisson-pmf k rate)
  (if (< k 0)
      -inf.0
      (- (- (* k (log rate)) rate)
         (log-factorial k))))

(define (log-factorial n)
  (if (<= n 1)
      0
      (log-gamma (+ n 1))))


(doc 'section 'variational-inference-elbo-computation)

(doc "Variational Inference: ELBO Computation")



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


(doc 'section 'bayesian-model-selection)

(doc "Bayesian Model Selection")



(define (log-marginal-likelihood-beta-binomial prior k n)
  (let ([alpha (beta-prior-alpha prior)]
        [beta (beta-prior-beta prior)])
       (+ (log-beta (+ alpha k) (+ beta (- n k)))
          (- (log-beta alpha beta))
          (log-binomial-coeff n k))))

(define (log-binomial-coeff n k)
  (- (log-factorial n)
     (+ (log-factorial k) (log-factorial (- n k)))))

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

(define (bayes-factor log-ml-1 log-ml-2)
  (exp (- log-ml-1 log-ml-2)))

(define (log-bayes-factor log-ml-1 log-ml-2)
  (- log-ml-1 log-ml-2))

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

(define (bic log-likelihood k n)
  (- (* k (log n)) (* 2 log-likelihood)))

(define (aic log-likelihood k)
  (- (* 2 k) (* 2 log-likelihood)))


(doc 'section 'sequential-bayesian-updates)

(doc "Sequential Bayesian Updates")



(define (sequential-beta-update prior observations)
  (fold-left (lambda (p obs)
                     (beta-binomial-posterior p (car obs) (cdr obs)))
             prior
             observations))

(define (sequential-normal-update prior observations known-variance)
  (fold-left (lambda (p x)
                     (normal-normal-posterior p x 1 known-variance))
             prior
             observations))

(define (sequential-gamma-update prior observations)
  (fold-left (lambda (p x)
                     (gamma-poisson-posterior p x 1))
             prior
             observations))


(doc 'section 'utility-functions)

(doc "Utility Functions")



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

(define (predictive-beta-binomial prior k n)
  (let* ([alpha (beta-prior-alpha prior)]
         [beta (beta-prior-beta prior)]
         [log-prob (+ (log-binomial-coeff n k)
                      (log-beta (+ alpha k) (+ beta (- n k)))
                      (- (log-beta alpha beta)))])
        (exp log-prob)))

(define (predictive-mean-beta-binomial prior n)
  (* n (beta-posterior-mean prior)))


(doc 'section 'exports-summary)

(doc "Exports Summary")


