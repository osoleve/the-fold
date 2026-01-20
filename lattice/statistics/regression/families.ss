(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")

(doc 'module 'families)
(doc 'description "GLM Families — Variance functions and deviance for GLM families")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'description "gaussian-family, binomial-family, poisson-family, gamma-family")

(doc 'note "A family specifies:")
(doc 'note "  - variance(mu): variance as a function of mean")
(doc 'note "  - deviance(y, mu): contribution to deviance")
(doc 'note "  - valid-mu?(mu): whether mu is in valid range")
(doc 'note "  - initialize(y): initial mu estimate")

(doc 'section 'family-structure)
(define (make-glm-family name variance deviance-contrib valid-mu? initialize)
  (doc 'type '(-> Symbol (-> Num Num) (-> Num Num Num) (-> Num Bool) (-> Num Num) GLMFamily))
  (doc 'description "Create a GLM family")
  (doc 'param 'name "family name (gaussian, binomial, poisson, gamma)")
  (doc 'param 'variance "variance function V(mu)")
  (doc 'param 'deviance-contrib "deviance contribution for single observation")
  (doc 'param 'valid-mu? "predicate for valid mu values")
  (doc 'param 'initialize "initial mu from y")
  (list 'glm-family name variance deviance-contrib valid-mu? initialize))

;;; glm-family? : Any → Bool
(define (glm-family? x)
  (and (pair? x) (eq? (car x) 'glm-family)))

;;; Accessors
(define (family-name f) (list-ref f 1))
(define (family-variance f) (list-ref f 2))
(define (family-deviance-contrib f) (list-ref f 3))
(define (family-valid-mu? f) (list-ref f 4))
(define (family-initialize f) (list-ref f 5))

;;; ====
;;; Gaussian Family
;;; ====

;;; Gaussian: Y ~ N(mu, sigma²)
;;; Variance: V(mu) = 1 (constant)
;;; Deviance: D = sum((y - mu)²)
;;; Canonical link: identity

(doc gaussian-family 'export #t)
(define gaussian-family
  (make-glm-family
   'gaussian
   ;; variance(mu) = 1
   (lambda (mu) 1)
   ;; deviance(y, mu) = (y - mu)²
   (lambda (y mu) (expt (- y mu) 2))
   ;; valid-mu?: any real
   (lambda (mu) #t)
   ;; initialize: y itself (clamped slightly from 0 for stability)
   (lambda (y) y)))

;;; ====
;;; Binomial Family
;;; ====

;;; Binomial: Y ~ Binomial(n, p), often n=1 (Bernoulli)
;;; Variance: V(mu) = mu(1 - mu)
;;; Deviance: D = 2 * sum(y*log(y/mu) + (1-y)*log((1-y)/(1-mu)))
;;; Canonical link: logit

(doc binomial-family 'export #t)
(define binomial-family
  (make-glm-family
   'binomial
   ;; variance(mu) = mu * (1 - mu)
   (lambda (mu) (* mu (- 1 mu)))
   ;; deviance(y, mu): binomial deviance
   ;; D_i = 2 * (y*log(y/mu) + (1-y)*log((1-y)/(1-mu)))
   ;; Handle y=0 and y=1 carefully
   (lambda (y mu)
           (let* ([mu-safe (max (min mu (- 1 1e-10)) 1e-10)]
                  [y-safe (max (min y (- 1 1e-10)) 1e-10)]
                  [term1 (if (> y 0) (* y (log (/ y-safe mu-safe))) 0)]
                  [term2 (if (< y 1) (* (- 1 y) (log (/ (- 1 y-safe) (- 1 mu-safe)))) 0)])
                 (* 2 (+ term1 term2))))
   ;; valid-mu?: 0 < mu < 1
   (lambda (mu) (and (> mu 0) (< mu 1)))
   ;; initialize: (y + 0.5) / 2, keeps in (0.25, 0.75)
   (lambda (y) (/ (+ y 0.5) 2))))

;;; ====
;;; Poisson Family
;;; ====

;;; Poisson: Y ~ Poisson(mu)
;;; Variance: V(mu) = mu
;;; Deviance: D = 2 * sum(y*log(y/mu) - (y - mu))
;;; Canonical link: log

(doc poisson-family 'export #t)
(define poisson-family
  (make-glm-family
   'poisson
   ;; variance(mu) = mu
   (lambda (mu) mu)
   ;; deviance(y, mu): poisson deviance
   ;; D_i = 2 * (y*log(y/mu) - (y - mu))
   (lambda (y mu)
           (let* ([mu-safe (max mu 1e-10)]
                  [y-safe (max y 1e-10)]
                  [term1 (if (> y 0) (* y (log (/ y-safe mu-safe))) 0)]
                  [term2 (- y mu)])
                 (* 2 (- term1 term2))))
   ;; valid-mu?: mu > 0
   (lambda (mu) (> mu 0))
   ;; initialize: max(y, 0.1)
   (lambda (y) (max y 0.1))))

;;; ====
;;; Gamma Family
;;; ====

;;; Gamma: Y ~ Gamma(shape, rate) with mean = mu
;;; Variance: V(mu) = mu²
;;; Deviance: D = -2 * sum(log(y/mu) - (y - mu)/mu)
;;; Canonical link: inverse (1/mu)

(doc gamma-family 'export #t)
(define gamma-family
  (make-glm-family
   'gamma
   ;; variance(mu) = mu²
   (lambda (mu) (* mu mu))
   ;; deviance(y, mu): gamma deviance
   ;; D_i = -2 * (log(y/mu) - (y - mu)/mu)
   ;;     = 2 * ((y - mu)/mu - log(y/mu))
   (lambda (y mu)
           (let* ([mu-safe (max mu 1e-10)]
                  [y-safe (max y 1e-10)])
                 (* 2 (- (/ (- y-safe mu-safe) mu-safe)
                         (log (/ y-safe mu-safe))))))
   ;; valid-mu?: mu > 0
   (lambda (mu) (> mu 0))
   ;; initialize: max(y, 1e-3)
   (lambda (y) (max y 1e-3))))

;;; ====
;;; Inverse Gaussian Family
;;; ====

;;; Inverse Gaussian (Wald): Y ~ IG(mu, lambda)
;;; Variance: V(mu) = mu³
;;; Canonical link: 1/mu²

(define inverse-gaussian-family
  (make-glm-family
   'inverse-gaussian
   ;; variance(mu) = mu³
   (lambda (mu) (expt mu 3))
   ;; deviance(y, mu): inverse gaussian deviance
   ;; D_i = (y - mu)² / (y * mu²)
   (lambda (y mu)
           (let* ([mu-safe (max mu 1e-10)]
                  [y-safe (max y 1e-10)])
                 (/ (expt (- y-safe mu-safe) 2)
                    (* y-safe mu-safe mu-safe))))
   ;; valid-mu?: mu > 0
   (lambda (mu) (> mu 0))
   ;; initialize: max(y, 1e-3)
   (lambda (y) (max y 1e-3))))

;;; ====
;;; Quasi Families
;;; ====

;;; Quasi families allow over/under-dispersion via a scale parameter.
;;; The variance is V(mu) * phi where phi is estimated from the data.

;;; quasi-poisson-family : GLMFamily
;;; Like Poisson but allows overdispersion.
(define quasi-poisson-family
  (make-glm-family
   'quasi-poisson
   (lambda (mu) mu)  ; V(mu) = mu
   (lambda (y mu)    ; Use Poisson deviance
           (let* ([mu-safe (max mu 1e-10)]
                  [y-safe (max y 1e-10)]
                  [term1 (if (> y 0) (* y (log (/ y-safe mu-safe))) 0)]
                  [term2 (- y mu)])
                 (* 2 (- term1 term2))))
   (lambda (mu) (> mu 0))
   (lambda (y) (max y 0.1))))

;;; quasi-binomial-family : GLMFamily
;;; Like Binomial but allows overdispersion.
(define quasi-binomial-family
  (make-glm-family
   'quasi-binomial
   (lambda (mu) (* mu (- 1 mu)))
   (lambda (y mu)
           (let* ([mu-safe (max (min mu (- 1 1e-10)) 1e-10)]
                  [y-safe (max (min y (- 1 1e-10)) 1e-10)]
                  [term1 (if (> y 0) (* y (log (/ y-safe mu-safe))) 0)]
                  [term2 (if (< y 1) (* (- 1 y) (log (/ (- 1 y-safe) (- 1 mu-safe)))) 0)])
                 (* 2 (+ term1 term2))))
   (lambda (mu) (and (> mu 0) (< mu 1)))
   (lambda (y) (/ (+ y 0.5) 2))))

;;; ====
;;; Utility Functions
;;; ====

;;; compute-deviance : GLMFamily × Vec × Vec → Num
;;; Compute total deviance for a fitted model.
(define (compute-deviance family y mu)
  (let* ([n (vector-length y)]
         [dev-fn (family-deviance-contrib family)])
        (let loop ([i 0] [total 0])
             (if (= i n)
                 total
                 (loop (+ i 1)
                       (+ total (dev-fn (vector-ref y i)
                                        (vector-ref mu i))))))))

;;; compute-null-deviance : GLMFamily × Vec → Num
;;; Compute null model deviance (intercept-only model).
(define (compute-null-deviance family y)
  (let* ([n (vector-length y)]
         [y-mean (let loop ([i 0] [s 0])
                      (if (= i n)
                          (/ s n)
                          (loop (+ i 1) (+ s (vector-ref y i)))))]
         [mu-null (make-vector n y-mean)])
        (compute-deviance family y mu-null)))

;;; compute-aic : Num × Nat × Nat → Num
;;; AIC = deviance + 2*p
;;; More precisely, AIC = -2*loglik + 2*p, and deviance = -2*(loglik - loglik_saturated)
(define (compute-aic deviance n p)
  (+ deviance (* 2 p)))
