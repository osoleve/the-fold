;;; lattice/statistics/regression/link-functions.ss — GLM Link Functions
;;;
;;; Link functions and their inverses for GLM.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - identity-link
;;;   - logit-link
;;;   - log-link
;;;   - probit-link
;;;   - inverse-link
;;;   - sqrt-link
;;;
;;; A link function specifies:
;;;   - g(mu): link function, maps mean to linear predictor
;;;   - g-inverse(eta): inverse link, maps linear predictor to mean
;;;   - g-derivative(mu): derivative dg/dmu
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/numeric/transcendental.ss
;;;   - fp/numeric/special-functions.ss

(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")
(load "lattice/fp/numeric/special-functions.ss")

;;; ====
;;; Link Function Structure
;;; ====

;;; make-link-function : Symbol × (Num → Num) × (Num → Num) × (Num → Num) → LinkFunction
;;; Create a link function.
;;;   name: link name
;;;   g: link function mu → eta
;;;   g-inverse: inverse link eta → mu
;;;   g-derivative: derivative d(eta)/d(mu)
(define (make-link-function name g g-inverse g-derivative)
  (list 'link-function name g g-inverse g-derivative))

;;; link-function? : Any → Bool
(define (link-function? x)
  (and (pair? x) (eq? (car x) 'link-function)))

;;; Accessors
(define (link-name lf) (list-ref lf 1))
(define (link-g lf) (list-ref lf 2))
(define (link-inverse lf) (list-ref lf 3))
(define (link-derivative lf) (list-ref lf 4))

;;; ====
;;; Identity Link
;;; ====

;;; g(mu) = mu
;;; Canonical link for Gaussian family.

(define identity-link
  (make-link-function
   'identity
   ;; g(mu) = mu
   (lambda (mu) mu)
   ;; g-inverse(eta) = eta
   (lambda (eta) eta)
   ;; g'(mu) = 1
   (lambda (mu) 1)))

;;; ====
;;; Logit Link
;;; ====

;;; g(mu) = log(mu / (1 - mu))
;;; Canonical link for Binomial family.

(define logit-link
  (make-link-function
   'logit
   ;; g(mu) = log(mu / (1 - mu))
   (lambda (mu)
           (let ([mu-safe (max (min mu (- 1 1e-10)) 1e-10)])
                (log (/ mu-safe (- 1 mu-safe)))))
   ;; g-inverse(eta) = 1 / (1 + exp(-eta)) = sigmoid
   (lambda (eta)
           (let ([eta-clamp (max (min eta 20) -20)])  ; Prevent overflow
                (/ 1 (+ 1 (exp (- eta-clamp))))))
   ;; g'(mu) = 1 / (mu * (1 - mu))
   (lambda (mu)
           (let ([mu-safe (max (min mu (- 1 1e-10)) 1e-10)])
                (/ 1 (* mu-safe (- 1 mu-safe)))))))

;;; ====
;;; Log Link
;;; ====

;;; g(mu) = log(mu)
;;; Canonical link for Poisson family.

(define log-link
  (make-link-function
   'log
   ;; g(mu) = log(mu)
   (lambda (mu)
           (log (max mu 1e-10)))
   ;; g-inverse(eta) = exp(eta)
   (lambda (eta)
           (let ([eta-clamp (min eta 20)])  ; Prevent overflow
                (exp eta-clamp)))
   ;; g'(mu) = 1/mu
   (lambda (mu)
           (/ 1 (max mu 1e-10)))))

;;; ====
;;; Probit Link
;;; ====

;;; g(mu) = Phi^(-1)(mu) where Phi is standard normal CDF
;;; Alternative link for Binomial family.

(define probit-link
  (make-link-function
   'probit
   ;; g(mu) = Phi^(-1)(mu)
   (lambda (mu)
           (let ([mu-safe (max (min mu (- 1 1e-10)) 1e-10)])
                (probit-quantile mu-safe)))
   ;; g-inverse(eta) = Phi(eta)
   (lambda (eta)
           (probit-cdf eta))
   ;; g'(mu) = 1/phi(Phi^(-1)(mu)) where phi is standard normal PDF
   (lambda (mu)
           (let* ([mu-safe (max (min mu (- 1 1e-10)) 1e-10)]
                  [z (probit-quantile mu-safe)]
                  [phi-z (probit-pdf z)])
                 (/ 1 (max phi-z 1e-10))))))

;;; probit-quantile : Num → Num
;;; Standard normal quantile function (Phi^(-1)).
(define (probit-quantile p)
  (if (or (<= p 0) (>= p 1))
      (error 'probit-quantile "p must be in (0, 1)" p)
      ;; Use rational approximation (Abramowitz & Stegun)
      (let* ([a0 2.515517]
             [a1 0.802853]
             [a2 0.010328]
             [b1 1.432788]
             [b2 0.189269]
             [b3 0.001308]
             [sign (if (< p 0.5) -1 1)]
             [p* (if (< p 0.5) p (- 1 p))]
             [t (sqrt (* -2 (log p*)))]
             [num (+ a0 (* a1 t) (* a2 t t))]
             [den (+ 1 (* b1 t) (* b2 t t) (* b3 t t t))])
            (* sign (- t (/ num den))))))

;;; probit-cdf : Num → Num
;;; Standard normal CDF (Phi).
(define (probit-cdf x)
  (* 0.5 (+ 1 (erf (/ x (sqrt 2))))))

;;; probit-pdf : Num → Num
;;; Standard normal PDF (phi).
(define (probit-pdf x)
  (/ (exp (* -0.5 x x))
     (sqrt (* 2 (pi-value)))))

;;; ====
;;; Inverse Link
;;; ====

;;; g(mu) = 1/mu
;;; Canonical link for Gamma family.

(define inverse-link
  (make-link-function
   'inverse
   ;; g(mu) = 1/mu
   (lambda (mu)
           (/ 1 (if (= mu 0) 1e-10 mu)))
   ;; g-inverse(eta) = 1/eta
   (lambda (eta)
           (/ 1 (if (= eta 0) 1e-10 eta)))
   ;; g'(mu) = -1/mu²
   (lambda (mu)
           (/ -1 (expt (if (= mu 0) 1e-10 mu) 2)))))

;;; ====
;;; Square Root Link
;;; ====

;;; g(mu) = sqrt(mu)
;;; Alternative link for Poisson (variance-stabilizing).

(define sqrt-link
  (make-link-function
   'sqrt
   ;; g(mu) = sqrt(mu)
   (lambda (mu)
           (sqrt (max mu 0)))
   ;; g-inverse(eta) = eta²
   (lambda (eta)
           (expt eta 2))
   ;; g'(mu) = 1/(2*sqrt(mu))
   (lambda (mu)
           (/ 1 (* 2 (sqrt (max mu 1e-10)))))))

;;; ====
;;; Complementary Log-Log Link
;;; ====

;;; g(mu) = log(-log(1 - mu))
;;; Alternative link for Binomial (asymmetric).

(define cloglog-link
  (make-link-function
   'cloglog
   ;; g(mu) = log(-log(1 - mu))
   (lambda (mu)
           (let ([mu-safe (max (min mu (- 1 1e-10)) 1e-10)])
                (log (- (log (- 1 mu-safe))))))
   ;; g-inverse(eta) = 1 - exp(-exp(eta))
   (lambda (eta)
           (let ([eta-clamp (max (min eta 10) -10)])
                (- 1 (exp (- (exp eta-clamp))))))
   ;; g'(mu) = 1 / ((1 - mu) * (-log(1 - mu)))
   (lambda (mu)
           (let* ([mu-safe (max (min mu (- 1 1e-10)) 1e-10)]
                  [one-minus-mu (- 1 mu-safe)]
                  [neg-log (- (log one-minus-mu))])
                 (/ 1 (* one-minus-mu (max neg-log 1e-10)))))))

;;; ====
;;; Cauchy Link (Cauchit)
;;; ====

;;; g(mu) = tan(pi * (mu - 0.5))
;;; Alternative link for Binomial (heavy tails).

(define cauchit-link
  (make-link-function
   'cauchit
   ;; g(mu) = tan(pi * (mu - 0.5))
   (lambda (mu)
           (let ([mu-safe (max (min mu 0.9999) 0.0001)])
                (tan-num (* (pi-value) (- mu-safe 0.5)))))
   ;; g-inverse(eta) = 0.5 + atan(eta)/pi
   (lambda (eta)
           (+ 0.5 (/ (atan eta) (pi-value))))
   ;; g'(mu) = pi / cos²(pi*(mu-0.5)) = pi * (1 + tan²(...))
   (lambda (mu)
           (let* ([mu-safe (max (min mu 0.9999) 0.0001)]
                  [t (tan-num (* (pi-value) (- mu-safe 0.5)))])
                 (* (pi-value) (+ 1 (* t t)))))))

;;; ====
;;; Power Links
;;; ====

;;; make-power-link : Num → LinkFunction
;;; g(mu) = mu^power for power != 0, log(mu) for power = 0
(define (make-power-link power)
  (if (= power 0)
      log-link
      (make-link-function
       'power
       ;; g(mu) = mu^power
       (lambda (mu)
               (expt (max mu 1e-10) power))
       ;; g-inverse(eta) = eta^(1/power)
       (lambda (eta)
               (expt (max eta 1e-10) (/ 1 power)))
       ;; g'(mu) = power * mu^(power-1)
       (lambda (mu)
               (* power (expt (max mu 1e-10) (- power 1)))))))

;;; ====
;;; Canonical Links
;;; ====

;;; canonical-link : Symbol → LinkFunction
;;; Return canonical link for given family.
(define (canonical-link family-name)
  (case family-name
        [(gaussian) identity-link]
        [(binomial) logit-link]
        [(poisson) log-link]
        [(gamma) inverse-link]
        [(inverse-gaussian) (make-power-link -2)]
        [else (error 'canonical-link "unknown family" family-name)]))
