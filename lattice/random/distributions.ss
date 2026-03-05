;;; lattice/random/distributions.ss — Probability Distributions
;;; @module distributions
;;; @requires prelude fp/control/state transcendental prng fp/protocol
;;; @description Probability distributions: normal, exponential, poisson, etc.
;;; @purity total
;;; @stability stable

(require 'prelude)
(require 'fp/control/state)
(require 'transcendental)
(require 'prng)
(require 'fp/protocol)

(doc 'module 'distributions)
(doc 'description "Probability distributions with protocol-based interface.
  Each distribution is a first-class tagged object supporting dist-pdf,
  dist-cdf, dist-quantile, and dist-sample protocols where implemented.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Helpers
;;; ============================================================

(doc 'section 'helpers)

(define (horner-eval coeffs x)
  (doc 'type '(-> (List Number) Number Number))
  (doc 'description "Evaluate polynomial a0 + a1*x + a2*x^2 + ... via Horner's method")
  (fold-right (lambda (c acc) (+ c (* acc x))) 0 coeffs))

(define (safe-log u)
  (doc 'type '(-> Number Number))
  (doc 'description "Natural log with guard against zero")
  (log-num (max u 1e-300)))

(define (rejection-sampler proposal try-accept)
  (doc 'type '(-> (State RNG a) (a -> (Or b #f)) (State RNG b)))
  (doc 'description "Rejection sampling: draw from proposal until try-accept returns non-#f")
  (make-state
    (lambda (gen)
      (let loop ([g gen])
        (let* ([result (run-state proposal g)]
               [candidate (car result)]
               [g2 (cdr result)]
               [accepted (try-accept candidate)])
          (if accepted
              (cons accepted g2)
              (loop g2)))))))

;;; ============================================================
;;; Distribution Protocols
;;; ============================================================

(doc 'section 'protocols)

(define-protocol (dist-pdf d x)
  "Evaluate probability density (continuous) or mass (discrete) at x")

(define-protocol (dist-cdf d x)
  "Evaluate cumulative distribution function at x")

(define-protocol (dist-quantile d p)
  "Evaluate quantile (inverse CDF) at probability p")

(define-protocol (dist-sample d)
  "Return State monad sampler for this distribution")

;;; ============================================================
;;; Math Helpers
;;; ============================================================

(doc 'section 'math-helpers)

(define (log-gamma x)
  (doc 'export #t)
  (doc 'type '(-> Number Number))
  (doc 'description "Log of the gamma function via Stirling's approximation")
  (if (<= x 0)
      (error 'log-gamma "x must be positive" x)
      (if (< x 7)
          (- (log-gamma (+ x 1)) (log-num x))
          (let* ([log-x (log-num x)]
                 [series (+ (/ 1 (* 12 x))
                            (/ -1 (* 360 x x x))
                            (/ 1 (* 1260 x x x x x)))])
            (+ (* (- x 0.5) log-x)
               (- x)
               (* 0.5 (log-num (* 2 (pi-value))))
               series)))))

(define (factorial n)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat))
  (doc 'description "Factorial of n")
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(define (binomial-coeff n k)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat))
  (doc 'description "Binomial coefficient C(n,k)")
  (if (or (< k 0) (> k n))
      0
      (/ (factorial n) (* (factorial k) (factorial (- n k))))))

(define (positive? x) (> x 0))

;;; ============================================================
;;; Standard Normal (standalone, used internally by other dists)
;;; ============================================================

(doc 'section 'standard-normal)

(doc random-normal-standard 'export #t)
(doc random-normal-standard 'type '(State RNG Number))
(doc random-normal-standard 'description "State monad sampler for standard normal via Box-Muller")
(define random-normal-standard
  (state-bind random-float
    (lambda (u1)
      (state-bind random-float
        (lambda (u2)
          (let* ([r (sqrt (* -2 (safe-log u1)))]
                 [theta (* 2 (pi-value) u2)])
            (state-pure (* r (cos-num theta)))))))))

(doc random-normal-pair 'export #t)
(doc random-normal-pair 'type '(State RNG (Pair Number Number)))
(doc random-normal-pair 'description "State monad sampler for pair of standard normals via Box-Muller")
(define random-normal-pair
  (state-bind random-float
    (lambda (u1)
      (state-bind random-float
        (lambda (u2)
          (let* ([r (sqrt (* -2 (safe-log u1)))]
                 [theta (* 2 (pi-value) u2)])
            (state-pure (cons (* r (cos-num theta))
                              (* r (sin-num theta))))))))))

(define *erfc-coeffs* '(0.254829592 -0.284496736 1.421413741 -1.453152027 1.061405429))
(define *erfc-p* 0.3275911)

(define (standard-normal-cdf x)
  (doc 'export #t)
  (doc 'type '(-> Number Number))
  (doc 'description "Standard normal CDF via Abramowitz & Stegun erfc approximation")
  (let* ([sign (if (< x 0) -1 1)]
         [x (abs x)]
         [t (/ 1 (+ 1 (* *erfc-p* (/ x (sqrt 2)))))]
         [y (- 1 (* (horner-eval *erfc-coeffs* t) t (exp-num (* -0.5 x x))))])
    (* 0.5 (+ 1 (* sign y)))))

(define (standard-normal-pdf x)
  (doc 'export #t)
  (doc 'type '(-> Number Number))
  (doc 'description "Standard normal probability density")
  (* (/ 1 (sqrt (* 2 (pi-value)))) (exp-num (* -0.5 x x))))

(define *quantile-num-coeffs* '(2.515517 0.802853 0.010328))
(define *quantile-den-coeffs* '(1 1.432788 0.189269 0.001308))

(define (standard-normal-quantile p)
  (doc 'export #t)
  (doc 'type '(-> Number Number))
  (doc 'description "Standard normal quantile via rational approximation")
  (if (or (<= p 0) (>= p 1))
      (error 'standard-normal-quantile "p must be in (0,1)" p)
      (let* ([sign (if (< p 0.5) -1 1)]
             [p* (if (< p 0.5) p (- 1 p))]
             [t (sqrt (* -2 (log-num p*)))])
        (* sign (- t (/ (horner-eval *quantile-num-coeffs* t)
                        (horner-eval *quantile-den-coeffs* t)))))))

;;; ============================================================
;;; Normal Distribution
;;; ============================================================

(doc 'section 'normal-distribution)

(define (normal mean stddev)
  (doc 'export #t)
  (doc 'type '(-> Number Number Distribution))
  (doc 'description "Create a normal distribution N(mean, stddev)")
  (list 'normal mean stddev))

(define (normal-pdf x mean stddev)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "Normal probability density at x")
  (let* ([z (/ (- x mean) stddev)]
         [coef (/ 1 (* stddev (sqrt (* 2 (pi-value)))))])
    (* coef (exp-num (* -0.5 z z)))))

(define (normal-cdf x mean stddev)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "Normal cumulative distribution at x")
  (standard-normal-cdf (/ (- x mean) stddev)))

(define (normal-quantile p mean stddev)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "Normal quantile at probability p")
  (+ mean (* stddev (standard-normal-quantile p))))

(define (random-normal mean stddev)
  (doc 'export #t)
  (doc 'type '(-> Number Number (State RNG Number)))
  (doc 'description "State monad sampler for normal distribution")
  (if (<= stddev 0)
      (error 'random-normal "stddev must be positive" stddev)
      (state-map (lambda (z) (+ mean (* stddev z)))
                 random-normal-standard)))

(implement-protocol! 'dist-pdf 'normal
  (lambda (d x) (normal-pdf x (cadr d) (caddr d))))
(implement-protocol! 'dist-cdf 'normal
  (lambda (d x) (normal-cdf x (cadr d) (caddr d))))
(implement-protocol! 'dist-quantile 'normal
  (lambda (d p) (normal-quantile p (cadr d) (caddr d))))
(implement-protocol! 'dist-sample 'normal
  (lambda (d) (random-normal (cadr d) (caddr d))))

;;; ============================================================
;;; Exponential Distribution
;;; ============================================================

(doc 'section 'exponential-distribution)

(define (exponential rate)
  (doc 'export #t)
  (doc 'type '(-> Number Distribution))
  (doc 'description "Create an exponential distribution Exp(rate)")
  (list 'exponential rate))

(define (exponential-pdf x rate)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number))
  (doc 'description "Exponential probability density at x")
  (if (< x 0) 0 (* rate (exp-num (* (- rate) x)))))

(define (exponential-cdf x rate)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number))
  (doc 'description "Exponential cumulative distribution at x")
  (if (< x 0) 0 (- 1 (exp-num (* (- rate) x)))))

(define (exponential-quantile p rate)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number))
  (doc 'description "Exponential quantile at probability p")
  (if (or (< p 0) (> p 1))
      (error 'exponential-quantile "p must be in [0,1]" p)
      (if (= p 1) +inf.0 (/ (- (log-num (- 1 p))) rate))))

(define (random-exponential rate)
  (doc 'export #t)
  (doc 'type '(-> Number (State RNG Number)))
  (doc 'description "State monad sampler for exponential distribution")
  (if (<= rate 0)
      (error 'random-exponential "rate must be positive" rate)
      (state-map (lambda (u) (/ (- (safe-log u)) rate))
                 random-float)))

(implement-protocol! 'dist-pdf 'exponential
  (lambda (d x) (exponential-pdf x (cadr d))))
(implement-protocol! 'dist-cdf 'exponential
  (lambda (d x) (exponential-cdf x (cadr d))))
(implement-protocol! 'dist-quantile 'exponential
  (lambda (d p) (exponential-quantile p (cadr d))))
(implement-protocol! 'dist-sample 'exponential
  (lambda (d) (random-exponential (cadr d))))

;;; ============================================================
;;; Uniform Distribution
;;; ============================================================

(doc 'section 'uniform-distribution)

(define (uniform a b)
  (doc 'export #t)
  (doc 'type '(-> Number Number Distribution))
  (doc 'description "Create a uniform distribution on [a, b]")
  (list 'uniform a b))

(define (uniform-pdf x a b)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "Uniform probability density at x")
  (if (or (< x a) (> x b)) 0 (/ 1 (- b a))))

(define (uniform-cdf x a b)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "Uniform cumulative distribution at x")
  (cond
    [(< x a) 0]
    [(> x b) 1]
    [else (/ (- x a) (- b a))]))

(define (uniform-quantile p a b)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "Uniform quantile at probability p")
  (if (or (< p 0) (> p 1))
      (error 'uniform-quantile "p must be in [0,1]" p)
      (+ a (* p (- b a)))))

(implement-protocol! 'dist-pdf 'uniform
  (lambda (d x) (uniform-pdf x (cadr d) (caddr d))))
(implement-protocol! 'dist-cdf 'uniform
  (lambda (d x) (uniform-cdf x (cadr d) (caddr d))))
(implement-protocol! 'dist-quantile 'uniform
  (lambda (d p) (uniform-quantile p (cadr d) (caddr d))))
(implement-protocol! 'dist-sample 'uniform
  (lambda (d) (random-float-range (cadr d) (caddr d))))

;;; ============================================================
;;; Bernoulli Distribution
;;; ============================================================

(doc 'section 'bernoulli-distribution)

(define (bernoulli p)
  (doc 'export #t)
  (doc 'type '(-> Number Distribution))
  (doc 'description "Create a Bernoulli distribution with probability p")
  (list 'bernoulli p))

(define (bernoulli-pmf success? p)
  (doc 'export #t)
  (doc 'type '(-> Bool Number Number))
  (doc 'description "Bernoulli probability mass")
  (if success? p (- 1 p)))

(define (random-bernoulli p)
  (doc 'export #t)
  (doc 'type '(-> Number (State RNG Bool)))
  (doc 'description "State monad sampler for Bernoulli distribution")
  (if (or (< p 0) (> p 1))
      (error 'random-bernoulli "probability must be in [0,1]" p)
      (state-map (lambda (u) (< u p)) random-float)))

(implement-protocol! 'dist-pdf 'bernoulli
  (lambda (d x) (bernoulli-pmf x (cadr d))))
(implement-protocol! 'dist-sample 'bernoulli
  (lambda (d) (random-bernoulli (cadr d))))

;;; ============================================================
;;; Geometric Distribution
;;; ============================================================

(doc 'section 'geometric-distribution)

(define (geometric p)
  (doc 'export #t)
  (doc 'type '(-> Number Distribution))
  (doc 'description "Create a geometric distribution with success probability p")
  (list 'geometric p))

(define (geometric-pmf k p)
  (doc 'export #t)
  (doc 'type '(-> Nat Number Number))
  (doc 'description "Geometric probability mass at k")
  (if (< k 1) 0 (* (expt (- 1 p) (- k 1)) p)))

(define (geometric-cdf k p)
  (doc 'export #t)
  (doc 'type '(-> Nat Number Number))
  (doc 'description "Geometric cumulative distribution at k")
  (if (< k 1) 0 (- 1 (expt (- 1 p) k))))

(define (geometric-quantile p prob)
  (doc 'export #t)
  (doc 'type '(-> Number Number Nat))
  (doc 'description "Geometric quantile at probability p")
  (if (or (< p 0) (> p 1))
      (error 'geometric-quantile "p must be in [0,1]" p)
      (if (= p 0) 1
          (ceiling (/ (log-num (- 1 p))
                      (log-num (- 1 prob)))))))

(define (random-geometric p)
  (doc 'export #t)
  (doc 'type '(-> Number (State RNG Nat)))
  (doc 'description "State monad sampler for geometric distribution")
  (if (or (<= p 0) (> p 1))
      (error 'random-geometric "p must be in (0,1]" p)
      (if (= p 1)
          (state-pure 1)
          (state-map (lambda (u)
                       (ceiling (/ (safe-log u) (log-num (- 1 p)))))
                     random-float))))

(implement-protocol! 'dist-pdf 'geometric
  (lambda (d x) (geometric-pmf x (cadr d))))
(implement-protocol! 'dist-cdf 'geometric
  (lambda (d x) (geometric-cdf x (cadr d))))
(implement-protocol! 'dist-quantile 'geometric
  (lambda (d p) (geometric-quantile p (cadr d))))
(implement-protocol! 'dist-sample 'geometric
  (lambda (d) (random-geometric (cadr d))))

;;; ============================================================
;;; Poisson Distribution
;;; ============================================================

(doc 'section 'poisson-distribution)

(define (poisson rate)
  (doc 'export #t)
  (doc 'type '(-> Number Distribution))
  (doc 'description "Create a Poisson distribution with given rate")
  (list 'poisson rate))

(define (poisson-pmf k rate)
  (doc 'export #t)
  (doc 'type '(-> Nat Number Number))
  (doc 'description "Poisson probability mass at k")
  (if (< k 0)
      0
      (/ (* (expt rate k) (exp-num (- rate)))
         (factorial k))))

(define (poisson-cdf k rate)
  (doc 'export #t)
  (doc 'type '(-> Nat Number Number))
  (doc 'description "Poisson cumulative distribution at k")
  (if (< k 0)
      0
      (let loop ([i 0] [sum 0])
        (if (> i k)
            sum
            (loop (+ i 1) (+ sum (poisson-pmf i rate)))))))

(define (random-poisson rate)
  (doc 'export #t)
  (doc 'type '(-> Number (State RNG Nat)))
  (doc 'description "State monad sampler for Poisson distribution")
  (if (<= rate 0)
      (error 'random-poisson "rate must be positive" rate)
      (if (< rate 30)
          ;; Knuth's algorithm for small rate
          (let ([L (exp-num (- rate))])
            (make-state
              (lambda (gen)
                (let loop ([k -1] [p 1.0] [g gen])
                  (let* ([result (run-state random-float g)]
                         [u (car result)]
                         [new-g (cdr result)]
                         [new-p (* p u)]
                         [new-k (+ k 1)])
                    (if (> new-p L)
                        (loop new-k new-p new-g)
                        (cons new-k new-g)))))))
          ;; Rejection method for large rate
          (let* ([sqrt-rate (sqrt rate)]
                 [log-rate (log-num rate)]
                 [mode (inexact->exact (floor rate))]
                 [c (- (* mode log-rate) (log-gamma (+ mode 1)))])
            (rejection-sampler
              (state-bind random-normal-standard
                (lambda (z)
                  (state-bind random-float
                    (lambda (u)
                      (let ([k (max 0 (inexact->exact
                                        (floor (+ rate (* sqrt-rate z) 0.5))))])
                        (state-pure (cons k u)))))))
              (lambda (ku)
                (let* ([k (car ku)]
                       [u (cdr ku)]
                       [log-pk (- (* k log-rate) (log-gamma (+ k 1)) c)])
                  (and (< (safe-log u) log-pk) k))))))))

(implement-protocol! 'dist-pdf 'poisson
  (lambda (d x) (poisson-pmf x (cadr d))))
(implement-protocol! 'dist-cdf 'poisson
  (lambda (d x) (poisson-cdf x (cadr d))))
(implement-protocol! 'dist-sample 'poisson
  (lambda (d) (random-poisson (cadr d))))

;;; ============================================================
;;; Binomial Distribution
;;; ============================================================

(doc 'section 'binomial-distribution)

(define (binomial n p)
  (doc 'export #t)
  (doc 'type '(-> Nat Number Distribution))
  (doc 'description "Create a binomial distribution B(n, p)")
  (list 'binomial n p))

(define (binomial-pmf k n p)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Number Number))
  (doc 'description "Binomial probability mass at k")
  (if (or (< k 0) (> k n))
      0
      (* (binomial-coeff n k)
         (expt p k)
         (expt (- 1 p) (- n k)))))

(define (random-binomial n p)
  (doc 'export #t)
  (doc 'type '(-> Nat Number (State RNG Nat)))
  (doc 'description "State monad sampler for binomial distribution")
  (cond
    [(< n 0) (error 'random-binomial "n must be non-negative" n)]
    [(or (< p 0) (> p 1)) (error 'random-binomial "p must be in [0,1]" p)]
    [(= n 0) (state-pure 0)]
    [(= p 0) (state-pure 0)]
    [(= p 1) (state-pure n)]
    [(< n 20)
     ;; Direct summation for small n
     (letrec ([sum-trials
                (lambda (k successes)
                  (if (= k 0)
                      (state-pure successes)
                      (state-bind (random-bernoulli p)
                        (lambda (success?)
                          (sum-trials (- k 1)
                                     (if success? (+ successes 1) successes))))))])
       (sum-trials n 0))]
    [else
     ;; Normal approximation for large n
     (let* ([mean (* n p)]
            [stddev (sqrt (* n p (- 1 p)))])
       (state-map (lambda (x)
                    (min n (max 0 (inexact->exact (round x)))))
                  (random-normal mean stddev)))]))

(implement-protocol! 'dist-pdf 'binomial
  (lambda (d x) (binomial-pmf x (cadr d) (caddr d))))
(implement-protocol! 'dist-sample 'binomial
  (lambda (d) (random-binomial (cadr d) (caddr d))))

;;; ============================================================
;;; Gamma Distribution
;;; ============================================================

(doc 'section 'gamma-distribution)

(define (gamma shape rate)
  (doc 'export #t)
  (doc 'type '(-> Number Number Distribution))
  (doc 'description "Create a gamma distribution Gamma(shape, rate)")
  (list 'gamma shape rate))

(define (random-gamma shape rate)
  (doc 'export #t)
  (doc 'type '(-> Number Number (State RNG Number)))
  (doc 'description "State monad sampler for gamma distribution")
  (cond
    [(<= shape 0) (error 'random-gamma "shape must be positive" shape)]
    [(<= rate 0) (error 'random-gamma "rate must be positive" rate)]
    [(< shape 1)
     ;; Boost for shape < 1: X ~ Gamma(shape+1, 1), then X * U^(1/shape)
     (state-bind (random-gamma (+ shape 1) 1)
       (lambda (x)
         (state-bind random-float
           (lambda (u)
             (state-pure
               (/ (* x (expt (max u 1e-300) (/ 1 shape))) rate))))))]
    [else
     ;; Marsaglia & Tsang for shape >= 1
     (let* ([d (- shape (/ 1 3))]
            [c (/ 1 (sqrt (* 9 d)))])
       (rejection-sampler
         (state-bind random-normal-standard
           (lambda (z)
             (let ([v (+ 1 (* c z))])
               (if (<= v 0)
                   (state-pure #f)
                   (state-bind random-float
                     (lambda (u) (state-pure (list z u v))))))))
         (lambda (zuv)
           (and zuv
                (let* ([z (car zuv)] [u (cadr zuv)] [v (caddr zuv)]
                       [v3 (* v v v)])
                  (and (or (< u (- 1 (* 0.0331 z z z z)))
                           (< (safe-log u)
                              (+ (* 0.5 z z)
                                 (* d (- 1 v3 (log-num v3))))))
                       (/ (* d v3) rate)))))))]))

(implement-protocol! 'dist-sample 'gamma
  (lambda (d) (random-gamma (cadr d) (caddr d))))

;;; ============================================================
;;; Beta Distribution
;;; ============================================================

(doc 'section 'beta-distribution)

(define (beta alpha beta-param)
  (doc 'export #t)
  (doc 'type '(-> Number Number Distribution))
  (doc 'description "Create a beta distribution Beta(alpha, beta)")
  (list 'beta alpha beta-param))

(define (random-beta alpha beta)
  (doc 'export #t)
  (doc 'type '(-> Number Number (State RNG Number)))
  (doc 'description "State monad sampler for beta distribution")
  (cond
    [(<= alpha 0) (error 'random-beta "alpha must be positive" alpha)]
    [(<= beta 0) (error 'random-beta "beta must be positive" beta)]
    [else
     (state-bind (random-gamma alpha 1)
       (lambda (x)
         (state-bind (random-gamma beta 1)
           (lambda (y)
             (state-pure (/ x (+ x y)))))))]))

(implement-protocol! 'dist-sample 'beta
  (lambda (d) (random-beta (cadr d) (caddr d))))

;;; ============================================================
;;; Categorical Distribution
;;; ============================================================

(doc 'section 'categorical-distribution)

(define (categorical weights)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Distribution))
  (doc 'description "Create a categorical distribution over indices with given weights")
  (list 'categorical weights))

(define (random-categorical weights)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (State RNG Nat)))
  (doc 'description "State monad sampler for categorical distribution")
  (if (null? weights)
      (error 'random-categorical "empty weight list")
      (let ([total (fold-left + 0 weights)])
        (if (<= total 0)
            (error 'random-categorical "weights must sum to positive" total)
            (state-bind (random-float-range 0 total)
              (lambda (r)
                (state-pure
                  (let loop ([ws weights] [acc 0] [i 0])
                    (if (null? ws)
                        (- i 1)
                        (let ([new-acc (+ acc (car ws))])
                          (if (< r new-acc)
                              i
                              (loop (cdr ws) new-acc (+ i 1)))))))))))))

(implement-protocol! 'dist-sample 'categorical
  (lambda (d) (random-categorical (cadr d))))

;;; ============================================================
;;; Multivariate (standalone, not protocol-ified)
;;; ============================================================

(doc 'section 'multivariate-distributions)

(define (random-normal-vector n mean stddev)
  (doc 'export #t)
  (doc 'type '(-> Nat Number Number (State RNG (List Number))))
  (doc 'description "State monad sampler for n independent normal samples")
  (random-list n (random-normal mean stddev)))

(define (random-dirichlet alphas)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (State RNG (List Number))))
  (doc 'description "State monad sampler for Dirichlet distribution")
  (if (null? alphas)
      (error 'random-dirichlet "empty alpha list")
      (if (not (andmap positive? alphas))
          (error 'random-dirichlet "all alphas must be positive" alphas)
          (state-bind (state-sequence (map (lambda (a) (random-gamma a 1)) alphas))
            (lambda (gammas)
              (let ([total (fold-left + 0 gammas)])
                (state-pure (map (lambda (g) (/ g total)) gammas))))))))
