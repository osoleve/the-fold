;;; fabric/stitches/random/distributions.ss — Probability Distributions
;;;
;;; Common probability distributions for random sampling.
;;; All functions return State monad computations for composability.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Uniform distributions (already in prng.ss)
;;;   - Normal/Gaussian distribution (Box-Muller)
;;;   - Exponential distribution
;;;   - Poisson distribution
;;;   - Geometric distribution
;;;   - Binomial distribution
;;;   - Bernoulli distribution
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/state.ss
;;;   - fp/transcendental.ss
;;;   - random/prng.ss

(load "core/base/prelude.ss")
(load "core/fp/control/state.ss")
(load "core/fp/numeric/transcendental.ss")
(load "core/random/prng.ss")

;;; ============================================================
;;; Bernoulli Distribution
;;; ============================================================
;;;
;;; Bernoulli(p): Returns #t with probability p, #f with probability 1-p

;;; random-bernoulli : Float -> State GenState Boolean
;;; Sample from Bernoulli(p). Returns #t with probability p.
(define (random-bernoulli p)
  (if (or (< p 0) (> p 1))
      (error 'random-bernoulli "probability must be in [0,1]" p)
      (state-map (lambda (u) (< u p)) random-float)))

;;; ============================================================
;;; Exponential Distribution
;;; ============================================================
;;;
;;; Exponential(λ): Mean = 1/λ, models waiting times

;;; random-exponential : Float -> State GenState Float
;;; Sample from Exponential(rate). Uses inverse transform method.
;;; Mean = 1/rate, Variance = 1/rate^2
(define (random-exponential rate)
  (if (<= rate 0)
      (error 'random-exponential "rate must be positive" rate)
      (state-map (lambda (u)
                         ;; -ln(1-u)/rate, but since u is uniform, so is 1-u
                         ;; We use -ln(u)/rate for simplicity (avoiding u=0 issues)
                         (let ([u-safe (max u 1e-300)])  ; Avoid log(0)
                              (/ (- (log-num u-safe)) rate)))
                 random-float)))

;;; ============================================================
;;; Normal/Gaussian Distribution
;;; ============================================================
;;;
;;; Normal(μ, σ): Bell curve with mean μ and standard deviation σ
;;; Uses Box-Muller transform.

;;; random-normal-standard : State GenState Float
;;; Sample from standard normal N(0, 1) using Box-Muller.
(define random-normal-standard
  (state-bind random-float
              (lambda (u1)
                      (state-bind random-float
                                  (lambda (u2)
                                          ;; Avoid log(0)
                                          (let* ([u1-safe (max u1 1e-300)]
                                                 [r (sqrt (* -2 (log-num u1-safe)))]
                                                 [theta (* 2 (pi-value) u2)]
                                                 [z0 (* r (cos-num theta))])
                                                ;; Note: Box-Muller produces two values (z0, z1)
                                                ;; We only use z0 for simplicity
                                                (state-pure z0)))))))

;;; random-normal : Float x Float -> State GenState Float
;;; Sample from Normal(mean, stddev).
(define (random-normal mean stddev)
  (if (<= stddev 0)
      (error 'random-normal "stddev must be positive" stddev)
      (state-map (lambda (z) (+ mean (* stddev z)))
                 random-normal-standard)))

;;; random-normal-pair : State GenState (Float . Float)
;;; Generate two independent standard normals (more efficient).
(define random-normal-pair
  (state-bind random-float
              (lambda (u1)
                      (state-bind random-float
                                  (lambda (u2)
                                          (let* ([u1-safe (max u1 1e-300)]
                                                 [r (sqrt (* -2 (log-num u1-safe)))]
                                                 [theta (* 2 (pi-value) u2)]
                                                 [z0 (* r (cos-num theta))]
                                                 [z1 (* r (sin-num theta))])
                                                (state-pure (cons z0 z1))))))))

;;; ============================================================
;;; Geometric Distribution
;;; ============================================================
;;;
;;; Geometric(p): Number of trials until first success
;;; P(X = k) = (1-p)^(k-1) * p for k = 1, 2, 3, ...

;;; random-geometric : Float -> State GenState Integer
;;; Sample from Geometric(p). Returns >= 1.
(define (random-geometric p)
  (if (or (<= p 0) (> p 1))
      (error 'random-geometric "p must be in (0,1]" p)
      (if (= p 1)
          (state-pure 1)
          (state-map (lambda (u)
                             ;; Inverse transform: ceil(ln(u) / ln(1-p))
                             (let ([u-safe (max u 1e-300)])
                                  (ceiling (/ (log-num u-safe)
                                              (log-num (- 1 p))))))
                     random-float))))

;;; ============================================================
;;; Poisson Distribution
;;; ============================================================
;;;
;;; Poisson(λ): Count of events in fixed interval, mean = λ
;;; Uses Knuth's algorithm for small λ, rejection for large λ

;;; random-poisson : Float -> State GenState Integer
;;; Sample from Poisson(rate).
(define (random-poisson rate)
  (if (<= rate 0)
      (error 'random-poisson "rate must be positive" rate)
      (if (< rate 30)
          ;; Knuth's algorithm for small rate
          ;; Standard algorithm: k=-1, p=1; do { k++; p*=U } while p>L; return k
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
          ;; Rejection method for large rate (more efficient)
          ;; Using normal approximation with correction
          (let* ([sqrt-rate (sqrt rate)]
                 [log-rate (log-num rate)]
                 [c (- (* rate log-rate) (log-gamma (+ rate 1)))])
                (make-state
                 (lambda (gen)
                         (let loop ([g gen])
                              (let* ([result1 (run-state random-normal-standard g)]
                                     [z (car result1)]
                                     [g1 (cdr result1)]
                                     [k (max 0 (inexact->exact
                                                (floor (+ rate (* sqrt-rate z) 0.5))))]
                                     [result2 (run-state random-float g1)]
                                     [u (car result2)]
                                     [g2 (cdr result2)]
                                     ;; Acceptance probability
                                     [log-pk (- (* k log-rate) (log-gamma (+ k 1)) c)])
                                    (if (< (log-num (max u 1e-300)) log-pk)
                                        (cons k g2)
                                        (loop g2))))))))))

;;; log-gamma : Float -> Float
;;; Natural log of gamma function, using Stirling's approximation.
;;; For positive integers: log-gamma(n+1) = log(n!)
(define (log-gamma x)
  (if (<= x 0)
      (error 'log-gamma "x must be positive" x)
      (if (< x 7)
          ;; Reflection for small values
          (- (log-gamma (+ x 1)) (log-num x))
          ;; Stirling's approximation for larger values
          (let* ([x-0.5 (- x 0.5)]
                 [log-x (log-num x)]
                 [series-term (+ (/ 1 (* 12 x))
                                 (/ -1 (* 360 x x x))
                                 (/ 1 (* 1260 x x x x x)))])
                (+ (* x-0.5 log-x)
                   (- x)
                   (* 0.5 (log-num (* 2 (pi-value))))
                   series-term)))))

;;; ============================================================
;;; Binomial Distribution
;;; ============================================================
;;;
;;; Binomial(n, p): Number of successes in n Bernoulli(p) trials
;;; Mean = n*p, Variance = n*p*(1-p)

;;; random-binomial : Integer x Float -> State GenState Integer
;;; Sample from Binomial(n, p).
(define (random-binomial n p)
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
                                                          (if success?
                                                              (+ successes 1)
                                                              successes))))))])
            (sum-trials n 0))]
   [else
    ;; Use inverse transform with normal approximation
    ;; and rejection for large n
    (let* ([mean (* n p)]
           [stddev (sqrt (* n p (- 1 p)))])
          (make-state
           (lambda (gen)
                   (let loop ([g gen])
                        (let* ([result (run-state (random-normal mean stddev) g)]
                               [x (car result)]
                               [g2 (cdr result)]
                               [k (min n (max 0 (inexact->exact (round x))))])
                              ;; Simple acceptance (normal approx is good for large n)
                              (cons k g2))))))]))

;;; ============================================================
;;; Gamma Distribution
;;; ============================================================
;;;
;;; Gamma(shape, rate): Generalization of exponential
;;; Mean = shape/rate, Variance = shape/rate^2

;;; random-gamma : Float x Float -> State GenState Float
;;; Sample from Gamma(shape, rate) using Marsaglia & Tsang's method.
(define (random-gamma shape rate)
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
                                             (/ (* x (expt (max u 1e-300) (/ 1 shape)))
                                                rate))))))]
   [else
    ;; Marsaglia & Tsang for shape >= 1
    (let* ([d (- shape (/ 1 3))]
           [c (/ 1 (sqrt (* 9 d)))])
          (make-state
           (lambda (gen)
                   (let loop ([g gen])
                        (let* ([result1 (run-state random-normal-standard g)]
                               [z (car result1)]
                               [g1 (cdr result1)]
                               [v (+ 1 (* c z))])
                              (if (<= v 0)
                                  (loop g1)
                                  (let* ([v3 (* v v v)]
                                         [result2 (run-state random-float g1)]
                                         [u (car result2)]
                                         [g2 (cdr result2)]
                                         [u-safe (max u 1e-300)])
                                        (if (or (< u (- 1 (* 0.0331 z z z z)))
                                                (< (log-num u-safe)
                                                   (+ (* 0.5 z z)
                                                      (* d (- 1 v3 (log-num v3))))))
                                            (cons (/ (* d v3) rate) g2)
                                            (loop g2)))))))))]))

;;; ============================================================
;;; Beta Distribution
;;; ============================================================
;;;
;;; Beta(α, β): Distribution on [0, 1]
;;; Mean = α/(α+β)

;;; random-beta : Float x Float -> State GenState Float
;;; Sample from Beta(alpha, beta) using gamma variates.
(define (random-beta alpha beta)
  (cond
   [(<= alpha 0) (error 'random-beta "alpha must be positive" alpha)]
   [(<= beta 0) (error 'random-beta "beta must be positive" beta)]
   [else
    (state-bind (random-gamma alpha 1)
                (lambda (x)
                        (state-bind (random-gamma beta 1)
                                    (lambda (y)
                                            (state-pure (/ x (+ x y)))))))]))

;;; ============================================================
;;; Categorical/Discrete Distribution
;;; ============================================================

;;; random-categorical : List Float -> State GenState Integer
;;; Sample index 0..n-1 according to probability weights.
;;; Weights are normalized automatically.
(define (random-categorical weights)
  (if (null? weights)
      (error 'random-categorical "empty weight list")
      (let* ([total (fold-left + 0 weights)])
            (if (<= total 0)
                (error 'random-categorical "weights must sum to positive" total)
                (state-bind (random-float-range 0 total)
                            (lambda (r)
                                    (state-pure
                                     (let loop ([ws weights] [acc 0] [i 0])
                                          (if (null? ws)
                                              (- i 1)  ; Shouldn't happen, but safe fallback
                                              (let ([new-acc (+ acc (car ws))])
                                                   (if (< r new-acc)
                                                       i
                                                       (loop (cdr ws) new-acc (+ i 1)))))))))))))

;;; ============================================================
;;; Multivariate Distributions
;;; ============================================================

;;; random-normal-vector : Integer x Float x Float -> State GenState (List Float)
;;; Generate n independent normal samples.
(define (random-normal-vector n mean stddev)
  (random-list n (random-normal mean stddev)))

;;; random-dirichlet : List Float -> State GenState (List Float)
;;; Sample from Dirichlet distribution with concentration parameters.
;;; Returns a probability vector that sums to 1.
(define (random-dirichlet alphas)
  (if (null? alphas)
      (error 'random-dirichlet "empty alpha list")
      (if (not (andmap positive? alphas))
          (error 'random-dirichlet "all alphas must be positive" alphas)
          (state-bind (state-sequence (map (lambda (a) (random-gamma a 1)) alphas))
                      (lambda (gammas)
                              (let ([total (fold-left + 0 gammas)])
                                   (state-pure (map (lambda (g) (/ g total)) gammas))))))))

;;; Helper: positive?
(define (positive? x) (> x 0))

;;; Helper: andmap for predicates
(define (andmap pred lst)
  (cond
   [(null? lst) #t]
   [(pred (car lst)) (andmap pred (cdr lst))]
   [else #f]))

;;; ============================================================
;;; Probability Density/Mass Functions (PDF/PMF)
;;; ============================================================

;;; Normal PDF
;;; normal-pdf : Float x Float x Float -> Float
;;; PDF of Normal(mean, stddev) at x.
(define (normal-pdf x mean stddev)
  (let* ([z (/ (- x mean) stddev)]
         [coef (/ 1 (* stddev (sqrt (* 2 (pi-value)))))])
        (* coef (exp-num (* -0.5 z z)))))

;;; Standard normal PDF
(define (standard-normal-pdf x)
  (normal-pdf x 0 1))

;;; Exponential PDF
;;; exponential-pdf : Float x Float -> Float
;;; PDF of Exponential(rate) at x.
(define (exponential-pdf x rate)
  (if (< x 0)
      0
      (* rate (exp-num (* (- rate) x)))))

;;; Uniform PDF
;;; uniform-pdf : Float x Float x Float -> Float
;;; PDF of Uniform(a, b) at x.
(define (uniform-pdf x a b)
  (if (or (< x a) (> x b))
      0
      (/ 1 (- b a))))

;;; Poisson PMF
;;; poisson-pmf : Integer x Float -> Float
;;; PMF of Poisson(rate) at k.
(define (poisson-pmf k rate)
  (if (< k 0)
      0
      (/ (* (expt rate k) (exp-num (- rate)))
         (factorial k))))

;;; Binomial PMF
;;; binomial-pmf : Integer x Integer x Float -> Float
;;; PMF of Binomial(n, p) at k.
(define (binomial-pmf k n p)
  (if (or (< k 0) (> k n))
      0
      (* (binomial-coeff n k)
         (expt p k)
         (expt (- 1 p) (- n k)))))

;;; Geometric PMF
;;; geometric-pmf : Integer x Float -> Float
;;; PMF of Geometric(p) at k (number of trials until first success).
;;; k >= 1
(define (geometric-pmf k p)
  (if (< k 1)
      0
      (* (expt (- 1 p) (- k 1)) p)))

;;; Bernoulli PMF
;;; bernoulli-pmf : Boolean x Float -> Float
(define (bernoulli-pmf success? p)
  (if success? p (- 1 p)))

;;; ============================================================
;;; Cumulative Distribution Functions (CDF)
;;; ============================================================

;;; Exponential CDF
;;; exponential-cdf : Float x Float -> Float
;;; CDF of Exponential(rate) at x.
(define (exponential-cdf x rate)
  (if (< x 0)
      0
      (- 1 (exp-num (* (- rate) x)))))

;;; Uniform CDF
;;; uniform-cdf : Float x Float x Float -> Float
;;; CDF of Uniform(a, b) at x.
(define (uniform-cdf x a b)
  (cond
   [(< x a) 0]
   [(> x b) 1]
   [else (/ (- x a) (- b a))]))

;;; Geometric CDF
;;; geometric-cdf : Integer x Float -> Float
;;; CDF of Geometric(p) at k.
(define (geometric-cdf k p)
  (if (< k 1)
      0
      (- 1 (expt (- 1 p) k))))

;;; Standard Normal CDF (Approximation)
;;; Uses Abramowitz and Stegun approximation (7.1.26)
;;; Accurate to about 1.5e-7
(define (standard-normal-cdf x)
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
                    t (exp-num (* -0.5 x x))))])
        (* 0.5 (+ 1 (* sign y)))))

;;; Normal CDF
;;; normal-cdf : Float x Float x Float -> Float
(define (normal-cdf x mean stddev)
  (standard-normal-cdf (/ (- x mean) stddev)))

;;; Poisson CDF
;;; poisson-cdf : Integer x Float -> Float
;;; CDF of Poisson(rate) at k (sum of PMF from 0 to k).
(define (poisson-cdf k rate)
  (if (< k 0)
      0
      (let loop ([i 0] [sum 0])
           (if (> i k)
               sum
               (loop (+ i 1) (+ sum (poisson-pmf i rate)))))))

;;; ============================================================
;;; Quantile Functions (Inverse CDF)
;;; ============================================================

;;; Exponential Quantile
;;; exponential-quantile : Float x Float -> Float
;;; Quantile (inverse CDF) of Exponential(rate) at p.
(define (exponential-quantile p rate)
  (if (or (< p 0) (> p 1))
      (error 'exponential-quantile "p must be in [0,1]" p)
      (if (= p 1)
          +inf.0  ; infinity
          (/ (- (log-num (- 1 p))) rate))))

;;; Uniform Quantile
;;; uniform-quantile : Float x Float x Float -> Float
(define (uniform-quantile p a b)
  (if (or (< p 0) (> p 1))
      (error 'uniform-quantile "p must be in [0,1]" p)
      (+ a (* p (- b a)))))

;;; Geometric Quantile
;;; geometric-quantile : Float x Float -> Integer
(define (geometric-quantile p prob)
  (if (or (< p 0) (> p 1))
      (error 'geometric-quantile "p must be in [0,1]" p)
      (if (= p 0)
          1
          (ceiling (/ (log-num (- 1 p))
                      (log-num (- 1 prob)))))))

;;; Standard Normal Quantile (Approximation)
;;; Uses Rational Approximation (Abramowitz and Stegun)
;;; Accurate for 0 < p < 1
(define (standard-normal-quantile p)
  (if (or (<= p 0) (>= p 1))
      (error 'standard-normal-quantile "p must be in (0,1)" p)
      (let* ([a0 2.515517]
             [a1 0.802853]
             [a2 0.010328]
             [b1 1.432788]
             [b2 0.189269]
             [b3 0.001308]
             [sign (if (< p 0.5) -1 1)]
             [p* (if (< p 0.5) p (- 1 p))]
             [t (sqrt (* -2 (log-num p*)))]
             [num (+ a0 (* a1 t) (* a2 t t))]
             [den (+ 1 (* b1 t) (* b2 t t) (* b3 t t t))])
            (* sign (- t (/ num den))))))

;;; Normal Quantile
;;; normal-quantile : Float x Float x Float -> Float
(define (normal-quantile p mean stddev)
  (+ mean (* stddev (standard-normal-quantile p))))

;;; ============================================================
;;; Helper Functions for Distributions
;;; ============================================================

;;; factorial : Integer -> Integer
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;;; binomial-coeff : Integer x Integer -> Integer
;;; C(n, k) = n! / (k! * (n-k)!)
(define (binomial-coeff n k)
  (if (or (< k 0) (> k n))
      0
      (/ (factorial n)
         (* (factorial k) (factorial (- n k))))))

