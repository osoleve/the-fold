;;; fabric/stitches/random/probability.ss — Probability Monad
;;;
;;; A monadic framework for probabilistic programming.
;;; Enables compositional construction of probabilistic models
;;; with support for conditioning, inference, and sampling.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Prob monad for probabilistic computations
;;;   - Conditioning via rejection sampling
;;;   - Importance weighting
;;;   - Inference primitives (sample-once, sample-many, expectation)
;;;   - Independence via Applicative
;;;   - Integration with distributions library
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/state.ss
;;;   - random/prng.ss
;;;   - random/distributions.ss

(load "core/base/prelude.ss")
(load "lattice/fp/control/state.ss")
(load "lattice/random/prng.ss")
(load "lattice/random/distributions.ss")

;;; ============================================================
;;; Probability Monad Representation
;;; ============================================================
;;;
;;; A Prob computation is a weighted State computation over PRNG state.
;;; We track likelihood weights for importance sampling and conditioning.
;;;
;;; Prob a = State (PRNG, Weight) a
;;;
;;; where Weight is the accumulated importance weight (log-space for numerical stability).

;;; The internal state is (prng . log-weight)

;;; make-prob : (State (PRNG . LogWeight) a) -> Prob a
;;; Tag a State computation as a probability computation.
(define (make-prob state-comp)
  (cons 'prob state-comp))

;;; prob? : Any -> Boolean
(define (prob? x)
  (and (pair? x) (eq? (car x) 'prob)))

;;; prob-state : Prob a -> State (PRNG . LogWeight) a
(define (prob-state p)
  (cdr p))

;;; ============================================================
;;; Internal State Manipulation
;;; ============================================================

;;; prob-get-prng : State (PRNG . Weight) PRNG
(define prob-get-prng
  (state-gets car))

;;; prob-put-prng : PRNG -> State (PRNG . Weight) ()
(define (prob-put-prng new-prng)
  (state-modify (lambda (s) (cons new-prng (cdr s)))))

;;; prob-get-weight : State (PRNG . Weight) Float
(define prob-get-weight
  (state-gets cdr))

;;; prob-add-weight : Float -> State (PRNG . Weight) ()
;;; Add to the log-weight (equivalent to multiplying in linear space).
(define (prob-add-weight log-w)
  (state-modify (lambda (s) (cons (car s) (+ (cdr s) log-w)))))

;;; ============================================================
;;; Running Probability Computations
;;; ============================================================

;;; run-prob : Prob a -> PRNG -> ((a . LogWeight) . PRNG)
;;; Run a probabilistic computation, returning value, log-weight, and final PRNG.
(define (run-prob p prng)
  (let ([result (run-state (prob-state p) (cons prng 0.0))])
       (let ([value (car result)]
             [final-state (cdr result)])
            (cons (cons value (cdr final-state))
                  (car final-state)))))

;;; sample-prob : Prob a -> PRNG -> a
;;; Run and extract just the sampled value.
(define (sample-prob p prng)
  (car (car (run-prob p prng))))

;;; weight-prob : Prob a -> PRNG -> Float
;;; Run and extract the log-weight.
(define (weight-prob p prng)
  (cdr (car (run-prob p prng))))

;;; ============================================================
;;; Monad Operations
;;; ============================================================

;;; prob-pure : a -> Prob a
;;; Lift a value into Prob (deterministic).
(define (prob-pure x)
  (make-prob (state-pure x)))

;;; prob-bind : Prob a -> (a -> Prob b) -> Prob b
;;; Monadic bind for Prob.
(define (prob-bind p f)
  (make-prob
   (state-bind (prob-state p)
               (lambda (a)
                       (prob-state (f a))))))

;;; prob-then : Prob a -> Prob b -> Prob b
;;; Sequence, discarding first result.
(define (prob-then p1 p2)
  (prob-bind p1 (lambda (_) p2)))

;;; prob-map : (a -> b) -> Prob a -> Prob b
;;; Functor map for Prob.
(define (prob-map f p)
  (prob-bind p (lambda (a) (prob-pure (f a)))))

;;; prob-ap : Prob (a -> b) -> Prob a -> Prob b
;;; Applicative apply for Prob (independent sampling).
(define (prob-ap pf pa)
  (prob-bind pf
             (lambda (f)
                     (prob-bind pa
                                (lambda (a)
                                        (prob-pure (f a)))))))

;;; ============================================================
;;; Sampling from Distributions
;;; ============================================================
;;;
;;; Convert State-based distributions to Prob computations.

;;; sample : State PRNG a -> Prob a
;;; Lift a PRNG State computation into Prob.
(define (sample dist)
  (make-prob
   (make-state
    (lambda (ps)
            (let* ([prng (car ps)]
                   [weight (cdr ps)]
                   [result (run-state dist prng)]
                   [value (car result)]
                   [new-prng (cdr result)])
                  (cons value (cons new-prng weight)))))))

;;; Convenient samplers wrapping the distributions library

;;; prob-uniform : Prob Float
;;; Sample uniform [0,1).
(define prob-uniform
  (sample random-float))

;;; prob-uniform-range : Float -> Float -> Prob Float
(define (prob-uniform-range lo hi)
  (sample (random-range lo hi)))

;;; prob-uniform-int : Int -> Int -> Prob Int
(define (prob-uniform-int lo hi)
  (sample (random-int-range lo hi)))

;;; prob-bernoulli : Float -> Prob Boolean
(define (prob-bernoulli p)
  (sample (random-bernoulli p)))

;;; prob-normal : Float -> Float -> Prob Float
(define (prob-normal mean stddev)
  (sample (random-normal mean stddev)))

;;; prob-exponential : Float -> Prob Float
(define (prob-exponential rate)
  (sample (random-exponential rate)))

;;; prob-poisson : Float -> Prob Int
(define (prob-poisson rate)
  (sample (random-poisson rate)))

;;; prob-geometric : Float -> Prob Int
(define (prob-geometric p)
  (sample (random-geometric p)))

;;; prob-binomial : Int -> Float -> Prob Int
(define (prob-binomial n p)
  (sample (random-binomial n p)))

;;; prob-gamma : Float -> Float -> Prob Float
(define (prob-gamma shape rate)
  (sample (random-gamma shape rate)))

;;; prob-beta : Float -> Float -> Prob Float
(define (prob-beta a b)
  (sample (random-beta a b)))

;;; prob-categorical : (List Float) -> Prob Int
(define (prob-categorical weights)
  (sample (random-categorical weights)))

;;; ============================================================
;;; Conditioning
;;; ============================================================
;;;
;;; Conditioning is the core of probabilistic inference.
;;; We support two styles:
;;; 1. Hard conditioning (rejection): condition on a boolean predicate
;;; 2. Soft conditioning (scoring): add log-probability to weight

;;; factor : Float -> Prob ()
;;; Add a log-probability factor (soft conditioning).
;;; factor(w) is equivalent to observing with likelihood exp(w).
(define (factor log-prob)
  (make-prob (prob-add-weight log-prob)))

;;; score : Float -> Prob ()
;;; Add a likelihood (in linear space).
;;; score(p) = factor(log(p))
(define (score likelihood)
  (if (<= likelihood 0)
      (factor -inf.0)  ; Zero probability
      (factor (log-num likelihood))))

;;; observe : (a -> Float) -> a -> Prob ()
;;; Observe a value with a given likelihood function.
;;; E.g., (observe (normal-pdf 0 1) 0.5) conditions on seeing 0.5 from N(0,1).
(define (observe likelihood-fn observed)
  (score (likelihood-fn observed)))

;;; observe-dist : (a -> Float) -> Prob a -> Prob a
;;; Observe the result of a probabilistic computation.
(define (observe-dist likelihood-fn p)
  (prob-bind p
             (lambda (x)
                     (prob-then (score (likelihood-fn x))
                                (prob-pure x)))))

;;; condition : (a -> Boolean) -> Prob a -> Prob a
;;; Hard conditioning via likelihood. Returns same value but
;;; sets weight to -inf if condition fails.
(define (condition pred p)
  (prob-bind p
             (lambda (x)
                     (if (pred x)
                         (prob-pure x)
                         (prob-then (factor -inf.0)
                                    (prob-pure x))))))

;;; assume : Boolean -> Prob ()
;;; Assert a condition (hard constraint).
(define (assume test)
  (if test
      (prob-pure '())
      (factor -inf.0)))

;;; ============================================================
;;; Inference Primitives
;;; ============================================================

;;; sample-once : Prob a -> Int -> a
;;; Draw a single sample with given seed.
(define (sample-once p seed)
  (sample-prob p (make-pcg seed 1)))

;;; sample-many : Prob a -> Int -> Int -> (List a)
;;; Draw n samples with given seed.
(define (sample-many p seed n)
  (if (<= n 0)
      '()
      (let loop ([prng (make-pcg seed 1)] [count n] [acc '()])
           (if (= count 0)
               (reverse acc)
               (let* ([result (run-prob p prng)]
                      [value (car (car result))]
                      [new-prng (cdr result)])
                     (loop new-prng (- count 1) (cons value acc)))))))

;;; weighted-samples : Prob a -> Int -> Int -> (List (a . Float))
;;; Draw n weighted samples (value . log-weight pairs).
(define (weighted-samples p seed n)
  (if (<= n 0)
      '()
      (let loop ([prng (make-pcg seed 1)] [count n] [acc '()])
           (if (= count 0)
               (reverse acc)
               (let* ([result (run-prob p prng)]
                      [value (car (car result))]
                      [log-w (cdr (car result))]
                      [new-prng (cdr result)])
                     (loop new-prng (- count 1) (cons (cons value log-w) acc)))))))

;;; ============================================================
;;; Importance Sampling Inference
;;; ============================================================

;;; log-sum-exp : (List Float) -> Float
;;; Numerically stable log(sum(exp(xs))).
(define (log-sum-exp xs)
  (if (null? xs)
      -inf.0
      (let ([max-x (apply max xs)])
           (if (= max-x -inf.0)
               -inf.0
               (+ max-x (log-num (fold-left + 0 (map (lambda (x) (exp-num (- x max-x))) xs))))))))

;;; normalize-log-weights : (List Float) -> (List Float)
;;; Normalize log-weights so they sum to 1 (in log-space: log-sum = 0).
(define (normalize-log-weights log-ws)
  (let ([total (log-sum-exp log-ws)])
       (if (= total -inf.0)
           (map (lambda (_) (/ 1.0 (length log-ws))) log-ws)  ; Uniform if all -inf
           (map (lambda (lw) (- lw total)) log-ws))))

;;; importance-expectation : (a -> Float) -> Prob a -> Int -> Int -> Float
;;; Estimate E[f(X)] using importance sampling with n samples.
(define (importance-expectation f p seed n)
  (let* ([samples (weighted-samples p seed n)]
         [log-ws (map cdr samples)]
         [values (map car samples)]
         [norm-log-ws (normalize-log-weights log-ws)]
         [weights (map exp-num norm-log-ws)]
         [f-values (map f values)])
        (fold-left + 0 (map * f-values weights))))

;;; importance-mean : Prob Float -> Int -> Int -> Float
;;; Estimate the mean of a real-valued Prob.
(define (importance-mean p seed n)
  (importance-expectation (lambda (x) x) p seed n))

;;; importance-variance : Prob Float -> Int -> Int -> Float
;;; Estimate the variance of a real-valued Prob.
(define (importance-variance p seed n)
  (let ([mean (importance-mean p seed n)])
       (importance-expectation (lambda (x) (* (- x mean) (- x mean))) p seed n)))

;;; ============================================================
;;; Rejection Sampling
;;; ============================================================
;;;
;;; For hard conditioning, we can use rejection sampling
;;; to get unweighted samples.

;;; rejection-sample : Prob a -> Int -> Int -> (Maybe a)
;;; Try to get a valid sample within max-tries attempts.
;;; Returns nothing if all attempts have zero weight.
(define (rejection-sample p seed max-tries)
  (let loop ([prng (make-pcg seed 1)] [tries max-tries])
       (if (<= tries 0)
           nothing
           (let* ([result (run-prob p prng)]
                  [value (car (car result))]
                  [log-w (cdr (car result))]
                  [new-prng (cdr result)])
                 (if (> log-w -inf.0)
                     (just value)
                     (loop new-prng (- tries 1)))))))

;;; rejection-samples : Prob a -> Int -> Int -> Int -> (List a)
;;; Get n valid samples via rejection, with max-tries per sample.
(define (rejection-samples p seed n max-tries-per)
  (let loop ([prng (make-pcg seed 1)] [count n] [acc '()])
       (if (= count 0)
           (reverse acc)
           (let* ([result (run-prob p prng)]
                  [value (car (car result))]
                  [log-w (cdr (car result))]
                  [new-prng (cdr result)])
                 (if (> log-w -inf.0)
                     (loop new-prng (- count 1) (cons value acc))
                     ;; Retry with updated PRNG
                     (let inner-loop ([tries max-tries-per] [g new-prng])
                          (if (<= tries 0)
                              ;; Give up on this sample, continue to next
                              (loop g (- count 1) acc)
                              (let* ([r (run-prob p g)]
                                     [v (car (car r))]
                                     [lw (cdr (car r))]
                                     [ng (cdr r)])
                                    (if (> lw -inf.0)
                                        (loop ng (- count 1) (cons v acc))
                                        (inner-loop (- tries 1) ng))))))))))

;;; ============================================================
;;; Combinators
;;; ============================================================

;;; prob-sequence : (List (Prob a)) -> Prob (List a)
;;; Sequence a list of Prob computations.
(define (prob-sequence probs)
  (if (null? probs)
      (prob-pure '())
      (prob-bind (car probs)
                 (lambda (x)
                         (prob-bind (prob-sequence (cdr probs))
                                    (lambda (xs)
                                            (prob-pure (cons x xs))))))))

;;; prob-replicate : Int -> Prob a -> Prob (List a)
;;; Sample n independent copies.
(define (prob-replicate n p)
  (if (<= n 0)
      (prob-pure '())
      (prob-sequence (make-list n p))))

;;; make-list helper
(define (make-list n x)
  (if (<= n 0)
      '()
      (cons x (make-list (- n 1) x))))

;;; prob-map-m : (a -> Prob b) -> (List a) -> Prob (List b)
;;; Map a Prob-returning function over a list.
(define (prob-map-m f xs)
  (prob-sequence (map f xs)))

;;; prob-filter : (a -> Boolean) -> (List a) -> Prob a
;;; Uniformly sample from elements satisfying predicate.
(define (prob-filter pred xs)
  (let ([valid (filter pred xs)])
       (if (null? valid)
           (prob-then (factor -inf.0) (prob-pure (car xs)))  ; No valid element
           (sample (random-choice valid)))))

;;; ============================================================
;;; Distributions as First-Class Values
;;; ============================================================
;;;
;;; A Distribution is a record with:
;;; - sampler: State PRNG a
;;; - pdf/pmf: a -> Float (optional)
;;; - support: description of support (optional)

;;; make-distribution : (State PRNG a) -> ((a -> Float) | #f) -> Distribution a
(define (make-distribution sampler pdf)
  (list 'distribution sampler pdf))

;;; distribution? : Any -> Boolean
(define (distribution? x)
  (and (pair? x) (eq? (car x) 'distribution)))

;;; dist-sampler : Distribution a -> State PRNG a
(define (dist-sampler d)
  (cadr d))

;;; dist-pdf : Distribution a -> (a -> Float) | #f
(define (dist-pdf d)
  (caddr d))

;;; sample-dist : Distribution a -> Prob a
;;; Sample from a first-class distribution.
(define (sample-dist d)
  (sample (dist-sampler d)))

;;; observe-from-dist : Distribution a -> a -> Prob ()
;;; Observe a value from a distribution (using its PDF for likelihood).
(define (observe-from-dist d observed)
  (let ([pdf (dist-pdf d)])
       (if pdf
           (score (pdf observed))
           (error 'observe-from-dist "distribution has no PDF" d))))

;;; ============================================================
;;; Common First-Class Distributions
;;; ============================================================

;;; normal-dist : Float -> Float -> Distribution Float
(define (normal-dist mean stddev)
  (make-distribution
   (random-normal mean stddev)
   (lambda (x) (normal-pdf x mean stddev))))

;;; exponential-dist : Float -> Distribution Float
(define (exponential-dist rate)
  (make-distribution
   (random-exponential rate)
   (lambda (x) (exponential-pdf rate x))))

;;; uniform-dist : Float -> Float -> Distribution Float
(define (uniform-dist lo hi)
  (make-distribution
   (random-range lo hi)
   (lambda (x) (uniform-pdf lo hi x))))

;;; bernoulli-dist : Float -> Distribution Boolean
(define (bernoulli-dist p)
  (make-distribution
   (random-bernoulli p)
   (lambda (x) (bernoulli-pmf p x))))

;;; poisson-dist : Float -> Distribution Int
(define (poisson-dist rate)
  (make-distribution
   (random-poisson rate)
   (lambda (k) (poisson-pmf rate k))))

;;; ============================================================
;;; Bayesian Inference Pattern
;;; ============================================================
;;;
;;; The typical pattern for Bayesian inference:
;;;
;;; (define posterior
;;;   (prob-bind prior
;;;              (lambda (theta)
;;;                (prob-then (observe likelihood observed-data)
;;;                           (prob-pure theta)))))
;;;
;;; Or using first-class distributions:
;;;
;;; (define model
;;;   (prob-bind (sample-dist (normal-dist 0 10))  ; prior
;;;              (lambda (mu)
;;;                (prob-then (observe-from-dist (normal-dist mu 1) 5.0)
;;;                           (prob-pure mu)))))

;;; ============================================================
;;; Example: Coin Flip Model
;;; ============================================================
;;;
;;; ;; Prior: uniform over [0,1]
;;; ;; Likelihood: 7 heads in 10 flips
;;; (define coin-model
;;;   (prob-bind prob-uniform
;;;              (lambda (theta)
;;;                (prob-then
;;;                 (prob-map-m
;;;                  (lambda (_) (prob-then (observe (bernoulli-pmf theta) #t)
;;;                                          (prob-pure '())))
;;;                  (make-list 7 '()))  ; 7 heads
;;;                 (prob-then
;;;                  (prob-map-m
;;;                   (lambda (_) (prob-then (observe (bernoulli-pmf theta) #f)
;;;                                           (prob-pure '())))
;;;                   (make-list 3 '()))  ; 3 tails
;;;                  (prob-pure theta))))))
;;;
;;; (importance-mean coin-model 42 10000)  ; Should be ~0.67 (Beta(8,4) mean)
