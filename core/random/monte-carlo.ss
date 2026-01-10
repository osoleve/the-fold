;;; core/random/monte-carlo.ss -- Monte Carlo Methods
;;;
;;; Comprehensive Monte Carlo simulation toolkit for The Fold.
;;; All methods use the State monad for pure, reproducible sampling.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Basic Monte Carlo integration
;;;   - Importance sampling
;;;   - Rejection sampling
;;;   - Markov Chain Monte Carlo (MCMC):
;;;     - Metropolis-Hastings algorithm
;;;     - Gibbs sampling
;;;   - Variance reduction techniques:
;;;     - Control variates
;;;     - Antithetic variates
;;;     - Stratified sampling
;;;   - Convergence diagnostics
;;;   - Statistical summaries of samples
;;;   - Batch/parallel sampling support
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/state.ss
;;;   - fp/transcendental.ss
;;;   - random/prng.ss
;;;   - random/distributions.ss

(load "core/base/prelude.ss")
(load "core/fp/control/state.ss")
(load "core/fp/numeric/transcendental.ss")
(load "core/random/prng.ss")
(load "core/random/distributions.ss")

;;; ============================================================
;;; Statistical Summary Functions
;;; ============================================================

;;; sample-mean : (List Number) -> Number
;;; Compute the arithmetic mean of a sample.
(define (sample-mean samples)
  (if (null? samples)
      0
      (/ (fold-left + 0 samples) (length samples))))

;;; sample-variance : (List Number) -> Number
;;; Compute the sample variance (unbiased estimator).
(define (sample-variance samples)
  (let ([n (length samples)])
       (if (<= n 1)
           0
           (let* ([mu (sample-mean samples)]
                  [sum-sq (fold-left + 0
                                     (map (lambda (x) (* (- x mu) (- x mu))) samples))])
                 (/ sum-sq (- n 1))))))

;;; sample-std : (List Number) -> Number
;;; Compute the sample standard deviation.
(define (sample-std samples)
  (sqrt (sample-variance samples)))

;;; sample-sem : (List Number) -> Number
;;; Compute standard error of the mean.
(define (sample-sem samples)
  (let ([n (length samples)])
       (if (<= n 0)
           0
           (/ (sample-std samples) (sqrt n)))))

;;; sample-quantile : (List Number) x Number -> Number
;;; Compute the p-th quantile (0 <= p <= 1).
(define (sample-quantile samples p)
  (if (null? samples)
      0
      (let* ([sorted (list-sort < samples)]
             [n (length sorted)]
             [idx (* p (- n 1))]
             [lo (inexact->exact (floor idx))]
             [hi (min (+ lo 1) (- n 1))]
             [frac (- idx lo)])
            (+ (* (- 1 frac) (list-ref sorted lo))
               (* frac (list-ref sorted hi))))))

;;; sample-median : (List Number) -> Number
;;; Compute the median.
(define (sample-median samples)
  (sample-quantile samples 0.5))

;;; sample-min : (List Number) -> Number
(define (sample-min samples)
  (if (null? samples)
      +inf.0
      (fold-left min (car samples) (cdr samples))))

;;; sample-max : (List Number) -> Number
(define (sample-max samples)
  (if (null? samples)
      -inf.0
      (fold-left max (car samples) (cdr samples))))

;;; sample-summary : (List Number) -> Alist
;;; Compute comprehensive summary statistics.
(define (sample-summary samples)
  (let ([n (length samples)])
       (if (= n 0)
           '((n . 0))
           `((n . ,n)
             (mean . ,(sample-mean samples))
             (std . ,(sample-std samples))
             (sem . ,(sample-sem samples))
             (min . ,(sample-min samples))
             (q25 . ,(sample-quantile samples 0.25))
             (median . ,(sample-median samples))
             (q75 . ,(sample-quantile samples 0.75))
             (max . ,(sample-max samples))))))

;;; ============================================================
;;; Basic Monte Carlo Integration
;;; ============================================================

;;; mc-integrate : (Number -> Number) x Number x Number x Integer
;;;                -> State GenState (Number . Number)
;;; Monte Carlo integration of f over [a, b].
;;; Returns (estimate . standard-error).
(define (mc-integrate f a b n)
  (let ([width (- b a)])
       (state-bind (random-list n (random-float-range a b))
                   (lambda (samples)
                           (let* ([fvals (map f samples)]
                                  [mu (sample-mean fvals)]
                                  [estimate (* width mu)]
                                  [variance (sample-variance fvals)]
                                  [se (* width (sqrt (/ variance n)))])
                                 (state-pure (cons estimate se)))))))

;;; mc-estimate : (Unit -> State g Number) x Integer -> State g (Number . Number)
;;; Generic Monte Carlo estimation. Samples n times and returns (mean . sem).
(define (mc-estimate sampler n)
  (state-bind (random-list n sampler)
              (lambda (samples)
                      (let ([mu (sample-mean samples)]
                            [se (sample-sem samples)])
                           (state-pure (cons mu se))))))

;;; ============================================================
;;; Importance Sampling
;;; ============================================================

;;; importance-sample : (a -> Number) x (a -> Number) x (State g a) x Integer
;;;                     -> State g (Number . Number)
;;; Importance sampling: estimate E[f(X)] where X ~ target by sampling from proposal.
;;; - f: function to estimate expectation of
;;; - weight-fn: returns target(x)/proposal(x) ratio
;;; - proposal-sampler: sampler from proposal distribution
;;; - n: number of samples
;;; Returns (estimate . effective-sample-size-ratio).
(define (importance-sample f weight-fn proposal-sampler n)
  (state-bind (random-list n proposal-sampler)
              (lambda (samples)
                      (let* ([weights (map weight-fn samples)]
                             [fvals (map f samples)]
                             [weighted-vals (map * fvals weights)]
                             [total-weight (fold-left + 0 weights)]
                             [sum-sq-weights (fold-left + 0 (map (lambda (w) (* w w)) weights))]
                             [estimate (/ (fold-left + 0 weighted-vals) total-weight)]
                             ;; Effective sample size ratio: (sum w)^2 / (n * sum w^2)
                             [ess-ratio (/ (* total-weight total-weight)
                                           (* n sum-sq-weights))])
                            (state-pure (cons estimate ess-ratio))))))

;;; self-normalized-importance-sample : (a -> Number) x (a -> Number) x (State g a) x Integer
;;;                                     -> State g (Number . Number)
;;; Self-normalized importance sampling (for unnormalized target densities).
;;; weight-fn now returns just w(x) = target(x)/proposal(x) (possibly unnormalized).
(define (self-normalized-importance-sample f weight-fn proposal-sampler n)
  (state-bind (random-list n proposal-sampler)
              (lambda (samples)
                      (let* ([weights (map weight-fn samples)]
                             [fvals (map f samples)]
                             [weighted-vals (map * fvals weights)]
                             [total-weight (fold-left + 0 weights)]
                             [sum-sq-weights (fold-left + 0 (map (lambda (w) (* w w)) weights))]
                             [estimate (if (> total-weight 0)
                                           (/ (fold-left + 0 weighted-vals) total-weight)
                                           0)]
                             [ess (if (> sum-sq-weights 0)
                                      (/ (* total-weight total-weight) sum-sq-weights)
                                      0)])
                            (state-pure (cons estimate ess))))))

;;; ============================================================
;;; Rejection Sampling
;;; ============================================================

;;; rejection-sample : (a -> Number) x Number x (State g a) x (a -> Number)
;;;                    -> State g a
;;; Single sample from target using rejection sampling.
;;; - target-density: unnormalized target density
;;; - M: bound such that target(x) <= M * proposal(x) for all x
;;; - proposal-sampler: sampler from proposal distribution
;;; - proposal-density: density of proposal at sampled point
(define (rejection-sample target-density M proposal-sampler proposal-density)
  (make-state
   (lambda (gen)
           (let loop ([g gen])
                (let* ([result1 (run-state proposal-sampler g)]
                       [x (car result1)]
                       [g1 (cdr result1)]
                       [result2 (run-state random-float g1)]
                       [u (car result2)]
                       [g2 (cdr result2)]
                       [acceptance-prob (/ (target-density x)
                                           (* M (proposal-density x)))])
                      (if (< u acceptance-prob)
                          (cons x g2)
                          (loop g2)))))))

;;; rejection-sample-n : ... -> State g (List a)
;;; Sample n values using rejection sampling.
(define (rejection-sample-n target-density M proposal-sampler proposal-density n)
  (random-list n (rejection-sample target-density M proposal-sampler proposal-density)))

;;; rejection-sample-bounded : (Number -> Number) x Number x Number x Number
;;;                            -> State g Number
;;; Rejection sampling for a bounded function on [a, b] with maximum M.
(define (rejection-sample-bounded f a b M)
  (make-state
   (lambda (gen)
           (let loop ([g gen])
                (let* ([result1 (run-state (random-float-range a b) g)]
                       [x (car result1)]
                       [g1 (cdr result1)]
                       [result2 (run-state random-float g1)]
                       [u (car result2)]
                       [g2 (cdr result2)])
                      (if (< (* u M) (f x))
                          (cons x g2)
                          (loop g2)))))))

;;; ============================================================
;;; Metropolis-Hastings MCMC
;;; ============================================================

;;; mh-step : (a -> Number) x (a -> State g a) x (a -> a -> Number) x a
;;;           -> State g a
;;; Single Metropolis-Hastings step.
;;; - log-target: log of (unnormalized) target density
;;; - proposal: proposal sampler given current state
;;; - log-proposal: log proposal density q(x'|x)
;;; - current: current state
(define (mh-step log-target proposal log-proposal current)
  (state-bind (proposal current)
              (lambda (proposed)
                      (state-bind random-float
                                  (lambda (u)
                                          (let* ([log-alpha (+ (- (log-target proposed) (log-target current))
                                                               (- (log-proposal current proposed)
                                                                  (log-proposal proposed current)))]
                                                 [alpha (min 1 (exp-num log-alpha))])
                                                (if (< u alpha)
                                                    (state-pure proposed)
                                                    (state-pure current))))))))

;;; mh-symmetric-step : (a -> Number) x (a -> State g a) x a -> State g a
;;; MH step with symmetric proposal (log-proposal cancels).
(define (mh-symmetric-step log-target proposal current)
  (state-bind (proposal current)
              (lambda (proposed)
                      (state-bind random-float
                                  (lambda (u)
                                          (let* ([log-alpha (- (log-target proposed) (log-target current))]
                                                 [alpha (min 1 (exp-num log-alpha))])
                                                (if (< u alpha)
                                                    (state-pure proposed)
                                                    (state-pure current))))))))

;;; mh-chain : (a -> Number) x (a -> State g a) x a x Integer
;;;            -> State g (List a)
;;; Run Metropolis-Hastings chain for n steps.
(define (mh-chain log-target proposal initial n)
  (let loop ([i 0] [current initial] [acc '()])
       (if (>= i n)
           (state-pure (reverse acc))
           (state-bind (mh-symmetric-step log-target proposal current)
                       (lambda (next)
                               (loop (+ i 1) next (cons next acc)))))))

;;; mh-sample : (a -> Number) x (a -> State g a) x a x Integer x Integer
;;;             -> State g (List a)
;;; Run MH with burn-in, returning samples after burn-in.
(define (mh-sample log-target proposal initial n-samples burn-in)
  (state-bind (mh-chain log-target proposal initial burn-in)
              (lambda (burn-samples)
                      (let ([last-burn (if (null? burn-samples) initial (car (reverse burn-samples)))])
                           (mh-chain log-target proposal last-burn n-samples)))))

;;; mh-thinned-sample : (a -> Number) x (a -> State g a) x a x Integer x Integer x Integer
;;;                     -> State g (List a)
;;; MH with burn-in and thinning (keep every k-th sample).
(define (mh-thinned-sample log-target proposal initial n-samples burn-in thin)
  (let ([total-steps (+ burn-in (* n-samples thin))])
       (state-bind (mh-chain log-target proposal initial total-steps)
                   (lambda (all-samples)
                           (state-pure
                            (let loop ([samples (list-tail all-samples burn-in)]
                                       [i 0]
                                       [acc '()])
                                 (cond
                                  [(null? samples) (reverse acc)]
                                  [(= (modulo i thin) 0)
                                   (loop (cdr samples) (+ i 1) (cons (car samples) acc))]
                                  [else
                                   (loop (cdr samples) (+ i 1) acc)])))))))

;;; random-walk-proposal : Number -> (Number -> State g Number)
;;; Create a Gaussian random walk proposal with given step size.
(define (random-walk-proposal step-size)
  (lambda (current)
          (state-bind random-normal-standard
                      (lambda (z)
                              (state-pure (+ current (* step-size z)))))))

;;; multivariate-random-walk-proposal : (List Number) -> ((List Number) -> State g (List Number))
;;; Create a multivariate Gaussian random walk proposal.
;;; step-sizes: list of step sizes for each dimension.
(define (multivariate-random-walk-proposal step-sizes)
  (lambda (current)
          (state-bind (random-list (length current) random-normal-standard)
                      (lambda (zs)
                              (state-pure (map (lambda (x s z) (+ x (* s z)))
                                               current step-sizes zs))))))

;;; ============================================================
;;; Gibbs Sampling
;;; ============================================================

;;; gibbs-step : (List (Integer x (List a) -> State g a)) x (List a)
;;;              -> State g (List a)
;;; Single Gibbs sampling step.
;;; - conditionals: list of (index, conditional-sampler) where
;;;   conditional-sampler takes current state and returns new value for index
;;; - current: current state vector (as list)
(define (gibbs-step conditionals current)
  (let loop ([conds conditionals] [state current])
       (if (null? conds)
           (state-pure state)
           (let ([idx (caar conds)]
                 [sampler (cdar conds)])
                (state-bind (sampler state)
                            (lambda (new-val)
                                    (loop (cdr conds)
                                          (list-update state idx new-val))))))))

;;; list-update : (List a) x Integer x a -> (List a)
;;; Update element at index in list.
(define (list-update lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- idx 1) val))))

;;; gibbs-chain : (List (Integer x (List a) -> State g a)) x (List a) x Integer
;;;               -> State g (List (List a))
;;; Run Gibbs sampler for n steps.
(define (gibbs-chain conditionals initial n)
  (let loop ([i 0] [current initial] [acc '()])
       (if (>= i n)
           (state-pure (reverse acc))
           (state-bind (gibbs-step conditionals current)
                       (lambda (next)
                               (loop (+ i 1) next (cons next acc)))))))

;;; gibbs-sample : (List (Integer x (List a) -> State g a)) x (List a)
;;;                x Integer x Integer -> State g (List (List a))
;;; Gibbs sampling with burn-in.
(define (gibbs-sample conditionals initial n-samples burn-in)
  (state-bind (gibbs-chain conditionals initial burn-in)
              (lambda (burn-samples)
                      (let ([last-burn (if (null? burn-samples) initial (car (reverse burn-samples)))])
                           (gibbs-chain conditionals last-burn n-samples)))))

;;; ============================================================
;;; Variance Reduction: Antithetic Variates
;;; ============================================================

;;; antithetic-estimate : (Number -> Number) x Integer -> State g (Number . Number)
;;; Use antithetic variates for variance reduction.
;;; f should be a function of U ~ Uniform(0,1).
;;; Pairs each U with 1-U to reduce variance.
(define (antithetic-estimate f n)
  (let ([half-n (quotient n 2)])
       (state-bind (random-list half-n random-float)
                   (lambda (us)
                           (let* ([f-us (map f us)]
                                  [f-antithetic (map (lambda (u) (f (- 1 u))) us)]
                                  [paired-means (map (lambda (a b) (/ (+ a b) 2)) f-us f-antithetic)]
                                  [mu (sample-mean paired-means)]
                                  [se (sample-sem paired-means)])
                                 (state-pure (cons mu se)))))))

;;; antithetic-integrate : (Number -> Number) x Number x Number x Integer
;;;                        -> State g (Number . Number)
;;; Monte Carlo integration with antithetic variates.
(define (antithetic-integrate f a b n)
  (let ([width (- b a)]
        [half-n (quotient n 2)])
       (state-bind (random-list half-n random-float)
                   (lambda (us)
                           (let* ([xs (map (lambda (u) (+ a (* u width))) us)]
                                  [xs-anti (map (lambda (u) (+ a (* (- 1 u) width))) us)]
                                  [f-xs (map f xs)]
                                  [f-anti (map f xs-anti)]
                                  [paired-means (map (lambda (a b) (/ (+ a b) 2)) f-xs f-anti)]
                                  [estimate (* width (sample-mean paired-means))]
                                  [se (* width (sample-sem paired-means))])
                                 (state-pure (cons estimate se)))))))

;;; ============================================================
;;; Variance Reduction: Control Variates
;;; ============================================================

;;; control-variate-estimate : (a -> Number) x (a -> Number) x Number
;;;                            x (State g a) x Integer -> State g (Number . Number)
;;; Use control variate to reduce variance.
;;; - f: function whose expectation we want
;;; - g: control variate function with known expectation
;;; - g-mean: known E[g(X)]
;;; - sampler: samples X
;;; Returns estimate of E[f(X)] with reduced variance.
(define (control-variate-estimate f g g-mean sampler n)
  (state-bind (random-list n sampler)
              (lambda (samples)
                      (let* ([f-vals (map f samples)]
                             [g-vals (map g samples)]
                             [f-mean (sample-mean f-vals)]
                             [g-sample-mean (sample-mean g-vals)]
                             ;; Optimal coefficient c = Cov(f,g)/Var(g)
                             [g-centered (map (lambda (gv) (- gv g-sample-mean)) g-vals)]
                             [f-centered (map (lambda (fv) (- fv f-mean)) f-vals)]
                             [cov-fg (/ (fold-left + 0 (map * f-centered g-centered)) (- n 1))]
                             [var-g (/ (fold-left + 0 (map (lambda (x) (* x x)) g-centered)) (- n 1))]
                             [c (if (> var-g 0) (/ cov-fg var-g) 0)]
                             ;; Adjusted estimate
                             [adjusted-vals (map (lambda (fv gv) (- fv (* c (- gv g-mean))))
                                                 f-vals g-vals)]
                             [estimate (sample-mean adjusted-vals)]
                             [se (sample-sem adjusted-vals)])
                            (state-pure (cons estimate se))))))

;;; ============================================================
;;; Variance Reduction: Stratified Sampling
;;; ============================================================

;;; stratified-sample : (Number -> Number) x Number x Number x Integer x Integer
;;;                     -> State g (Number . Number)
;;; Stratified Monte Carlo integration.
;;; Divides [a, b] into k strata and samples n/k points per stratum.
(define (stratified-sample f a b k n)
  (let* ([per-stratum (max 1 (quotient n k))]
         [width (- b a)]
         [stratum-width (/ width k)])
        (letrec ([sample-stratum
                  (lambda (i)
                          (let ([lo (+ a (* i stratum-width))]
                                [hi (+ a (* (+ i 1) stratum-width))])
                               (state-bind (random-list per-stratum (random-float-range lo hi))
                                           (lambda (xs)
                                                   (state-pure (map f xs))))))])
                (state-bind (state-sequence
                             (map sample-stratum (iota k)))
                            (lambda (stratum-results)
                                    (let* ([all-means (map sample-mean stratum-results)]
                                           [estimate (* width (sample-mean all-means))]
                                           [stratum-vars (map sample-variance stratum-results)]
                                           [total-var (fold-left + 0 stratum-vars)]
                                           [se (* width (sqrt (/ total-var (* k k per-stratum))))])
                                          (state-pure (cons estimate se))))))))

;;; iota : Integer -> (List Integer)
;;; Generate list [0, 1, ..., n-1].
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; Convergence Diagnostics
;;; ============================================================

;;; effective-sample-size : (List Number) -> Number
;;; Estimate effective sample size accounting for autocorrelation.
;;; Uses batch means method.
(define (effective-sample-size samples)
  (let* ([n (length samples)]
         [batch-size (max 1 (inexact->exact (floor (sqrt n))))]
         [n-batches (quotient n batch-size)]
         [batches (split-into-batches samples batch-size)]
         [batch-means (map sample-mean batches)]
         [overall-mean (sample-mean samples)]
         [overall-var (sample-variance samples)]
         [batch-var (sample-variance batch-means)])
        (if (> batch-var 0)
            (/ (* n overall-var) (* batch-size batch-var))
            n)))

;;; split-into-batches : (List a) x Integer -> (List (List a))
;;; Split list into batches of given size.
(define (split-into-batches lst batch-size)
  (if (or (null? lst) (< (length lst) batch-size))
      '()
      (cons (take batch-size lst)
            (split-into-batches (list-tail lst batch-size) batch-size))))

;;; gelman-rubin : (List (List Number)) -> Number
;;; Gelman-Rubin convergence diagnostic (R-hat).
;;; Input: list of chains (each chain is a list of samples).
;;; R-hat should be close to 1.0 for convergence.
(define (gelman-rubin chains)
  (let* ([m (length chains)]
         [n (if (null? chains) 0 (length (car chains)))]
         [chain-means (map sample-mean chains)]
         [chain-vars (map sample-variance chains)]
         [overall-mean (sample-mean chain-means)]
         ;; Between-chain variance
         [B (/ (* n (fold-left + 0
                               (map (lambda (mu) (* (- mu overall-mean) (- mu overall-mean)))
                                    chain-means)))
               (- m 1))]
         ;; Within-chain variance
         [W (sample-mean chain-vars)]
         ;; Pooled variance estimate
         [var-hat (+ (* (/ (- n 1) n) W) (/ B n))]
         ;; R-hat
         [R-hat (if (> W 0) (sqrt (/ var-hat W)) 1)])
        R-hat))

;;; autocorrelation : (List Number) x Integer -> Number
;;; Compute autocorrelation at lag k.
(define (autocorrelation samples k)
  (let* ([n (length samples)]
         [mu (sample-mean samples)]
         [centered (map (lambda (x) (- x mu)) samples)]
         [c0 (fold-left + 0 (map (lambda (x) (* x x)) centered))]
         [ck (let loop ([i 0] [sum 0])
                  (if (>= (+ i k) n)
                      sum
                      (loop (+ i 1)
                            (+ sum (* (list-ref centered i)
                                      (list-ref centered (+ i k)))))))])
        (if (> c0 0)
            (/ ck c0)
            0)))

;;; integrated-autocorrelation-time : (List Number) -> Number
;;; Estimate integrated autocorrelation time (IAT).
;;; Truncates when autocorrelation drops below threshold.
(define (integrated-autocorrelation-time samples)
  (let* ([n (length samples)]
         [max-lag (min 100 (quotient n 4))])
        (let loop ([k 1] [sum 0.5])  ; Start at 0.5 for k=0 contribution
             (if (>= k max-lag)
                 (* 2 sum)
                 (let ([rho-k (autocorrelation samples k)])
                      (if (< (abs rho-k) 0.05)  ; Truncate when small
                          (* 2 sum)
                          (loop (+ k 1) (+ sum rho-k))))))))

;;; mcmc-diagnostics : (List Number) -> Alist
;;; Compute comprehensive MCMC diagnostics for a single chain.
(define (mcmc-diagnostics samples)
  (let ([n (length samples)])
       (if (< n 10)
           '((n . 0) (ess . 0) (iat . +inf.0))
           (let* ([ess (effective-sample-size samples)]
                  [iat (integrated-autocorrelation-time samples)]
                  [accept-rate 'unknown])  ; Would need to track accepts
                 `((n . ,n)
                   (ess . ,ess)
                   (ess-ratio . ,(/ ess n))
                   (iat . ,iat)
                   ,@(sample-summary samples))))))

;;; ============================================================
;;; Batch and Parallel Sampling
;;; ============================================================

;;; parallel-chains : (a -> State g (List b)) x (List a) -> State g (List (List b))
;;; Run multiple chains from different initial points.
;;; Note: "parallel" is conceptual; execution is sequential but independent.
(define (parallel-chains chain-runner initial-states)
  (state-sequence (map chain-runner initial-states)))

;;; batch-sample : (State g a) x Integer x Integer -> State g (List (List a))
;;; Generate n-batches batches, each with batch-size samples.
(define (batch-sample sampler batch-size n-batches)
  (random-list n-batches (random-list batch-size sampler)))

;;; progressive-sample : (State g a) x Integer x (List a -> Boolean)
;;;                      -> State g (List a)
;;; Sample until convergence criterion is met (up to max-n samples).
(define (progressive-sample sampler max-n converged?)
  (let ([batch-size 100])
       (let loop ([collected '()] [remaining max-n])
            (if (<= remaining 0)
                (state-pure (reverse collected))
                (let ([n-sample (min batch-size remaining)])
                     (state-bind (random-list n-sample sampler)
                                 (lambda (new-samples)
                                         (let ([all-samples (append (reverse collected) new-samples)])
                                              (if (converged? all-samples)
                                                  (state-pure all-samples)
                                                  (loop (reverse all-samples) (- remaining n-sample)))))))))))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; run-mc : Integer x State g a -> a
;;; Run Monte Carlo computation with PCG generator.
(define (run-mc seed computation)
  (eval-state computation (make-pcg seed 1)))

;;; mc-pi-estimate : Integer -> State g Number
;;; Classic Monte Carlo estimate of pi using circle/square method.
(define (mc-pi-estimate n)
  (state-bind (random-list n
                           (state-bind (random-float-range -1 1)
                                       (lambda (x)
                                               (state-bind (random-float-range -1 1)
                                                           (lambda (y)
                                                                   (state-pure (if (<= (+ (* x x) (* y y)) 1) 1 0)))))))
              (lambda (hits)
                      (state-pure (* 4.0 (sample-mean hits))))))

;;; mc-normal-tail-prob : Number x Integer -> State g Number
;;; Estimate P(Z > x) for standard normal using importance sampling.
(define (mc-normal-tail-prob threshold n)
  (let ([shifted-exponential
         (state-bind (random-exponential 1)
                     (lambda (e)
                             (state-pure (+ threshold e))))])
       (state-bind (random-list n shifted-exponential)
                   (lambda (samples)
                           (let* ([weights (map (lambda (x)
                                                        (/ (exp-num (* -0.5 x x))
                                                           (exp-num (- threshold x))))
                                                samples)]
                                  [total-weight (fold-left + 0 weights)]
                                  [estimate (/ total-weight n (sqrt (* 2 (pi-value))))])
                                 (state-pure estimate))))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; take : Integer x (List a) -> (List a)
(define (take n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;;; list-tail-safe : (List a) x Integer -> (List a)
(define (list-tail-safe lst n)
  (if (or (<= n 0) (null? lst))
      lst
      (list-tail-safe (cdr lst) (- n 1))))

;;; list-sort : (a x a -> Boolean) x (List a) -> (List a)
;;; Simple merge sort.
(define (list-sort less? lst)
  (letrec ([merge
            (lambda (xs ys)
                    (cond
                     [(null? xs) ys]
                     [(null? ys) xs]
                     [(less? (car xs) (car ys))
                      (cons (car xs) (merge (cdr xs) ys))]
                     [else
                      (cons (car ys) (merge xs (cdr ys)))]))]
           [split
            (lambda (lst)
                    (let loop ([slow lst] [fast lst] [acc '()])
                         (if (or (null? fast) (null? (cdr fast)))
                             (cons (reverse acc) slow)
                             (loop (cdr slow) (cddr fast) (cons (car slow) acc)))))]
           [sort
            (lambda (lst)
                    (if (or (null? lst) (null? (cdr lst)))
                        lst
                        (let ([halves (split lst)])
                             (merge (sort (car halves)) (sort (cdr halves))))))])
          (sort lst)))
