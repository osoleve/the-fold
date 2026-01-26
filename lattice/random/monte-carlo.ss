(load "core/base/prelude.ss")
(load "lattice/fp/control/state.ss")
(load "lattice/fp/numeric/transcendental.ss")
(load "lattice/random/prng.ss")
(load "lattice/random/distributions.ss")



(doc 'module 'monte-carlo)
(doc 'description "Comprehensive Monte Carlo simulation toolkit for The Fold.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'statistical-summary-functions)

(doc "Statistical Summary Functions")







(define (sample-mean samples)
  (if (null? samples)
      0
      (/ (fold-left + 0 samples) (length samples))))

(define (sample-variance samples)
  (let ([n (length samples)])
       (if (<= n 1)
           0
           (let* ([mu (sample-mean samples)]
                  [sum-sq (fold-left + 0
                                     (map (lambda (x) (* (- x mu) (- x mu))) samples))])
                 (/ sum-sq (- n 1))))))

(define (sample-std samples)
  (sqrt (sample-variance samples)))

(define (sample-sem samples)
  (let ([n (length samples)])
       (if (<= n 0)
           0
           (/ (sample-std samples) (sqrt n)))))

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

(define (sample-median samples)
  (sample-quantile samples 0.5))

(define (sample-min samples)
  (if (null? samples)
      +inf.0
      (fold-left min (car samples) (cdr samples))))

(define (sample-max samples)
  (if (null? samples)
      -inf.0
      (fold-left max (car samples) (cdr samples))))

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


(doc 'section 'basic-monte-carlo-integration)

(doc "Basic Monte Carlo Integration")



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

(define (mc-estimate sampler n)
  (state-bind (random-list n sampler)
              (lambda (samples)
                      (let ([mu (sample-mean samples)]
                            [se (sample-sem samples)])
                           (state-pure (cons mu se))))))


(doc 'section 'importance-sampling)

(doc "Importance Sampling")



(define (importance-sample f weight-fn proposal-sampler n)
  (doc 'export #t)
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


(doc 'section 'rejection-sampling)

(doc "Rejection Sampling")



(define (rejection-sample target-density M proposal-sampler proposal-density)
  (doc 'export #t)
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

(define (rejection-sample-n target-density M proposal-sampler proposal-density n)
  (random-list n (rejection-sample target-density M proposal-sampler proposal-density)))

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


(doc 'section 'metropolis-hastings-mcmc)

(doc "Metropolis-Hastings MCMC")



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

(define (mh-chain log-target proposal initial n)
  (let loop ([i 0] [current initial] [acc '()])
       (if (>= i n)
           (state-pure (reverse acc))
           (state-bind (mh-symmetric-step log-target proposal current)
                       (lambda (next)
                               (loop (+ i 1) next (cons next acc)))))))

(define (mh-sample log-target proposal initial n-samples burn-in)
  (state-bind (mh-chain log-target proposal initial burn-in)
              (lambda (burn-samples)
                      (let ([last-burn (if (null? burn-samples) initial (car (reverse burn-samples)))])
                           (mh-chain log-target proposal last-burn n-samples)))))

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

(define (random-walk-proposal step-size)
  (lambda (current)
          (state-bind random-normal-standard
                      (lambda (z)
                              (state-pure (+ current (* step-size z)))))))

(define (multivariate-random-walk-proposal step-sizes)
  (lambda (current)
          (state-bind (random-list (length current) random-normal-standard)
                      (lambda (zs)
                              (state-pure (map (lambda (x s z) (+ x (* s z)))
                                               current step-sizes zs))))))


(doc 'section 'gibbs-sampling)

(doc "Gibbs Sampling")



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

(define (list-update lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- idx 1) val))))

(define (gibbs-chain conditionals initial n)
  (let loop ([i 0] [current initial] [acc '()])
       (if (>= i n)
           (state-pure (reverse acc))
           (state-bind (gibbs-step conditionals current)
                       (lambda (next)
                               (loop (+ i 1) next (cons next acc)))))))

(define (gibbs-sample conditionals initial n-samples burn-in)
  (state-bind (gibbs-chain conditionals initial burn-in)
              (lambda (burn-samples)
                      (let ([last-burn (if (null? burn-samples) initial (car (reverse burn-samples)))])
                           (gibbs-chain conditionals last-burn n-samples)))))


(doc 'section 'variance-reduction-antithetic-variates)

(doc "Variance Reduction: Antithetic Variates")



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


(doc 'section 'variance-reduction-control-variates)

(doc "Variance Reduction: Control Variates")



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


(doc 'section 'variance-reduction-stratified-sampling)

(doc "Variance Reduction: Stratified Sampling")



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

;; iota is provided by prelude


(doc 'section 'convergence-diagnostics)

(doc "Convergence Diagnostics")



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

(define (split-into-batches lst batch-size)
  (if (or (null? lst) (< (length lst) batch-size))
      '()
      (cons (take batch-size lst)
            (split-into-batches (list-tail lst batch-size) batch-size))))

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


(doc 'section 'batch-and-parallel-sampling)

(doc "Batch and Parallel Sampling")



(define (parallel-chains chain-runner initial-states)
  (state-sequence (map chain-runner initial-states)))

(define (batch-sample sampler batch-size n-batches)
  (random-list n-batches (random-list batch-size sampler)))

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


(doc 'section 'convenience-functions)

(doc "Convenience Functions")



(define (run-mc seed computation)
  (eval-state computation (make-pcg seed 1)))

(define (mc-pi-estimate n)
  (state-bind (random-list n
                           (state-bind (random-float-range -1 1)
                                       (lambda (x)
                                               (state-bind (random-float-range -1 1)
                                                           (lambda (y)
                                                                   (state-pure (if (<= (+ (* x x) (* y y)) 1) 1 0)))))))
              (lambda (hits)
                      (state-pure (* 4.0 (sample-mean hits))))))

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


(doc 'section 'helper-functions)

(doc "Helper Functions")



(define (take n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (list-tail-safe lst n)
  (if (or (<= n 0) (null? lst))
      lst
      (list-tail-safe (cdr lst) (- n 1))))

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
