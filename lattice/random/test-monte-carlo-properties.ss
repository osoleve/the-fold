;;; lattice/random/test-monte-carlo-properties.ss — QuickCheck properties for Monte Carlo APIs

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'monte-carlo)

;;; ============================================================================
;;; Helpers and generators
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (all-satisfy? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all-satisfy? pred (cdr xs)))))

(define (finite-number? x)
  (and (number? x)
       (= x x)
       (not (= x +inf.0))
       (not (= x -inf.0))))

(define gen-seed
  (gen-int-range 0 1000000))

(define gen-nonempty-int-list
  (gen-bind (gen-int-range 1 40)
    (lambda (n)
      (gen-list-of n (gen-int-range -50 50)))))

(define gen-constant-samples
  (gen-bind (gen-int-range -30 30)
    (lambda (c)
      (gen-map (lambda (n) (cons c n))
               (gen-int-range 1 50)))))

(define gen-seed-n
  (gen-bind gen-seed
    (lambda (seed)
      (gen-map (lambda (n) (cons seed n))
               (gen-int-range 20 200)))))

(define gen-interval
  (gen-bind (gen-int-range -50 50)
    (lambda (a-int)
      (gen-map (lambda (w-int)
                 (let ([a (/ a-int 10.0)]
                       [w (/ w-int 10.0)])
                   (list a (+ a w))))
               (gen-int-range 1 60)))))

(define gen-seed-interval
  (gen-bind gen-seed
    (lambda (seed)
      (gen-map (lambda (ab) (cons seed ab))
               gen-interval))))

(define gen-mh-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 1 30)
        (lambda (step-int)
          (gen-map (lambda (n) (list seed (/ step-int 10.0) n))
                   (gen-int-range 1 80)))))))

(define gen-gibbs-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-map (lambda (n) (cons seed n))
               (gen-int-range 1 80)))))

(define gen-thin-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 1 25)
        (lambda (step-int)
          (gen-bind (gen-int-range 1 30)
            (lambda (n-samples)
              (gen-bind (gen-int-range 0 20)
                (lambda (burn-in)
                  (gen-map (lambda (thin)
                             (list seed (/ step-int 10.0) n-samples burn-in thin))
                           (gen-int-range 1 6)))))))))))

(define gen-stratified-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind gen-interval
        (lambda (ab)
          (gen-bind (gen-int-range 1 20)
            (lambda (k)
              (gen-map (lambda (n)
                         (list seed (car ab) (cadr ab) k n))
                       (gen-int-range k 200)))))))))

(define gen-progressive-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 20 120)
        (lambda (target)
          (gen-map (lambda (extra)
                     (list seed target (+ target extra)))
                   (gen-int-range 0 120)))))))

(define log-normal
  (lambda (x) (* -0.5 x x)))

;;; ============================================================================
;;; Summary properties
;;; ============================================================================

(test-group monte-carlo-summary-properties

  (define-property "sample-mean of a constant list is the constant"
    gen-constant-samples
    (lambda (pair)
      (let* ([c (car pair)]
             [n (cdr pair)]
             [samples (make-list n c)])
        (= (sample-mean samples) c)))
    'tests 220)

  (define-property "sample-variance of a constant list is zero"
    gen-constant-samples
    (lambda (pair)
      (let* ([c (car pair)]
             [n (cdr pair)]
             [samples (make-list n c)])
        (approx= (sample-variance samples) 0 1e-12)))
    'tests 220)

  (define-property "sample-std squares back to sample-variance"
    gen-nonempty-int-list
    (lambda (xs)
      (let ([s (sample-std xs)]
            [v (sample-variance xs)])
        (approx= (* s s) v 1e-9)))
    'tests 200)

  (define-property "sample-sem matches std/sqrt(n)"
    gen-nonempty-int-list
    (lambda (xs)
      (approx= (sample-sem xs)
               (/ (sample-std xs) (sqrt (length xs)))
               1e-9))
    'tests 200)

  (define-property "sample-median equals quantile at 0.5"
    gen-nonempty-int-list
    (lambda (xs)
      (approx= (sample-median xs)
               (sample-quantile xs 0.5)
               1e-12))
    'tests 180)

  (define-property "quantile endpoints agree with min and max"
    gen-nonempty-int-list
    (lambda (xs)
      (and (= (sample-quantile xs 0.0) (sample-min xs))
           (= (sample-quantile xs 1.0) (sample-max xs))))
    'tests 200)

  (define-property "sample-summary contains standard fields"
    gen-nonempty-int-list
    (lambda (xs)
      (let ([s (sample-summary xs)])
        (and (= (cdr (assq 'n s)) (length xs))
             (assq 'mean s)
             (assq 'std s)
             (assq 'sem s)
             (assq 'min s)
             (assq 'q25 s)
             (assq 'median s)
             (assq 'q75 s)
             (assq 'max s))))
    'tests 160)
)

;;; ============================================================================
;;; Core MC methods
;;; ============================================================================

(test-group monte-carlo-core-properties

  (define-property "mc-integrate constant tracks interval width"
    gen-seed-interval
    (lambda (seed+ab)
      (let* ([seed (car seed+ab)]
             [a (cadr seed+ab)]
             [b (caddr seed+ab)]
             [width (- b a)]
             [result (run-mc seed (mc-integrate (lambda (_) 1.0) a b 120))])
        (and (approx= (car result) width 0.35)
             (>= (cdr result) 0.0))))
    'tests 120)

  (define-property "mc-estimate on random-float stays in [0,1]"
    gen-seed-n
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [result (run-mc seed (mc-estimate random-float n))]
             [mu (car result)]
             [se (cdr result)])
        (and (>= mu 0.0)
             (<= mu 1.0)
             (>= se 0.0))))
    'tests 140)

  (define-property "importance-sample identity case has bounded estimate/ESS-ratio"
    gen-seed-n
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [result (run-mc seed
                             (importance-sample
                              (lambda (x) x)
                              (lambda (_) 1.0)
                              random-float
                              n))]
             [estimate (car result)]
             [ess-ratio (cdr result)])
        (and (>= estimate 0.0)
             (<= estimate 1.0)
             (> ess-ratio 0.0)
             (<= ess-ratio 1.0))))
    'tests 120)

  (define-property "self-normalized importance sample has positive finite ESS"
    gen-seed-n
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [result (run-mc seed
                             (self-normalized-importance-sample
                              (lambda (x) x)
                              (lambda (_) 1.0)
                              random-float
                              n))]
             [ess (cdr result)])
        (and (>= ess 1.0)
             (<= ess n))))
    'tests 120)

  (define-property "rejection-sample from triangular density returns in-range value"
    gen-seed
    (lambda (seed)
      (let ([x (run-mc seed
                       (rejection-sample
                        (lambda (u) (* 2.0 u))
                        2.0
                        random-float
                        (lambda (_) 1.0)))])
        (and (>= x 0.0)
             (<= x 1.0))))
    'tests 120)

  (define-property "rejection-sample-n length and range are valid"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (n) (cons seed n))
                 (gen-int-range 1 30))))
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [xs (run-mc seed
                         (rejection-sample-n
                          (lambda (u) (* 2.0 u))
                          2.0
                          random-float
                          (lambda (_) 1.0)
                          n))])
        (and (= (length xs) n)
             (all-satisfy? (lambda (x) (and (>= x 0.0) (<= x 1.0))) xs))))
    'tests 120)

  (define-property "rejection-sample-bounded returns values in [a,b]"
    gen-seed-interval
    (lambda (seed+ab)
      (let* ([seed (car seed+ab)]
             [a (cadr seed+ab)]
             [b (caddr seed+ab)]
             [x (run-mc seed (rejection-sample-bounded (lambda (_) 1.0) a b 1.0))])
        (and (<= a x) (<= x b))))
    'tests 120)
)

;;; ============================================================================
;;; MCMC and Gibbs
;;; ============================================================================

(test-group monte-carlo-mcmc-properties

  (define-property "random-walk-proposal returns a number"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (step-int) (cons seed (/ step-int 10.0)))
                 (gen-int-range 1 40))))
    (lambda (pair)
      (let* ([seed (car pair)]
             [step (cdr pair)]
             [proposal (random-walk-proposal step)]
             [x (run-mc seed (proposal 0.0))])
        (number? x)))
    'tests 120)

  (define-property "mh-step returns a numeric state"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (step-int) (cons seed (/ step-int 10.0)))
                 (gen-int-range 1 25))))
    (lambda (pair)
      (let* ([seed (car pair)]
             [step (cdr pair)]
             [proposal (random-walk-proposal step)]
             [x (run-mc seed (mh-step log-normal proposal (lambda (_a _b) 0.0) 0.0))])
        (number? x)))
    'tests 100)

  (define-property "mh-symmetric-step returns a numeric state"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (step-int) (cons seed (/ step-int 10.0)))
                 (gen-int-range 1 25))))
    (lambda (pair)
      (let* ([seed (car pair)]
             [step (cdr pair)]
             [proposal (random-walk-proposal step)]
             [x (run-mc seed (mh-symmetric-step log-normal proposal 0.0))])
        (number? x)))
    'tests 100)

  (define-property "mh-chain length matches request"
    gen-mh-args
    (lambda (args)
      (let* ([seed (car args)]
             [step (cadr args)]
             [n (caddr args)]
             [proposal (random-walk-proposal step)]
             [samples (run-mc seed (mh-chain log-normal proposal 0.0 n))])
        (= (length samples) n)))
    'tests 100)

  (define-property "mh-sample returns requested sample count"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-bind (gen-int-range 1 30)
          (lambda (step-int)
            (gen-bind (gen-int-range 1 50)
              (lambda (n-samples)
                (gen-map (lambda (burn-in)
                           (list seed (/ step-int 10.0) n-samples burn-in))
                         (gen-int-range 0 20))))))))
    (lambda (args)
      (let* ([seed (car args)]
             [step (cadr args)]
             [n-samples (caddr args)]
             [burn-in (cadddr args)]
             [proposal (random-walk-proposal step)]
             [samples (run-mc seed
                              (mh-sample log-normal proposal 0.0 n-samples burn-in))])
        (= (length samples) n-samples)))
    'tests 90)

  (define-property "mh-thinned-sample returns requested sample count"
    gen-thin-args
    (lambda (args)
      (let* ([seed (car args)]
             [step (cadr args)]
             [n-samples (caddr args)]
             [burn-in (cadddr args)]
             [thin (list-ref args 4)]
             [proposal (random-walk-proposal step)]
             [samples (run-mc seed
                              (mh-thinned-sample log-normal proposal 0.0 n-samples burn-in thin))])
        (= (length samples) n-samples)))
    'tests 80)

  (define-property "multivariate-random-walk-proposal preserves dimension"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (n) (cons seed n))
                 (gen-int-range 1 6))))
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [step-sizes (make-list n 0.5)]
             [proposal (multivariate-random-walk-proposal step-sizes)]
             [next (run-mc seed (proposal (make-list n 0.0)))])
        (and (= (length next) n)
             (all-satisfy? number? next))))
    'tests 100)

  (define-property "gibbs-step keeps state length"
    gen-gibbs-args
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [state (make-list 3 0.0)]
             [conds (list (cons 0 (lambda (_) random-normal-standard))
                          (cons 1 (lambda (_) random-normal-standard))
                          (cons 2 (lambda (_) random-normal-standard)))]
             [next (run-mc seed (gibbs-step conds state))])
        (= (length next) (length state))))
    'tests 100)

  (define-property "gibbs-chain length matches request"
    gen-gibbs-args
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [conds (list (cons 0 (lambda (_) random-normal-standard))
                          (cons 1 (lambda (_) random-normal-standard)))]
             [samples (run-mc seed (gibbs-chain conds '(0.0 0.0) n))])
        (= (length samples) n)))
    'tests 90)

  (define-property "gibbs-sample length matches request"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-bind (gen-int-range 1 50)
          (lambda (n-samples)
            (gen-map (lambda (burn-in)
                       (list seed n-samples burn-in))
                     (gen-int-range 0 20))))))
    (lambda (args)
      (let* ([seed (car args)]
             [n-samples (cadr args)]
             [burn-in (caddr args)]
             [conds (list (cons 0 (lambda (_) random-normal-standard))
                          (cons 1 (lambda (_) random-normal-standard)))]
             [samples (run-mc seed (gibbs-sample conds '(0.0 0.0) n-samples burn-in))])
        (= (length samples) n-samples)))
    'tests 80)
)

;;; ============================================================================
;;; Variance reduction and diagnostics
;;; ============================================================================

(test-group monte-carlo-advanced-properties

  (define-property "antithetic-estimate preserves constants"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-bind (gen-int-range -20 20)
          (lambda (c)
            (gen-map (lambda (n) (list seed c n))
                     (gen-int-range 2 200))))))
    (lambda (args)
      (let* ([seed (car args)]
             [c (cadr args)]
             [n (caddr args)]
             [result (run-mc seed (antithetic-estimate (lambda (_) c) n))])
        (and (approx= (car result) c 1e-12)
             (>= (cdr result) 0.0))))
    'tests 120)

  (define-property "antithetic-integrate constant equals c*(b-a)"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-bind (gen-int-range -10 10)
          (lambda (c)
            (gen-bind gen-interval
              (lambda (ab)
                (gen-map (lambda (n)
                           (list seed c (car ab) (cadr ab) n))
                         (gen-int-range 2 200))))))))
    (lambda (args)
      (let* ([seed (car args)]
             [c (cadr args)]
             [a (caddr args)]
             [b (cadddr args)]
             [n (list-ref args 4)]
             [expected (* c (- b a))]
             [result (run-mc seed (antithetic-integrate (lambda (_) c) a b n))])
        (and (approx= (car result) expected 0.15)
             (>= (cdr result) 0.0))))
    'tests 100)

  (define-property "control-variate-estimate returns bounded expectation for identity"
    gen-seed-n
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [result (run-mc seed
                             (control-variate-estimate
                              (lambda (x) x)
                              (lambda (x) x)
                              0.5
                              random-float
                              n))]
             [estimate (car result)]
             [se (cdr result)])
        (and (>= estimate 0.0)
             (<= estimate 1.0)
             (>= se 0.0))))
    'tests 100)

  (define-property "stratified-sample constant function tracks width"
    gen-stratified-args
    (lambda (args)
      (let* ([seed (car args)]
             [a (cadr args)]
             [b (caddr args)]
             [k (cadddr args)]
             [n (list-ref args 4)]
             [width (- b a)]
             [result (run-mc seed (stratified-sample (lambda (_) 1.0) a b k n))])
        (and (approx= (car result) width 0.25)
             (>= (cdr result) 0.0))))
    'tests 100)

  (define-property "effective-sample-size returns n for constant chains"
    gen-constant-samples
    (lambda (pair)
      (let* ([c (car pair)]
             [n (cdr pair)]
             [samples (make-list n c)])
        (= (effective-sample-size samples) n)))
    'tests 220)

  (define-property "effective-sample-size is positive"
    gen-seed-n
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [samples (run-mc seed (random-list n random-normal-standard))]
             [ess (effective-sample-size samples)])
        (> ess 0)))
    'tests 120)

  (define-property "gelman-rubin is positive for generated chains"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (n) (cons seed n))
                 (gen-int-range 20 150))))
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (cdr pair)]
             [chain1 (run-mc seed (random-list n random-normal-standard))]
             [chain2 (run-mc (+ seed 1) (random-list n random-normal-standard))]
             [chain3 (run-mc (+ seed 2) (random-list n random-normal-standard))]
             [rhat (gelman-rubin (list chain1 chain2 chain3))])
        (> rhat 0.0)))
    'tests 90)

  (define-property "autocorrelation at lag 0 is approximately 1"
    gen-seed-n
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (max 20 (cdr pair))]
             [samples (run-mc seed (random-list n random-normal-standard))]
             [rho0 (autocorrelation samples 0)])
        (approx= rho0 1.0 1e-9)))
    'tests 110)

  (define-property "integrated autocorrelation time is finite"
    gen-seed-n
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (max 30 (cdr pair))]
             [samples (run-mc seed (random-list n random-normal-standard))]
             [iat (integrated-autocorrelation-time samples)])
        (finite-number? iat)))
    'tests 100)

  (define-property "mcmc-diagnostics returns required keys for n>=10"
    gen-seed-n
    (lambda (pair)
      (let* ([seed (car pair)]
             [n (max 10 (cdr pair))]
             [samples (run-mc seed (random-list n random-normal-standard))]
             [diag (mcmc-diagnostics samples)])
        (and (assq 'n diag)
             (assq 'ess diag)
             (assq 'iat diag)
             (assq 'mean diag))))
    'tests 100)
)

;;; ============================================================================
;;; Batch and convenience APIs
;;; ============================================================================

(test-group monte-carlo-runner-properties

  (define-property "parallel-chains returns one chain per initial state"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (m) (cons seed m))
                 (gen-int-range 1 6))))
    (lambda (pair)
      (let* ([seed (car pair)]
             [m (cdr pair)]
             [runner (lambda (_) (random-list 8 random-float))]
             [chains (run-mc seed (parallel-chains runner (iota m)))])
        (and (= (length chains) m)
             (all-satisfy? (lambda (c) (= (length c) 8)) chains))))
    'tests 100)

  (define-property "batch-sample shape is n-batches x batch-size"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-bind (gen-int-range 1 20)
          (lambda (batch-size)
            (gen-map (lambda (n-batches)
                       (list seed batch-size n-batches))
                     (gen-int-range 1 20))))))
    (lambda (args)
      (let* ([seed (car args)]
             [batch-size (cadr args)]
             [n-batches (caddr args)]
             [batches (run-mc seed (batch-sample random-float batch-size n-batches))])
        (and (= (length batches) n-batches)
             (all-satisfy? (lambda (b) (= (length b) batch-size)) batches))))
    'tests 100)

  (define-property "progressive-sample respects target and max limits"
    gen-progressive-args
    (lambda (args)
      (let* ([seed (car args)]
             [target (cadr args)]
             [max-n (caddr args)]
             [samples (run-mc seed
                              (progressive-sample
                               random-float
                               max-n
                               (lambda (xs) (>= (length xs) target))))])
        (and (>= (length samples) target)
             (<= (length samples) max-n))))
    'tests 100)

  (define-property "run-mc is deterministic for fixed seed and computation"
    gen-seed
    (lambda (seed)
      (= (run-mc seed random-float)
         (run-mc seed random-float)))
    'tests 180)

  (define-property "mc-pi-estimate stays in plausible bounds"
    gen-seed
    (lambda (seed)
      (let ([pi-est (run-mc seed (mc-pi-estimate 300))])
        (and (>= pi-est 2.0)
             (<= pi-est 4.0))))
    'tests 120)

  (define-property "mc-normal-tail-prob stays in [0,1]"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (t-int) (list seed (/ t-int 10.0)))
                 (gen-int-range 0 30))))
    (lambda (args)
      (let* ([seed (car args)]
             [threshold (cadr args)]
             [p (run-mc seed (mc-normal-tail-prob threshold 1500))])
        (and (>= p 0.0)
             (<= p 1.0))))
    'tests 100)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
