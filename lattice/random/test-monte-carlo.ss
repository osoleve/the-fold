;;; core/random/test-monte-carlo.ss -- Monte Carlo Methods Tests
;;;
;;; Comprehensive test suite for Monte Carlo integration, sampling,
;;; MCMC, variance reduction, and convergence diagnostics.

(load "core/test-framework.ss")
(load "lattice/random/monte-carlo.ss")
(load "lattice/data/sort.ss")

(display "
====
                    Monte Carlo Methods Test Suite
====
")

;;; ====
;;; Statistical Summary Tests
;;; ====

(test-group statistical-summary
            
            (define-test sample-mean-empty
              (assert-equal 0 (sample-mean '())))
            
            (define-test sample-mean-single
              (assert-equal 5 (sample-mean '(5))))
            
            (define-test sample-mean-integers
              (assert-equal 3 (sample-mean '(1 2 3 4 5))))
            
            (define-test sample-mean-floats
              (let ([mu (sample-mean '(1.0 2.0 3.0 4.0 5.0))])
                   (assert-true (< (abs (- mu 3.0)) 1e-10))))
            
            (define-test sample-variance-empty
              (assert-equal 0 (sample-variance '())))
            
            (define-test sample-variance-single
              (assert-equal 0 (sample-variance '(5))))
            
            (define-test sample-variance-uniform
              (assert-equal 0 (sample-variance '(5 5 5 5))))
            
            (define-test sample-variance-small
              ;; Var([1,2,3,4,5]) = 2.5 (sample variance)
              (let ([v (sample-variance '(1 2 3 4 5))])
                   (assert-true (< (abs (- v 2.5)) 1e-10))))
            
            (define-test sample-std-correct
              (let ([s (sample-std '(1 2 3 4 5))])
                   (assert-true (< (abs (- s (sqrt 2.5))) 1e-10))))
            
            (define-test sample-sem-correct
              (let ([sem (sample-sem '(1 2 3 4 5))])
                   (assert-true (< (abs (- sem (/ (sqrt 2.5) (sqrt 5)))) 1e-10))))
            
            (define-test sample-quantile-median
              (let ([med (sample-quantile '(1 2 3 4 5) 0.5)])
                   (assert-true (< (abs (- med 3)) 0.01))))
            
            (define-test sample-quantile-q25
              (let ([q (sample-quantile '(1 2 3 4 5 6 7 8 9 10) 0.25)])
                   (assert-true (and (> q 2) (< q 4)))))
            
            (define-test sample-quantile-q75
              (let ([q (sample-quantile '(1 2 3 4 5 6 7 8 9 10) 0.75)])
                   (assert-true (and (> q 7) (< q 9)))))
            
            (define-test sample-min-max
              (assert-equal 1 (sample-min '(3 1 4 1 5 9)))
              (assert-equal 9 (sample-max '(3 1 4 1 5 9))))
            
            (define-test sample-summary-complete
              (let ([s (sample-summary '(1 2 3 4 5))])
                   (assert-equal 5 (cdr (assq 'n s)))
                   (assert-equal 3 (cdr (assq 'mean s)))
                   (assert-equal 1 (cdr (assq 'min s)))
                   (assert-equal 5 (cdr (assq 'max s)))))
            
            (define-test sample-summary-empty
              (let ([s (sample-summary '())])
                   (assert-equal 0 (cdr (assq 'n s)))))
            )

;;; ====
;;; Monte Carlo Integration Tests
;;; ====

(test-group mc-integration
            
            (define-test mc-integrate-constant
              ;; Integral of 1 from 0 to 1 = 1
              (let ([result (run-mc 42 (mc-integrate (lambda (x) 1) 0 1 1000))])
                   (assert-true (< (abs (- (car result) 1.0)) 0.1))))
            
            (define-test mc-integrate-linear
              ;; Integral of x from 0 to 1 = 0.5
              (let ([result (run-mc 42 (mc-integrate (lambda (x) x) 0 1 1000))])
                   (assert-true (< (abs (- (car result) 0.5)) 0.1))))
            
            (define-test mc-integrate-quadratic
              ;; Integral of x^2 from 0 to 1 = 1/3
              (let ([result (run-mc 42 (mc-integrate (lambda (x) (* x x)) 0 1 1000))])
                   (assert-true (< (abs (- (car result) 0.333)) 0.1))))
            
            (define-test mc-integrate-sine
              ;; Integral of sin(x) from 0 to pi = 2
              (let ([result (run-mc 42 (mc-integrate sin-num 0 (pi-value) 2000))])
                   (assert-true (< (abs (- (car result) 2.0)) 0.2))))
            
            (define-test mc-integrate-returns-error
              ;; Check that standard error is returned and positive
              (let ([result (run-mc 42 (mc-integrate (lambda (x) x) 0 1 1000))])
                   (assert-true (> (cdr result) 0))))
            
            (define-test mc-estimate-uniform
              (let ([result (run-mc 42 (mc-estimate random-float 1000))])
                   (assert-true (< (abs (- (car result) 0.5)) 0.05))))
            )

;;; ====
;;; Importance Sampling Tests
;;; ====

(test-group importance-sampling
            
            (define-test importance-sample-uniform
              ;; Sample E[X] where X ~ U(0,1) using proposal U(0,1)
              ;; Weight = 1 (same distribution), E[X] = 0.5
              (let ([result (run-mc 42
                                    (importance-sample
                                     (lambda (x) x)           ; f(x) = x
                                     (lambda (x) 1)           ; weight = 1 (identical)
                                     random-float
                                     1000))])
                   (assert-true (< (abs (- (car result) 0.5)) 0.1))))
            
            (define-test self-normalized-importance
              ;; Self-normalized should also work
              (let ([result (run-mc 42
                                    (self-normalized-importance-sample
                                     (lambda (x) x)
                                     (lambda (x) 1)
                                     random-float
                                     1000))])
                   (assert-true (< (abs (- (car result) 0.5)) 0.1))))
            
            (define-test importance-sample-ess
              ;; Check that ESS ratio is returned (should be close to 1 for identical dists)
              (let ([result (run-mc 42
                                    (importance-sample
                                     (lambda (x) x)
                                     (lambda (x) 1)
                                     random-float
                                     1000))])
                   (assert-true (> (cdr result) 0.8))))  ; ESS ratio close to 1
            )

;;; ====
;;; Rejection Sampling Tests
;;; ====

(test-group rejection-sampling
            
            (define-test rejection-sample-bounded-uniform
              ;; Sample from f(x) = 2x on [0,1] using rejection
              ;; f(x) = 2x is bounded by M = 2
              (let* ([samples (run-mc 42
                                      (rejection-sample-n
                                       (lambda (x) (* 2 x))      ; target ~ 2x
                                       2                          ; M = 2
                                       random-float               ; proposal: U(0,1)
                                       (lambda (x) 1)             ; proposal density
                                       1000))]
                     [mu (sample-mean samples)])
                    ;; E[X] for f(x) = 2x is 2/3
                    (assert-true (< (abs (- mu 0.667)) 0.1))))
            
            (define-test rejection-sample-bounded-triangle
              ;; Sample from triangular density on [0,1]
              (let* ([samples (run-mc 42
                                      (random-list 500
                                                   (rejection-sample-bounded (lambda (x) (* 2 x)) 0 1 2)))]
                     [mu (sample-mean samples)])
                    (assert-true (< (abs (- mu 0.667)) 0.1))))
            )

;;; ====
;;; Metropolis-Hastings MCMC Tests
;;; ====

(test-group metropolis-hastings
            
            (define-test mh-symmetric-step-accepts
              ;; Simple test: flat target should accept everything
              (let* ([log-flat (lambda (x) 0)]  ; Uniform on R
                     [proposal (random-walk-proposal 1)]
                     [next (run-mc 42 (mh-symmetric-step log-flat proposal 0))])
                    (assert-true (number? next))))
            
            (define-test mh-chain-produces-samples
              ;; Check chain produces correct number of samples
              (let* ([log-normal (lambda (x) (* -0.5 x x))]  ; N(0,1)
                     [proposal (random-walk-proposal 1)]
                     [samples (run-mc 42 (mh-chain log-normal proposal 0 100))])
                    (assert-equal 100 (length samples))))
            
            (define-test mh-sample-normal-mean
              ;; Sample from N(0,1) and check mean is near 0
              (let* ([log-normal (lambda (x) (* -0.5 x x))]
                     [proposal (random-walk-proposal 1)]
                     [samples (run-mc 42 (mh-sample log-normal proposal 0 1000 200))]
                     [mu (sample-mean samples)])
                    (assert-true (< (abs mu) 0.2))))
            
            (define-test mh-sample-normal-variance
              ;; Sample from N(0,1) and check variance is near 1
              (let* ([log-normal (lambda (x) (* -0.5 x x))]
                     [proposal (random-walk-proposal 0.8)]
                     [samples (run-mc 42 (mh-sample log-normal proposal 0 2000 500))]
                     [v (sample-variance samples)])
                    (assert-true (< (abs (- v 1.0)) 0.3))))
            
            (define-test mh-thinned-sample-length
              ;; Check thinned sampling produces correct number
              (let* ([log-normal (lambda (x) (* -0.5 x x))]
                     [proposal (random-walk-proposal 1)]
                     [samples (run-mc 42 (mh-thinned-sample log-normal proposal 0 100 50 5))])
                    (assert-equal 100 (length samples))))
            
            (define-test mh-multivariate-proposal
              ;; Test multivariate random walk proposal
              (let* ([proposal (multivariate-random-walk-proposal '(0.5 0.5))]
                     [next (run-mc 42 (proposal '(0 0)))])
                    (assert-equal 2 (length next))
                    (assert-true (number? (car next)))
                    (assert-true (number? (cadr next)))))
            
            (define-test mh-sample-bimodal
              ;; Sample from bimodal: mixture of N(-2,1) and N(2,1)
              (let* ([log-bimodal (lambda (x)
                                          (log-num (+ (exp-num (* -0.5 (expt (+ x 2) 2)))
                                                      (exp-num (* -0.5 (expt (- x 2) 2))))))]
                     [proposal (random-walk-proposal 1.5)]
                     [samples (run-mc 42 (mh-sample log-bimodal proposal 0 2000 500))]
                     [mu (sample-mean samples)])
                    ;; Mean should be near 0 for symmetric bimodal
                    (assert-true (< (abs mu) 1.0))))
            )

;;; ====
;;; Gibbs Sampling Tests
;;; ====

(test-group gibbs-sampling
            
            (define-test gibbs-step-updates
              ;; Simple 2D case: conditionals just sample standard normal
              (let* ([cond0 (cons 0 (lambda (state) random-normal-standard))]
                     [cond1 (cons 1 (lambda (state) random-normal-standard))]
                     [result (run-mc 42 (gibbs-step (list cond0 cond1) '(0 0)))])
                    (assert-equal 2 (length result))
                    (assert-true (number? (car result)))
                    (assert-true (number? (cadr result)))))
            
            (define-test gibbs-chain-length
              (let* ([cond0 (cons 0 (lambda (state) random-normal-standard))]
                     [cond1 (cons 1 (lambda (state) random-normal-standard))]
                     [samples (run-mc 42 (gibbs-chain (list cond0 cond1) '(0 0) 100))])
                    (assert-equal 100 (length samples))))
            
            (define-test gibbs-sample-independent
              ;; Two independent N(0,1) coordinates
              (let* ([cond0 (cons 0 (lambda (state) random-normal-standard))]
                     [cond1 (cons 1 (lambda (state) random-normal-standard))]
                     [samples (run-mc 42 (gibbs-sample (list cond0 cond1) '(0 0) 500 100))]
                     [xs (map car samples)]
                     [ys (map cadr samples)]
                     [mu-x (sample-mean xs)]
                     [mu-y (sample-mean ys)])
                    (assert-true (< (abs mu-x) 0.2))
                    (assert-true (< (abs mu-y) 0.2))))
            
            (define-test list-update-works
              (assert-equal '(9 2 3) (list-update '(1 2 3) 0 9))
              (assert-equal '(1 9 3) (list-update '(1 2 3) 1 9))
              (assert-equal '(1 2 9) (list-update '(1 2 3) 2 9)))
            )

;;; ====
;;; Antithetic Variates Tests
;;; ====

(test-group antithetic-variates
            
            (define-test antithetic-estimate-uniform
              ;; E[U] = 0.5 for U ~ Uniform(0,1)
              (let ([result (run-mc 42 (antithetic-estimate (lambda (u) u) 1000))])
                   (assert-true (< (abs (- (car result) 0.5)) 0.05))))
            
            (define-test antithetic-estimate-quadratic
              ;; E[U^2] = 1/3 for U ~ Uniform(0,1)
              (let ([result (run-mc 42 (antithetic-estimate (lambda (u) (* u u)) 1000))])
                   (assert-true (< (abs (- (car result) 0.333)) 0.05))))
            
            (define-test antithetic-integrate-linear
              ;; Integral of x from 0 to 1 = 0.5
              (let ([result (run-mc 42 (antithetic-integrate (lambda (x) x) 0 1 1000))])
                   (assert-true (< (abs (- (car result) 0.5)) 0.05))))
            
            (define-test antithetic-integrate-quadratic
              ;; Integral of x^2 from 0 to 1 = 1/3
              (let ([result (run-mc 42 (antithetic-integrate (lambda (x) (* x x)) 0 1 1000))])
                   (assert-true (< (abs (- (car result) 0.333)) 0.05))))
            
            (define-test antithetic-reduces-variance
              ;; Antithetic should have lower SE than standard MC for monotonic f
              (let* ([f (lambda (x) x)]
                     [standard (run-mc 42 (mc-integrate f 0 1 1000))]
                     [antithetic (run-mc 42 (antithetic-integrate f 0 1 1000))])
                    ;; Antithetic SE should typically be smaller
                    (assert-true (<= (cdr antithetic) (* 1.5 (cdr standard))))))
            )

;;; ====
;;; Control Variates Tests
;;; ====

(test-group control-variates
            
            (define-test control-variate-identity
              ;; Use X as control for X, should recover mean exactly
              (let ([result (run-mc 42
                                    (control-variate-estimate
                                     (lambda (x) x)    ; f = X
                                     (lambda (x) x)    ; g = X
                                     0.5               ; E[X] = 0.5 for U(0,1)
                                     random-float
                                     1000))])
                   (assert-true (< (abs (- (car result) 0.5)) 0.1))))
            
            (define-test control-variate-correlated
              ;; Estimate E[X^2] using X as control variate
              (let ([result (run-mc 42
                                    (control-variate-estimate
                                     (lambda (x) (* x x))  ; f = X^2
                                     (lambda (x) x)        ; g = X
                                     0.5                   ; E[X] = 0.5
                                     random-float
                                     1000))])
                   (assert-true (< (abs (- (car result) 0.333)) 0.1))))
            )

;;; ====
;;; Stratified Sampling Tests
;;; ====

(test-group stratified-sampling
            
            (define-test stratified-sample-constant
              ;; Integral of 1 from 0 to 1 = 1
              (let ([result (run-mc 42 (stratified-sample (lambda (x) 1) 0 1 10 1000))])
                   (assert-true (< (abs (- (car result) 1.0)) 0.1))))
            
            (define-test stratified-sample-linear
              ;; Integral of x from 0 to 1 = 0.5
              (let ([result (run-mc 42 (stratified-sample (lambda (x) x) 0 1 10 1000))])
                   (assert-true (< (abs (- (car result) 0.5)) 0.1))))
            
            (define-test stratified-sample-quadratic
              ;; Integral of x^2 from 0 to 1 = 1/3
              (let ([result (run-mc 42 (stratified-sample (lambda (x) (* x x)) 0 1 10 1000))])
                   (assert-true (< (abs (- (car result) 0.333)) 0.1))))
            
            (define-test iota-works
              (assert-equal '() (iota 0))
              (assert-equal '(0) (iota 1))
              (assert-equal '(0 1 2 3 4) (iota 5)))
            )

;;; ====
;;; Convergence Diagnostics Tests
;;; ====

(test-group convergence-diagnostics
            
            (define-test effective-sample-size-iid
              ;; For IID samples, ESS should be close to n
              (let* ([samples (run-mc 42 (random-list 1000 random-normal-standard))]
                     [ess (effective-sample-size samples)])
                    ;; ESS should be reasonably close to n for iid
                    (assert-true (> ess 500))))
            
            (define-test split-into-batches-works
              (let ([batches (split-into-batches '(1 2 3 4 5 6 7 8 9 10) 3)])
                   (assert-equal 3 (length batches))
                   (assert-equal '(1 2 3) (car batches))
                   (assert-equal '(4 5 6) (cadr batches))
                   (assert-equal '(7 8 9) (caddr batches))))
            
            (define-test gelman-rubin-converged
              ;; Multiple chains from same distribution should have R-hat near 1
              (let* ([gen-chain (lambda (seed)
                                        (eval-state (random-list 500 random-normal-standard)
                                                    (make-pcg seed 1)))]
                     [chains (map gen-chain '(1 2 3 4))]
                     [rhat (gelman-rubin chains)])
                    (assert-true (< rhat 1.2))))
            
            (define-test gelman-rubin-not-converged
              ;; Chains with very different means should have high R-hat
              (let* ([chain1 (map (lambda (x) (+ x 0)) (run-mc 1 (random-list 100 random-float)))]
                     [chain2 (map (lambda (x) (+ x 5)) (run-mc 2 (random-list 100 random-float)))]
                     [rhat (gelman-rubin (list chain1 chain2))])
                    (assert-true (> rhat 2))))
            
            (define-test autocorrelation-lag-0
              ;; Autocorrelation at lag 0 should be 1
              (let* ([samples (run-mc 42 (random-list 100 random-normal-standard))]
                     [rho0 (autocorrelation samples 0)])
                    (assert-true (< (abs (- rho0 1)) 0.001))))
            
            (define-test autocorrelation-iid-near-zero
              ;; For IID samples, autocorrelation at lag > 0 should be small
              (let* ([samples (run-mc 42 (random-list 1000 random-normal-standard))]
                     [rho5 (autocorrelation samples 5)])
                    (assert-true (< (abs rho5) 0.2))))
            
            (define-test integrated-autocorrelation-time-iid
              ;; For IID, IAT should be close to 1
              (let* ([samples (run-mc 42 (random-list 500 random-normal-standard))]
                     [iat (integrated-autocorrelation-time samples)])
                    (assert-true (< iat 2))))
            
            (define-test mcmc-diagnostics-complete
              (let* ([samples (run-mc 42 (random-list 500 random-normal-standard))]
                     [diag (mcmc-diagnostics samples)])
                    (assert-true (pair? (assq 'n diag)))
                    (assert-true (pair? (assq 'ess diag)))
                    (assert-true (pair? (assq 'iat diag)))
                    (assert-true (pair? (assq 'mean diag)))))
            )

;;; ====
;;; Batch Sampling Tests
;;; ====

(test-group batch-sampling
            
            (define-test batch-sample-structure
              (let ([batches (run-mc 42 (batch-sample random-float 10 5))])
                   (assert-equal 5 (length batches))
                   (assert-equal 10 (length (car batches)))))
            
            (define-test parallel-chains-structure
              (let* ([runner (lambda (init) (random-list 10 random-float))]
                     [chains (run-mc 42 (parallel-chains runner '(1 2 3)))])
                    (assert-equal 3 (length chains))
                    (assert-equal 10 (length (car chains)))))
            
            (define-test progressive-sample-convergence
              (let* ([converged? (lambda (s) (>= (length s) 100))]
                     [samples (run-mc 42 (progressive-sample random-float 500 converged?))])
                    (assert-true (>= (length samples) 100))))
            )

;;; ====
;;; Convenience Function Tests
;;; ====

(test-group convenience-functions
            
            (define-test run-mc-deterministic
              ;; Same seed should give same result
              (let ([r1 (run-mc 42 random-float)]
                    [r2 (run-mc 42 random-float)])
                   (assert-equal r1 r2)))
            
            (define-test run-mc-different-seeds
              ;; Different seeds should give different results
              (let ([r1 (run-mc 42 random-float)]
                    [r2 (run-mc 43 random-float)])
                   (assert-false (= r1 r2))))
            
            (define-test mc-pi-estimate-accuracy
              (let ([pi-est (run-mc 42 (mc-pi-estimate 10000))])
                   (assert-true (< (abs (- pi-est (pi-value))) 0.1))))
            
            (define-test mc-normal-tail-prob-reasonable
              ;; P(Z > 0) should be about 0.5
              (let ([prob (run-mc 42 (mc-normal-tail-prob 0 5000))])
                   (assert-true (< (abs (- prob 0.5)) 0.1))))
            
            (define-test mc-normal-tail-prob-extreme
              ;; P(Z > 2) should be about 0.0228
              (let ([prob (run-mc 42 (mc-normal-tail-prob 2 5000))])
                   (assert-true (< (abs (- prob 0.023)) 0.02))))
            )

;;; ====
;;; Helper Function Tests
;;; ====

(test-group helper-functions
            
            (define-test take-empty
              (assert-equal '() (take 0 '(1 2 3))))
            
            (define-test take-partial
              (assert-equal '(1 2) (take 2 '(1 2 3 4 5))))
            
            (define-test take-all
              (assert-equal '(1 2 3) (take 5 '(1 2 3))))
            
            (define-test sort-by-empty
              (assert-equal '() (sort-by < '())))
            
            (define-test sort-by-single
              (assert-equal '(5) (sort-by < '(5))))
            
            (define-test sort-by-ascending
              (assert-equal '(1 1 2 3 4 5 9) (sort-by < '(3 1 4 1 5 9 2))))
            
            (define-test sort-by-descending
              (assert-equal '(9 5 4 3 2 1 1) (sort-by > '(3 1 4 1 5 9 2))))
            
            (define-test sort-by-already-sorted
              (assert-equal '(1 2 3 4 5) (sort-by < '(1 2 3 4 5))))
            )

;;; ====
;;; Integration Tests (End-to-End)
;;; ====

(test-group integration-tests
            
            (define-test full-mcmc-workflow
              ;; Complete workflow: sample, diagnose, summarize
              (let* ([log-normal (lambda (x) (* -0.5 x x))]
                     [proposal (random-walk-proposal 1)]
                     [samples (run-mc 42 (mh-sample log-normal proposal 0 1000 200))]
                     [summary (sample-summary samples)]
                     [diag (mcmc-diagnostics samples)])
                    ;; Check summary
                    (assert-equal 1000 (cdr (assq 'n summary)))
                    (assert-true (< (abs (cdr (assq 'mean summary))) 0.3))
                    ;; Check diagnostics
                    (assert-true (> (cdr (assq 'ess diag)) 100))))
            
            (define-test variance-reduction-comparison
              ;; Compare standard MC with variance reduction techniques
              (let* ([f (lambda (x) (* x x))]  ; f(x) = x^2
                     [standard (run-mc 42 (mc-integrate f 0 1 1000))]
                     [antithetic (run-mc 42 (antithetic-integrate f 0 1 1000))]
                     [stratified (run-mc 42 (stratified-sample f 0 1 10 1000))])
                    ;; All should be close to 1/3
                    (assert-true (< (abs (- (car standard) 0.333)) 0.1))
                    (assert-true (< (abs (- (car antithetic) 0.333)) 0.1))
                    (assert-true (< (abs (- (car stratified) 0.333)) 0.1))))
            
            (define-test importance-sampling-tail
              ;; Use importance sampling to estimate rare event probability
              ;; P(X > 3) for X ~ N(0,1) is about 0.00135
              (let* ([threshold 3]
                     [proposal (random-exponential 1)]
                     [result (run-mc 42
                                     (state-bind (random-list 5000
                                                              (state-bind proposal
                                                                          (lambda (e)
                                                                                  (let* ([x (+ threshold e)]
                                                                                         [target-log (* -0.5 x x)]
                                                                                         [proposal-log (- e)]
                                                                                         [weight (exp-num (- target-log proposal-log))])
                                                                                        (state-pure weight)))))
                                                 (lambda (weights)
                                                         (let ([estimate (/ (fold-left + 0 weights)
                                                                            (* 5000 (sqrt (* 2 (pi-value)))))])
                                                              (state-pure estimate)))))])
                    ;; Should be roughly 0.00135
                    (assert-true (< result 0.01))))
            )

;;; ====
;;; Run All Tests
;;; ====

(newline)
(display "Test Results Summary:")
(newline)
(print-summary)
(exit-with-summary)
