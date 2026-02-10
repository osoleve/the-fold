;;; fabric/stitches/random/test-probability.ss — Tests for Probability Monad
;;;
;;; Tests for probabilistic programming constructs.

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/random/probability.ss")

;;; ====
;;; Helper Utilities
;;; ====

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (mean lst)
  (if (null? lst) 0
      (/ (fold-left + 0 lst) (length lst))))

(define (variance lst)
  (if (null? lst) 0
      (let ([m (mean lst)]
            [n (length lst)])
           (/ (fold-left + 0 (map (lambda (x) (* (- x m) (- x m))) lst)) n))))

;;; ====
;;; Basic Monad Tests
;;; ====

(test-group prob-basic
            ;; Pure/return
            (define-test prob-pure-value
              (let ([result (sample-once (prob-pure 42) 1)])
                   (assert-equal 42 result)))
            
            ;; Bind/sequence
            (define-test prob-bind-threads-state
              (let ([p (prob-bind (prob-pure 10)
                                  (lambda (x)
                                          (prob-pure (* x 2))))])
                   (assert-equal 20 (sample-once p 1))))
            
            ;; Map
            (define-test prob-map-applies-function
              (let ([p (prob-map (lambda (x) (+ x 1)) (prob-pure 5))])
                   (assert-equal 6 (sample-once p 1))))
            
            ;; Then (sequence)
            (define-test prob-then-sequences
              (let ([p (prob-then (prob-pure 1)
                                  (prob-pure 2))])
                   (assert-equal 2 (sample-once p 1))))
            
            ;; Applicative
            (define-test prob-ap-applies-wrapped-fn
              (let ([p (prob-ap (prob-pure (lambda (x) (* x 3)))
                                (prob-pure 7))])
                   (assert-equal 21 (sample-once p 1)))))

;;; ====
;;; Sampling Tests
;;; ====

(test-group prob-sampling
            ;; Uniform sampling
            (define-test prob-uniform-range
              (let* ([samples (sample-many prob-uniform 42 100)]
                     [all-valid (for-all (lambda (x) (and (>= x 0) (< x 1))) samples)])
                    (assert-true all-valid)))
            
            ;; Normal sampling
            (define-test prob-normal-mean
              (let* ([samples (sample-many (prob-normal 5.0 1.0) 42 1000)]
                     [m (mean samples)])
                    (assert-true (approx= m 5.0 0.2))))
            
            ;; Bernoulli sampling
            (define-test prob-bernoulli-proportion
              (let* ([samples (sample-many (prob-bernoulli 0.7) 42 1000)]
                     [count-true (length (filter (lambda (x) x) samples))]
                     [prop (/ count-true 1000.0)])
                    (assert-true (approx= prop 0.7 0.1))))
            
            ;; Poisson sampling
            (define-test prob-poisson-mean
              (let* ([samples (sample-many (prob-poisson 5.0) 42 1000)]
                     [m (mean samples)])
                    (assert-true (approx= m 5.0 0.5))))
            
            ;; sample-many produces correct count
            (define-test sample-many-count
              (let ([samples (sample-many prob-uniform 1 50)])
                   (assert-equal 50 (length samples)))))

;;; ====
;;; Conditioning Tests
;;; ====

(test-group prob-conditioning
            ;; Factor adds to log-weight
            (define-test factor-affects-weight
              (let* ([p (prob-then (factor (log-num 0.5))
                                   (prob-pure 1))]
                     [w (weight-prob p (make-pcg 1 1))])
                    (assert-true (approx= w (log-num 0.5) 0.001))))
            
            ;; Score with likelihood
            (define-test score-1-zero-weight
              (let* ([p (prob-then (score 1.0)
                                   (prob-pure 1))]
                     [w (weight-prob p (make-pcg 1 1))])
                    (assert-true (approx= w 0.0 0.001))))
            
            ;; Score with 0.5
            (define-test score-half-weight
              (let* ([p (prob-then (score 0.5)
                                   (prob-pure 1))]
                     [w (weight-prob p (make-pcg 1 1))])
                    (assert-true (approx= w (log-num 0.5) 0.001))))
            
            ;; Condition with true predicate
            (define-test condition-true-passes
              (let* ([p (condition (lambda (x) (> x 0)) (prob-pure 5))]
                     [result (sample-once p 1)])
                    (assert-equal 5 result)))
            
            ;; Condition with false predicate sets -inf weight
            (define-test condition-false-inf-weight
              (let* ([p (condition (lambda (x) (> x 10)) (prob-pure 5))]
                     [w (weight-prob p (make-pcg 1 1))])
                    (assert-equal -inf.0 w)))
            
            ;; Assume true
            (define-test assume-true-zero-weight
              (let ([w (weight-prob (assume #t) (make-pcg 1 1))])
                   (assert-equal 0.0 w)))
            
            ;; Assume false
            (define-test assume-false-inf-weight
              (let ([w (weight-prob (assume #f) (make-pcg 1 1))])
                   (assert-equal -inf.0 w))))

;;; ====
;;; Weighted Sampling Tests
;;; ====

(test-group prob-weighted
            ;; Weighted samples include weights
            (define-test weighted-samples-pairs
              (let* ([p (prob-then (factor -0.5) (prob-pure 42))]
                     [samples (weighted-samples p 1 5)])
                    (assert-equal 5 (length samples))
                    (assert-equal 42 (car (car samples)))
                    (assert-true (approx= -0.5 (cdr (car samples)) 0.001))))
            
            ;; Log-sum-exp basic
            (define-test log-sum-exp-equal
              (let ([result (log-sum-exp (list 0.0 0.0))])
                   (assert-true (approx= result (log-num 2) 0.001))))
            
            ;; Log-sum-exp with different values
            (define-test log-sum-exp-different
              (let ([result (log-sum-exp (list 0.0 (log-num 2.0)))])
                   ;; exp(0) + exp(log(2)) = 1 + 2 = 3, so log(3)
                   (assert-true (approx= result (log-num 3) 0.001))))
            
            ;; Normalize log-weights
            (define-test normalize-log-weights-sum
              (let* ([log-ws (list 0.0 (log-num 2))]
                     [normalized (normalize-log-weights log-ws)]
                     [sum-check (log-sum-exp normalized)])
                    (assert-true (approx= sum-check 0.0 0.001)))))

;;; ====
;;; Importance Sampling Tests
;;; ====

(test-group prob-importance
            ;; Mean of uniform
            (define-test importance-uniform-mean
              (let ([m (importance-mean prob-uniform 42 1000)])
                   (assert-true (approx= m 0.5 0.1))))
            
            ;; Mean of normal
            (define-test importance-normal-mean
              (let ([m (importance-mean (prob-normal 10.0 2.0) 42 1000)])
                   (assert-true (approx= m 10.0 0.5))))
            
            ;; Expectation of f(x) = x^2 for uniform
            (define-test importance-x-squared
              ;; For U[0,1], E[X^2] = 1/3
              (let ([e (importance-expectation (lambda (x) (* x x)) prob-uniform 42 1000)])
                   (assert-true (approx= e 0.333 0.1)))))

;;; ====
;;; Rejection Sampling Tests
;;; ====

(test-group prob-rejection
            ;; Rejection with always-valid returns value
            (define-test rejection-valid-just
              (let ([result (rejection-sample (prob-pure 42) 1 10)])
                   (assert-true (just? result))
                   (assert-equal 42 (from-just result))))
            
            ;; Rejection with never-valid returns nothing (after trying)
            (define-test rejection-invalid-nothing
              (let* ([p (prob-then (factor -inf.0) (prob-pure 42))]
                     [result (rejection-sample p 1 10)])
                    (assert-true (nothing? result))))
            
            ;; Rejection sampling with condition
            (define-test rejection-respects-condition
              (let* ([p (condition (lambda (x) (> x 0.5)) prob-uniform)]
                     [samples (rejection-samples p 42 100 100)])
                    (assert-true (> (length samples) 0))
                    (assert-true (for-all (lambda (x) (> x 0.5)) samples)))))

;;; ====
;;; Combinator Tests
;;; ====

(test-group prob-combinators
            ;; Sequence
            (define-test prob-sequence-order
              (let* ([ps (list (prob-pure 1) (prob-pure 2) (prob-pure 3))]
                     [result (sample-once (prob-sequence ps) 1)])
                    (assert-true (equal? '(1 2 3) result))))
            
            ;; Replicate
            (define-test prob-replicate-count
              (let ([result (sample-once (prob-replicate 5 (prob-pure 7)) 1)])
                   (assert-equal 5 (length result))
                   (assert-true (for-all (lambda (x) (= x 7)) result))))
            
            ;; Map-m
            (define-test prob-map-m-applies
              (let* ([f (lambda (x) (prob-pure (* x 2)))]
                     [result (sample-once (prob-map-m f '(1 2 3)) 1)])
                    (assert-true (equal? '(2 4 6) result)))))

;;; ====
;;; First-Class Distribution Tests
;;; ====

(test-group prob-distributions
            ;; Normal distribution sampling
            (define-test normal-dist-samples
              (let* ([d (normal-dist 0.0 1.0)]
                     [samples (sample-many (sample-dist d) 42 1000)]
                     [m (mean samples)])
                    (assert-true (approx= m 0.0 0.2))))
            
            ;; Distribution has PDF
            (define-test normal-dist-pdf
              (let* ([d (normal-dist 0.0 1.0)]
                     [pdf (dist-pdf d)]
                     [at-zero (pdf 0.0)]
                     ;; N(0,1) PDF at 0 = 1/sqrt(2*pi) ≈ 0.3989
                     [expected (/ 1 (sqrt (* 2 (pi-value))))])
                    (assert-true (approx= at-zero expected 0.001))))
            
            ;; Exponential distribution
            (define-test exponential-dist-mean
              (let* ([d (exponential-dist 0.5)]  ; mean = 2
                     [samples (sample-many (sample-dist d) 42 1000)]
                     [m (mean samples)])
                    (assert-true (approx= m 2.0 0.5)))))

;;; ====
;;; Bayesian Inference Pattern Tests
;;; ====

(test-group prob-bayesian
            ;; Simple inference: prior + observation
            (define-test posterior-shifts
              ;; Prior: N(0, 10), Observe: 5.0 from N(mu, 1)
              ;; Posterior mean should shift toward 5
              (let* ([model (prob-bind (prob-normal 0.0 10.0)
                                       (lambda (mu)
                                               (prob-then (observe (lambda (x) (normal-pdf x mu 1.0)) 5.0)
                                                          (prob-pure mu))))]
                     [posterior-mean (importance-mean model 42 5000)])
                    ;; With weak prior, posterior should be close to observation
                    (assert-true (approx= posterior-mean 5.0 1.0))))
            
            ;; Coin flip inference
            (define-test coin-flip-posterior
              ;; Prior: uniform on [0,1]
              ;; Data: 7 heads, 3 tails
              ;; Posterior: Beta(8,4), mean = 8/12 ≈ 0.667
              (let* ([heads 7]
                     [tails 3]
                     [model (prob-bind prob-uniform
                                       (lambda (theta)
                                               (prob-then
                                                ;; Log-likelihood of binomial
                                                (factor (+ (* heads (log-num (max theta 1e-10)))
                                                           (* tails (log-num (max (- 1 theta) 1e-10)))))
                                                (prob-pure theta))))]
                     [posterior-mean (importance-mean model 42 10000)])
                    (assert-true (approx= posterior-mean 0.667 0.1)))))

;;; ====
;;; Independence Tests (via Applicative)
;;; ====

(test-group prob-independence
            ;; Sum of two dice
            (define-test dice-sum-mean
              ;; E[D1 + D2] = 3.5 + 3.5 = 7
              (let* ([die (prob-uniform-int 1 6)]
                     [two-dice (prob-bind die
                                          (lambda (d1)
                                                  (prob-bind die
                                                             (lambda (d2)
                                                                     (prob-pure (+ d1 d2))))))]
                     [samples (sample-many two-dice 42 1000)]
                     [m (mean samples)])
                    (assert-true (approx= m 7.0 0.3))))
            
            ;; Multiple independent samples are different
            (define-test independent-samples-vary
              (let* ([two-uniforms (prob-bind prob-uniform
                                              (lambda (x1)
                                                      (prob-bind prob-uniform
                                                                 (lambda (x2)
                                                                         (prob-pure (cons x1 x2))))))]
                     [sample1 (sample-once two-uniforms 1)]
                     [sample2 (sample-once two-uniforms 2)])
                    ;; Different seeds should give different results
                    (assert-true (not (equal? sample1 sample2))))))

;;; ====
;;; Run All Tests
;;; ====

(display "\n====\n")
(display "Running Probability Monad Tests\n")
(display "====\n\n")

(run-all-tests)
