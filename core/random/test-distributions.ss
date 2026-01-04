;;; core/random/test-distributions.ss — Tests for Probability Distributions
;;;
;;; Comprehensive tests for probability distributions.

(load "core/random/distributions.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "  ✗ ")
       (display name)
       (newline)
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-approx name expected actual tolerance)
  (set! *test-count* (+ *test-count* 1))
  (let ([diff (abs (- expected actual))])
       (if (< diff tolerance)
           (begin
            (set! *pass-count* (+ *pass-count* 1))
            (display "  ✓ ")
            (display name)
            (newline))
           (begin
            (set! *fail-count* (+ *fail-count* 1))
            (display "  ✗ ")
            (display name)
            (newline)
            (display "    Expected: ")
            (write expected)
            (display " ± ")
            (write tolerance)
            (newline)
            (display "    Actual:   ")
            (write actual)
            (display " (diff: ")
            (write diff)
            (display ")")
            (newline)))))

;;; Helper to compute sample mean
(define (sample-mean samples)
  (/ (fold-left + 0 samples) (length samples)))

;;; Helper to compute sample variance
(define (sample-variance samples)
  (let* ([n (length samples)]
         [mean (sample-mean samples)]
         [sum-sq (fold-left + 0 (map (lambda (x) (expt (- x mean) 2)) samples))])
        (/ sum-sq n)))

;;; Helper: for-all
(define (for-all pred lst)
  (cond
   [(null? lst) #t]
   [(pred (car lst)) (for-all pred (cdr lst))]
   [else #f]))

(define (run-tests)
  (display "\n=== Probability Distributions Test Suite ===\n")
  
  ;;; ============================================================
  ;;; Bernoulli Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Bernoulli Distribution ---\n")
  
  ;; Parameter validation would error - skip for now
  
  ;; Sampling produces booleans
  (let* ([gen (make-pcg 42 1)]
         [samples (let loop ([g gen] [n 100] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-bernoulli 0.5) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))])
        (test-true "random-bernoulli: all results are booleans"
                   (for-all boolean? samples))
        (test-true "random-bernoulli: produces #t" (and (member #t samples) #t))
        (test-true "random-bernoulli: produces #f" (and (member #f samples) #t)))
  
  ;; p=0 always false, p=1 always true
  (let* ([gen (make-pcg 42 1)]
         [result (car (run-state (random-bernoulli 0) gen))])
        (test "random-bernoulli p=0: always false" #f result))
  
  (let* ([gen (make-pcg 42 1)]
         [result (car (run-state (random-bernoulli 1) gen))])
        (test "random-bernoulli p=1: always true" #t result))
  
  ;;; ============================================================
  ;;; Exponential Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Exponential Distribution ---\n")
  
  ;; Sampling produces positive floats
  (let* ([gen (make-pcg 42 1)]
         [samples (let loop ([g gen] [n 100] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-exponential 1.0) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))])
        (test-true "random-exponential: all values >= 0"
                   (for-all (lambda (x) (>= x 0)) samples)))
  
  ;; Mean should be approximately 1/rate
  (let* ([gen (make-pcg 42 1)]
         [rate 2.0]
         [samples (let loop ([g gen] [n 1000] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-exponential rate) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))]
         [mean (sample-mean samples)]
         [expected-mean (/ 1 rate)])
        (test-approx "random-exponential: mean ≈ 1/rate"
                     expected-mean mean 0.1))
  
  ;;; ============================================================
  ;;; Normal Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Normal Distribution ---\n")
  
  ;; Standard normal
  (let* ([gen (make-pcg 42 1)]
         [samples (let loop ([g gen] [n 1000] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state random-normal-standard g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))]
         [mean (sample-mean samples)]
         [var (sample-variance samples)])
        (test-approx "random-normal-standard: mean ≈ 0" 0 mean 0.1)
        (test-approx "random-normal-standard: variance ≈ 1" 1 var 0.2))
  
  ;; Custom mean and stddev
  (let* ([gen (make-pcg 42 1)]
         [target-mean 5.0]
         [target-stddev 2.0]
         [samples (let loop ([g gen] [n 1000] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-normal target-mean target-stddev) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))]
         [mean (sample-mean samples)]
         [var (sample-variance samples)])
        (test-approx "random-normal: mean ≈ target" target-mean mean 0.2)
        (test-approx "random-normal: variance ≈ stddev^2"
                     (* target-stddev target-stddev) var 0.5))
  
  ;; Normal pair produces two values
  (let* ([gen (make-pcg 42 1)]
         [pair (car (run-state random-normal-pair gen))])
        (test-true "random-normal-pair: returns pair" (pair? pair))
        (test-true "random-normal-pair: first is number" (number? (car pair)))
        (test-true "random-normal-pair: second is number" (number? (cdr pair))))
  
  ;;; ============================================================
  ;;; Geometric Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Geometric Distribution ---\n")
  
  ;; All values >= 1
  (let* ([gen (make-pcg 42 1)]
         [samples (let loop ([g gen] [n 100] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-geometric 0.3) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))])
        (test-true "random-geometric: all values >= 1"
                   (for-all (lambda (x) (>= x 1)) samples))
        (test-true "random-geometric: all values are integers"
                   (for-all integer? samples)))
  
  ;; p=1 always returns 1
  (let* ([gen (make-pcg 42 1)]
         [result (car (run-state (random-geometric 1) gen))])
        (test "random-geometric p=1: always 1" 1 result))
  
  ;;; ============================================================
  ;;; Poisson Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Poisson Distribution ---\n")
  
  ;; All values >= 0
  (let* ([gen (make-pcg 42 1)]
         [samples (let loop ([g gen] [n 100] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-poisson 5.0) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))])
        (test-true "random-poisson: all values >= 0"
                   (for-all (lambda (x) (>= x 0)) samples))
        (test-true "random-poisson: all values are integers"
                   (for-all integer? samples)))
  
  ;; Mean ≈ rate (for Poisson, mean = variance = rate)
  (let* ([gen (make-pcg 42 1)]
         [rate 7.0]
         [samples (let loop ([g gen] [n 500] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-poisson rate) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))]
         [mean (sample-mean samples)])
        (test-approx "random-poisson: mean ≈ rate" rate mean 0.5))
  
  ;; Large rate (tests rejection method)
  (let* ([gen (make-pcg 42 1)]
         [rate 50.0]
         [samples (let loop ([g gen] [n 200] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-poisson rate) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))]
         [mean (sample-mean samples)])
        (test-approx "random-poisson large: mean ≈ rate" rate mean 3.0))
  
  ;;; ============================================================
  ;;; Binomial Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Binomial Distribution ---\n")
  
  ;; Bounds: 0 <= k <= n
  (let* ([gen (make-pcg 42 1)]
         [n 10]
         [p 0.5]
         [samples (let loop ([g gen] [i 100] [acc '()])
                       (if (= i 0) acc
                           (let ([r (run-state (random-binomial n p) g)])
                                (loop (cdr r) (- i 1) (cons (car r) acc)))))])
        (test-true "random-binomial: all values >= 0"
                   (for-all (lambda (x) (>= x 0)) samples))
        (test-true "random-binomial: all values <= n"
                   (for-all (lambda (x) (<= x n)) samples)))
  
  ;; Edge cases
  (let* ([gen (make-pcg 42 1)])
        (test "random-binomial n=0: always 0" 0
              (car (run-state (random-binomial 0 0.5) gen)))
        (test "random-binomial p=0: always 0" 0
              (car (run-state (random-binomial 10 0) gen)))
        (test "random-binomial p=1: always n" 10
              (car (run-state (random-binomial 10 1) gen))))
  
  ;; Mean ≈ n*p
  (let* ([gen (make-pcg 42 1)]
         [n 15]
         [p 0.4]
         [samples (let loop ([g gen] [i 500] [acc '()])
                       (if (= i 0) acc
                           (let ([r (run-state (random-binomial n p) g)])
                                (loop (cdr r) (- i 1) (cons (car r) acc)))))]
         [mean (sample-mean samples)]
         [expected (* n p)])
        (test-approx "random-binomial: mean ≈ n*p" expected mean 0.5))
  
  ;;; ============================================================
  ;;; Gamma Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Gamma Distribution ---\n")
  
  ;; All values > 0
  (let* ([gen (make-pcg 42 1)]
         [samples (let loop ([g gen] [n 100] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-gamma 2.0 1.0) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))])
        (test-true "random-gamma: all values > 0"
                   (for-all (lambda (x) (> x 0)) samples)))
  
  ;; Mean ≈ shape/rate
  (let* ([gen (make-pcg 42 1)]
         [shape 3.0]
         [rate 2.0]
         [samples (let loop ([g gen] [n 500] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-gamma shape rate) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))]
         [mean (sample-mean samples)]
         [expected (/ shape rate)])
        (test-approx "random-gamma: mean ≈ shape/rate" expected mean 0.2))
  
  ;; Shape < 1 (tests boost method)
  (let* ([gen (make-pcg 42 1)]
         [samples (let loop ([g gen] [n 100] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-gamma 0.5 1.0) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))])
        (test-true "random-gamma shape<1: all values > 0"
                   (for-all (lambda (x) (> x 0)) samples)))
  
  ;;; ============================================================
  ;;; Beta Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Beta Distribution ---\n")
  
  ;; All values in (0, 1)
  (let* ([gen (make-pcg 42 1)]
         [samples (let loop ([g gen] [n 100] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-beta 2.0 5.0) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))])
        (test-true "random-beta: all values > 0"
                   (for-all (lambda (x) (> x 0)) samples))
        (test-true "random-beta: all values < 1"
                   (for-all (lambda (x) (< x 1)) samples)))
  
  ;; Mean ≈ alpha/(alpha+beta)
  (let* ([gen (make-pcg 42 1)]
         [alpha 2.0]
         [beta 5.0]
         [samples (let loop ([g gen] [n 500] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-beta alpha beta) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))]
         [mean (sample-mean samples)]
         [expected (/ alpha (+ alpha beta))])
        (test-approx "random-beta: mean ≈ α/(α+β)" expected mean 0.05))
  
  ;;; ============================================================
  ;;; Categorical Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Categorical Distribution ---\n")
  
  ;; Returns valid indices
  (let* ([gen (make-pcg 42 1)]
         [weights '(0.2 0.3 0.5)]
         [samples (let loop ([g gen] [n 100] [acc '()])
                       (if (= n 0) acc
                           (let ([r (run-state (random-categorical weights) g)])
                                (loop (cdr r) (- n 1) (cons (car r) acc)))))])
        (test-true "random-categorical: all indices >= 0"
                   (for-all (lambda (x) (>= x 0)) samples))
        (test-true "random-categorical: all indices < length"
                   (for-all (lambda (x) (< x (length weights))) samples)))
  
  ;;; ============================================================
  ;;; Dirichlet Distribution Tests
  ;;; ============================================================
  
  (display "\n--- Dirichlet Distribution ---\n")
  
  ;; Returns probability vector summing to 1
  (let* ([gen (make-pcg 42 1)]
         [alphas '(1.0 2.0 3.0)]
         [result (car (run-state (random-dirichlet alphas) gen))]
         [sum (fold-left + 0 result)])
        (test "random-dirichlet: correct length"
              (length alphas) (length result))
        (test-approx "random-dirichlet: sums to 1" 1.0 sum 1e-10)
        (test-true "random-dirichlet: all values > 0"
                   (for-all (lambda (x) (> x 0)) result))
        (test-true "random-dirichlet: all values < 1"
                   (for-all (lambda (x) (< x 1)) result)))
  
  ;;; ============================================================
  ;;; PDF/PMF Tests
  ;;; ============================================================
  
  (display "\n--- PDF/PMF Functions ---\n")
  
  ;; Normal PDF
  (test-approx "normal-pdf at mean: 1/(σ√(2π))"
               (/ 1 (* 1 (sqrt (* 2 (pi-value)))))
               (normal-pdf 0 0 1)
               1e-10)
  (test-approx "standard-normal-pdf at 0"
               0.3989422804014327
               (standard-normal-pdf 0)
               1e-6)
  
  ;; Exponential PDF
  (test "exponential-pdf at 0: rate" 1.0 (exponential-pdf 0 1.0))
  (test-approx "exponential-pdf at 1: rate*e^(-rate)"
               (exp-num -1.0)
               (exponential-pdf 1 1.0)
               1e-10)
  (test "exponential-pdf negative: 0" 0 (exponential-pdf -1 1.0))
  
  ;; Uniform PDF
  (test-approx "uniform-pdf inside: 1/(b-a)" 0.5 (uniform-pdf 0.5 0 2) 1e-10)
  (test "uniform-pdf outside below: 0" 0 (uniform-pdf -1 0 2))
  (test "uniform-pdf outside above: 0" 0 (uniform-pdf 3 0 2))
  
  ;; Poisson PMF
  (test-approx "poisson-pmf at 0: e^(-rate)"
               (exp-num -5.0)
               (poisson-pmf 0 5.0)
               1e-10)
  (test "poisson-pmf negative: 0" 0 (poisson-pmf -1 5.0))
  
  ;; Binomial PMF
  (test-approx "binomial-pmf: C(5,2) * 0.5^5"
               (* 10 (expt 0.5 5))
               (binomial-pmf 2 5 0.5)
               1e-10)
  (test "binomial-pmf k > n: 0" 0 (binomial-pmf 6 5 0.5))
  (test "binomial-pmf k < 0: 0" 0 (binomial-pmf -1 5 0.5))
  
  ;; Geometric PMF
  (test "geometric-pmf k=1: p" 0.3 (geometric-pmf 1 0.3))
  (test-approx "geometric-pmf k=2: (1-p)*p"
               (* 0.7 0.3)
               (geometric-pmf 2 0.3)
               1e-10)
  (test "geometric-pmf k < 1: 0" 0 (geometric-pmf 0 0.3))
  
  ;; Bernoulli PMF
  (test "bernoulli-pmf #t: p" 0.7 (bernoulli-pmf #t 0.7))
  (test-approx "bernoulli-pmf #f: 1-p" 0.3 (bernoulli-pmf #f 0.7) 1e-10)
  
  ;;; ============================================================
  ;;; CDF Tests
  ;;; ============================================================
  
  (display "\n--- CDF Functions ---\n")
  
  ;; Exponential CDF
  (test "exponential-cdf at 0: 0" 0 (exponential-cdf 0 1.0))
  (test-approx "exponential-cdf at 1: 1-e^(-1)"
               (- 1 (exp-num -1.0))
               (exponential-cdf 1 1.0)
               1e-10)
  (test "exponential-cdf negative: 0" 0 (exponential-cdf -1 1.0))
  
  ;; Uniform CDF
  (test "uniform-cdf below: 0" 0 (uniform-cdf -1 0 1))
  (test "uniform-cdf above: 1" 1 (uniform-cdf 2 0 1))
  (test "uniform-cdf middle: (x-a)/(b-a)" 0.5 (uniform-cdf 0.5 0 1))
  
  ;; Geometric CDF
  (test "geometric-cdf k < 1: 0" 0 (geometric-cdf 0 0.5))
  (test "geometric-cdf k=1: p" 0.5 (geometric-cdf 1 0.5))
  (test-approx "geometric-cdf k=2: 1-(1-p)^2"
               (- 1 (expt 0.5 2))
               (geometric-cdf 2 0.5)
               1e-10)
  
  ;; Standard Normal CDF
  (test-approx "standard-normal-cdf at 0: 0.5" 0.5 (standard-normal-cdf 0) 1e-6)
  (test-true "standard-normal-cdf at -3: small"
             (< (standard-normal-cdf -3) 0.01))
  (test-true "standard-normal-cdf at 3: near 1"
             (> (standard-normal-cdf 3) 0.99))
  
  ;; Normal CDF
  (test-approx "normal-cdf at mean: 0.5" 0.5 (normal-cdf 5 5 1) 1e-6)
  
  ;; Poisson CDF
  (test-approx "poisson-cdf at 0: e^(-rate)"
               (exp-num -5.0)
               (poisson-cdf 0 5.0)
               1e-10)
  (test-true "poisson-cdf monotone: P(X<=5) < P(X<=10)"
             (< (poisson-cdf 5 5.0) (poisson-cdf 10 5.0)))
  
  ;;; ============================================================
  ;;; Quantile Tests
  ;;; ============================================================
  
  (display "\n--- Quantile Functions ---\n")
  
  ;; Exponential quantile
  (test-approx "exponential-quantile at 0.5: ln(2)/rate"
               (/ (log-num 2) 1.0)
               (exponential-quantile 0.5 1.0)
               1e-10)
  (test "exponential-quantile at 0: 0" 0 (exponential-quantile 0 1.0))
  
  ;; Uniform quantile
  (test "uniform-quantile at 0: a" 0 (uniform-quantile 0 0 1))
  (test "uniform-quantile at 1: b" 1 (uniform-quantile 1 0 1))
  (test "uniform-quantile at 0.5: (a+b)/2" 0.5 (uniform-quantile 0.5 0 1))
  
  ;; Standard normal quantile
  (test-approx "standard-normal-quantile at 0.5: 0" 0
               (standard-normal-quantile 0.5) 0.01)
  (test-approx "standard-normal-quantile at 0.975: ~1.96"
               1.96
               (standard-normal-quantile 0.975)
               0.02)
  
  ;; Inverse relationship: CDF(quantile(p)) ≈ p
  ;; The approximations have ~0.04 error, so use loose tolerance
  (let* ([p 0.7]
         [q (standard-normal-quantile p)]
         [back (standard-normal-cdf q)])
        (test-approx "quantile-cdf inverse: CDF(Q(p)) ≈ p" p back 0.05))
  
  ;;; ============================================================
  ;;; Helper Function Tests
  ;;; ============================================================
  
  (display "\n--- Helper Functions ---\n")
  
  ;; factorial
  (test "factorial 0: 1" 1 (factorial 0))
  (test "factorial 1: 1" 1 (factorial 1))
  (test "factorial 5: 120" 120 (factorial 5))
  (test "factorial 10: 3628800" 3628800 (factorial 10))
  
  ;; binomial-coeff
  (test "C(5,0): 1" 1 (binomial-coeff 5 0))
  (test "C(5,5): 1" 1 (binomial-coeff 5 5))
  (test "C(5,2): 10" 10 (binomial-coeff 5 2))
  (test "C(10,3): 120" 120 (binomial-coeff 10 3))
  (test "C(5,6): 0 (k > n)" 0 (binomial-coeff 5 6))
  
  ;; log-gamma (via factorial relationship)
  (test-approx "log-gamma(6) = log(5!)"
               (log-num 120)
               (log-gamma 6)
               0.001)
  
  ;;; ============================================================
  ;;; Summary
  ;;; ============================================================
  
  (display "\n=== Test Summary ===\n")
  (display "Total:  ")
  (display *test-count*)
  (newline)
  (display "Passed: ")
  (display *pass-count*)
  (newline)
  (display "Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "\n✓ All tests passed!\n")
      (begin
       (display "\n✗ Some tests failed.\n")
       (exit 1))))

;; Run tests when loaded
(run-tests)
