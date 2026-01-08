;;; core/random/test-distributions.ss --- Tests for Probability Distributions
;;;
;;; Comprehensive tests for probability distributions:
;;;   - Bernoulli, Exponential, Normal, Geometric
;;;   - Poisson, Binomial, Gamma, Beta
;;;   - Categorical, Dirichlet
;;;   - PDF, PMF, CDF, Quantile functions
;;;   - Statistical property verification (mean, variance)
;;;
;;; This is a test file for core/random/distributions.ss

(load "core/test-framework.ss")
(load "core/random/prng.ss")
(load "core/random/distributions.ss")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

;;; mean : (List Number) -> Number
(define (mean lst)
  (if (null? lst)
      0
      (/ (fold-left + 0 lst) (length lst))))

;;; variance : (List Number) -> Number
(define (variance lst)
  (if (null? lst)
      0
      (let ([m (mean lst)]
            [n (length lst)])
           (/ (fold-left + 0 (map (lambda (x) (expt (- x m) 2)) lst))
              n))))

;;; stddev : (List Number) -> Number
(define (stddev lst)
  (sqrt (variance lst)))

;;; within-tolerance : Number x Number x Number -> Boolean
(define (within-tolerance expected actual tolerance)
  (< (abs (- expected actual)) tolerance))

;;; all-satisfy : (a -> Bool) x (List a) -> Bool
(define (all-satisfy pred lst)
  (andmap pred lst))

;;; count-true : (List Bool) -> Int
(define (count-true lst)
  (length (filter identity lst)))

(display "\n")
(display "==============================================================\n")
(display "         Probability Distribution Tests\n")
(display "==============================================================\n")
(display "\n")

;;; ============================================================
;;; Bernoulli Distribution Tests
;;; ============================================================

(test-group bernoulli-distribution
            
            (define-test bernoulli-p-1
              ;; p=1 should always return true
              (let ([samples (with-random 42 (random-list 100 (random-bernoulli 1.0)))])
                   (assert-true (all-satisfy identity samples))))
            
            (define-test bernoulli-p-0
              ;; p=0 should always return false
              (let ([samples (with-random 42 (random-list 100 (random-bernoulli 0.0)))])
                   (assert-true (all-satisfy not samples))))
            
            (define-test bernoulli-p-half
              ;; p=0.5 should give approximately 50% true
              (let* ([samples (with-random 42 (random-list 10000 (random-bernoulli 0.5)))]
                     [true-count (count-true samples)])
                    (assert-true (within-tolerance 5000 true-count 300))))
            
            (define-test bernoulli-p-biased
              ;; p=0.8 should give approximately 80% true
              (let* ([samples (with-random 42 (random-list 10000 (random-bernoulli 0.8)))]
                     [true-count (count-true samples)]
                     [proportion (/ true-count 10000)])
                    (assert-true (within-tolerance 0.8 proportion 0.02))))
            
            (define-test bernoulli-pmf
              ;; PMF should give correct probabilities
              (assert-true (within-tolerance 0.7 (bernoulli-pmf #t 0.7) 0.0001))
              (assert-true (within-tolerance 0.3 (bernoulli-pmf #f 0.7) 0.0001))))

;;; ============================================================
;;; Exponential Distribution Tests
;;; ============================================================

(test-group exponential-distribution
            
            (define-test exponential-positive
              ;; All samples should be positive
              (let ([samples (with-random 42 (random-list 1000 (random-exponential 1.0)))])
                   (assert-true (all-satisfy positive? samples))))
            
            (define-test exponential-mean-rate-1
              ;; Mean should be 1/rate
              (let* ([rate 1.0]
                     [expected-mean 1.0]
                     [samples (with-random 42 (random-list 10000 (random-exponential rate)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.05))))
            
            (define-test exponential-mean-rate-2
              ;; Mean should be 1/rate = 0.5
              (let* ([rate 2.0]
                     [expected-mean 0.5]
                     [samples (with-random 42 (random-list 10000 (random-exponential rate)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.03))))
            
            (define-test exponential-variance
              ;; Variance should be 1/rate^2
              (let* ([rate 2.0]
                     [expected-var 0.25]
                     [samples (with-random 42 (random-list 10000 (random-exponential rate)))]
                     [v (variance samples)])
                    (assert-true (within-tolerance expected-var v 0.03))))
            
            (define-test exponential-pdf-at-zero
              ;; PDF(0) = rate
              (assert-true (within-tolerance 2.0 (exponential-pdf 0 2.0) 0.0001)))
            
            (define-test exponential-pdf-positive
              ;; PDF should be positive for positive x
              (assert-true (> (exponential-pdf 1.0 1.0) 0)))
            
            (define-test exponential-pdf-negative-x
              ;; PDF(x) = 0 for x < 0
              (assert-equal (exponential-pdf -1 1.0) 0))
            
            (define-test exponential-cdf-at-median
              ;; CDF at median (ln(2)/rate) should be 0.5
              (let ([cdf (exponential-cdf 0.693 1.0)])
                   (assert-true (within-tolerance 0.5 cdf 0.01))))
            
            (define-test exponential-quantile-median
              ;; Quantile(0.5) should be ln(2)/rate
              (let ([q (exponential-quantile 0.5 1.0)])
                   (assert-true (within-tolerance 0.693 q 0.01))))
            
            (define-test exponential-cdf-quantile-inverse
              ;; CDF(Quantile(p)) = p
              (let* ([p 0.7]
                     [q (exponential-quantile p 2.0)]
                     [back (exponential-cdf q 2.0)])
                    (assert-true (within-tolerance p back 0.001)))))

;;; ============================================================
;;; Normal Distribution Tests
;;; ============================================================

(test-group normal-distribution
            
            (define-test normal-standard-mean
              ;; Standard normal should have mean 0
              (let* ([samples (with-random 42 (random-list 10000 random-normal-standard))]
                     [m (mean samples)])
                    (assert-true (within-tolerance 0 m 0.05))))
            
            (define-test normal-standard-stddev
              ;; Standard normal should have stddev 1
              (let* ([samples (with-random 42 (random-list 10000 random-normal-standard))]
                     [sd (stddev samples)])
                    (assert-true (within-tolerance 1 sd 0.05))))
            
            (define-test normal-shifted-mean
              ;; Normal(100, 15) should have mean 100
              (let* ([samples (with-random 42 (random-list 10000 (random-normal 100 15)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance 100 m 1.0))))
            
            (define-test normal-shifted-stddev
              ;; Normal(100, 15) should have stddev 15
              (let* ([samples (with-random 42 (random-list 10000 (random-normal 100 15)))]
                     [sd (stddev samples)])
                    (assert-true (within-tolerance 15 sd 1.0))))
            
            (define-test normal-pair-independent
              ;; Box-Muller pair should produce two independent normals
              (let* ([pairs (with-random 42 (random-list 5000 random-normal-pair))]
                     [first-values (map car pairs)]
                     [second-values (map cdr pairs)]
                     [mean1 (mean first-values)]
                     [mean2 (mean second-values)])
                    (assert-true (within-tolerance 0 mean1 0.05))
                    (assert-true (within-tolerance 0 mean2 0.05))))
            
            (define-test normal-pdf-at-mean
              ;; PDF is highest at mean
              (let ([pdf-at-mean (normal-pdf 0 0 1)])
                   ;; 1/sqrt(2*pi) ~ 0.3989
                   (assert-true (within-tolerance 0.3989 pdf-at-mean 0.001))))
            
            (define-test standard-normal-cdf-at-zero
              ;; CDF(0) = 0.5
              (assert-true (within-tolerance 0.5 (standard-normal-cdf 0) 0.001)))
            
            (define-test standard-normal-cdf-1-96
              ;; CDF(1.96) ~ 0.975
              (assert-true (within-tolerance 0.975 (standard-normal-cdf 1.96) 0.01)))
            
            (define-test standard-normal-quantile-half
              ;; Quantile(0.5) = 0
              (assert-true (within-tolerance 0 (standard-normal-quantile 0.5) 0.01)))
            
            (define-test standard-normal-quantile-975
              ;; Quantile(0.975) ~ 1.96
              (assert-true (within-tolerance 1.96 (standard-normal-quantile 0.975) 0.05))))

;;; ============================================================
;;; Geometric Distribution Tests
;;; ============================================================

(test-group geometric-distribution
            
            (define-test geometric-positive
              ;; All samples should be >= 1
              (let ([samples (with-random 42 (random-list 1000 (random-geometric 0.3)))])
                   (assert-true (all-satisfy (lambda (x) (>= x 1)) samples))))
            
            (define-test geometric-mean
              ;; Mean should be 1/p
              (let* ([p 0.25]
                     [expected-mean 4.0]
                     [samples (with-random 42 (random-list 10000 (random-geometric p)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.3))))
            
            (define-test geometric-p-1
              ;; p=1 should always return 1
              (let ([samples (with-random 42 (random-list 100 (random-geometric 1.0)))])
                   (assert-true (all-satisfy (lambda (x) (= x 1)) samples))))
            
            (define-test geometric-pmf
              ;; PMF(k) = (1-p)^(k-1) * p
              ;; PMF(3, 0.5) = 0.5^2 * 0.5 = 0.125
              (assert-true (within-tolerance 0.125 (geometric-pmf 3 0.5) 0.0001)))
            
            (define-test geometric-cdf
              ;; CDF(k) = 1 - (1-p)^k
              ;; CDF(3, 0.5) = 1 - 0.5^3 = 0.875
              (assert-true (within-tolerance 0.875 (geometric-cdf 3 0.5) 0.001))))

;;; ============================================================
;;; Poisson Distribution Tests
;;; ============================================================

(test-group poisson-distribution
            
            (define-test poisson-non-negative
              ;; All samples should be >= 0
              (let ([samples (with-random 42 (random-list 1000 (random-poisson 5.0)))])
                   (assert-true (all-satisfy (lambda (x) (>= x 0)) samples))))
            
            (define-test poisson-mean-small-rate
              ;; Mean should equal rate (small lambda case)
              (let* ([rate 3.0]
                     [samples (with-random 42 (random-list 10000 (random-poisson rate)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance rate m 0.2))))
            
            (define-test poisson-mean-large-rate
              ;; Mean should equal rate (large lambda case)
              (let* ([rate 50.0]
                     [samples (with-random 42 (random-list 5000 (random-poisson rate)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance rate m 2.0))))
            
            (define-test poisson-variance
              ;; Variance should equal rate
              (let* ([rate 5.0]
                     [samples (with-random 42 (random-list 10000 (random-poisson rate)))]
                     [v (variance samples)])
                    (assert-true (within-tolerance rate v 0.5))))
            
            (define-test poisson-pmf
              ;; PMF(k, rate) = e^(-rate) * rate^k / k!
              ;; PMF(2, 3.0) ~ 0.224
              (let ([pmf (poisson-pmf 2 3.0)])
                   (assert-true (within-tolerance 0.224 pmf 0.01))))
            
            (define-test poisson-cdf
              ;; CDF should be sum of PMF
              (let ([cdf (poisson-cdf 2 3.0)]
                    [manual (+ (poisson-pmf 0 3.0) (poisson-pmf 1 3.0) (poisson-pmf 2 3.0))])
                   (assert-true (within-tolerance manual cdf 0.0001)))))

;;; ============================================================
;;; Binomial Distribution Tests
;;; ============================================================

(test-group binomial-distribution
            
            (define-test binomial-range
              ;; All samples should be in [0, n]
              (let* ([n 10]
                     [samples (with-random 42 (random-list 1000 (random-binomial n 0.5)))])
                    (assert-true (all-satisfy (lambda (x) (and (>= x 0) (<= x n))) samples))))
            
            (define-test binomial-mean
              ;; Mean should be n*p
              (let* ([n 20]
                     [p 0.3]
                     [expected-mean 6.0]
                     [samples (with-random 42 (random-list 5000 (random-binomial n p)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.3))))
            
            (define-test binomial-variance
              ;; Variance should be n*p*(1-p)
              (let* ([n 20]
                     [p 0.4]
                     [expected-var (* n p (- 1 p))]  ; = 4.8
                     [samples (with-random 42 (random-list 10000 (random-binomial n p)))]
                     [v (variance samples)])
                    (assert-true (within-tolerance expected-var v 0.5))))
            
            (define-test binomial-p-0
              ;; p=0 should always return 0
              (let ([samples (with-random 42 (random-list 100 (random-binomial 10 0.0)))])
                   (assert-true (all-satisfy (lambda (x) (= x 0)) samples))))
            
            (define-test binomial-p-1
              ;; p=1 should always return n
              (let ([samples (with-random 42 (random-list 100 (random-binomial 10 1.0)))])
                   (assert-true (all-satisfy (lambda (x) (= x 10)) samples))))
            
            (define-test binomial-pmf
              ;; PMF(5, 10, 0.5) = C(10,5) * 0.5^10 ~ 0.246
              (let ([pmf (binomial-pmf 5 10 0.5)])
                   (assert-true (within-tolerance 0.246 pmf 0.01))))
            
            (define-test binomial-pmf-out-of-range
              ;; PMF should be 0 for k < 0 or k > n
              (assert-equal (binomial-pmf -1 10 0.5) 0)
              (assert-equal (binomial-pmf 11 10 0.5) 0)))

;;; ============================================================
;;; Gamma Distribution Tests
;;; ============================================================

(test-group gamma-distribution
            
            (define-test gamma-positive
              ;; All samples should be positive
              (let ([samples (with-random 42 (random-list 1000 (random-gamma 2.0 1.0)))])
                   (assert-true (all-satisfy positive? samples))))
            
            (define-test gamma-mean
              ;; Mean should be shape/rate
              (let* ([shape 5.0]
                     [rate 2.0]
                     [expected-mean 2.5]
                     [samples (with-random 42 (random-list 10000 (random-gamma shape rate)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.15))))
            
            (define-test gamma-variance
              ;; Variance should be shape/rate^2
              (let* ([shape 5.0]
                     [rate 2.0]
                     [expected-var 1.25]
                     [samples (with-random 42 (random-list 10000 (random-gamma shape rate)))]
                     [v (variance samples)])
                    ;; Widen tolerance due to natural sampling variance
                    (assert-true (within-tolerance expected-var v 0.3))))
            
            (define-test gamma-small-shape
              ;; Shape < 1 case should work
              (let* ([shape 0.5]
                     [samples (with-random 42 (random-list 1000 (random-gamma shape 1.0)))])
                    (assert-true (all-satisfy positive? samples))))
            
            (define-test gamma-large-shape
              ;; Large shape should work
              (let* ([shape 50.0]
                     [rate 5.0]
                     [expected-mean 10.0]
                     [samples (with-random 42 (random-list 5000 (random-gamma shape rate)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.5)))))

;;; ============================================================
;;; Beta Distribution Tests
;;; ============================================================

(test-group beta-distribution
            
            (define-test beta-range
              ;; All samples should be in [0, 1]
              (let ([samples (with-random 42 (random-list 1000 (random-beta 2.0 5.0)))])
                   (assert-true (all-satisfy (lambda (x) (and (>= x 0) (<= x 1))) samples))))
            
            (define-test beta-mean
              ;; Mean should be alpha/(alpha+beta)
              (let* ([alpha 2.0]
                     [beta 5.0]
                     [expected-mean (/ alpha (+ alpha beta))]  ; 2/7 ~ 0.286
                     [samples (with-random 42 (random-list 10000 (random-beta alpha beta)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.02))))
            
            (define-test beta-symmetric
              ;; Beta(a, a) should have mean 0.5
              (let* ([samples (with-random 42 (random-list 10000 (random-beta 5.0 5.0)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance 0.5 m 0.02))))
            
            (define-test beta-uniform
              ;; Beta(1, 1) is uniform on [0, 1]
              (let* ([samples (with-random 42 (random-list 10000 (random-beta 1.0 1.0)))]
                     [m (mean samples)]
                     [v (variance samples)])
                    (assert-true (within-tolerance 0.5 m 0.02))
                    ;; Variance of uniform is 1/12
                    (assert-true (within-tolerance (/ 1 12) v 0.01)))))

;;; ============================================================
;;; Categorical Distribution Tests
;;; ============================================================

(test-group categorical-distribution
            
            (define-test categorical-range
              ;; All samples should be valid indices
              (let* ([weights '(1 2 3 4)]
                     [samples (with-random 42 (random-list 1000 (random-categorical weights)))])
                    (assert-true (all-satisfy (lambda (x) (and (>= x 0) (< x 4))) samples))))
            
            (define-test categorical-proportions
              ;; Samples should approximately match weights
              (let* ([weights '(10 1 1 1)]  ; First category has 10/13 ~ 77% probability
                     [samples (with-random 42 (random-list 10000 (random-categorical weights)))]
                     [zeros (length (filter (lambda (x) (= x 0)) samples))]
                     [proportion (/ zeros 10000)])
                    (assert-true (within-tolerance (/ 10 13) proportion 0.02))))
            
            (define-test categorical-uniform
              ;; Equal weights should give roughly equal proportions
              (let* ([weights '(1 1 1 1)]
                     [samples (with-random 42 (random-list 4000 (random-categorical weights)))]
                     [counts (map (lambda (i)
                                          (length (filter (lambda (x) (= x i)) samples)))
                                  '(0 1 2 3))])
                    ;; Each should be roughly 1000
                    (assert-true (all-satisfy (lambda (c) (within-tolerance 1000 c 100)) counts)))))

;;; ============================================================
;;; Dirichlet Distribution Tests
;;; ============================================================

(test-group dirichlet-distribution
            
            (define-test dirichlet-sums-to-one
              ;; Sample should sum to 1
              (let* ([sample (with-random 42 (random-dirichlet '(1 1 1 1)))]
                     [total (fold-left + 0 sample)])
                    (assert-true (within-tolerance 1.0 total 0.0001))))
            
            (define-test dirichlet-positive
              ;; All components should be positive
              (let ([sample (with-random 42 (random-dirichlet '(2 3 4)))])
                   (assert-true (all-satisfy positive? sample))))
            
            (define-test dirichlet-length
              ;; Should return correct number of components
              (let ([sample (with-random 42 (random-dirichlet '(1 2 3 4 5)))])
                   (assert-equal (length sample) 5)))
            
            (define-test dirichlet-concentration
              ;; Higher concentration should give more concentrated results
              (let* ([samples-low (with-random 42 (random-list 100 (random-dirichlet '(0.1 0.1 0.1))))]
                     [samples-high (with-random 42 (random-list 100 (random-dirichlet '(10 10 10))))]
                     [variance-low (mean (map (lambda (s) (variance s)) samples-low))]
                     [variance-high (mean (map (lambda (s) (variance s)) samples-high))])
                    ;; Low concentration should have higher variance
                    (assert-true (> variance-low variance-high)))))

;;; ============================================================
;;; Uniform Distribution PDF/CDF Tests
;;; ============================================================

(test-group uniform-pdf-cdf
            
            (define-test uniform-pdf-in-range
              ;; PDF should be 1/(b-a) in range
              (assert-true (within-tolerance 1.0 (uniform-pdf 0.5 0 1) 0.0001))
              (assert-true (within-tolerance 0.5 (uniform-pdf 1.0 0 2) 0.0001)))
            
            (define-test uniform-pdf-out-of-range
              ;; PDF should be 0 outside range
              (assert-equal (uniform-pdf -1 0 1) 0)
              (assert-equal (uniform-pdf 2 0 1) 0))
            
            (define-test uniform-cdf
              ;; CDF should be linear in range
              (assert-true (within-tolerance 0.5 (uniform-cdf 0.5 0 1) 0.0001))
              (assert-true (within-tolerance 0.25 (uniform-cdf 0.5 0 2) 0.0001))
              (assert-equal (uniform-cdf -1 0 1) 0)
              (assert-equal (uniform-cdf 2 0 1) 1))
            
            (define-test uniform-quantile
              ;; Quantile should be linear
              (assert-true (within-tolerance 0.5 (uniform-quantile 0.5 0 1) 0.0001))
              (assert-true (within-tolerance 1.0 (uniform-quantile 0.5 0 2) 0.0001))))

;;; ============================================================
;;; Normal Vector Tests
;;; ============================================================

(test-group normal-vector
            
            (define-test normal-vector-length
              ;; Should return n samples
              (let ([samples (with-random 42 (random-normal-vector 100 0 1))])
                   (assert-equal (length samples) 100)))
            
            (define-test normal-vector-mean
              ;; Should have correct mean
              (let* ([samples (with-random 42 (random-normal-vector 10000 50 10))]
                     [m (mean samples)])
                    (assert-true (within-tolerance 50 m 1.0)))))

;;; ============================================================
;;; Helper Function Tests
;;; ============================================================

(test-group helper-functions
            
            (define-test factorial-basic
              ;; factorial should compute correctly
              (assert-equal (factorial 0) 1)
              (assert-equal (factorial 1) 1)
              (assert-equal (factorial 5) 120)
              (assert-equal (factorial 10) 3628800))
            
            (define-test binomial-coeff-basic
              ;; binomial coefficient should compute correctly
              (assert-equal (binomial-coeff 5 0) 1)
              (assert-equal (binomial-coeff 5 5) 1)
              (assert-equal (binomial-coeff 5 2) 10)
              (assert-equal (binomial-coeff 10 3) 120))
            
            (define-test binomial-coeff-out-of-range
              ;; binomial coefficient should be 0 for invalid inputs
              (assert-equal (binomial-coeff 5 -1) 0)
              (assert-equal (binomial-coeff 5 6) 0))
            
            (define-test log-gamma-factorial
              ;; log-gamma(n+1) = log(n!)
              (let* ([n 5]
                     [log-fact (log (factorial n))]
                     [lg (log-gamma (+ n 1))])
                    (assert-true (within-tolerance log-fact lg 0.01)))))

;;; ============================================================
;;; Log-gamma and Special Functions Tests
;;; ============================================================

(test-group log-gamma-function
            
            (define-test log-gamma-positive
              ;; log-gamma should work for positive values
              (let ([lg (log-gamma 5)])
                   (assert-true (> lg 0))))
            
            (define-test log-gamma-small-values
              ;; Should handle small values correctly
              (let* ([lg1 (log-gamma 1.5)]
                     [lg2 (log-gamma 2.5)])
                    (assert-true (< lg1 lg2)))))

;;; ============================================================
;;; Reproducibility Tests
;;; ============================================================

(test-group distribution-reproducibility
            
            (define-test normal-reproducible
              ;; Same seed should give same normal samples
              (let* ([seq1 (with-random 12345 (random-list 100 random-normal-standard))]
                     [seq2 (with-random 12345 (random-list 100 random-normal-standard))])
                    (assert-equal seq1 seq2)))
            
            (define-test exponential-reproducible
              ;; Same seed should give same exponential samples
              (let* ([seq1 (with-random 12345 (random-list 100 (random-exponential 2.0)))]
                     [seq2 (with-random 12345 (random-list 100 (random-exponential 2.0)))])
                    (assert-equal seq1 seq2)))
            
            (define-test poisson-reproducible
              ;; Same seed should give same Poisson samples
              (let* ([seq1 (with-random 12345 (random-list 100 (random-poisson 5.0)))]
                     [seq2 (with-random 12345 (random-list 100 (random-poisson 5.0)))])
                    (assert-equal seq1 seq2))))

;;; ============================================================
;;; Print Summary
;;; ============================================================

(display "\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)
(newline)

(if (= *tests-failed* 0)
    (display "[SUCCESS] All distribution tests passed.\n")
    (display "[FAILURE] Some distribution tests failed.\n"))
