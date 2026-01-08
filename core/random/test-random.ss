;;; fabric/stitches/random/test-random.ss — Tests for Random Number Generation
;;;
;;; Tests PRNG algorithms and probability distributions.

(load "core/test-framework.ss")
(load "core/random/prng.ss")
(load "core/random/distributions.ss")

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; mean : List Number -> Number
(define (mean lst)
  (if (null? lst)
      0
      (/ (fold-left + 0 lst) (length lst))))

;;; variance : List Number -> Number
(define (variance lst)
  (if (null? lst)
      0
      (let ([m (mean lst)]
            [n (length lst)])
           (/ (fold-left + 0 (map (lambda (x) (expt (- x m) 2)) lst))
              n))))

;;; stddev : List Number -> Number
(define (stddev lst)
  (sqrt (variance lst)))

;;; within-tolerance : Number x Number x Number -> Boolean
(define (within-tolerance expected actual tolerance)
  (< (abs (- expected actual)) tolerance))

;;; ============================================================
;;; Bit Manipulation Tests
;;; ============================================================

(test-group bit-manipulation
            (define-test u32-mask
              (assert-equal (u32 #xFFFFFFFF) #xFFFFFFFF)
              (assert-equal (u32 #x100000000) 0)
              (assert-equal (u32 #x1FFFFFFFF) #xFFFFFFFF))
            
            (define-test u64-mask
              (assert-equal (u64 #xFFFFFFFFFFFFFFFF) #xFFFFFFFFFFFFFFFF)
              (assert-equal (u64 #x10000000000000000) 0))
            
            (define-test rotl32-basic
              (assert-equal (rotl32 1 1) 2)
              (assert-equal (rotl32 1 31) #x80000000)
              (assert-equal (rotl32 #x80000000 1) 1))
            
            (define-test rotr32-basic
              (assert-equal (rotr32 2 1) 1)
              (assert-equal (rotr32 1 1) #x80000000)))

;;; ============================================================
;;; Splitmix64 Tests
;;; ============================================================

(test-group splitmix64
            (define-test splitmix-creation
              (let ([sm (make-splitmix 12345)])
                   (assert-true (splitmix? sm))
                   (assert-equal (splitmix-state sm) 12345)))
            
            (define-test splitmix-deterministic
              (let* ([sm (make-splitmix 42)]
                     [r1 (splitmix-next sm)]
                     [r2 (splitmix-next (cdr r1))]
                     ;; Same seed should give same sequence
                     [sm2 (make-splitmix 42)]
                     [r1b (splitmix-next sm2)])
                    (assert-equal (car r1) (car r1b))))
            
            (define-test splitmix-different-seeds
              (let* ([sm1 (make-splitmix 1)]
                     [sm2 (make-splitmix 2)]
                     [r1 (car (splitmix-next sm1))]
                     [r2 (car (splitmix-next sm2))])
                    (assert-true (not (= r1 r2)))))
            
            (define-test splitmix-state-monad
              (let* ([sm (make-splitmix 999)]
                     [result (run-state splitmix-random sm)])
                    (assert-true (integer? (car result)))
                    (assert-true (splitmix? (cdr result))))))

;;; ============================================================
;;; PCG Tests
;;; ============================================================

(test-group pcg
            (define-test pcg-creation
              (let ([p (make-pcg 12345 1)])
                   (assert-true (pcg? p))))
            
            (define-test pcg-deterministic
              (let* ([p1 (make-pcg 42 1)]
                     [p2 (make-pcg 42 1)]
                     [r1 (car (pcg-next p1))]
                     [r2 (car (pcg-next p2))])
                    (assert-equal r1 r2)))
            
            (define-test pcg-different-streams
              (let* ([p1 (make-pcg 42 1)]
                     [p2 (make-pcg 42 2)]
                     [r1 (car (pcg-next p1))]
                     [r2 (car (pcg-next p2))])
                    (assert-true (not (= r1 r2)))))
            
            (define-test pcg-range
              (let* ([p (make-pcg 0 1)]
                     [result (car (pcg-next p))])
                    (assert-true (<= 0 result))
                    (assert-true (< result (expt 2 32))))))

;;; ============================================================
;;; Xorshift128+ Tests
;;; ============================================================

(test-group xorshift128
            (define-test xorshift-creation
              (let ([xs (make-xorshift128 12345)])
                   (assert-true (xorshift128? xs))))
            
            (define-test xorshift-deterministic
              (let* ([xs1 (make-xorshift128 42)]
                     [xs2 (make-xorshift128 42)]
                     [r1 (car (xorshift128-next xs1))]
                     [r2 (car (xorshift128-next xs2))])
                    (assert-equal r1 r2)))
            
            (define-test xorshift-range
              (let* ([xs (make-xorshift128 0)]
                     [result (car (xorshift128-next xs))])
                    (assert-true (<= 0 result))
                    (assert-true (< result (expt 2 64))))))

;;; ============================================================
;;; Uniform Random Tests
;;; ============================================================

(test-group uniform-random
            (define-test random-float-range-test
              (let* ([samples (with-random 42 (random-list 1000 random-float))]
                     [min-val (apply min samples)]
                     [max-val (apply max samples)])
                    (assert-true (>= min-val 0))
                    (assert-true (< max-val 1))))
            
            (define-test random-float-mean
              (let* ([samples (with-random 42 (random-list 10000 random-float))]
                     [m (mean samples)])
                    ;; Uniform [0,1) should have mean ~0.5
                    (assert-true (within-tolerance 0.5 m 0.02))))
            
            (define-test random-int-range-test
              (let* ([samples (with-random 42 (random-list 1000 (random-int-range 1 6)))]
                     [min-val (apply min samples)]
                     [max-val (apply max samples)])
                    (assert-true (>= min-val 1))
                    (assert-true (<= max-val 6))))
            
            (define-test random-int-range-mean
              (let* ([samples (with-random 42 (random-list 5000 (random-int-range 1 6)))]
                     [m (mean samples)])
                    ;; Mean of 1-6 should be 3.5
                    (assert-true (within-tolerance 3.5 m 0.15))))
            
            (define-test random-bool-test
              (let* ([samples (with-random 42 (random-list 1000 random-bool))]
                     [true-count (length (filter identity samples))])
                    ;; Should be roughly 50% true
                    (assert-true (< (abs (- true-count 500)) 100)))))

;;; ============================================================
;;; Sampling Tests
;;; ============================================================

(test-group sampling
            (define-test random-element-test
              (let* ([lst '(a b c d e)]
                     [samples (with-random 42 (random-list 100 (random-element lst)))])
                    (assert-true (andmap (lambda (x) (memq x lst)) samples))))
            
            (define-test shuffle-preserves-elements
              (let* ([lst '(1 2 3 4 5)]
                     [shuffled (with-random 42 (shuffle lst))])
                    (assert-equal (sort < shuffled) (sort < lst))))
            
            (define-test shuffle-randomizes
              (let* ([lst '(1 2 3 4 5 6 7 8 9 10)]
                     [s1 (with-random 1 (shuffle lst))]
                     [s2 (with-random 2 (shuffle lst))])
                    ;; Different seeds should (usually) give different orders
                    (assert-true (not (equal? s1 s2)))))
            
            (define-test sample-correct-size
              (let* ([lst '(1 2 3 4 5 6 7 8 9 10)]
                     [sampled (with-random 42 (sample 3 lst))])
                    (assert-equal (length sampled) 3)))
            
            (define-test sample-no-duplicates
              ;; Note: Using inline check to avoid test framework exception display bug
              (let* ([lst '(1 2 3 4 5 6 7 8 9 10)]
                     [sampled (with-random 42 (sample 5 lst))])
                    ;; Check that all elements are unique (no duplicates from sample)
                    (assert-equal 5 (length sampled))
                    (assert-true (equal? sampled (let loop ([xs sampled] [seen '()])
                                                      (cond
                                                       [(null? xs) (reverse seen)]
                                                       [(member (car xs) seen) (loop (cdr xs) seen)]
                                                       [else (loop (cdr xs) (cons (car xs) seen))]))))))
            
            (define-test weighted-choice-test
              (let* ([weighted '((a . 100) (b . 1) (c . 1))]
                     [samples (with-random 42 (random-list 1000 (weighted-choice weighted)))]
                     [a-count (length (filter (lambda (x) (eq? x 'a)) samples))])
                    ;; 'a should appear much more often (100/102 ≈ 98%)
                    (assert-true (> a-count 900)))))

;;; NOTE: remove-duplicates is provided by core/base/prelude.ss (loaded via test-framework.ss)

;;; ============================================================
;;; Bernoulli Distribution Tests
;;; ============================================================

(test-group bernoulli
            (define-test bernoulli-always-true
              (let ([samples (with-random 42 (random-list 100 (random-bernoulli 1.0)))])
                   (assert-true (andmap identity samples))))
            
            (define-test bernoulli-always-false
              (let ([samples (with-random 42 (random-list 100 (random-bernoulli 0.0)))])
                   (assert-true (andmap not samples))))
            
            (define-test bernoulli-half
              (let* ([samples (with-random 42 (random-list 10000 (random-bernoulli 0.5)))]
                     [true-count (length (filter identity samples))])
                    ;; Should be ~50%
                    (assert-true (within-tolerance 5000 true-count 300)))))

;;; ============================================================
;;; Exponential Distribution Tests
;;; ============================================================

(test-group exponential
            (define-test exponential-positive
              (let ([samples (with-random 42 (random-list 100 (random-exponential 1.0)))])
                   (assert-true (andmap positive? samples))))
            
            (define-test exponential-mean
              (let* ([rate 2.0]
                     [expected-mean (/ 1 rate)]
                     [samples (with-random 42 (random-list 10000 (random-exponential rate)))]
                     [m (mean samples)])
                    ;; Mean should be ~1/rate = 0.5
                    (assert-true (within-tolerance expected-mean m 0.05)))))

;;; ============================================================
;;; Normal Distribution Tests
;;; ============================================================

(test-group normal
            (define-test normal-standard-mean
              (let* ([samples (with-random 42 (random-list 10000 random-normal-standard))]
                     [m (mean samples)])
                    ;; Mean should be ~0
                    (assert-true (within-tolerance 0 m 0.05))))
            
            (define-test normal-standard-stddev
              (let* ([samples (with-random 42 (random-list 10000 random-normal-standard))]
                     [sd (stddev samples)])
                    ;; Stddev should be ~1
                    (assert-true (within-tolerance 1 sd 0.05))))
            
            (define-test normal-shifted-mean
              (let* ([samples (with-random 42 (random-list 10000 (random-normal 100 15)))]
                     [m (mean samples)])
                    ;; Mean should be ~100
                    (assert-true (within-tolerance 100 m 1.0))))
            
            (define-test normal-shifted-stddev
              (let* ([samples (with-random 42 (random-list 10000 (random-normal 100 15)))]
                     [sd (stddev samples)])
                    ;; Stddev should be ~15
                    (assert-true (within-tolerance 15 sd 1.0)))))

;;; ============================================================
;;; Geometric Distribution Tests
;;; ============================================================

(test-group geometric
            (define-test geometric-positive
              (let ([samples (with-random 42 (random-list 100 (random-geometric 0.3)))])
                   (assert-true (andmap (lambda (x) (>= x 1)) samples))))
            
            (define-test geometric-mean
              (let* ([p 0.25]
                     [expected-mean (/ 1 p)]  ; = 4
                     [samples (with-random 42 (random-list 10000 (random-geometric p)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.3)))))

;;; ============================================================
;;; Poisson Distribution Tests
;;; ============================================================

(test-group poisson
            (define-test poisson-non-negative
              (let ([samples (with-random 42 (random-list 100 (random-poisson 5.0)))])
                   (assert-true (andmap (lambda (x) (>= x 0)) samples))))
            
            (define-test poisson-mean-small
              (let* ([rate 3.0]
                     [samples (with-random 42 (random-list 10000 (random-poisson rate)))]
                     [m (mean samples)])
                    ;; Mean should be ~rate (widen tolerance for small lambda)
                    (assert-true (within-tolerance rate m 0.3))))
            
            (define-test poisson-mean-large
              (let* ([rate 50.0]
                     [samples (with-random 42 (random-list 5000 (random-poisson rate)))]
                     [m (mean samples)])
                    ;; Mean should be ~rate
                    (assert-true (within-tolerance rate m 2.0)))))

;;; ============================================================
;;; Binomial Distribution Tests
;;; ============================================================

(test-group binomial
            (define-test binomial-range
              (let* ([n 10]
                     [samples (with-random 42 (random-list 100 (random-binomial n 0.5)))])
                    (assert-true (andmap (lambda (x) (and (>= x 0) (<= x n))) samples))))
            
            (define-test binomial-mean
              (let* ([n 20]
                     [p 0.3]
                     [expected-mean (* n p)]  ; = 6
                     [samples (with-random 42 (random-list 5000 (random-binomial n p)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.3)))))

;;; ============================================================
;;; Gamma Distribution Tests
;;; ============================================================

(test-group gamma
            (define-test gamma-positive
              (let ([samples (with-random 42 (random-list 100 (random-gamma 2.0 1.0)))])
                   (assert-true (andmap positive? samples))))
            
            (define-test gamma-mean
              (let* ([shape 5.0]
                     [rate 2.0]
                     [expected-mean (/ shape rate)]  ; = 2.5
                     [samples (with-random 42 (random-list 10000 (random-gamma shape rate)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.15))))
            
            (define-test gamma-small-shape
              ;; Test shape < 1 case
              (let* ([shape 0.5]
                     [rate 1.0]
                     [samples (with-random 42 (random-list 1000 (random-gamma shape rate)))])
                    (assert-true (andmap positive? samples)))))

;;; ============================================================
;;; Beta Distribution Tests
;;; ============================================================

(test-group beta
            (define-test beta-range
              (let ([samples (with-random 42 (random-list 100 (random-beta 2.0 5.0)))])
                   (assert-true (andmap (lambda (x) (and (>= x 0) (<= x 1))) samples))))
            
            (define-test beta-mean
              (let* ([alpha 2.0]
                     [beta 5.0]
                     [expected-mean (/ alpha (+ alpha beta))]  ; = 2/7 ≈ 0.286
                     [samples (with-random 42 (random-list 10000 (random-beta alpha beta)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance expected-mean m 0.02)))))

;;; ============================================================
;;; Categorical Distribution Tests
;;; ============================================================

(test-group categorical
            (define-test categorical-range
              (let* ([weights '(1 2 3 4)]
                     [samples (with-random 42 (random-list 100 (random-categorical weights)))])
                    (assert-true (andmap (lambda (x) (and (>= x 0) (< x 4))) samples))))
            
            (define-test categorical-weighted
              (let* ([weights '(10 1 1 1)]  ; First category much more likely
                     [samples (with-random 42 (random-list 1000 (random-categorical weights)))]
                     [zeros (length (filter (lambda (x) (= x 0)) samples))])
                    ;; ~77% should be 0
                    (assert-true (> zeros 600)))))

;;; ============================================================
;;; Dirichlet Distribution Tests
;;; ============================================================

(test-group dirichlet
            (define-test dirichlet-sums-to-one
              (let* ([sample (with-random 42 (random-dirichlet '(1 1 1 1)))]
                     [total (fold-left + 0 sample)])
                    (assert-true (within-tolerance 1.0 total 0.0001))))
            
            (define-test dirichlet-positive
              (let ([sample (with-random 42 (random-dirichlet '(2 3 4)))])
                   (assert-true (andmap positive? sample))))
            
            (define-test dirichlet-length
              (let ([sample (with-random 42 (random-dirichlet '(1 2 3 4 5)))])
                   (assert-equal (length sample) 5))))

;;; ============================================================
;;; Reproducibility Tests
;;; ============================================================

(test-group reproducibility
            (define-test same-seed-same-sequence
              (let* ([comp (random-list 100 random-float)]
                     [seq1 (with-random 12345 comp)]
                     [seq2 (with-random 12345 comp)])
                    (assert-equal seq1 seq2)))
            
            (define-test different-seed-different-sequence
              (let* ([comp (random-list 100 random-float)]
                     [seq1 (with-random 12345 comp)]
                     [seq2 (with-random 54321 comp)])
                    (assert-true (not (equal? seq1 seq2))))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(display "         Random Number Generation Tests\n")
(display "==============================================================\n")
(display "\n")

;;; ============================================================
;;; PDF Tests
;;; ============================================================

(test-group pdf-tests
            (define-test normal-pdf-at-mean
              ;; PDF is highest at mean
              (let ([pdf-at-mean (normal-pdf 0 0 1)])
                   (assert-true (within-tolerance 0.3989 pdf-at-mean 0.001))))
            
            (define-test exponential-pdf-at-zero
              ;; PDF(0) = rate for exponential
              (let ([pdf (exponential-pdf 0 2.0)])
                   (assert-true (within-tolerance 2.0 pdf 0.001))))
            
            (define-test uniform-pdf-in-range
              (let ([pdf (uniform-pdf 0.5 0 1)])
                   (assert-true (within-tolerance 1.0 pdf 0.001))))
            
            (define-test uniform-pdf-outside-range
              (let ([pdf (uniform-pdf 2 0 1)])
                   (assert-equal pdf 0)))
            
            (define-test poisson-pmf-test
              ;; P(X=2) for Poisson(3) ≈ 0.224
              (let ([pmf (poisson-pmf 2 3.0)])
                   (assert-true (within-tolerance 0.224 pmf 0.01))))
            
            (define-test binomial-pmf-test
              ;; P(X=5) for Binomial(10, 0.5) = C(10,5) * 0.5^10 ≈ 0.246
              (let ([pmf (binomial-pmf 5 10 0.5)])
                   (assert-true (within-tolerance 0.246 pmf 0.01)))))

;;; ============================================================
;;; CDF Tests
;;; ============================================================

(test-group cdf-tests
            (define-test exponential-cdf-test
              ;; CDF at median should be 0.5
              ;; Median = ln(2)/rate = 0.693 for rate=1
              (let ([cdf (exponential-cdf 0.693 1.0)])
                   (assert-true (within-tolerance 0.5 cdf 0.01))))
            
            (define-test uniform-cdf-test
              (assert-true (within-tolerance 0.5 (uniform-cdf 0.5 0 1) 0.001))
              (assert-equal (uniform-cdf -1 0 1) 0)
              (assert-equal (uniform-cdf 2 0 1) 1))
            
            (define-test standard-normal-cdf-test
              ;; CDF(0) = 0.5 for standard normal
              (assert-true (within-tolerance 0.5 (standard-normal-cdf 0) 0.001))
              ;; CDF(1.96) ≈ 0.975
              (assert-true (within-tolerance 0.975 (standard-normal-cdf 1.96) 0.01))
              ;; CDF(-1.96) ≈ 0.025
              (assert-true (within-tolerance 0.025 (standard-normal-cdf -1.96) 0.01)))
            
            (define-test geometric-cdf-test
              ;; P(X <= 3) for Geometric(0.5)
              ;; = 1 - (1-0.5)^3 = 0.875
              (let ([cdf (geometric-cdf 3 0.5)])
                   (assert-true (within-tolerance 0.875 cdf 0.001)))))

;;; ============================================================
;;; Quantile Tests
;;; ============================================================

(test-group quantile-tests
            (define-test exponential-quantile-test
              ;; Quantile(0.5) = ln(2)/rate for exponential
              (let ([q (exponential-quantile 0.5 1.0)])
                   (assert-true (within-tolerance 0.693 q 0.01))))
            
            (define-test uniform-quantile-test
              (assert-true (within-tolerance 0.5 (uniform-quantile 0.5 0 1) 0.001))
              (assert-true (within-tolerance 0.25 (uniform-quantile 0.25 0 1) 0.001)))
            
            (define-test standard-normal-quantile-test
              ;; Q(0.5) = 0 for standard normal
              (assert-true (within-tolerance 0 (standard-normal-quantile 0.5) 0.01))
              ;; Q(0.975) ≈ 1.96
              (assert-true (within-tolerance 1.96 (standard-normal-quantile 0.975) 0.05))
              ;; Q(0.025) ≈ -1.96
              (assert-true (within-tolerance -1.96 (standard-normal-quantile 0.025) 0.05)))
            
            (define-test quantile-cdf-inverse
              ;; CDF(Quantile(p)) ≈ p
              (let* ([p 0.7]
                     [q (exponential-quantile p 2.0)]
                     [back (exponential-cdf q 2.0)])
                    (assert-true (within-tolerance p back 0.001)))))

;; Print summary
(display "\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)
(newline)

(if (= *tests-failed* 0)
    (display "[SUCCESS] All random tests passed.\n")
    (display "[FAILURE] Some random tests failed.\n"))

