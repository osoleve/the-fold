;;; lattice/random/test-distribution-properties.ss — QuickCheck properties for distribution APIs

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'prng)
(require 'distributions)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (sum xs)
  (fold-left + 0 xs))

(define (all-satisfy? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all-satisfy? pred (cdr xs)))))

(define (int->prob n)
  (/ n 1000.0))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-seed
  (gen-int-range 0 1000000))

(define gen-prob-closed
  (gen-map int->prob (gen-int-range 0 1000)))

(define gen-prob-open
  (gen-map int->prob (gen-int-range 1 999)))

(define gen-rate
  (gen-map (lambda (n) (/ n 10.0))
           (gen-int-range 1 500)))

(define gen-uniform-params
  (gen-bind (gen-int-range -1000 1000)
    (lambda (a-int)
      (gen-map (lambda (w-int)
                 (let ([a (/ a-int 10.0)]
                       [w (/ w-int 10.0)])
                   (list a (+ a w))))
               (gen-int-range 1 500)))))

(define gen-exp-inverse-args
  (gen-bind gen-prob-open
    (lambda (p)
      (gen-map (lambda (rate) (list p rate))
               gen-rate))))

(define gen-exp-monotone-args
  (gen-bind (gen-int-range 0 500)
    (lambda (x1-int)
      (gen-bind (gen-int-range 0 500)
        (lambda (delta-int)
          (gen-map (lambda (rate)
                     (let ([x1 (/ x1-int 10.0)]
                           [x2 (/ (+ x1-int delta-int) 10.0)])
                       (list x1 x2 rate)))
                   gen-rate))))))

(define gen-k-monotone
  (gen-bind (gen-int-range 0 40)
    (lambda (k1)
      (gen-map (lambda (delta) (list k1 (+ k1 delta)))
               (gen-int-range 0 40)))))

(define gen-binomial-args
  (gen-bind (gen-int-range 0 10)
    (lambda (n)
      (gen-map (lambda (p) (list n p))
               gen-prob-closed))))

(define gen-normal-inverse-args
  (gen-bind gen-prob-open
    (lambda (p)
      (gen-bind (gen-int-range -500 500)
        (lambda (mu-int)
          (gen-map (lambda (sigma-int)
                     (list p (/ mu-int 10.0) (/ sigma-int 10.0)))
                   (gen-int-range 1 200)))))))

(define gen-categorical-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 1 8)
        (lambda (n)
          (gen-map (lambda (weights) (list seed weights))
                   (gen-list-of n (gen-int-range 1 20))))))))

(define gen-beta-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 1 100)
        (lambda (a-int)
          (gen-map (lambda (b-int)
                     (list seed (/ a-int 10.0) (/ b-int 10.0)))
                   (gen-int-range 1 100)))))))

(define gen-dirichlet-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 1 6)
        (lambda (n)
          (gen-map (lambda (alphas) (list seed alphas))
                   (gen-list-of n (gen-int-range 1 10))))))))

;;; ============================================================================
;;; Functional properties
;;; ============================================================================

(test-group distribution-function-properties

  (define-property "bernoulli PMF normalizes"
    gen-prob-closed
    (lambda (p)
      (approx= (+ (bernoulli-pmf #t p)
                  (bernoulli-pmf #f p))
               1.0
               1e-12))
    'tests 300)

  (define-property "geometric CDF increments by PMF"
    (gen-bind (gen-int-range 1 40)
      (lambda (k)
        (gen-map (lambda (p) (list k p))
                 gen-prob-open)))
    (lambda (args)
      (let ([k (car args)] [p (cadr args)])
        (approx= (- (geometric-cdf k p)
                    (geometric-cdf (- k 1) p))
                 (geometric-pmf k p)
                 1e-9)))
    'tests 250)

  (define-property "exponential quantile is inverse of CDF"
    gen-exp-inverse-args
    (lambda (args)
      (let* ([p (car args)]
             [rate (cadr args)]
             [q (exponential-quantile p rate)]
             [back (exponential-cdf q rate)])
        (approx= back p 1e-6)))
    'tests 250)

  (define-property "exponential CDF is monotone"
    gen-exp-monotone-args
    (lambda (args)
      (let ([x1 (car args)] [x2 (cadr args)] [rate (caddr args)])
        (<= (exponential-cdf x1 rate)
            (exponential-cdf x2 rate))))
    'tests 220)

  (define-property "uniform quantile is inverse of CDF"
    (gen-bind gen-uniform-params
      (lambda (ab)
        (gen-map (lambda (p) (list (car ab) (cadr ab) p))
                 gen-prob-closed)))
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [p (caddr args)]
             [q (uniform-quantile p a b)]
             [back (uniform-cdf q a b)])
        (approx= back p 1e-12)))
    'tests 250)

  (define-property "standard normal CDF is symmetric"
    (gen-map (lambda (x-int) (/ x-int 10.0))
             (gen-int-range -80 80))
    (lambda (x)
      (approx= (+ (standard-normal-cdf x)
                  (standard-normal-cdf (- x)))
               1.0
               2e-4))
    'tests 250)

  (define-property "normal quantile is inverse of CDF"
    gen-normal-inverse-args
    (lambda (args)
      (let* ([p (car args)]
             [mu (cadr args)]
             [sigma (caddr args)]
             [q (normal-quantile p mu sigma)]
             [back (normal-cdf q mu sigma)])
        (approx= back p 2e-2)))
    'tests 220)

  (define-property "Poisson CDF is monotone in k"
    (gen-bind gen-k-monotone
      (lambda (ks)
        (gen-map (lambda (rate) (list (car ks) (cadr ks) rate))
                 gen-rate)))
    (lambda (args)
      (let ([k1 (car args)] [k2 (cadr args)] [rate (caddr args)])
        (<= (poisson-cdf k1 rate)
            (poisson-cdf k2 rate))))
    'tests 220)

  (define-property "binomial PMF sums to 1"
    gen-binomial-args
    (lambda (args)
      (let* ([n (car args)]
             [p (cadr args)]
             [ks (iota (+ n 1))]
             [mass (sum (map (lambda (k) (binomial-pmf k n p)) ks))])
        (approx= mass 1.0 1e-9)))
    'tests 220)
)

;;; ============================================================================
;;; Sampling output invariants
;;; ============================================================================

(test-group distribution-sampling-properties

  (define-property "random-categorical returns an in-range index"
    gen-categorical-args
    (lambda (args)
      (let* ([seed (car args)]
             [weights (cadr args)]
             [idx (with-random seed (random-categorical weights))]
             [n (length weights)])
        (and (>= idx 0)
             (< idx n))))
    'tests 220)

  (define-property "random-beta stays in [0,1]"
    gen-beta-args
    (lambda (args)
      (let* ([seed (car args)]
             [alpha (cadr args)]
             [beta (caddr args)]
             [x (with-random seed (random-beta alpha beta))])
        (and (>= x 0.0) (<= x 1.0))))
    'tests 200)

  (define-property "random-dirichlet outputs positive entries summing to 1"
    gen-dirichlet-args
    (lambda (args)
      (let* ([seed (car args)]
             [alpha-ints (cadr args)]
             [alphas (map (lambda (a) (/ a 1.0)) alpha-ints)]
             [xs (with-random seed (random-dirichlet alphas))]
             [total (sum xs)])
        (and (= (length xs) (length alphas))
             (all-satisfy? (lambda (x) (> x 0.0)) xs)
             (approx= total 1.0 1e-9))))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
