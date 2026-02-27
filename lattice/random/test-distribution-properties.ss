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

(define (finite-number? x)
  (and (number? x)
       (= x x)
       (not (= x +inf.0))
       (not (= x -inf.0))))

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

(define gen-log-gamma-arg
  (gen-map (lambda (n) (/ n 20.0))
           (gen-int-range 1 400)))

(define gen-exp-pdf-args
  (gen-bind (gen-int-range -200 400)
    (lambda (x-int)
      (gen-map (lambda (rate) (list (/ x-int 10.0) rate))
               gen-rate))))

(define gen-uniform-pdf-args
  (gen-bind gen-uniform-params
    (lambda (ab)
      (gen-map (lambda (p)
                 (list (car ab) (cadr ab) p))
               gen-prob-closed))))

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

(define gen-random-geometric-args
  (gen-bind gen-seed
    (lambda (seed)
      (gen-map (lambda (p) (list seed p))
               gen-prob-open))))

(define gen-standard-normal-inverse-arg
  gen-prob-open)

(define gen-standard-normal-shape-arg
  (gen-map (lambda (n) (/ n 10.0))
           (gen-int-range -80 80)))

(define gen-normal-constructor-args
  (gen-bind (gen-int-range -400 400)
    (lambda (mu-int)
      (gen-map (lambda (sigma-int)
                 (list (/ mu-int 10.0)
                       (/ sigma-int 10.0)))
               (gen-int-range 1 200)))))

(define gen-exponential-constructor-arg
  gen-rate)

(define gen-uniform-constructor-args
  gen-uniform-params)

(define gen-bernoulli-constructor-arg
  gen-prob-closed)

(define gen-geometric-quantile-args
  (gen-bind gen-prob-open
    (lambda (prob)
      (gen-map (lambda (p) (list prob p))
               gen-prob-open))))

(define gen-poisson-constructor-arg
  gen-rate)

(define gen-binomial-constructor-args
  gen-binomial-args)

(define gen-gamma-constructor-args
  (gen-bind (gen-int-range 1 200)
    (lambda (shape-int)
      (gen-map (lambda (rate-int)
                 (list (/ shape-int 10.0)
                       (/ rate-int 10.0)))
               (gen-int-range 1 200)))))

(define gen-beta-constructor-args
  (gen-bind (gen-int-range 1 200)
    (lambda (a-int)
      (gen-map (lambda (b-int)
                 (list (/ a-int 10.0)
                       (/ b-int 10.0)))
               (gen-int-range 1 200)))))

(define gen-categorical-constructor-arg
  (gen-bind (gen-int-range 1 8)
    (lambda (n)
      (gen-list-of n (gen-int-range 1 20)))))

(define gen-factorial-arg
  (gen-int-range 0 10))

(define gen-binom-coeff-args
  (gen-bind (gen-int-range 0 30)
    (lambda (n)
      (gen-map (lambda (k) (list n k))
               (gen-int-range 0 n)))))

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

  (define-property "exponential-pdf is nonnegative and zero on negative support"
    gen-exp-pdf-args
    (lambda (args)
      (let* ([x (car args)]
             [rate (cadr args)]
             [p (exponential-pdf x rate)])
        (and (>= p 0.0)
             (if (< x 0)
                 (= p 0.0)
                 #t))))
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

  (define-property "uniform-pdf is constant inside bounds and zero outside"
    gen-uniform-pdf-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [p (caddr args)]
             [x (+ a (* p (- b a)))]
             [inside (uniform-pdf x a b)]
             [outside-left (uniform-pdf (- a 1.0) a b)]
             [outside-right (uniform-pdf (+ b 1.0) a b)])
        (and (approx= inside (/ 1.0 (- b a)) 1e-12)
             (= outside-left 0.0)
             (= outside-right 0.0))))
    'tests 220)

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

  (define-property "log-gamma recurrence logΓ(x+1)=log(x)+logΓ(x)"
    gen-log-gamma-arg
    (lambda (x)
      (approx= (log-gamma (+ x 1.0))
               (+ (log-num x) (log-gamma x))
               5e-5))
    'tests 220)

  (define-property "standard normal quantile inverts CDF"
    gen-standard-normal-inverse-arg
    (lambda (p)
      (approx= (standard-normal-cdf (standard-normal-quantile p))
               p
               2e-3))
    'tests 220)

  (define-property "standard normal PDF is symmetric and nonnegative"
    gen-standard-normal-shape-arg
    (lambda (x)
      (and (approx= (standard-normal-pdf x)
                    (standard-normal-pdf (- x))
                    1e-12)
           (>= (standard-normal-pdf x) 0.0)))
    'tests 220)

  (define-property "normal constructor preserves tag and parameters"
    gen-normal-constructor-args
    (lambda (args)
      (let* ([mu (car args)]
             [sigma (cadr args)]
             [d (normal mu sigma)])
        (and (equal? (car d) 'normal)
             (= (cadr d) mu)
             (= (caddr d) sigma))))
    'tests 180)

  (define-property "exponential constructor preserves tag and rate"
    gen-exponential-constructor-arg
    (lambda (rate)
      (let ([d (exponential rate)])
        (and (equal? (car d) 'exponential)
             (= (cadr d) rate))))
    'tests 180)

  (define-property "uniform constructor preserves bounds"
    gen-uniform-constructor-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [d (uniform a b)])
        (and (equal? (car d) 'uniform)
             (= (cadr d) a)
             (= (caddr d) b))))
    'tests 180)

  (define-property "bernoulli constructor preserves probability"
    gen-bernoulli-constructor-arg
    (lambda (p)
      (let ([d (bernoulli p)])
        (and (equal? (car d) 'bernoulli)
             (= (cadr d) p))))
    'tests 180)

  (define-property "geometric quantile brackets probability mass"
    gen-geometric-quantile-args
    (lambda (args)
      (let* ([u (car args)]
             [p (cadr args)]
             [k (geometric-quantile u p)]
             [cdf-k (geometric-cdf k p)]
             [cdf-prev (geometric-cdf (- k 1) p)])
        (and (>= k 1)
             (<= cdf-prev u)
             (<= u cdf-k))))
    'tests 220)

  (define-property "poisson constructor preserves rate"
    gen-poisson-constructor-arg
    (lambda (rate)
      (let ([d (poisson rate)])
        (and (equal? (car d) 'poisson)
             (= (cadr d) rate))))
    'tests 180)

  (define-property "binomial constructor preserves n and p"
    gen-binomial-constructor-args
    (lambda (args)
      (let* ([n (car args)]
             [p (cadr args)]
             [d (binomial n p)])
        (and (equal? (car d) 'binomial)
             (= (cadr d) n)
             (= (caddr d) p))))
    'tests 180)

  (define-property "gamma constructor preserves shape and rate"
    gen-gamma-constructor-args
    (lambda (args)
      (let* ([shape (car args)]
             [rate (cadr args)]
             [d (gamma shape rate)])
        (and (equal? (car d) 'gamma)
             (= (cadr d) shape)
             (= (caddr d) rate))))
    'tests 180)

  (define-property "beta constructor preserves alpha and beta"
    gen-beta-constructor-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [d (beta a b)])
        (and (equal? (car d) 'beta)
             (= (cadr d) a)
             (= (caddr d) b))))
    'tests 180)

  (define-property "categorical constructor preserves weights"
    gen-categorical-constructor-arg
    (lambda (weights)
      (let ([d (categorical weights)])
        (and (equal? (car d) 'categorical)
             (equal? (cadr d) weights))))
    'tests 180)

  (define-property "factorial recurrence holds for small n"
    gen-factorial-arg
    (lambda (n)
      (= (factorial (+ n 1))
         (* (+ n 1) (factorial n))))
    'tests 200)

  (define-property "binomial coefficient is symmetric"
    gen-binom-coeff-args
    (lambda (args)
      (let ([n (car args)] [k (cadr args)])
        (= (binomial-coeff n k)
           (binomial-coeff n (- n k)))))
    'tests 200)
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

  (define-property "random-normal-standard returns finite values"
    gen-seed
    (lambda (seed)
      (finite-number? (with-random seed random-normal-standard)))
    'tests 220)

  (define-property "random-normal-pair returns finite pair"
    gen-seed
    (lambda (seed)
      (let ([pair (with-random seed random-normal-pair)])
        (and (pair? pair)
             (finite-number? (car pair))
             (finite-number? (cdr pair)))))
    'tests 220)

  (define-property "random-bernoulli returns booleans"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (p) (list seed p)) gen-prob-closed)))
    (lambda (args)
      (let* ([seed (car args)]
             [p (cadr args)]
             [x (with-random seed (random-bernoulli p))])
        (or (eq? x #t) (eq? x #f))))
    'tests 220)

  (define-property "random-exponential outputs are nonnegative"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (rate) (list seed rate)) gen-rate)))
    (lambda (args)
      (let* ([seed (car args)]
             [rate (cadr args)]
             [x (with-random seed (random-exponential rate))])
        (and (finite-number? x) (>= x 0.0))))
    'tests 220)

  (define-property "random-geometric outputs positive integers"
    gen-random-geometric-args
    (lambda (args)
      (let* ([seed (car args)]
             [p (cadr args)]
             [x (with-random seed (random-geometric p))])
        (and (integer? x) (>= x 1))))
    'tests 220)

  (define-property "random-poisson outputs are nonnegative integers"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (rate) (list seed rate)) gen-rate)))
    (lambda (args)
      (let* ([seed (car args)]
             [rate (cadr args)]
             [x (with-random seed (random-poisson rate))])
        (and (integer? x) (>= x 0))))
    'tests 180)

  (define-property "random-binomial outputs are in [0,n]"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (np) (list seed (car np) (cadr np)))
                 gen-binomial-args)))
    (lambda (args)
      (let* ([seed (car args)]
             [n (cadr args)]
             [p (caddr args)]
             [x (with-random seed (random-binomial n p))])
        (and (integer? x) (>= x 0) (<= x n))))
    'tests 200)

  (define-property "random-gamma outputs are positive"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (sr) (list seed (car sr) (cadr sr)))
                 gen-gamma-constructor-args)))
    (lambda (args)
      (let* ([seed (car args)]
             [shape (cadr args)]
             [rate (caddr args)]
             [x (with-random seed (random-gamma shape rate))])
        (and (finite-number? x) (> x 0.0))))
    'tests 200)

  (define-property "random-normal-vector length matches request"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-bind (gen-int-range 0 20)
          (lambda (n)
            (gen-bind (gen-int-range -200 200)
              (lambda (mu-int)
                (gen-map (lambda (sigma-int)
                           (list seed n (/ mu-int 10.0) (/ sigma-int 10.0)))
                         (gen-int-range 1 100))))))))
    (lambda (args)
      (let* ([seed (car args)]
             [n (cadr args)]
             [mu (caddr args)]
             [sigma (cadddr args)]
             [xs (with-random seed (random-normal-vector n mu sigma))])
        (and (= (length xs) n)
             (all-satisfy? finite-number? xs))))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
