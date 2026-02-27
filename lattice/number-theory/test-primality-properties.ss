;;; lattice/number-theory/test-primality-properties.ss — QuickCheck properties for primality utilities

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'primality)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define odd-primes-under-100
  '(3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

(define (all-satisfy? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all-satisfy? pred (cdr xs)))))

(define (strictly-increasing? xs)
  (or (null? xs)
      (null? (cdr xs))
      (and (< (car xs) (cadr xs))
           (strictly-increasing? (cdr xs)))))

(define (last-item xs)
  (if (null? (cdr xs))
      (car xs)
      (last-item (cdr xs))))

(define (product xs)
  (if (null? xs) 1 (apply * xs)))

(define (no-prime-in-open-interval? lo hi)
  (let loop ([k (+ lo 1)])
    (or (>= k hi)
        (and (not (prime? k))
             (loop (+ k 1))))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -100 50000))

(define gen-pos-small
  (gen-int-range 1 20000))

(define gen-k-small
  (gen-int-range 1 400))

(define gen-perfect-power-args
  (gen-bind (gen-int-range 2 20)
    (lambda (a)
      (gen-map (lambda (k) (cons a k))
               (gen-int-range 2 6)))))

(define gen-jacobi-args
  (gen-bind (gen-int-range -400 400)
    (lambda (a)
      (gen-map (lambda (p) (cons a p))
               (gen-elements odd-primes-under-100)))))

;;; ============================================================================
;;; Prime navigation / counting
;;; ============================================================================

(test-group prime-navigation-properties

  (define-property "composite? matches definition"
    gen-int-small
    (lambda (n)
      (equal? (composite? n)
              (and (> n 1) (not (prime? n)))))
    'tests 250)

  (define-property "next-prime returns a prime greater than input"
    gen-int-small
    (lambda (n)
      (let ([p (next-prime n)])
        (and (prime? p)
             (> p n))))
    'tests 250)

  (define-property "next-prime is minimal above input"
    (gen-int-range -50 10000)
    (lambda (n)
      (let ([p (next-prime n)])
        (no-prime-in-open-interval? n p)))
    'tests 220)

  (define-property "prev-prime returns a prime below input"
    (gen-int-range 3 50000)
    (lambda (n)
      (let ([p (prev-prime n)])
        (and p
             (prime? p)
             (< p n))))
    'tests 250)

  (define-property "prev-prime is maximal below input"
    (gen-int-range 4 10000)
    (lambda (n)
      (let ([p (prev-prime n)])
        (and p
             (no-prime-in-open-interval? p n))))
    'tests 220)

  (define-property "nth-prime inverts prime counting at prime arguments"
    gen-k-small
    (lambda (k)
      (let ([p (nth-prime k)])
        (= (prime-pi p) k)))
    'tests 220)

  (define-property "primes-up-to is sorted, prime-only, and length = prime-pi"
    (gen-int-range 2 8000)
    (lambda (n)
      (let ([ps (primes-up-to n)])
        (and (strictly-increasing? ps)
             (all-satisfy? prime? ps)
             (all-satisfy? (lambda (p) (<= p n)) ps)
             (= (length ps) (prime-pi n)))))
    'tests 200)

  (define-property "last prime in primes-up-to n matches prev-prime (n+1)"
    (gen-int-range 2 8000)
    (lambda (n)
      (let ([ps (primes-up-to n)])
        (and (not (null? ps))
             (= (last-item ps)
                (prev-prime (+ n 1))))))
    'tests 180)
)

;;; ============================================================================
;;; Arithmetic function invariants
;;; ============================================================================

(test-group arithmetic-function-properties

  (define-property "Euler totient is in [1, n] for n >= 1"
    gen-pos-small
    (lambda (n)
      (let ([phi (euler-totient n)])
        (and (<= 1 phi)
             (<= phi n))))
    'tests 220)

  (define-property "Euler totient is even for n > 2"
    (gen-int-range 3 20000)
    (lambda (n)
      (even? (euler-totient n)))
    'tests 220)

  (define-property "Carmichael lambda divides Euler totient"
    (gen-int-range 1 12000)
    (lambda (n)
      (let ([phi (euler-totient n)]
            [lam (carmichael-lambda n)])
        (and (> lam 0)
             (= 0 (modulo phi lam)))))
    'tests 220)

  (define-property "Mobius only takes values in {-1,0,1}"
    gen-pos-small
    (lambda (n)
      (memv (mobius n) '(-1 0 1)))
    'tests 220)

  (define-property "radical(n) divides n and is <= n"
    gen-pos-small
    (lambda (n)
      (let ([r (radical n)])
        (and (> r 0)
             (= 0 (modulo n r))
             (<= r n))))
    'tests 220)

  (define-property "trial-division factors reconstruct n and are prime"
    (gen-int-range 2 20000)
    (lambda (n)
      (let ([factors (trial-division n)])
        (and (= n (product factors))
             (all-satisfy? prime? factors))))
    'tests 200)

  (define-property "coprime? matches gcd = 1"
    (gen-bind (gen-int-range 1 20000)
      (lambda (a)
        (gen-map (lambda (b) (cons a b))
                 (gen-int-range 1 20000))))
    (lambda (pair)
      (let ([a (car pair)] [b (cdr pair)])
        (equal? (coprime? a b)
                (= 1 (gcd a b)))))
    'tests 220)

  (define-property "lcm and gcd satisfy lcm(a,b)*gcd(a,b)=a*b for positive ints"
    (gen-bind (gen-int-range 1 20000)
      (lambda (a)
        (gen-map (lambda (b) (cons a b))
                 (gen-int-range 1 20000))))
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [g (gcd a b)]
             [l (lcm a b)])
        (= (* l g) (* a b))))
    'tests 220)

  (define-property "gcd* and lcm* are lower/upper divisibility bounds"
    (gen-list (gen-int-range 1 500))
    (lambda (xs)
      (if (null? xs)
          (and (= 0 (gcd* xs))
               (= 1 (lcm* xs)))
          (let ([g (gcd* xs)]
                [l (lcm* xs)])
            (and (all-satisfy? (lambda (x) (= 0 (modulo x g))) xs)
                 (all-satisfy? (lambda (x) (= 0 (modulo l x))) xs)))))
    'tests 200
    'max-size 8)
)

;;; ============================================================================
;;; Perfect powers / symbol consistency
;;; ============================================================================

(test-group power-and-symbol-properties

  (define-property "is-perfect-square? accepts exact squares"
    (gen-int-range 0 3000)
    (lambda (k)
      (let ([n (* k k)])
        (and (is-perfect-square? n)
             (= (isqrt n) k))))
    'tests 220)

  (define-property "integer-root finds exact roots on perfect powers"
    gen-perfect-power-args
    (lambda (args)
      (let* ([a (car args)]
             [k (cdr args)]
             [n (expt a k)])
        (= (integer-root n k) a)))
    'tests 220)

  (define-property "is-perfect-power? recognizes exact powers"
    gen-perfect-power-args
    (lambda (args)
      (let* ([a (car args)]
             [k (cdr args)]
             [n (expt a k)]
             [pp (is-perfect-power? n)])
        (and pp
             (= (expt (car pp) (cdr pp)) n))))
    'tests 200)

  (define-property "Jacobi agrees with Legendre for odd primes"
    gen-jacobi-args
    (lambda (pair)
      (let ([a (car pair)]
            [p (cdr pair)])
        (= (jacobi-symbol a p)
           (legendre-symbol a p))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
