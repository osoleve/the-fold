;;; lattice/number-theory/test-modular-properties.ss — QuickCheck properties for number theory

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'modular)
(require 'primality)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define small-primes
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

(define (product xs)
  (if (null? xs) 1 (apply * xs)))

(define (all-satisfy? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all-satisfy? pred (cdr xs)))))

(define (last-item xs)
  (if (null? (cdr xs))
      (car xs)
      (last-item (cdr xs))))

(define (pow-int b e)
  (let loop ([k e] [acc 1])
    (if (<= k 0)
        acc
        (loop (- k 1) (* acc b)))))

(define (prime-factorization->integer pf)
  (fold-left (lambda (acc pe)
               (* acc (pow-int (car pe) (cdr pe))))
             1
             pf))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-mod-args
  (gen-bind (gen-int-range 2 257)
    (lambda (m)
      (gen-bind (gen-int-range -10000 10000)
        (lambda (a)
          (gen-map (lambda (b) (list a b m))
                   (gen-int-range -10000 10000)))))))

(define gen-expt-args
  (gen-bind (gen-int-range -200 200)
    (lambda (a)
      (gen-bind (gen-int-range 0 20)
        (lambda (e)
          (gen-map (lambda (m) (list a e m))
                   (gen-int-range 2 257)))))))

(define gen-gcd-args
  (gen-pair (gen-int-range 0 20000)
            (gen-int-range 0 20000)))

(define gen-prime-mod-and-a
  (gen-bind (gen-elements small-primes)
    (lambda (p)
      (gen-map (lambda (a) (list a p))
               (gen-int-range 1 (- p 1))))))

(define gen-crt-args
  (gen-bind (gen-int-range 0 2)
    (lambda (r1)
      (gen-bind (gen-int-range 0 4)
        (lambda (r2)
          (gen-map (lambda (r3) (list r1 r2 r3))
                   (gen-int-range 0 6)))))))

;;; ============================================================================
;;; Modular arithmetic laws
;;; ============================================================================

(test-group modular-laws

  (define-property "mod+ agrees with integer congruence"
    gen-mod-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)] [m (caddr args)])
        (= (mod+ a b m)
           (modulo (+ a b) m))))
    'tests 300)

  (define-property "mod- agrees with integer congruence"
    gen-mod-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)] [m (caddr args)])
        (= (mod- a b m)
           (modulo (- a b) m))))
    'tests 300)

  (define-property "mod* agrees with integer congruence"
    gen-mod-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)] [m (caddr args)])
        (= (mod* a b m)
           (modulo (* a b) m))))
    'tests 300)

  (define-property "mod+ is commutative"
    gen-mod-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)] [m (caddr args)])
        (= (mod+ a b m)
           (mod+ b a m))))
    'tests 250)

  (define-property "mod* is commutative"
    gen-mod-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)] [m (caddr args)])
        (= (mod* a b m)
           (mod* b a m))))
    'tests 250)

  (define-property "mod-expt satisfies one-step recurrence"
    gen-expt-args
    (lambda (args)
      (let ([a (car args)] [e (cadr args)] [m (caddr args)])
        (= (mod-expt a (+ e 1) m)
           (mod* (mod-expt a e m) a m))))
    'tests 250)
)

;;; ============================================================================
;;; GCD / inverse / CRT properties
;;; ============================================================================

(test-group number-theory-ops

  (define-property "gcd divides both operands"
    gen-gcd-args
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [g (gcd a b)])
        (or (= g 0)
            (and (= 0 (modulo a g))
                 (= 0 (modulo b g))))))
    'tests 250)

  (define-property "extended-gcd satisfies Bezout identity"
    gen-gcd-args
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [result (extended-gcd a b)]
             [g (car result)]
             [x (cadr result)]
             [y (caddr result)])
        (and (= g (+ (* a x) (* b y)))
             (= (abs g) (gcd a b)))))
    'tests 250)

  (define-property "mod-inverse works for non-zero values mod prime p"
    gen-prime-mod-and-a
    (lambda (args)
      (let* ([a (car args)]
             [p (cadr args)]
             [inv (mod-inverse a p)])
        (and inv
             (= 1 (mod* a inv p)))))
    'tests 250)

  (define-property "CRT solution satisfies all congruences for (3,5,7)"
    gen-crt-args
    (lambda (args)
      (let* ([r1 (car args)]
             [r2 (cadr args)]
             [r3 (caddr args)]
             [x (crt (list r1 r2 r3) '(3 5 7))])
        (and x
             (= r1 (modulo x 3))
             (= r2 (modulo x 5))
             (= r3 (modulo x 7)))))
    'tests 200)
)

;;; ============================================================================
;;; Primality / factorization properties
;;; ============================================================================

(test-group primality-and-factorization

  (define-property "prime? agrees with deterministic Miller-Rabin"
    (gen-int-range 0 20000)
    (lambda (n)
      (equal? (prime? n)
              (miller-rabin? n)))
    'tests 250)

  (define-property "factorize reconstructs the original integer"
    (gen-int-range 1 20000)
    (lambda (n)
      (= n (product (factorize n))))
    'tests 200)

  (define-property "factorize only returns prime factors"
    (gen-int-range 1 20000)
    (lambda (n)
      (all-satisfy? prime? (factorize n)))
    'tests 200)

  (define-property "prime-factorization reconstructs the original integer"
    (gen-int-range 1 20000)
    (lambda (n)
      (= n (prime-factorization->integer (prime-factorization n))))
    'tests 200)

  (define-property "divisors are valid and bounded"
    (gen-int-range 1 5000)
    (lambda (n)
      (let ([ds (divisors n)])
        (and (not (null? ds))
             (= 1 (car ds))
             (= n (last-item ds))
             (all-satisfy? (lambda (d) (= 0 (modulo n d))) ds))))
    'tests 200)

  (define-property "Euler totient satisfies phi(p) = p - 1 for primes"
    (gen-elements small-primes)
    (lambda (p)
      (= (euler-totient p) (- p 1)))
    'tests 150)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
