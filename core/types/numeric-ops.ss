(load "core/base/prelude.ss")
(load "core/types/numeric-tower.ss")
(load "core/types/numeric-instances.ss")

(doc 'module 'numeric-ops)
(doc 'description "Pure numeric operations with explicit promotion-aware arithmetic. These are opt-in operators that don't shadow Scheme primitives. Naming convention: n+ n- n* n/ for numeric tower-aware ops.")
(doc 'layer 'core)

(doc 'section 'core-arithmetic-promotion-aware)

(define (n+ a b)
  (doc 'type '(-> Num a => a a a))
  (doc 'description "Promotion-aware addition. Automatically promotes operands to common type, preserving exactness when both operands are exact.")
  (doc 'export #t)
  (let-values ([(a* b*) (promote-exact-pair a b)])
    (+ a* b*)))

(define (n- a b)
  (doc 'type '(-> Num a => a a a))
  (doc 'description "Promotion-aware subtraction.")
  (doc 'export #t)
  (let-values ([(a* b*) (promote-exact-pair a b)])
    (- a* b*)))

(define (n* a b)
  (doc 'type '(-> Num a => a a a))
  (doc 'description "Promotion-aware multiplication.")
  (doc 'export #t)
  (let-values ([(a* b*) (promote-exact-pair a b)])
    (* a* b*)))

(define (n/ a b)
  (doc 'type '(-> Fractional a => a a a))
  (doc 'description "Promotion-aware division. Integer division produces exact rational, not truncated.")
  (doc 'export #t)
  (/ a b))

(define (n-neg a)
  (doc 'type '(-> Num a => a a))
  (doc 'description "Negation.")
  (doc 'export #t)
  (- a))

(define (n-abs a)
  (doc 'type '(-> Num a => a a))
  (doc 'description "Absolute value.")
  (doc 'export #t)
  (abs a))

(doc 'section 'integer-operations-integral)

(define (n-quot a b)
  (doc 'type '(-> Integral a => a a a))
  (doc 'description "Integer quotient (truncates toward zero).")
  (doc 'export #t)
  (quotient a b))

(define (n-rem a b)
  (doc 'type '(-> Integral a => a a a))
  (doc 'description "Integer remainder (sign follows dividend).")
  (doc 'export #t)
  (remainder a b))

(define (n-div a b)
  (doc 'type '(-> Integral a => a a a))
  (doc 'description "Integer division (truncates toward negative infinity).")
  (doc 'export #t)
  (floor (/ a b)))

(define (n-mod a b)
  (doc 'type '(-> Integral a => a a a))
  (doc 'description "Modulo (sign follows divisor).")
  (doc 'export #t)
  (modulo a b))

(define (n-gcd a b)
  (doc 'type '(-> Integral a => a a a))
  (doc 'description "Greatest common divisor.")
  (doc 'export #t)
  (gcd a b))

(define (n-lcm a b)
  (doc 'type '(-> Integral a => a a a))
  (doc 'description "Least common multiple.")
  (doc 'export #t)
  (lcm a b))

(doc 'section 'fractional-operations)

(define (n-recip a)
  (doc 'type '(-> Fractional a => a a))
  (doc 'description "Reciprocal (1/a).")
  (doc 'export #t)
  (/ 1 a))

(doc 'section 'floating-operations)
(doc 'note "These operations convert to inexact if given exact input")

(define (n-sqrt a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (sqrt (if (and (exact? a) (real? a)) (exact->inexact a) a)))

(define (n-exp a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (exp (if (and (exact? a) (real? a)) (exact->inexact a) a)))

(define (n-log a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (log (if (and (exact? a) (real? a)) (exact->inexact a) a)))

(define (n-pow base exponent)
  (doc 'type '(-> Floating a => a a a))
  (doc 'description "Exponentiation.")
  (doc 'export #t)
  (expt base exponent))

(define (n-sin a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (sin (if (and (exact? a) (real? a)) (exact->inexact a) a)))

(define (n-cos a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (cos (if (and (exact? a) (real? a)) (exact->inexact a) a)))

(define (n-tan a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (tan (if (and (exact? a) (real? a)) (exact->inexact a) a)))

(define (n-asin a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (asin (if (and (exact? a) (real? a)) (exact->inexact a) a)))

(define (n-acos a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (acos (if (and (exact? a) (real? a)) (exact->inexact a) a)))

(define (n-atan a)
  (doc 'type '(-> Floating a => a a))
  (doc 'description "Single-argument arctangent.")
  (doc 'export #t)
  (atan (if (and (exact? a) (real? a)) (exact->inexact a) a)))

(define (n-atan2 y x)
  (doc 'type '(-> Floating a => a a a))
  (doc 'description "Two-argument arctangent (atan2).")
  (doc 'export #t)
  (atan y x))

(doc 'section 'comparison-operations)

(define (n= a b)
  (doc 'type '(-> Eq a => a a Bool))
  (doc 'description "Numeric equality (works across tower).")
  (doc 'export #t)
  (= a b))

(define (n< a b)
  (doc 'type '(-> Ord a => a a Bool))
  (doc 'export #t)
  (< a b))

(define (n<= a b)
  (doc 'type '(-> Ord a => a a Bool))
  (doc 'export #t)
  (<= a b))

(define (n> a b)
  (doc 'type '(-> Ord a => a a Bool))
  (doc 'export #t)
  (> a b))

(define (n>= a b)
  (doc 'type '(-> Ord a => a a Bool))
  (doc 'export #t)
  (>= a b))

(define (n-compare a b)
  (doc 'type '(-> Ord a => a a Symbol))
  (doc 'description "Returns 'LT, 'EQ, or 'GT.")
  (doc 'export #t)
  (cond [(< a b) 'LT]
        [(> a b) 'GT]
        [else 'EQ]))

(define (n-min a b)
  (doc 'type '(-> Ord a => a a a))
  (doc 'export #t)
  (if (< a b) a b))

(define (n-max a b)
  (doc 'type '(-> Ord a => a a a))
  (doc 'export #t)
  (if (> a b) a b))

(doc 'section 'variadic-operations)

(define (n-sum xs)
  (doc 'type '(-> Num a => (List a) a))
  (doc 'description "Sum of a list of numbers.")
  (doc 'export #t)
  (if (null? xs)
      0
      (fold-left n+ (car xs) (cdr xs))))

(define (n-product xs)
  (doc 'type '(-> Num a => (List a) a))
  (doc 'description "Product of a list of numbers.")
  (doc 'export #t)
  (if (null? xs)
      1
      (fold-left n* (car xs) (cdr xs))))

(define (n-minimum xs)
  (doc 'type '(-> Ord a => (List a) a))
  (doc 'description "Minimum of a non-empty list.")
  (doc 'export #t)
  (if (null? xs)
      (error 'n-minimum "empty list")
      (fold-left n-min (car xs) (cdr xs))))

(define (n-maximum xs)
  (doc 'type '(-> Ord a => (List a) a))
  (doc 'description "Maximum of a non-empty list.")
  (doc 'export #t)
  (if (null? xs)
      (error 'n-maximum "empty list")
      (fold-left n-max (car xs) (cdr xs))))

(doc 'section 'constants)

(doc n-pi 'type '(-> Floating a => a))
(doc n-pi 'export #t)
(define n-pi 3.141592653589793)

(doc n-e 'type '(-> Floating a => a))
(doc n-e 'export #t)
(define n-e 2.718281828459045)

(doc n-tau 'type '(-> Floating a => a))
(doc n-tau 'description "2*pi")
(doc n-tau 'export #t)
(define n-tau 6.283185307179586)
