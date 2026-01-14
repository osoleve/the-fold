;;; core/types/numeric-ops.ss — Pure Numeric Operations
;;;
;;; Explicit, promotion-aware arithmetic operations for Core/Lattice code.
;;; These are the "opt-in" operators that don't shadow Scheme primitives.
;;;
;;; Naming convention: n+ n- n* n/ for numeric tower-aware ops
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - numeric-tower.ss
;;;   - numeric-instances.ss

(load "core/base/prelude.ss")
(load "core/types/numeric-tower.ss")
(load "core/types/numeric-instances.ss")

;;; ============================================================================
;;; Core Arithmetic (Promotion-Aware)
;;; ============================================================================

;;; These operators automatically promote operands to a common type.
;;; They preserve exactness when both operands are exact.

;;; n+ : Num a => a × a → a
;;; Promotion-aware addition.
(define (n+ a b)
  (let-values ([(a* b*) (promote-exact-pair a b)])
    (+ a* b*)))

;;; n- : Num a => a × a → a
;;; Promotion-aware subtraction.
(define (n- a b)
  (let-values ([(a* b*) (promote-exact-pair a b)])
    (- a* b*)))

;;; n* : Num a => a × a → a
;;; Promotion-aware multiplication.
(define (n* a b)
  (let-values ([(a* b*) (promote-exact-pair a b)])
    (* a* b*)))

;;; n/ : Fractional a => a × a → a
;;; Promotion-aware division.
;;; Note: Integer division produces exact rational (not truncated).
(define (n/ a b)
  (/ a b))  ; Chez handles promotion correctly

;;; n-neg : Num a => a → a
;;; Negation.
(define (n-neg a)
  (- a))

;;; n-abs : Num a => a → a
;;; Absolute value.
(define (n-abs a)
  (abs a))

;;; ============================================================================
;;; Integer Operations (Integral)
;;; ============================================================================

;;; n-quot : Integral a => a × a → a
;;; Integer quotient (truncates toward zero).
(define (n-quot a b)
  (quotient a b))

;;; n-rem : Integral a => a × a → a
;;; Integer remainder (sign follows dividend).
(define (n-rem a b)
  (remainder a b))

;;; n-div : Integral a => a × a → a
;;; Integer division (truncates toward negative infinity).
(define (n-div a b)
  (floor (/ a b)))

;;; n-mod : Integral a => a × a → a
;;; Modulo (sign follows divisor).
(define (n-mod a b)
  (modulo a b))

;;; n-gcd : Integral a => a × a → a
;;; Greatest common divisor.
(define (n-gcd a b)
  (gcd a b))

;;; n-lcm : Integral a => a × a → a
;;; Least common multiple.
(define (n-lcm a b)
  (lcm a b))

;;; ============================================================================
;;; Fractional Operations
;;; ============================================================================

;;; n-recip : Fractional a => a → a
;;; Reciprocal (1/a).
(define (n-recip a)
  (/ 1 a))

;;; ============================================================================
;;; Floating Operations
;;; ============================================================================

;;; These operations convert to inexact if given exact input.

;;; n-sqrt : Floating a => a → a
(define (n-sqrt a)
  (sqrt (if (and (exact? a) (real? a)) (exact->inexact a) a)))

;;; n-exp : Floating a => a → a
(define (n-exp a)
  (exp (if (and (exact? a) (real? a)) (exact->inexact a) a)))

;;; n-log : Floating a => a → a
(define (n-log a)
  (log (if (and (exact? a) (real? a)) (exact->inexact a) a)))

;;; n-pow : Floating a => a × a → a
;;; Exponentiation.
(define (n-pow base exponent)
  (expt base exponent))

;;; n-sin : Floating a => a → a
(define (n-sin a)
  (sin (if (and (exact? a) (real? a)) (exact->inexact a) a)))

;;; n-cos : Floating a => a → a
(define (n-cos a)
  (cos (if (and (exact? a) (real? a)) (exact->inexact a) a)))

;;; n-tan : Floating a => a → a
(define (n-tan a)
  (tan (if (and (exact? a) (real? a)) (exact->inexact a) a)))

;;; n-asin : Floating a => a → a
(define (n-asin a)
  (asin (if (and (exact? a) (real? a)) (exact->inexact a) a)))

;;; n-acos : Floating a => a → a
(define (n-acos a)
  (acos (if (and (exact? a) (real? a)) (exact->inexact a) a)))

;;; n-atan : Floating a => a → a
;;; Single-argument arctangent.
(define (n-atan a)
  (atan (if (and (exact? a) (real? a)) (exact->inexact a) a)))

;;; n-atan2 : Floating a => a × a → a
;;; Two-argument arctangent (atan2).
(define (n-atan2 y x)
  (atan y x))

;;; ============================================================================
;;; Comparison Operations
;;; ============================================================================

;;; n= : Eq a => a × a → Bool
;;; Numeric equality (works across tower).
(define (n= a b)
  (= a b))

;;; n< : Ord a => a × a → Bool
(define (n< a b)
  (< a b))

;;; n<= : Ord a => a × a → Bool
(define (n<= a b)
  (<= a b))

;;; n> : Ord a => a × a → Bool
(define (n> a b)
  (> a b))

;;; n>= : Ord a => a × a → Bool
(define (n>= a b)
  (>= a b))

;;; n-compare : Ord a => a × a → Symbol
;;; Returns 'LT, 'EQ, or 'GT.
(define (n-compare a b)
  (cond [(< a b) 'LT]
        [(> a b) 'GT]
        [else 'EQ]))

;;; n-min : Ord a => a × a → a
(define (n-min a b)
  (if (< a b) a b))

;;; n-max : Ord a => a × a → a
(define (n-max a b)
  (if (> a b) a b))

;;; ============================================================================
;;; Variadic Operations
;;; ============================================================================

;;; n-sum : Num a => (List a) → a
;;; Sum of a list of numbers.
(define (n-sum xs)
  (if (null? xs)
      0
      (fold-left n+ (car xs) (cdr xs))))

;;; n-product : Num a => (List a) → a
;;; Product of a list of numbers.
(define (n-product xs)
  (if (null? xs)
      1
      (fold-left n* (car xs) (cdr xs))))

;;; n-minimum : Ord a => (List a) → a
;;; Minimum of a non-empty list.
(define (n-minimum xs)
  (if (null? xs)
      (error 'n-minimum "empty list")
      (fold-left n-min (car xs) (cdr xs))))

;;; n-maximum : Ord a => (List a) → a
;;; Maximum of a non-empty list.
(define (n-maximum xs)
  (if (null? xs)
      (error 'n-maximum "empty list")
      (fold-left n-max (car xs) (cdr xs))))

;;; ============================================================================
;;; Constants
;;; ============================================================================

;;; n-pi : Floating a => a
(define n-pi 3.141592653589793)

;;; n-e : Floating a => a
(define n-e 2.718281828459045)

;;; n-tau : Floating a => a (2*pi)
(define n-tau 6.283185307179586)

;;; ============================================================================
;;; Summary
;;; ============================================================================

;;; Core Arithmetic:
;;;   n+, n-, n*, n/, n-neg, n-abs
;;;
;;; Integer Operations:
;;;   n-quot, n-rem, n-div, n-mod, n-gcd, n-lcm
;;;
;;; Fractional:
;;;   n-recip
;;;
;;; Floating:
;;;   n-sqrt, n-exp, n-log, n-pow
;;;   n-sin, n-cos, n-tan, n-asin, n-acos, n-atan, n-atan2
;;;
;;; Comparison:
;;;   n=, n<, n<=, n>, n>=, n-compare, n-min, n-max
;;;
;;; Variadic:
;;;   n-sum, n-product, n-minimum, n-maximum
;;;
;;; Constants:
;;;   n-pi, n-e, n-tau
