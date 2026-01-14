;;; core/types/numeric-tower.ss — Numeric Type Tower and Promotion
;;;
;;; Unified numeric type hierarchy leveraging Chez Scheme's native tower:
;;;   Fixnum ⊂ Bignum ⊂ Integer ⊂ Rational ⊂ Real ⊂ Complex
;;;
;;; Provides:
;;;   - Unified predicates for native + custom types
;;;   - Type promotion logic with documented asymmetries
;;;   - Conversion functions between tower levels
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Key Design Decisions:
;;;   1. Leverage Chez's native tower (no custom BigInt)
;;;   2. Bridge to lattice's custom Complex for CAS serialization
;;;   3. Document rational↔flonum asymmetry explicitly
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================================
;;; Type Classification
;;; ============================================================================

;;; The numeric tower, from most specific to most general:
;;;
;;;   Fixnum   : Machine-word integers (fast, bounded)
;;;   Bignum   : Arbitrary-precision integers
;;;   Integer  : Abstract integer (Fixnum ∪ Bignum)
;;;   Rational : Exact ratios (p/q where p,q are integers)
;;;   Real     : Inexact floating-point (IEEE 754)
;;;   Complex  : a + bi (native or custom)
;;;
;;; Note: Chez distinguishes exact vs inexact at each level.
;;; We treat exactness as orthogonal to the tower position.

(define numeric-tower-order
  '(Fixnum Bignum Integer Rational Real Complex))

;;; numeric-type : Value → Symbol
;;; Returns the most specific numeric type for a value.
;;; Returns 'NotNumeric for non-numeric values.
;;;
;;; Handles both Chez native numbers and lattice custom Complex.
(define (numeric-type x)
  (cond
    ;; Lattice custom complex: (complex r i)
    [(and (pair? x) (eq? (car x) 'complex))
     'Complex]
    ;; Chez native complex (has non-zero imaginary part)
    [(and (number? x) (not (real? x)))
     'Complex]
    ;; Fixnum (machine-word integer)
    [(fixnum? x) 'Fixnum]
    ;; Bignum (arbitrary-precision integer beyond fixnum range)
    [(and (integer? x) (exact? x) (not (fixnum? x)))
     'Bignum]
    ;; Exact integer (abstract - covers Fixnum and Bignum)
    [(and (integer? x) (exact? x))
     'Integer]
    ;; Exact rational (non-integer ratio)
    [(and (rational? x) (exact? x))
     'Rational]
    ;; Inexact real (floating-point)
    [(real? x) 'Real]
    ;; Fallback
    [else 'NotNumeric]))

;;; numeric? : Value → Boolean
;;; Unified numeric predicate covering native + custom types.
(define (numeric? x)
  (not (eq? (numeric-type x) 'NotNumeric)))

;;; tower-index : Symbol → Integer | #f
;;; Returns position in tower (0 = most specific).
(define (tower-index type)
  (let loop ([types numeric-tower-order] [i 0])
    (cond
      [(null? types) #f]
      [(eq? (car types) type) i]
      [else (loop (cdr types) (+ i 1))])))

;;; ============================================================================
;;; Type Predicates (Unified)
;;; ============================================================================

;;; These predicates work on both Chez native and lattice custom types.

;;; exact-integer? : Value → Boolean
;;; True for exact integers (Fixnum or Bignum).
(define (exact-integer? x)
  (and (integer? x) (exact? x)))

;;; exact-rational? : Value → Boolean
;;; True for exact rationals (integers are rational).
(define (exact-rational? x)
  (and (rational? x) (exact? x)))

;;; inexact-real? : Value → Boolean
;;; True for inexact (floating-point) reals.
(define (inexact-real? x)
  (and (real? x) (inexact? x)))

;;; complex-number? : Value → Boolean
;;; True for any complex number (native or custom).
(define (complex-number? x)
  (or (and (number? x) (not (real? x)))           ; Chez native complex
      (and (pair? x) (eq? (car x) 'complex))))    ; Lattice custom complex

;;; native-complex? : Value → Boolean
;;; True only for Chez native complex numbers.
(define (native-complex? x)
  (and (number? x) (not (real? x))))

;;; custom-complex? : Value → Boolean
;;; True only for lattice custom complex: (complex r i)
(define (custom-complex? x)
  (and (pair? x) (eq? (car x) 'complex)))

;;; ============================================================================
;;; Common Type Computation
;;; ============================================================================

;;; common-type : Symbol × Symbol → Symbol
;;; Returns the least upper bound in the numeric tower.
;;; Both types must be valid tower types.
(define (common-type t1 t2)
  (let ([i1 (tower-index t1)]
        [i2 (tower-index t2)])
    (cond
      [(not i1) (error 'common-type "invalid type" t1)]
      [(not i2) (error 'common-type "invalid type" t2)]
      [else (list-ref numeric-tower-order (max i1 i2))])))

;;; common-type-of : Value × Value → Symbol
;;; Returns common type for two values.
(define (common-type-of a b)
  (common-type (numeric-type a) (numeric-type b)))

;;; ============================================================================
;;; Type Promotion
;;; ============================================================================

;;; Promotion is widening: moving up the tower.
;;; Key asymmetry: Rational → Real loses exactness and is NOT reversible
;;; without explicit rationalize.

;;; promote : Value × Symbol → Value
;;; Promote a numeric value to the target type.
;;; Promotion is always safe (no information loss within exact types).
;;;
;;; WARNING: Promoting exact to inexact (Rational → Real) loses exactness.
;;; This is documented behavior matching standard Scheme semantics.
(define (promote x target)
  (let ([current (numeric-type x)])
    (cond
      ;; Already at or above target
      [(>= (or (tower-index current) 999)
           (or (tower-index target) 0))
       x]

      ;; Promote to Rational (make exact ratio)
      [(eq? target 'Rational)
       (if (integer? x)
           (/ x 1)  ; Integer → Rational: x/1
           x)]

      ;; Promote to Real (inexact)
      [(eq? target 'Real)
       (if (exact? x)
           (exact->inexact x)
           x)]

      ;; Promote to Complex
      [(eq? target 'Complex)
       (cond
         [(custom-complex? x) x]
         [(native-complex? x) x]
         [else (make-rectangular (if (exact? x) (exact->inexact x) x) 0)])]

      ;; Default: no change
      [else x])))

;;; promote-pair : Value × Value → (Values Value Value)
;;; Promote both values to their common type.
;;; Returns two values via values form.
(define (promote-pair a b)
  (let ([tc (common-type-of a b)])
    (values (promote a tc) (promote b tc))))

;;; promote-exact-pair : Value × Value → (Values Value Value)
;;; Promote to common type while preserving exactness if possible.
;;; Only promotes to Real if one operand is already inexact.
(define (promote-exact-pair a b)
  (let ([ta (numeric-type a)]
        [tb (numeric-type b)])
    (cond
      ;; Both exact integers or rationals: stay exact
      [(and (exact? a) (exact? b)
            (memq ta '(Fixnum Bignum Integer Rational))
            (memq tb '(Fixnum Bignum Integer Rational)))
       (let ([tc (common-type ta tb)])
         (values (promote a tc) (promote b tc)))]
      ;; Otherwise, standard promotion
      [else (promote-pair a b)])))

;;; ============================================================================
;;; Explicit Conversions
;;; ============================================================================

;;; These are explicit conversions for when you need specific types.
;;; Unlike automatic promotion, these are intentional operations.

;;; as-integer : Value → Integer | Error
;;; Convert to exact integer. Fails if not an integer.
(define (as-integer x)
  (cond
    [(exact-integer? x) x]
    [(and (integer? x) (inexact? x)) (inexact->exact x)]
    [else (error 'as-integer "not an integer" x)]))

;;; as-rational : Value → Rational
;;; Convert to exact rational. Integers become n/1.
(define (as-rational x)
  (cond
    [(exact-rational? x) x]
    [(exact-integer? x) (/ x 1)]
    [(inexact? x) (rationalize (inexact->exact x) 0)]
    [else (error 'as-rational "cannot convert to rational" x)]))

;;; as-real : Value → Real
;;; Convert to inexact real.
(define (as-real x)
  (if (real? x)
      (if (exact? x) (exact->inexact x) x)
      (error 'as-real "not a real number" x)))

;;; as-native-complex : Value → Chez Complex
;;; Convert any numeric to Chez native complex.
(define (as-native-complex x)
  (cond
    [(native-complex? x) x]
    [(custom-complex? x)
     ;; Convert (complex r i) to native
     (make-rectangular (cadr x) (caddr x))]
    [(real? x)
     (make-rectangular (if (exact? x) (exact->inexact x) x) 0)]
    [else (error 'as-native-complex "not a number" x)]))

;;; ============================================================================
;;; Exactness Utilities
;;; ============================================================================

;;; exact-if-possible : Value → Value
;;; Convert to exact representation if no precision is lost.
(define (exact-if-possible x)
  (cond
    [(exact? x) x]
    [(and (inexact? x) (integer? x))
     ;; Inexact integer (e.g., 5.0) → exact integer
     (inexact->exact x)]
    [(and (inexact? x) (= x (inexact->exact x)))
     ;; Inexact that round-trips exactly
     (inexact->exact x)]
    [else x]))

;;; exact-arithmetic? : Value × Value → Boolean
;;; Returns #t if arithmetic on these values will preserve exactness.
(define (exact-arithmetic? a b)
  (and (exact? a) (exact? b)))

;;; ============================================================================
;;; Documentation: Asymmetries
;;; ============================================================================

;;; DOCUMENTED ASYMMETRY 1: Rational → Real
;;;
;;; Converting from Rational to Real loses exactness:
;;;   (exact->inexact 1/3) → 0.3333333333333333 (cannot recover 1/3)
;;;
;;; Going back requires rationalize with a tolerance:
;;;   (rationalize 0.333333 1/1000000) → 333333/1000000 ≠ 1/3
;;;
;;; This is standard Scheme behavior. Users must be aware.

;;; DOCUMENTED ASYMMETRY 2: Native Complex ↔ Custom Complex
;;;
;;; Chez native: 1+2i (efficient, works with +, *, sin, etc.)
;;; Lattice custom: (complex 1 2) (serializable, content-addressable)
;;;
;;; Conversion functions provided:
;;;   as-native-complex : (complex r i) → Chez complex
;;;   (complex-bridge.ss will provide reverse)

;;; ============================================================================
;;; Type Class Integration Points
;;; ============================================================================

;;; These predicates help with type class instance selection.

;;; integral-type? : Symbol → Boolean
;;; True for types that satisfy Integral.
(define (integral-type? t)
  (memq t '(Fixnum Bignum Integer)))

;;; fractional-type? : Symbol → Boolean
;;; True for types that satisfy Fractional.
(define (fractional-type? t)
  (memq t '(Rational Real Complex)))

;;; floating-type? : Symbol → Boolean
;;; True for types that satisfy Floating.
(define (floating-type? t)
  (memq t '(Real Complex)))

;;; ============================================================================
;;; Summary
;;; ============================================================================

;;; Exports:
;;;   numeric-type, numeric?, tower-index
;;;   exact-integer?, exact-rational?, inexact-real?
;;;   complex-number?, native-complex?, custom-complex?
;;;   common-type, common-type-of
;;;   promote, promote-pair, promote-exact-pair
;;;   as-integer, as-rational, as-real, as-native-complex
;;;   exact-if-possible, exact-arithmetic?
;;;   integral-type?, fractional-type?, floating-type?
