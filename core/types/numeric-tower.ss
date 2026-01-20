(load "core/base/prelude.ss")

(doc 'module 'numeric-tower)
(doc 'description "Unified numeric type hierarchy leveraging Chez Scheme's native tower: Fixnum ⊂ Bignum ⊂ Integer ⊂ Rational ⊂ Real ⊂ Complex. Provides unified predicates, type promotion logic with documented asymmetries, and conversion functions between tower levels.")
(doc 'layer 'core)
(doc 'note "Key Design Decisions: 1) Leverage Chez's native tower (no custom BigInt), 2) Bridge to lattice's custom Complex for CAS serialization, 3) Document rational↔flonum asymmetry explicitly")

(doc 'section 'type-classification)
(doc 'note "The numeric tower, from most specific to most general: Fixnum (machine-word integers, fast, bounded), Bignum (arbitrary-precision integers), Integer (abstract integer, Fixnum ∪ Bignum), Rational (exact ratios p/q where p,q are integers), Real (inexact floating-point IEEE 754), Complex (a + bi, native or custom). Chez distinguishes exact vs inexact at each level; we treat exactness as orthogonal to tower position.")

(doc numeric-tower-order 'type 'List)
(doc numeric-tower-order 'export #t)
(define numeric-tower-order
  '(Fixnum Bignum Integer Rational Real Complex))

(define (numeric-type x)
  (doc 'type '(-> Value Symbol))
  (doc 'description "Returns the most specific numeric type for a value. Returns 'NotNumeric for non-numeric values. Handles both Chez native numbers and lattice custom Complex.")
  (doc 'export #t)
  (cond
    [(and (pair? x) (eq? (car x) 'complex))
     'Complex]
    [(and (number? x) (not (real? x)))
     'Complex]
    [(fixnum? x) 'Fixnum]
    [(and (integer? x) (exact? x) (not (fixnum? x)))
     'Bignum]
    [(and (integer? x) (exact? x))
     'Integer]
    [(and (rational? x) (exact? x))
     'Rational]
    [(real? x) 'Real]
    [else 'NotNumeric]))

(define (numeric? x)
  (doc 'type '(-> Value Boolean))
  (doc 'description "Unified numeric predicate covering native + custom types.")
  (doc 'export #t)
  (not (eq? (numeric-type x) 'NotNumeric)))

(define (tower-index type)
  (doc 'type '(-> Symbol (Maybe Integer)))
  (doc 'description "Returns position in tower (0 = most specific).")
  (doc 'export #t)
  (let loop ([types numeric-tower-order] [i 0])
    (cond
      [(null? types) #f]
      [(eq? (car types) type) i]
      [else (loop (cdr types) (+ i 1))])))

(doc 'section 'type-predicates-unified)
(doc 'note "These predicates work on both Chez native and lattice custom types")

(define (exact-integer? x)
  (doc 'type '(-> Value Boolean))
  (doc 'description "True for exact integers (Fixnum or Bignum).")
  (doc 'export #t)
  (and (integer? x) (exact? x)))

(define (exact-rational? x)
  (doc 'type '(-> Value Boolean))
  (doc 'description "True for exact rationals (integers are rational).")
  (doc 'export #t)
  (and (rational? x) (exact? x)))

(define (inexact-real? x)
  (doc 'type '(-> Value Boolean))
  (doc 'description "True for inexact (floating-point) reals.")
  (doc 'export #t)
  (and (real? x) (inexact? x)))

(define (complex-number? x)
  (doc 'type '(-> Value Boolean))
  (doc 'description "True for any complex number (native or custom).")
  (doc 'export #t)
  (or (and (number? x) (not (real? x)))
      (and (pair? x) (eq? (car x) 'complex))))

(define (native-complex? x)
  (doc 'type '(-> Value Boolean))
  (doc 'description "True only for Chez native complex numbers.")
  (doc 'export #t)
  (and (number? x) (not (real? x))))

(define (custom-complex? x)
  (doc 'type '(-> Value Boolean))
  (doc 'description "True only for lattice custom complex: (complex r i)")
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'complex)))

(doc 'section 'common-type-computation)

(define (common-type t1 t2)
  (doc 'type '(-> Symbol Symbol Symbol))
  (doc 'description "Returns the least upper bound in the numeric tower. Both types must be valid tower types.")
  (doc 'export #t)
  (let ([i1 (tower-index t1)]
        [i2 (tower-index t2)])
    (cond
      [(not i1) (error 'common-type "invalid type" t1)]
      [(not i2) (error 'common-type "invalid type" t2)]
      [else (list-ref numeric-tower-order (max i1 i2))])))

(define (common-type-of a b)
  (doc 'type '(-> Value Value Symbol))
  (doc 'description "Returns common type for two values.")
  (doc 'export #t)
  (common-type (numeric-type a) (numeric-type b)))

(doc 'section 'type-promotion)
(doc 'note "Promotion is widening: moving up the tower. Key asymmetry: Rational → Real loses exactness and is NOT reversible without explicit rationalize.")

(define (promote x target)
  (doc 'type '(-> Value Symbol Value))
  (doc 'description "Promote a numeric value to the target type. Promotion is always safe (no information loss within exact types).")
  (doc 'warning "Promoting exact to inexact (Rational → Real) loses exactness. This is documented behavior matching standard Scheme semantics.")
  (doc 'export #t)
  (let ([current (numeric-type x)])
    (cond
      [(>= (or (tower-index current) 999)
           (or (tower-index target) 0))
       x]
      [(eq? target 'Rational)
       (if (integer? x)
           (/ x 1)
           x)]
      [(eq? target 'Real)
       (if (exact? x)
           (exact->inexact x)
           x)]
      [(eq? target 'Complex)
       (cond
         [(custom-complex? x) x]
         [(native-complex? x) x]
         [else (make-rectangular (if (exact? x) (exact->inexact x) x) 0)])]
      [else x])))

(define (promote-pair a b)
  (doc 'type '(-> Value Value (Values Value Value)))
  (doc 'description "Promote both values to their common type. Returns two values via values form.")
  (doc 'export #t)
  (let ([tc (common-type-of a b)])
    (values (promote a tc) (promote b tc))))

(define (promote-exact-pair a b)
  (doc 'type '(-> Value Value (Values Value Value)))
  (doc 'description "Promote to common type while preserving exactness if possible. Only promotes to Real if one operand is already inexact.")
  (doc 'export #t)
  (let ([ta (numeric-type a)]
        [tb (numeric-type b)])
    (cond
      [(and (exact? a) (exact? b)
            (memq ta '(Fixnum Bignum Integer Rational))
            (memq tb '(Fixnum Bignum Integer Rational)))
       (let ([tc (common-type ta tb)])
         (values (promote a tc) (promote b tc)))]
      [else (promote-pair a b)])))

(doc 'section 'explicit-conversions)
(doc 'note "These are explicit conversions for when you need specific types. Unlike automatic promotion, these are intentional operations.")

(define (as-integer x)
  (doc 'type '(-> Value (Maybe Integer)))
  (doc 'description "Convert to exact integer. Fails if not an integer.")
  (doc 'export #t)
  (cond
    [(exact-integer? x) x]
    [(and (integer? x) (inexact? x)) (inexact->exact x)]
    [else (error 'as-integer "not an integer" x)]))

(define (as-rational x)
  (doc 'type '(-> Value Rational))
  (doc 'description "Convert to exact rational. Integers become n/1.")
  (doc 'export #t)
  (cond
    [(exact-rational? x) x]
    [(exact-integer? x) (/ x 1)]
    [(inexact? x) (rationalize (inexact->exact x) 0)]
    [else (error 'as-rational "cannot convert to rational" x)]))

(define (as-real x)
  (doc 'type '(-> Value Real))
  (doc 'description "Convert to inexact real.")
  (doc 'export #t)
  (if (real? x)
      (if (exact? x) (exact->inexact x) x)
      (error 'as-real "not a real number" x)))

(define (as-native-complex x)
  (doc 'type '(-> Value Complex))
  (doc 'description "Convert any numeric to Chez native complex.")
  (doc 'export #t)
  (cond
    [(native-complex? x) x]
    [(custom-complex? x)
     (make-rectangular (cadr x) (caddr x))]
    [(real? x)
     (make-rectangular (if (exact? x) (exact->inexact x) x) 0)]
    [else (error 'as-native-complex "not a number" x)]))

(doc 'section 'exactness-utilities)

(define (exact-if-possible x)
  (doc 'type '(-> Value Value))
  (doc 'description "Convert to exact representation if no precision is lost.")
  (doc 'export #t)
  (cond
    [(exact? x) x]
    [(and (inexact? x) (integer? x))
     (inexact->exact x)]
    [(and (inexact? x) (= x (inexact->exact x)))
     (inexact->exact x)]
    [else x]))

(define (exact-arithmetic? a b)
  (doc 'type '(-> Value Value Boolean))
  (doc 'description "Returns #t if arithmetic on these values will preserve exactness.")
  (doc 'export #t)
  (and (exact? a) (exact? b)))

(doc 'section 'documented-asymmetries)
(doc 'note "ASYMMETRY 1: Rational → Real - Converting from Rational to Real loses exactness: (exact->inexact 1/3) → 0.3333333333333333 (cannot recover 1/3). Going back requires rationalize with a tolerance: (rationalize 0.333333 1/1000000) → 333333/1000000 ≠ 1/3. This is standard Scheme behavior. Users must be aware.")
(doc 'note "ASYMMETRY 2: Native Complex ↔ Custom Complex - Chez native: 1+2i (efficient, works with +, *, sin, etc.). Lattice custom: (complex 1 2) (serializable, content-addressable). Conversion functions provided: as-native-complex : (complex r i) → Chez complex (complex-bridge.ss will provide reverse).")

(doc 'section 'type-class-integration-points)
(doc 'note "These predicates help with type class instance selection")

(define (integral-type? t)
  (doc 'type '(-> Symbol Boolean))
  (doc 'description "True for types that satisfy Integral.")
  (doc 'export #t)
  (memq t '(Fixnum Bignum Integer)))

(define (fractional-type? t)
  (doc 'type '(-> Symbol Boolean))
  (doc 'description "True for types that satisfy Fractional.")
  (doc 'export #t)
  (memq t '(Rational Real Complex)))

(define (floating-type? t)
  (doc 'type '(-> Symbol Boolean))
  (doc 'description "True for types that satisfy Floating.")
  (doc 'export #t)
  (memq t '(Real Complex)))
