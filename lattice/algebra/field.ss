(load "core/base/prelude.ss")
(load "lattice/algebra/ring.ss")

(doc 'module 'field)
(doc 'description "Field theory library")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Pure, functional implementation of field structures:")
(doc 'note "- Field representation and operations")
(doc 'note "- Field extends Ring with multiplicative inverses/division")
(doc 'note "- Standard fields: Q (rationals), R (reals via Scheme numbers)")

(doc 'note "A Field is a commutative ring where every non-zero element has a")
(doc 'note "multiplicative inverse. This enables exact division.")

(doc 'section 'field-representation)

(doc 'note "A Field is represented as:")
(doc 'note "(field elements add-op mul-op zero one neg-fn div-fn equal-fn)")
(doc 'note "- elements: list of field elements (may be empty for infinite fields)")
(doc 'note "- add-op: addition (a, b) → a + b")
(doc 'note "- mul-op: multiplication (a, b) → a × b")
(doc 'note "- zero: additive identity (0)")
(doc 'note "- one: multiplicative identity (1)")
(doc 'note "- neg-fn: additive inverse a → -a")
(doc 'note "- div-fn: division (a, b) → a / b (b ≠ 0)")
(doc 'note "- equal-fn: equality predicate for elements")
(doc 'note "")
(doc 'note "Field axioms (in addition to ring axioms):")
(doc 'note "1. Commutativity of multiplication: a × b = b × a")
(doc 'note "2. Multiplicative inverses: for a ≠ 0, ∃ a⁻¹ such that a × a⁻¹ = 1")
(doc 'note "3. Division: a / b = a × b⁻¹")
(define (make-field elements add-op mul-op zero one neg-fn div-fn equal-fn)
  (doc 'export #t)
  (list 'field elements add-op mul-op zero one neg-fn div-fn equal-fn))

;;; field? : α → Boolean
(define (field? x)
  (doc 'export #t)
  (and (list? x)
       (>= (length x) 9)
       (eq? (car x) 'field)))

;;; Field accessors
(define (field-elements f) (list-ref f 1))
(doc 'export #t)
(define (field-add-op f) (list-ref f 2))
(doc 'export #t)
(define (field-mul-op f) (list-ref f 3))
(doc 'export #t)
(define (field-zero f) (list-ref f 4))
(doc 'export #t)
(define (field-one f) (list-ref f 5))
(doc 'export #t)
(define (field-neg-fn f) (list-ref f 6))
(doc 'export #t)
(define (field-div-fn f) (list-ref f 7))
(doc 'export #t)
(define (field-equal-fn f) (list-ref f 8))
(doc 'export #t)

;;; ====
;;; Field Operations
;;; ====

;;; field-add : Field × Element × Element → Element
(define (field-add f a b)
  (doc 'export #t)
  ((field-add-op f) a b))

;;; field-mul : Field × Element × Element → Element
(define (field-mul f a b)
  (doc 'export #t)
  ((field-mul-op f) a b))

;;; field-neg : Field × Element → Element
(define (field-neg f a)
  (doc 'export #t)
  ((field-neg-fn f) a))

;;; field-sub : Field × Element × Element → Element
(define (field-sub f a b)
  (doc 'export #t)
  (field-add f a (field-neg f b)))

;;; field-div : Field × Element × Element → Element
;;; Division: a / b. Assumes b ≠ 0.
(define (field-div f a b)
  (doc 'export #t)
  ((field-div-fn f) a b))

;;; field-inv : Field × Element → Element
;;; Multiplicative inverse: 1 / a. Assumes a ≠ 0.
(define (field-inv f a)
  (doc 'export #t)
  (field-div f (field-one f) a))

;;; field-equal? : Field × Element × Element → Boolean
(define (field-equal? f a b)
  (doc 'export #t)
  ((field-equal-fn f) a b))

;;; field-power : Field × Element × Integer → Element
;;; Compute a^n. Supports negative exponents (a^{-n} = 1/a^n).
(define (field-power f a n)
  (doc 'export #t)
  (cond
    [(= n 0) (field-one f)]
    [(< n 0) (field-inv f (field-power f a (- n)))]
    [(= n 1) a]
    [(even? n)
     (let ([half (field-power f a (/ n 2))])
       (field-mul f half half))]
    [else
     (field-mul f a (field-power f a (- n 1)))]))

;;; ====
;;; Field → Ring Conversion
;;; ====

;;; field->ring : Field → Ring
;;; Extract the underlying ring structure from a field.
(define (field->ring f)
  (doc 'export #t)
  (make-ring
   (field-elements f)
   (field-add-op f)
   (field-mul-op f)
   (field-zero f)
   (field-one f)
   (field-neg-fn f)
   (field-equal-fn f)))

;;; ====
;;; Standard Fields
;;; ====

;;; Q-field : Field
;;; The field of rational numbers (using Scheme's exact rationals).
(doc Q-field 'export #t)
(define Q-field
  (make-field
   '()           ; Elements not enumerable (infinite)
   +             ; Addition
   *             ; Multiplication
   0             ; Zero
   1             ; One
   -             ; Negation
   /             ; Division
   =))           ; Equality

;;; R-field : Field
;;; The field of real numbers (using Scheme's inexact numbers).
;;; Note: This is approximate arithmetic, not exact reals.
(doc R-field 'export #t)
(define R-field
  (make-field
   '()           ; Elements not enumerable
   +             ; Addition
   *             ; Multiplication
   0.0           ; Zero
   1.0           ; One
   -             ; Negation
   /             ; Division
   (lambda (a b) (< (abs (- a b)) 1e-10))))  ; Approximate equality

;;; make-field-zp : Prime → Field
;;; Create the finite field Z_p of integers modulo prime p.
;;; WARNING: Does not verify p is prime; caller must ensure this.
(define (make-field-zp p)
  (doc 'export #t)
  (make-field
   (range 0 p)
   (lambda (a b) (modulo (+ a b) p))         ; Addition mod p
   (lambda (a b) (modulo (* a b) p))         ; Multiplication mod p
   0                                          ; Zero
   1                                          ; One
   (lambda (a) (modulo (- a) p))             ; Negation
   (lambda (a b) (modulo (* a (mod-inverse b p)) p))  ; Division
   =))

;;; mod-inverse : Integer × Integer → Integer
;;; Compute modular multiplicative inverse using extended Euclidean algorithm.
;;; Returns a⁻¹ such that a × a⁻¹ ≡ 1 (mod m).
(define (mod-inverse a m)
  (doc 'export #t)
  (let loop ([old-r m] [r (modulo a m)]
             [old-s 0] [s 1])
    (if (= r 0)
        (if (= old-r 1)
            (error 'mod-inverse "no inverse exists")
            (modulo old-s m))
        (let ([q (quotient old-r r)])
          (loop r (- old-r (* q r))
                s (- old-s (* q s)))))))

;;; ====
;;; Utility
;;; ====

;;; range : Nat × Nat → (List Nat)
(define (range start end)
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))
