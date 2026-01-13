;;; lattice/algebra/poly-bridge.ss — Bridge Between Polynomial Representations
;;;
;;; Unifies the two polynomial implementations in the lattice:
;;;   - lattice/numeric/polynomial.ss: Descending order, vector-based, numeric
;;;   - lattice/algebra/polynomial.ss: Ascending order, list-based, Field-parametric
;;;
;;; This module provides:
;;;   1. Conversion functions between representations
;;;   2. Prefixed algebra operations (alg-*) for use without name collision
;;;   3. Documentation of conventions and when to use each
;;;
;;; Conventions:
;;;   - NUMERIC (descending): Control systems, signal processing, transfer functions
;;;     p(x) = a_n*x^n + ... + a_0 stored as (poly #(a_n ... a_0))
;;;   - ALGEBRA (ascending): Abstract algebra, GCD, factorization, symbolic math
;;;     p(x) = a_0 + a_1*x + ... + a_n*x^n stored as (polynomial F (a_0 a_1 ... a_n))
;;;
;;; IMPORTANT: This module only loads algebra/polynomial.ss. If you also need
;;; numeric/polynomial.ss, load it BEFORE this module to avoid name collisions.
;;; The alg-* prefixed functions are safe to use alongside numeric polynomials.
;;;
;;; This is Lattice code: pure, functional, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - lattice/algebra/field.ss
;;;   - lattice/algebra/polynomial.ss

(load "lattice/algebra/field.ss")
(load "lattice/algebra/polynomial.ss")

;;; Save references to algebra polynomial functions before any potential collision
(define alg-poly-coeffs poly-coeffs)
(define alg-poly-degree poly-degree)
(define alg-poly-field poly-field)
(define alg-poly-zero? poly-zero?)
(define alg-poly-leading-coeff poly-leading-coeff)
(define alg-poly-add poly-add)
(define alg-poly-sub poly-sub)
(define alg-poly-mul poly-mul)
(define alg-poly-scale poly-scale)
(define alg-poly-divmod poly-divmod)
(define alg-poly-div poly-div)
(define alg-poly-mod poly-mod)
(define alg-poly-gcd poly-gcd)
(define alg-poly-extended-gcd poly-extended-gcd)
(define alg-poly-derivative poly-derivative)
(define alg-poly-eval poly-eval)
(define alg-poly-monic poly-make-monic)
(define alg-poly->string poly->string)
(define alg-make-polynomial make-polynomial)

;;; Note: poly-from-roots is only in numeric/polynomial.ss
;;; We'll implement a simple version for algebra polynomials.

;;; ============================================================
;;; Standard Rational Field
;;; ============================================================

;;; Q-field : Field
;;; The standard field of rational numbers for exact arithmetic.
;;; Use this as the default field for most polynomial operations.
(define Q-field
  (make-field
   '()                                    ; Infinite field
   +                                      ; Addition
   *                                      ; Multiplication
   0                                      ; Zero
   1                                      ; One
   -                                      ; Negation
   (lambda (a b) (/ a b))                 ; Division
   =))                                    ; Equality

;;; ============================================================
;;; Conversion Functions
;;; ============================================================

;;; numeric->algebra : NumericPoly → AlgebraPoly
;;; Convert numeric polynomial (descending vector) to algebra polynomial (ascending list).
;;; Uses Q-field as the coefficient field.
;;; Accepts either tagged (poly #(...)) or raw #(...) vector.
(define (numeric->algebra p)
  (numeric->algebra-with-field p Q-field))

;;; numeric->algebra-with-field : NumericPoly × Field → AlgebraPoly
;;; Convert with explicit field specification.
(define (numeric->algebra-with-field p field)
  (let* ([coeffs-vec (cond
                       [(vector? p) p]                    ; Raw vector
                       [(and (pair? p) (eq? (car p) 'poly))
                        (cadr p)]                         ; Tagged (poly #(...))
                       [(and (pair? p) (vector? (cdr p)))
                        (cdr p)]                          ; (tag . #(...))
                       [else (error 'numeric->algebra "invalid polynomial format")])]
         [coeffs-list (vector->list coeffs-vec)]
         [ascending (reverse coeffs-list)])
    (alg-make-polynomial field ascending)))

;;; algebra->numeric : AlgebraPoly → NumericPoly
;;; Convert algebra polynomial (ascending list) to numeric polynomial (descending vector).
(define (algebra->numeric p)
  (let* ([coeffs-list (alg-poly-coeffs p)]
         [descending (reverse coeffs-list)]
         [coeffs-vec (list->vector descending)])
    (list 'poly coeffs-vec)))

;;; ============================================================
;;; Prefixed Algebra Operations (Avoid Name Collision)
;;; ============================================================

;;; These functions wrap algebra/polynomial.ss operations with alg- prefix,
;;; allowing them to be used alongside numeric/polynomial.ss without collision.
;;; They use the saved references from module load time.

;;; alg-make : Field × (List Coeff) → AlgebraPoly
(define alg-make alg-make-polynomial)

;;; alg-coeffs : AlgebraPoly → (List Coeff)
(define alg-coeffs alg-poly-coeffs)

;;; alg-degree : AlgebraPoly → Nat
(define alg-degree alg-poly-degree)

;;; alg-field : AlgebraPoly → Field
(define alg-field alg-poly-field)

;;; alg-zero? : AlgebraPoly → Boolean
(define alg-zero? alg-poly-zero?)

;;; alg-leading : AlgebraPoly → Coeff
(define alg-leading alg-poly-leading-coeff)

;;; alg-add : AlgebraPoly × AlgebraPoly → AlgebraPoly
(define alg-add alg-poly-add)

;;; alg-sub : AlgebraPoly × AlgebraPoly → AlgebraPoly
(define alg-sub alg-poly-sub)

;;; alg-mul : AlgebraPoly × AlgebraPoly → AlgebraPoly
(define alg-mul alg-poly-mul)

;;; alg-scale : AlgebraPoly × Coeff → AlgebraPoly
(define alg-scale alg-poly-scale)

;;; alg-divmod : AlgebraPoly × AlgebraPoly → (AlgebraPoly . AlgebraPoly)
(define alg-divmod alg-poly-divmod)

;;; alg-div : AlgebraPoly × AlgebraPoly → AlgebraPoly
(define alg-div alg-poly-div)

;;; alg-mod : AlgebraPoly × AlgebraPoly → AlgebraPoly
(define alg-mod alg-poly-mod)

;;; alg-gcd : AlgebraPoly × AlgebraPoly → AlgebraPoly
(define alg-gcd alg-poly-gcd)

;;; alg-extended-gcd : AlgebraPoly × AlgebraPoly → (AlgebraPoly × AlgebraPoly × AlgebraPoly)
(define alg-extended-gcd alg-poly-extended-gcd)

;;; alg-derivative : AlgebraPoly → AlgebraPoly
(define alg-derivative alg-poly-derivative)

;;; alg-eval : AlgebraPoly × Coeff → Coeff
(define alg-eval alg-poly-eval)

;;; alg-monic : AlgebraPoly → AlgebraPoly
(define alg-monic alg-poly-monic)

;;; ============================================================
;;; Bridge Operations (Work on Either Representation)
;;; ============================================================

;;; bridge-gcd : NumericPoly × NumericPoly → NumericPoly
;;; Compute GCD of two numeric polynomials using algebra operations.
(define (bridge-gcd p1 p2)
  (let* ([a1 (numeric->algebra p1)]
         [a2 (numeric->algebra p2)]
         [gcd-alg (alg-gcd a1 a2)])
    (algebra->numeric gcd-alg)))

;;; bridge-divmod : NumericPoly × NumericPoly → (NumericPoly . NumericPoly)
;;; Polynomial division with remainder for numeric polynomials.
(define (bridge-divmod p1 p2)
  (let* ([a1 (numeric->algebra p1)]
         [a2 (numeric->algebra p2)]
         [qr (alg-divmod a1 a2)])
    (cons (algebra->numeric (car qr))
          (algebra->numeric (cdr qr)))))

;;; bridge-simplify : NumericPoly × NumericPoly → (NumericPoly . NumericPoly)
;;; Simplify a rational function by canceling common factors.
;;; Returns (simplified-numerator . simplified-denominator).
(define (bridge-simplify num den)
  (let* ([num-alg (numeric->algebra num)]
         [den-alg (numeric->algebra den)]
         [gcd-alg (alg-gcd num-alg den-alg)])
    (if (<= (alg-degree gcd-alg) 0)
        (cons num den)  ; Already coprime
        (cons (algebra->numeric (alg-div num-alg gcd-alg))
              (algebra->numeric (alg-div den-alg gcd-alg))))))

;;; bridge-coprime? : NumericPoly × NumericPoly → Boolean
;;; Check if two numeric polynomials are coprime (GCD has degree 0).
(define (bridge-coprime? p1 p2)
  (let* ([a1 (numeric->algebra p1)]
         [a2 (numeric->algebra p2)]
         [gcd-alg (alg-gcd a1 a2)])
    (<= (alg-degree gcd-alg) 0)))

;;; ============================================================
;;; Polynomial String Conversion
;;; ============================================================

;;; alg->string : AlgebraPoly × Symbol → String
;;; Convert algebra polynomial to string.
(define (alg->string p var)
  (alg-poly->string p var))

;;; numeric->string : NumericPoly × Symbol → String
;;; Convert numeric polynomial to string.
(define (numeric->string p var)
  (alg->string (numeric->algebra p) var))

;;; ============================================================
;;; Polynomial From Roots
;;; ============================================================

;;; alg-poly-from-roots : Field × (List Coeff) → AlgebraPoly
;;; Create polynomial from roots: (x - r_1)(x - r_2)...(x - r_n)
;;; For real roots only (complex roots need numeric/polynomial.ss)
(define (alg-poly-from-roots field roots)
  (if (null? roots)
      (alg-make-polynomial field '(1))  ; Constant 1
      (let loop ([rs roots]
                 [acc (alg-make-polynomial field '(1))])
        (if (null? rs)
            acc
            ;; Multiply acc by (x - r) = (-r) + 1*x
            (let* ([r (car rs)]
                   [linear (alg-make-polynomial field (list (- r) 1))])
              (loop (cdr rs) (alg-poly-mul acc linear)))))))

;;; ============================================================
;;; Factory Functions (Create in Either Representation)
;;; ============================================================

;;; make-numeric-from-roots : (List Number) → NumericPoly
;;; Create numeric polynomial from its roots.
;;; p(x) = (x - r_1)(x - r_2)...(x - r_n)
(define (make-numeric-from-roots roots)
  (algebra->numeric (alg-poly-from-roots Q-field roots)))

;;; make-numeric-from-ascending : (List Number) → NumericPoly
;;; Create numeric polynomial from ascending-order coefficients.
;;; Convenience for when you have data in ascending order.
(define (make-numeric-from-ascending coeffs)
  (algebra->numeric (alg-make-polynomial Q-field coeffs)))

;;; make-algebra-from-descending : Field × (List Number) → AlgebraPoly
;;; Create algebra polynomial from descending-order coefficients.
;;; Convenience for when you have data in descending order.
(define (make-algebra-from-descending field coeffs)
  (alg-make-polynomial field (reverse coeffs)))

;;; ============================================================
;;; Compatibility Aliases
;;; ============================================================

;;; For modules that were using inline implementations, provide
;;; equivalent functions they can switch to.

;;; These match the sig-* functions in signal-poly.ss:
(define sig-make-polynomial alg-make)
(define sig-poly-field alg-field)
(define sig-poly-coeffs alg-coeffs)
(define sig-poly-degree alg-degree)
(define sig-poly-zero? alg-zero?)
(define sig-poly-leading alg-leading)
(define sig-poly-add alg-add)
(define sig-poly-sub alg-sub)
(define sig-poly-mul alg-mul)
(define sig-poly-scale alg-scale)
(define sig-poly-div alg-div)
(define sig-poly-mod alg-mod)
(define sig-poly-gcd alg-gcd)
(define sig-poly-divmod alg-divmod)
