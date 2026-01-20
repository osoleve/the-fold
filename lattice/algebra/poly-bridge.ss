(load "lattice/algebra/field.ss")
(load "lattice/algebra/polynomial.ss")

(doc 'module 'poly-bridge)
(doc 'description "Bridge between polynomial representations")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Unifies two polynomial implementations:")
(doc 'note "  - lattice/numeric/polynomial.ss: Descending order, vector-based, numeric")
(doc 'note "  - lattice/algebra/polynomial.ss: Ascending order, list-based, Field-parametric")

(doc 'note "Module provides:")
(doc 'note "  1. Conversion functions between representations")
(doc 'note "  2. Prefixed algebra operations (alg-*) for use without name collision")
(doc 'note "  3. Documentation of conventions and when to use each")

(doc 'note "Conventions:")
(doc 'note "  - NUMERIC (descending): Control systems, signal processing, transfer functions")
(doc 'note "    p(x) = a_n*x^n + ... + a_0 stored as (poly #(a_n ... a_0))")
(doc 'note "  - ALGEBRA (ascending): Abstract algebra, GCD, factorization, symbolic math")
(doc 'note "    p(x) = a_0 + a_1*x + ... + a_n*x^n stored as (polynomial F (a_0 a_1 ... a_n))")

(doc 'note "IMPORTANT: This module only loads algebra/polynomial.ss. If you also need")
(doc 'note "numeric/polynomial.ss, load it BEFORE this module to avoid name collisions.")
(doc 'note "The alg-* prefixed functions are safe to use alongside numeric polynomials.")

(doc 'note "Save references to algebra polynomial functions before any potential collision")
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

(doc 'note "poly-from-roots is only in numeric/polynomial.ss")
(doc 'note "We implement a simple version for algebra polynomials")

(doc 'section 'standard-rational-field)
(define Q-field
  (doc 'type 'Field)
  (doc 'description "The standard field of rational numbers for exact arithmetic")
  (doc 'description "Use this as the default field for most polynomial operations")
  (make-field
   '()                                    ; Infinite field
   +                                      ; Addition
   *                                      ; Multiplication
   0                                      ; Zero
   1                                      ; One
   -                                      ; Negation
   (lambda (a b) (/ a b))                 ; Division
   =))                                    ; Equality

(doc 'section 'conversion-functions)
(define (numeric->algebra p)
  (doc 'type '(-> NumericPoly AlgebraPoly))
  (doc 'description "Convert numeric polynomial (descending vector) to algebra polynomial (ascending list)")
  (doc 'description "Uses Q-field as the coefficient field")
  (doc 'description "Accepts either tagged (poly #(...)) or raw #(...) vector")
  (numeric->algebra-with-field p Q-field))
(define (numeric->algebra-with-field p field)
  (doc 'type '(-> NumericPoly Field AlgebraPoly))
  (doc 'description "Convert with explicit field specification")
  (let* ([coeffs-vec (cond
                       [(vector? p) p]                    ; Raw vector
                       [(and (pair? p) (eq? (car p) 'poly) (vector? (cadr p)))
                        (cadr p)]                         ; Tagged (poly #(...))
                       [(and (pair? p) (vector? (cdr p)))
                        (cdr p)]                          ; (tag . #(...))
                       [else (error 'numeric->algebra "invalid polynomial format")])]
         [coeffs-list (vector->list coeffs-vec)]
         [ascending (reverse coeffs-list)])
    (alg-make-polynomial field ascending)))
(define (algebra->numeric p)
  (doc 'type '(-> AlgebraPoly NumericPoly))
  (doc 'description "Convert algebra polynomial (ascending list) to numeric polynomial (descending vector)")
  (let* ([coeffs-list (alg-poly-coeffs p)]
         [descending (reverse coeffs-list)]
         [coeffs-vec (list->vector descending)])
    (list 'poly coeffs-vec)))

(doc 'section 'prefixed-algebra-operations)

(doc 'note "These functions wrap algebra/polynomial.ss operations with alg- prefix")
(doc 'note "allowing them to be used alongside numeric/polynomial.ss without collision")
(doc 'note "They use the saved references from module load time")
(define alg-make alg-make-polynomial)
(doc alg-make 'type '(-> Field (List Coeff) AlgebraPoly))

(define alg-coeffs alg-poly-coeffs)
(doc alg-coeffs 'type '(-> AlgebraPoly (List Coeff)))

(define alg-degree alg-poly-degree)
(doc alg-degree 'type '(-> AlgebraPoly Nat))

(define alg-field alg-poly-field)
(doc alg-field 'type '(-> AlgebraPoly Field))

(define alg-zero? alg-poly-zero?)
(doc alg-zero? 'type '(-> AlgebraPoly Boolean))

(define alg-leading alg-poly-leading-coeff)
(doc alg-leading 'type '(-> AlgebraPoly Coeff))

(define alg-add alg-poly-add)
(doc alg-add 'type '(-> AlgebraPoly AlgebraPoly AlgebraPoly))

(define alg-sub alg-poly-sub)
(doc alg-sub 'type '(-> AlgebraPoly AlgebraPoly AlgebraPoly))

(define alg-mul alg-poly-mul)
(doc alg-mul 'type '(-> AlgebraPoly AlgebraPoly AlgebraPoly))

(define alg-scale alg-poly-scale)
(doc alg-scale 'type '(-> AlgebraPoly Coeff AlgebraPoly))

(define alg-divmod alg-poly-divmod)
(doc alg-divmod 'type '(-> AlgebraPoly AlgebraPoly (Pair AlgebraPoly AlgebraPoly)))

(define alg-div alg-poly-div)
(doc alg-div 'type '(-> AlgebraPoly AlgebraPoly AlgebraPoly))

(define alg-mod alg-poly-mod)
(doc alg-mod 'type '(-> AlgebraPoly AlgebraPoly AlgebraPoly))

(define alg-gcd alg-poly-gcd)
(doc alg-gcd 'type '(-> AlgebraPoly AlgebraPoly AlgebraPoly))

(define alg-extended-gcd alg-poly-extended-gcd)
(doc alg-extended-gcd 'type '(-> AlgebraPoly AlgebraPoly (Triple AlgebraPoly AlgebraPoly AlgebraPoly)))

(define alg-derivative alg-poly-derivative)
(doc alg-derivative 'type '(-> AlgebraPoly AlgebraPoly))

(define alg-eval alg-poly-eval)
(doc alg-eval 'type '(-> AlgebraPoly Coeff Coeff))

(define alg-monic alg-poly-monic)
(doc alg-monic 'type '(-> AlgebraPoly AlgebraPoly))

(doc 'section 'bridge-operations)
(define (bridge-gcd p1 p2)
  (doc 'type '(-> NumericPoly NumericPoly NumericPoly))
  (doc 'description "Compute GCD of two numeric polynomials using algebra operations")
  (let* ([a1 (numeric->algebra p1)]
         [a2 (numeric->algebra p2)]
         [gcd-alg (alg-gcd a1 a2)])
    (algebra->numeric gcd-alg)))
(define (bridge-divmod p1 p2)
  (doc 'type '(-> NumericPoly NumericPoly (Pair NumericPoly NumericPoly)))
  (doc 'description "Polynomial division with remainder for numeric polynomials")
  (let* ([a1 (numeric->algebra p1)]
         [a2 (numeric->algebra p2)]
         [qr (alg-divmod a1 a2)])
    (cons (algebra->numeric (car qr))
          (algebra->numeric (cdr qr)))))
(define (bridge-simplify num den)
  (doc 'type '(-> NumericPoly NumericPoly (Pair NumericPoly NumericPoly)))
  (doc 'description "Simplify a rational function by canceling common factors")
  (doc 'returns "(simplified-numerator . simplified-denominator)")
  (let* ([num-alg (numeric->algebra num)]
         [den-alg (numeric->algebra den)]
         [gcd-alg (alg-gcd num-alg den-alg)])
    (if (<= (alg-degree gcd-alg) 0)
        (cons num den)  ; Already coprime
        (cons (algebra->numeric (alg-div num-alg gcd-alg))
              (algebra->numeric (alg-div den-alg gcd-alg))))))
(define (bridge-coprime? p1 p2)
  (doc 'type '(-> NumericPoly NumericPoly Boolean))
  (doc 'description "Check if two numeric polynomials are coprime (GCD has degree 0)")
  (let* ([a1 (numeric->algebra p1)]
         [a2 (numeric->algebra p2)]
         [gcd-alg (alg-gcd a1 a2)])
    (<= (alg-degree gcd-alg) 0)))

(doc 'section 'polynomial-string-conversion)
(define (alg->string p var)
  (doc 'type '(-> AlgebraPoly Symbol String))
  (doc 'description "Convert algebra polynomial to string")
  (alg-poly->string p var))
(define (numeric->string p var)
  (doc 'type '(-> NumericPoly Symbol String))
  (doc 'description "Convert numeric polynomial to string")
  (alg->string (numeric->algebra p) var))

(doc 'section 'polynomial-from-roots)
(define (alg-poly-from-roots field roots)
  (doc 'type '(-> Field (List Coeff) AlgebraPoly))
  (doc 'description "Create polynomial from roots: (x - r_1)(x - r_2)...(x - r_n)")
  (doc 'note "For real roots only (complex roots need numeric/polynomial.ss)")
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

(doc 'section 'factory-functions)
(define (make-numeric-from-roots roots)
  (doc 'type '(-> (List Number) NumericPoly))
  (doc 'description "Create numeric polynomial from its roots")
  (doc 'description "p(x) = (x - r_1)(x - r_2)...(x - r_n)")
  (doc 'note "Does not handle complex conjugate pairs. For complex roots that produce")
  (doc 'note "real coefficients, use numeric/polynomial.ss:poly-from-roots instead")
  (algebra->numeric (alg-poly-from-roots Q-field roots)))
(define (make-numeric-from-ascending coeffs)
  (doc 'type '(-> (List Number) NumericPoly))
  (doc 'description "Create numeric polynomial from ascending-order coefficients")
  (doc 'description "Convenience for when you have data in ascending order")
  (algebra->numeric (alg-make-polynomial Q-field coeffs)))
(define (make-algebra-from-descending field coeffs)
  (doc 'type '(-> Field (List Number) AlgebraPoly))
  (doc 'description "Create algebra polynomial from descending-order coefficients")
  (doc 'description "Convenience for when you have data in descending order")
  (alg-make-polynomial field (reverse coeffs)))

(doc 'section 'compatibility-aliases)

(doc 'note "For modules that were using inline implementations, provide")
(doc 'note "equivalent functions they can switch to")
(doc 'note "These match the sig-* functions in signal-poly.ss")
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
