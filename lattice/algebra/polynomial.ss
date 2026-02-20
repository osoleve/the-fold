;;; @module algebra/polynomial
;;; @requires prelude field

(require 'prelude)
(require 'field)

(doc 'module 'polynomial)
(doc 'description "Polynomial rings over fields")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Pure, functional implementation of polynomial algebra:")
(doc 'note "- Polynomial ring F[x] over any coefficient field F")
(doc 'note "- Division with remainder")
(doc 'note "- GCD via Extended Euclidean algorithm")
(doc 'note "- Factorization (square-free decomposition)")
(doc 'note "- Interpolation (Lagrange, Newton)")

(doc 'note "Polynomials are parametric over a Field structure, which provides")
(doc 'note "all ring operations plus division. This enables the library to work")
(doc 'note "with any field: Q (rationals), R (reals), finite fields Z_p, etc.")

(doc 'section 'polynomial-representation)

(doc 'note "Polynomials over a coefficient field F are represented as:")
(doc 'note "  (polynomial F coeffs)")
(doc 'note "where coeffs is a list of coefficients in ASCENDING power order:")
(doc 'note "  [a_0, a_1, ..., a_n] represents a_0 + a_1*x + ... + a_n*x^n")
(doc 'note "")
(doc 'note "This differs from lattice/numeric/polynomial.ss which uses")
(doc 'note "descending order (for numeric/control theory applications).")
(doc 'note "Ascending order is more natural for algebraic operations.")
(define (make-polynomial field coeffs)
  (doc 'export #t)
  (doc 'type '(-> Field (List Coeff) Polynomial))
  (doc 'description "Create a polynomial over field F with given coefficients")
  (doc 'description "Automatically normalizes (strips trailing zeros)")
  (list 'polynomial field (poly-normalize-coeffs field coeffs)))
(define (polynomial? p)
  (doc 'export #t)
  (and (pair? p)
       (eq? (car p) 'polynomial)))

;;; poly-field : Polynomial → Field
;;; Get the coefficient field.
(define (poly-field p)
  (doc 'export #t)
  (cadr p))

;;; poly-coeffs : Polynomial → (List Coeff)
;;; Get coefficient list in ascending power order.
(define (poly-coeffs p)
  (doc 'export #t)
  (caddr p))

;;; poly-degree : Polynomial → Nat
;;; Degree of polynomial. Zero polynomial has degree -1 by convention.
(define (poly-degree p)
  (doc 'export #t)
  (let ([coeffs (poly-coeffs p)])
    (- (length coeffs) 1)))

;;; poly-leading-coeff : Polynomial → Coeff
;;; Get leading (highest-degree) coefficient.
(define (poly-leading-coeff p)
  (doc 'export #t)
  (let ([coeffs (poly-coeffs p)])
    (if (null? coeffs)
        (field-zero (poly-field p))
        (list-ref coeffs (- (length coeffs) 1)))))

;;; poly-coeff-at : Polynomial × Nat → Coeff
;;; Get coefficient of x^k.
(define (poly-coeff-at p k)
  (doc 'export #t)
  (let ([coeffs (poly-coeffs p)]
        [F (poly-field p)])
    (if (>= k (length coeffs))
        (field-zero F)
        (list-ref coeffs k))))

;;; poly-normalize-coeffs : Field × (List Coeff) → (List Coeff)
;;; Remove trailing zero coefficients, keeping at least empty list or [0].
(define (poly-normalize-coeffs field coeffs)
  (let ([zero (field-zero field)]
        [eq-fn (field-equal-fn field)])
    (let loop ([cs (reverse coeffs)])
      (cond
        [(null? cs) (list zero)]
        [(eq-fn (car cs) zero) (loop (cdr cs))]
        [else (reverse cs)]))))

;;; poly-zero? : Polynomial → Boolean
;;; Check if polynomial is zero.
(define (poly-zero? p)
  (doc 'export #t)
  (let ([F (poly-field p)]
        [coeffs (poly-coeffs p)])
    (and (= (length coeffs) 1)
         ((field-equal-fn F) (car coeffs) (field-zero F)))))

;;; ====
;;; Polynomial Construction
;;; ====

;;; poly-zero-over : Field → Polynomial
;;; The zero polynomial over F.
(define (poly-zero-over field)
  (doc 'export #t)
  (make-polynomial field (list (field-zero field))))

;;; poly-one-over : Field → Polynomial
;;; The constant polynomial 1 over F.
(define (poly-one-over field)
  (doc 'export #t)
  (make-polynomial field (list (field-one field))))

;;; poly-constant : Field × Coeff → Polynomial
;;; Create constant polynomial.
(define (poly-constant field c)
  (doc 'export #t)
  (make-polynomial field (list c)))

;;; poly-monomial : Field × Coeff × Nat → Polynomial
;;; Create monomial c*x^n.
(define (poly-monomial field coeff degree)
  (doc 'export #t)
  (let ([zero (field-zero field)])
    (if ((field-equal-fn field) coeff zero)
        (poly-zero-over field)
        (make-polynomial field
          (append (make-list degree zero) (list coeff))))))

;;; poly-x : Field → Polynomial
;;; The polynomial x (the indeterminate).
(define (poly-x field)
  (doc 'export #t)
  (poly-monomial field (field-one field) 1))

;;; ====
;;; Polynomial Arithmetic
;;; ====

;;; poly-add : Polynomial × Polynomial → Polynomial
;;; Add two polynomials over the same field.
(doc poly-add 'fuel-cost '(linear n))
(define (poly-add p1 p2)
  (doc 'export #t)
  (let* ([F (poly-field p1)]
         [c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)]
         [add (field-add-op F)])
    (make-polynomial F (poly-add-coeffs add c1 c2 (field-zero F)))))

;;; poly-add-coeffs : (α×α→α) × (List α) × (List α) × α → (List α)
(define (poly-add-coeffs add c1 c2 zero)
  (cond
    [(null? c1) c2]
    [(null? c2) c1]
    [else (cons (add (car c1) (car c2))
                (poly-add-coeffs add (cdr c1) (cdr c2) zero))]))

;;; poly-neg : Polynomial → Polynomial
;;; Negate a polynomial.
(define (poly-neg p)
  (doc 'export #t)
  (let* ([F (poly-field p)]
         [neg (field-neg-fn F)])
    (make-polynomial F (map neg (poly-coeffs p)))))

;;; poly-sub : Polynomial × Polynomial → Polynomial
;;; Subtract polynomials.
(define (poly-sub p1 p2)
  (doc 'export #t)
  (poly-add p1 (poly-neg p2)))

;;; poly-scale : Polynomial × Coeff → Polynomial
;;; Multiply polynomial by scalar.
(define (poly-scale p c)
  (doc 'export #t)
  (let* ([F (poly-field p)]
         [mul (field-mul-op F)])
    (make-polynomial F (map (lambda (a) (mul c a)) (poly-coeffs p)))))

;;; poly-mul : Polynomial × Polynomial → Polynomial
;;; Multiply two polynomials (convolution).
(doc poly-mul 'fuel-cost '(quadratic n))
(define (poly-mul p1 p2)
  (doc 'export #t)
  (let* ([F (poly-field p1)]
         [c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)]
         [zero (field-zero F)]
         [add (field-add-op F)]
         [mul (field-mul-op F)]
         [n1 (length c1)]
         [n2 (length c2)]
         [n (+ n1 n2 -1)])
    (if (or (poly-zero? p1) (poly-zero? p2))
        (poly-zero-over F)
        (make-polynomial F
          (let loop ([k 0] [result '()])
            (if (= k n)
                (reverse result)
                (loop (+ k 1)
                      (cons (poly-mul-coeff-at add mul c1 c2 k n1 n2 zero)
                            result))))))))

;;; poly-mul-coeff-at : compute k-th coefficient of product
(define (poly-mul-coeff-at add mul c1 c2 k n1 n2 zero)
  (let loop ([i 0] [sum zero])
    (if (> i k)
        sum
        (let ([j (- k i)])
          (if (and (< i n1) (< j n2))
              (loop (+ i 1) (add sum (mul (list-ref c1 i) (list-ref c2 j))))
              (loop (+ i 1) sum))))))

;;; poly-power : Polynomial × Nat → Polynomial
;;; Raise polynomial to power n using repeated squaring.
(define (poly-power p n)
  (doc 'export #t)
  (cond
    [(= n 0) (poly-one-over (poly-field p))]
    [(= n 1) p]
    [(even? n)
     (let ([half (poly-power p (/ n 2))])
       (poly-mul half half))]
    [else
     (poly-mul p (poly-power p (- n 1)))]))

;;; ====
;;; Polynomial Equality
;;; ====

;;; poly-equal? : Polynomial × Polynomial → Boolean
;;; Check if two polynomials are equal.
(define (poly-equal? p1 p2)
  (doc 'export #t)
  (let* ([F (poly-field p1)]
         [eq-fn (field-equal-fn F)]
         [c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)])
    (and (= (length c1) (length c2))
         (let loop ([l1 c1] [l2 c2])
           (or (null? l1)
               (and (eq-fn (car l1) (car l2))
                    (loop (cdr l1) (cdr l2))))))))

;;; ====
;;; Polynomial Division
;;; ====

;;; poly-divmod : Polynomial × Polynomial → (Polynomial × Polynomial)
;;; Division with remainder: p1 = q * p2 + r where deg(r) < deg(p2).
;;; Returns (quotient . remainder).
(define (poly-divmod p1 p2)
  (doc 'export #t)
  (let* ([F (poly-field p1)]
         [d2 (poly-degree p2)])
    (if (poly-zero? p2)
        (error 'poly-divmod "division by zero polynomial")
        (poly-divmod-loop p1 p2 (poly-zero-over F) F))))

;;; poly-divmod-loop : Polynomial × Polynomial × Polynomial × Field
;;;                    → (Polynomial × Polynomial)
(define (poly-divmod-loop remainder divisor quotient F)
  (let ([d-rem (poly-degree remainder)]
        [d-div (poly-degree divisor)])
    (if (or (poly-zero? remainder) (< d-rem d-div))
        (cons quotient remainder)
        (let* ([lc-rem (poly-leading-coeff remainder)]
               [lc-div (poly-leading-coeff divisor)]
               ;; Use field division
               [coeff (field-div F lc-rem lc-div)]
               [deg-diff (- d-rem d-div)]
               [term (poly-monomial F coeff deg-diff)]
               [new-quotient (poly-add quotient term)]
               [subtrahend (poly-mul term divisor)]
               [new-remainder (poly-sub remainder subtrahend)])
          (poly-divmod-loop new-remainder divisor new-quotient F)))))

;;; poly-div : Polynomial × Polynomial → Polynomial
;;; Get quotient of division.
(define (poly-div p1 p2)
  (doc 'export #t)
  (car (poly-divmod p1 p2)))

;;; poly-mod : Polynomial × Polynomial → Polynomial
;;; Get remainder of division.
(define (poly-mod p1 p2)
  (doc 'export #t)
  (cdr (poly-divmod p1 p2)))

;;; poly-divides? : Polynomial × Polynomial → Boolean
;;; Check if p1 divides p2 (p2 = q * p1 for some q).
(define (poly-divides? p1 p2)
  (doc 'export #t)
  (poly-zero? (poly-mod p2 p1)))

;;; ====
;;; GCD and Extended Euclidean Algorithm
;;; ====

;;; poly-gcd : Polynomial × Polynomial → Polynomial
;;; Compute GCD using Euclidean algorithm.
;;; Returns monic GCD (leading coeff = 1).
(define (poly-gcd p1 p2)
  (doc 'export #t)
  (let ([F (poly-field p1)])
    (if (poly-zero? p2)
        (poly-make-monic p1)
        (poly-gcd p2 (poly-mod p1 p2)))))

;;; poly-make-monic : Polynomial → Polynomial
;;; Scale polynomial so leading coefficient is 1.
(define (poly-make-monic p)
  (doc 'export #t)
  (if (poly-zero? p)
      p
      (let* ([F (poly-field p)]
             [lc (poly-leading-coeff p)]
             [inv-lc (field-inv F lc)])
        (poly-scale p inv-lc))))

;;; poly-extended-gcd : Polynomial × Polynomial → (Polynomial × Polynomial × Polynomial)
;;; Extended Euclidean algorithm.
;;; Returns (gcd, s, t) such that gcd = s*p1 + t*p2.
(define (poly-extended-gcd p1 p2)
  (doc 'export #t)
  (let ([F (poly-field p1)])
    (poly-ext-gcd-loop p1 p2
                       (poly-one-over F) (poly-zero-over F)
                       (poly-zero-over F) (poly-one-over F)
                       F)))

;;; poly-ext-gcd-loop : iterative extended GCD
(define (poly-ext-gcd-loop r0 r1 s0 s1 t0 t1 F)
  (if (poly-zero? r1)
      ;; Make gcd monic and scale Bezout coefficients accordingly
      (if (poly-zero? r0)
          (list r0 s0 t0)
          (let* ([lc (poly-leading-coeff r0)]
                 [inv-lc (field-inv F lc)])
            (list (poly-scale r0 inv-lc)
                  (poly-scale s0 inv-lc)
                  (poly-scale t0 inv-lc))))
      (let* ([qr (poly-divmod r0 r1)]
             [q (car qr)]
             [r2 (cdr qr)]
             [s2 (poly-sub s0 (poly-mul q s1))]
             [t2 (poly-sub t0 (poly-mul q t1))])
        (poly-ext-gcd-loop r1 r2 s1 s2 t1 t2 F))))

;;; poly-lcm : Polynomial × Polynomial → Polynomial
;;; Least common multiple.
(define (poly-lcm p1 p2)
  (doc 'export #t)
  (if (or (poly-zero? p1) (poly-zero? p2))
      (poly-zero-over (poly-field p1))
      (poly-div (poly-mul p1 p2) (poly-gcd p1 p2))))

;;; ====
;;; Evaluation
;;; ====

;;; poly-eval : Polynomial × Coeff → Coeff
;;; Evaluate polynomial at point using Horner's method.
(doc poly-eval 'fuel-cost '(linear n))
(define (poly-eval p x)
  (doc 'export #t)
  (let* ([F (poly-field p)]
         [coeffs (reverse (poly-coeffs p))]  ; descending for Horner
         [add (field-add-op F)]
         [mul (field-mul-op F)])
    (if (null? coeffs)
        (field-zero F)
        (let loop ([cs (cdr coeffs)] [acc (car coeffs)])
          (if (null? cs)
              acc
              (loop (cdr cs) (add (mul acc x) (car cs))))))))

;;; ====
;;; Derivative
;;; ====

;;; poly-derivative : Polynomial → Polynomial
;;; Formal derivative: d/dx (sum a_k x^k) = sum k*a_k x^{k-1}
(define (poly-derivative p)
  (doc 'export #t)
  (let* ([F (poly-field p)]
         [coeffs (poly-coeffs p)]
         [add (field-add-op F)])
    (if (<= (length coeffs) 1)
        (poly-zero-over F)
        (make-polynomial F
          (let loop ([cs (cdr coeffs)] [k 1] [result '()])
            (if (null? cs)
                (reverse result)
                (loop (cdr cs) (+ k 1)
                      ;; k * a_k (multiply coefficient by power)
                      (cons (poly-scalar-mul-int F (car cs) k) result))))))))

;;; poly-scalar-mul-int : Field × Coeff × Int → Coeff
;;; Multiply coefficient by integer (repeated addition).
(define (poly-scalar-mul-int F c n)
  (let ([add (field-add-op F)]
        [zero (field-zero F)])
    (if (= n 0)
        zero
        (let loop ([i 1] [acc c])
          (if (= i n)
              acc
              (loop (+ i 1) (add acc c)))))))

;;; ====
;;; Factorization
;;; ====

;;; poly-square-free : Polynomial → Polynomial
;;; Square-free part: p / gcd(p, p')
;;; Removes repeated roots.
(define (poly-square-free p)
  (doc 'export #t)
  (let ([p-prime (poly-derivative p)])
    (if (poly-zero? p-prime)
        p  ; Constant polynomial
        (poly-div p (poly-gcd p p-prime)))))

;;; poly-square-free-factorization : Polynomial → (List (Polynomial × Nat))
;;; Yun's algorithm for square-free factorization.
;;; Returns list of (factor . multiplicity) pairs.
(define (poly-square-free-factorization p)
  (doc 'export #t)
  (let ([F (poly-field p)])
    (if (poly-zero? p)
        '()
        (let* ([p-prime (poly-derivative p)]
               [a0 (poly-gcd p p-prime)]
               [b0 (poly-div p a0)]
               [c0 (poly-div p-prime a0)])
          (poly-sqf-loop b0 (poly-sub c0 (poly-derivative b0)) 1 '() F)))))

;;; poly-sqf-loop : Yun's algorithm iteration
(define (poly-sqf-loop b c i result F)
  (if (poly-equal? b (poly-one-over F))
      (reverse result)
      (let* ([a (poly-gcd b c)]
             [b-next (poly-div b a)]
             [c-next (poly-sub (poly-div c a) (poly-derivative b-next))]
             [new-result (if (poly-equal? a (poly-one-over F))
                            result
                            (cons (cons a i) result))])
        (poly-sqf-loop b-next c-next (+ i 1) new-result F))))

;;; ====
;;; Interpolation
;;; ====

;;; poly-lagrange-interpolate : Field × (List (Coeff × Coeff)) → Polynomial
;;; Lagrange interpolation through points [(x_0, y_0), ..., (x_n, y_n)].
;;; Returns unique polynomial p of degree ≤ n with p(x_i) = y_i.
(define (poly-lagrange-interpolate F points)
  (doc 'export #t)
  (if (null? points)
      (poly-zero-over F)
      (let ([n (length points)]
            [xs (map car points)]
            [ys (map cdr points)])
        (poly-lagrange-sum F xs ys 0 (poly-zero-over F)))))

;;; poly-lagrange-sum : build interpolation polynomial
(define (poly-lagrange-sum F xs ys i acc)
  (if (= i (length xs))
      acc
      (let* ([xi (list-ref xs i)]
             [yi (list-ref ys i)]
             [Li (poly-lagrange-basis F xs i)]
             [term (poly-scale Li yi)])
        (poly-lagrange-sum F xs ys (+ i 1) (poly-add acc term)))))

;;; poly-lagrange-basis : Field × (List Coeff) × Nat → Polynomial
;;; Compute i-th Lagrange basis polynomial L_i(x) = prod_{j≠i} (x - x_j)/(x_i - x_j)
(define (poly-lagrange-basis F xs i)
  (let ([xi (list-ref xs i)]
        [n (length xs)]
        [sub (lambda (a b) (field-sub F a b))])
    (let loop ([j 0] [num (poly-one-over F)] [denom (field-one F)])
      (if (= j n)
          (poly-scale num (field-inv F denom))
          (if (= j i)
              (loop (+ j 1) num denom)
              (let* ([xj (list-ref xs j)]
                     ;; (x - x_j)
                     [factor (make-polynomial F (list (field-neg F xj) (field-one F)))]
                     ;; (x_i - x_j)
                     [d (sub xi xj)])
                (loop (+ j 1)
                      (poly-mul num factor)
                      (field-mul F denom d))))))))

;;; poly-newton-interpolate : Field × (List (Coeff × Coeff)) → Polynomial
;;; Newton interpolation using divided differences.
;;; Often more efficient for incremental point addition.
(define (poly-newton-interpolate F points)
  (doc 'export #t)
  (if (null? points)
      (poly-zero-over F)
      (let* ([xs (map car points)]
             [ys (map cdr points)]
             [n (length points)]
             [coeffs (poly-divided-differences F xs ys)])
        (poly-newton-form F xs coeffs))))

;;; poly-divided-differences : Field × (List Coeff) × (List Coeff) → (List Coeff)
;;; Compute Newton divided difference coefficients.
(define (poly-divided-differences F xs ys)
  (let* ([n (length xs)]
         [table (make-vector n)])
    ;; Initialize with y values
    (let init-loop ([i 0])
      (when (< i n)
        (vector-set! table i (list-ref ys i))
        (init-loop (+ i 1))))
    ;; Build divided difference table
    (let outer-loop ([j 1] [coeffs (list (vector-ref table 0))])
      (if (= j n)
          (reverse coeffs)
          (begin
            (let inner-loop ([i (- n 1)])
              (when (>= i j)
                (let ([xi (list-ref xs i)]
                      [xij (list-ref xs (- i j))]
                      [fi (vector-ref table i)]
                      [fim1 (vector-ref table (- i 1))])
                  ;; Use field operations for division
                  (vector-set! table i
                               (field-div F
                                          (field-sub F fi fim1)
                                          (field-sub F xi xij))))
                (inner-loop (- i 1))))
            (outer-loop (+ j 1) (cons (vector-ref table j) coeffs)))))))

;;; poly-newton-form : Field × (List Coeff) × (List Coeff) → Polynomial
;;; Build polynomial from Newton form using Horner's method:
;;; p(x) = c_0 + (x-x_0)*(c_1 + (x-x_1)*(c_2 + (x-x_2)*c_3 + ...))
;;; We work from inside out: start with c_{n-1}, work back to c_0.
(define (poly-newton-form F xs coeffs)
  (if (null? coeffs)
      (poly-zero-over F)
      (let* ([n (length coeffs)]
             [rev-coeffs (reverse coeffs)])  ; [c_{n-1}, ..., c_1, c_0]
        ;; Start with last coefficient
        (let loop ([cs (cdr rev-coeffs)]          ; [c_{n-2}, ..., c_0]
                   [acc (poly-constant F (car rev-coeffs))]  ; c_{n-1}
                   [k (- n 2)])                   ; index for x_k
          (if (null? cs)
              acc
              (let* ([xk (list-ref xs k)]
                     [ck (car cs)]
                     ;; acc = c_k + (x - x_k) * old_acc
                     [factor (make-polynomial F (list (field-neg F xk) (field-one F)))]
                     [new-acc (poly-add (poly-constant F ck)
                                        (poly-mul factor acc))])
                (loop (cdr cs) new-acc (- k 1))))))))

;;; ====
;;; Polynomial Ring as Ring
;;; ====

;;; make-polynomial-ring : Field → Ring
;;; Construct the polynomial ring F[x] over coefficient field F.
;;; Note: Elements are polynomials, operations are polynomial operations.
;;; Returns a Ring (not Field) since F[x] is not a field.
(define (make-polynomial-ring F)
  (doc 'export #t)
  (make-ring
   '()  ; Elements not enumerable (infinite)
   (lambda (p1 p2) (poly-add p1 p2))    ; Addition
   (lambda (p1 p2) (poly-mul p1 p2))    ; Multiplication
   (poly-zero-over F)                    ; Zero
   (poly-one-over F)                     ; One
   (lambda (p) (poly-neg p))            ; Negation
   (lambda (p1 p2) (poly-equal? p1 p2)) ; Equality
   ))

;;; ====
;;; Display
;;; ====

;;; poly->string : Polynomial × [Symbol] → String
;;; Pretty-print polynomial. Optional variable name (default 'x).
(define (poly->string p . opts)
  (doc 'export #t)
  (let* ([var (if (null? opts) 'x (car opts))]
         [F (poly-field p)]
         [coeffs (poly-coeffs p)]
         [n (length coeffs)])
    (if (and (= n 1) ((field-equal-fn F) (car coeffs) (field-zero F)))
        "0"
        (let loop ([i (- n 1)] [first? #t] [result ""])
          (if (< i 0)
              result
              (let* ([c (list-ref coeffs i)]
                     [term (poly-term->string c i var first? F)])
                (loop (- i 1)
                      (and first? (string=? term ""))
                      (string-append result term))))))))

;;; poly-term->string : Coeff × Nat × Symbol × Boolean × Field → String
(define (poly-term->string c power var first? F)
  (let ([zero (field-zero F)]
        [one (field-one F)]
        [eq-fn (field-equal-fn F)])
    (cond
      [(eq-fn c zero) ""]
      [(= power 0)
       (let ([cs (coeff->string c)])
         (if first?
             cs
             (if (and (number? c) (< c 0))
                 (string-append " - " (coeff->string (- c)))
                 (string-append " + " cs))))]
      [(= power 1)
       (let ([cs (if (eq-fn c one) "" (coeff->string c))])
         (if first?
             (string-append cs (symbol->string var))
             (string-append " + " cs (symbol->string var))))]
      [else
       (let ([cs (if (eq-fn c one) "" (coeff->string c))])
         (if first?
             (string-append cs (symbol->string var) "^" (number->string power))
             (string-append " + " cs (symbol->string var) "^" (number->string power))))])))

;;; coeff->string : Coeff → String
(define (coeff->string c)
  (cond
    [(number? c) (number->string c)]
    [(symbol? c) (symbol->string c)]
    [else (format "~a" c)]))

;; make-list is provided by prelude (alias for replicate)

;;; ====
;;; Backwards Compatibility: poly-ring alias
;;; ====

;;; poly-ring : Polynomial → Field
;;; Alias for poly-field for backwards compatibility.
(doc poly-ring 'export #t)
(define poly-ring poly-field)
