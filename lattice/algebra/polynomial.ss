;;; lattice/algebra/polynomial.ss — Polynomial Rings over Arbitrary Coefficients
;;;
;;; Pure, functional implementation of polynomial algebra:
;;; - Polynomial ring R[x] over any coefficient ring R
;;; - Division with remainder (for fields/Euclidean domains)
;;; - GCD via Extended Euclidean algorithm
;;; - Factorization (square-free decomposition)
;;; - Interpolation (Lagrange, Newton)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/algebra/ring.ss

(load "core/base/prelude.ss")
(load "lattice/algebra/ring.ss")

;;; ============================================================
;;; Polynomial Representation
;;; ============================================================

;;; Polynomials over a coefficient ring R are represented as:
;;;   (polynomial R coeffs)
;;; where coeffs is a list of coefficients in ASCENDING power order:
;;;   [a_0, a_1, ..., a_n] represents a_0 + a_1*x + ... + a_n*x^n
;;;
;;; This differs from lattice/numeric/polynomial.ss which uses
;;; descending order (for numeric/control theory applications).
;;; Ascending order is more natural for algebraic operations.

;;; make-polynomial : Ring × (List Coeff) → Polynomial
;;; Create a polynomial over ring R with given coefficients.
;;; Automatically normalizes (strips trailing zeros).
(define (make-polynomial ring coeffs)
  (list 'polynomial ring (poly-normalize-coeffs ring coeffs)))

;;; polynomial? : Any → Boolean
(define (polynomial? p)
  (and (pair? p)
       (eq? (car p) 'polynomial)))

;;; poly-ring : Polynomial → Ring
;;; Get the coefficient ring.
(define (poly-ring p)
  (cadr p))

;;; poly-coeffs : Polynomial → (List Coeff)
;;; Get coefficient list in ascending power order.
(define (poly-coeffs p)
  (caddr p))

;;; poly-degree : Polynomial → Nat
;;; Degree of polynomial. Zero polynomial has degree -1 by convention.
(define (poly-degree p)
  (let ([coeffs (poly-coeffs p)])
    (- (length coeffs) 1)))

;;; poly-leading-coeff : Polynomial → Coeff
;;; Get leading (highest-degree) coefficient.
(define (poly-leading-coeff p)
  (let ([coeffs (poly-coeffs p)])
    (if (null? coeffs)
        (ring-zero (poly-ring p))
        (list-ref coeffs (- (length coeffs) 1)))))

;;; poly-coeff-at : Polynomial × Nat → Coeff
;;; Get coefficient of x^k.
(define (poly-coeff-at p k)
  (let ([coeffs (poly-coeffs p)]
        [R (poly-ring p)])
    (if (>= k (length coeffs))
        (ring-zero R)
        (list-ref coeffs k))))

;;; poly-normalize-coeffs : Ring × (List Coeff) → (List Coeff)
;;; Remove trailing zero coefficients, keeping at least empty list or [0].
(define (poly-normalize-coeffs ring coeffs)
  (let ([zero (ring-zero ring)]
        [eq-fn (ring-equal-fn ring)])
    (let loop ([cs (reverse coeffs)])
      (cond
        [(null? cs) (list zero)]
        [(eq-fn (car cs) zero) (loop (cdr cs))]
        [else (reverse cs)]))))

;;; poly-zero? : Polynomial → Boolean
;;; Check if polynomial is zero.
(define (poly-zero? p)
  (let ([R (poly-ring p)]
        [coeffs (poly-coeffs p)])
    (and (= (length coeffs) 1)
         ((ring-equal-fn R) (car coeffs) (ring-zero R)))))

;;; ============================================================
;;; Polynomial Construction
;;; ============================================================

;;; poly-zero : Ring → Polynomial
;;; The zero polynomial over R.
(define (poly-zero-over ring)
  (make-polynomial ring (list (ring-zero ring))))

;;; poly-one : Ring → Polynomial
;;; The constant polynomial 1 over R.
(define (poly-one-over ring)
  (make-polynomial ring (list (ring-one ring))))

;;; poly-constant : Ring × Coeff → Polynomial
;;; Create constant polynomial.
(define (poly-constant ring c)
  (make-polynomial ring (list c)))

;;; poly-monomial : Ring × Coeff × Nat → Polynomial
;;; Create monomial c*x^n.
(define (poly-monomial ring coeff degree)
  (let ([zero (ring-zero ring)])
    (if ((ring-equal-fn ring) coeff zero)
        (poly-zero-over ring)
        (make-polynomial ring
          (append (make-list degree zero) (list coeff))))))

;;; poly-x : Ring → Polynomial
;;; The polynomial x (the indeterminate).
(define (poly-x ring)
  (poly-monomial ring (ring-one ring) 1))

;;; ============================================================
;;; Polynomial Arithmetic
;;; ============================================================

;;; poly-add : Polynomial × Polynomial → Polynomial
;;; Add two polynomials over the same ring.
(define (poly-add p1 p2)
  (let* ([R (poly-ring p1)]
         [c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)]
         [add (ring-add-op R)])
    (make-polynomial R (poly-add-coeffs add c1 c2 (ring-zero R)))))

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
  (let* ([R (poly-ring p)]
         [neg (ring-neg-fn R)])
    (make-polynomial R (map neg (poly-coeffs p)))))

;;; poly-sub : Polynomial × Polynomial → Polynomial
;;; Subtract polynomials.
(define (poly-sub p1 p2)
  (poly-add p1 (poly-neg p2)))

;;; poly-scale : Polynomial × Coeff → Polynomial
;;; Multiply polynomial by scalar.
(define (poly-scale p c)
  (let* ([R (poly-ring p)]
         [mul (ring-mul-op R)])
    (make-polynomial R (map (lambda (a) (mul c a)) (poly-coeffs p)))))

;;; poly-mul : Polynomial × Polynomial → Polynomial
;;; Multiply two polynomials (convolution).
(define (poly-mul p1 p2)
  (let* ([R (poly-ring p1)]
         [c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)]
         [zero (ring-zero R)]
         [add (ring-add-op R)]
         [mul (ring-mul-op R)]
         [n1 (length c1)]
         [n2 (length c2)]
         [n (+ n1 n2 -1)])
    (if (or (poly-zero? p1) (poly-zero? p2))
        (poly-zero-over R)
        (make-polynomial R
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
  (cond
    [(= n 0) (poly-one-over (poly-ring p))]
    [(= n 1) p]
    [(even? n)
     (let ([half (poly-power p (/ n 2))])
       (poly-mul half half))]
    [else
     (poly-mul p (poly-power p (- n 1)))]))

;;; ============================================================
;;; Polynomial Equality
;;; ============================================================

;;; poly-equal? : Polynomial × Polynomial → Boolean
;;; Check if two polynomials are equal.
(define (poly-equal? p1 p2)
  (let* ([R (poly-ring p1)]
         [eq-fn (ring-equal-fn R)]
         [c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)])
    (and (= (length c1) (length c2))
         (let loop ([l1 c1] [l2 c2])
           (or (null? l1)
               (and (eq-fn (car l1) (car l2))
                    (loop (cdr l1) (cdr l2))))))))

;;; ============================================================
;;; Polynomial Division (for fields/Euclidean domains)
;;; ============================================================

;;; poly-divmod : Polynomial × Polynomial → (Polynomial × Polynomial)
;;; Division with remainder: p1 = q * p2 + r where deg(r) < deg(p2).
;;; Requires invertible leading coefficient (field, or divisibility).
;;; Returns (quotient . remainder).
(define (poly-divmod p1 p2)
  (let* ([R (poly-ring p1)]
         [d2 (poly-degree p2)])
    (if (poly-zero? p2)
        (error 'poly-divmod "division by zero polynomial")
        (poly-divmod-loop p1 p2 (poly-zero-over R) R))))

;;; poly-divmod-loop : Polynomial × Polynomial × Polynomial × Ring
;;;                    → (Polynomial × Polynomial)
(define (poly-divmod-loop remainder divisor quotient R)
  (let ([d-rem (poly-degree remainder)]
        [d-div (poly-degree divisor)])
    (if (or (poly-zero? remainder) (< d-rem d-div))
        (cons quotient remainder)
        (let* ([lc-rem (poly-leading-coeff remainder)]
               [lc-div (poly-leading-coeff divisor)]
               ;; Compute lc-rem / lc-div (requires field or exact division)
               [coeff (ring-exact-div R lc-rem lc-div)]
               [deg-diff (- d-rem d-div)]
               [term (poly-monomial R coeff deg-diff)]
               [new-quotient (poly-add quotient term)]
               [subtrahend (poly-mul term divisor)]
               [new-remainder (poly-sub remainder subtrahend)])
          (poly-divmod-loop new-remainder divisor new-quotient R)))))

;;; ring-exact-div : Ring × Coeff × Coeff → Coeff
;;; Exact division in the coefficient ring.
;;; For integer ring, this requires divisibility.
;;; For fields, this is just a/b.
(define (ring-exact-div R a b)
  ;; For general rings, we assume exact division exists
  ;; In practice, caller ensures this works (e.g., over rationals/fields)
  (/ a b))

;;; poly-div : Polynomial × Polynomial → Polynomial
;;; Get quotient of division.
(define (poly-div p1 p2)
  (car (poly-divmod p1 p2)))

;;; poly-mod : Polynomial × Polynomial → Polynomial
;;; Get remainder of division.
(define (poly-mod p1 p2)
  (cdr (poly-divmod p1 p2)))

;;; poly-divides? : Polynomial × Polynomial → Boolean
;;; Check if p1 divides p2 (p2 = q * p1 for some q).
(define (poly-divides? p1 p2)
  (poly-zero? (poly-mod p2 p1)))

;;; ============================================================
;;; GCD and Extended Euclidean Algorithm
;;; ============================================================

;;; poly-gcd : Polynomial × Polynomial → Polynomial
;;; Compute GCD using Euclidean algorithm.
;;; Returns monic GCD (leading coeff = 1).
(define (poly-gcd p1 p2)
  (let ([R (poly-ring p1)])
    (if (poly-zero? p2)
        (poly-make-monic p1)
        (poly-gcd p2 (poly-mod p1 p2)))))

;;; poly-make-monic : Polynomial → Polynomial
;;; Scale polynomial so leading coefficient is 1.
(define (poly-make-monic p)
  (if (poly-zero? p)
      p
      (let ([lc (poly-leading-coeff p)])
        (poly-scale p (/ 1 lc)))))

;;; poly-extended-gcd : Polynomial × Polynomial → (Polynomial × Polynomial × Polynomial)
;;; Extended Euclidean algorithm.
;;; Returns (gcd, s, t) such that gcd = s*p1 + t*p2.
(define (poly-extended-gcd p1 p2)
  (let ([R (poly-ring p1)])
    (poly-ext-gcd-loop p1 p2
                       (poly-one-over R) (poly-zero-over R)
                       (poly-zero-over R) (poly-one-over R)
                       R)))

;;; poly-ext-gcd-loop : iterative extended GCD
(define (poly-ext-gcd-loop r0 r1 s0 s1 t0 t1 R)
  (if (poly-zero? r1)
      ;; Make gcd monic and scale Bezout coefficients accordingly
      (if (poly-zero? r0)
          (list r0 s0 t0)
          (let* ([lc (poly-leading-coeff r0)]
                 [scale (/ 1 lc)])
            (list (poly-scale r0 scale)
                  (poly-scale s0 scale)
                  (poly-scale t0 scale))))
      (let* ([qr (poly-divmod r0 r1)]
             [q (car qr)]
             [r2 (cdr qr)]
             [s2 (poly-sub s0 (poly-mul q s1))]
             [t2 (poly-sub t0 (poly-mul q t1))])
        (poly-ext-gcd-loop r1 r2 s1 s2 t1 t2 R))))

;;; poly-lcm : Polynomial × Polynomial → Polynomial
;;; Least common multiple.
(define (poly-lcm p1 p2)
  (if (or (poly-zero? p1) (poly-zero? p2))
      (poly-zero-over (poly-ring p1))
      (poly-div (poly-mul p1 p2) (poly-gcd p1 p2))))

;;; ============================================================
;;; Evaluation
;;; ============================================================

;;; poly-eval : Polynomial × Coeff → Coeff
;;; Evaluate polynomial at point using Horner's method.
(define (poly-eval p x)
  (let* ([R (poly-ring p)]
         [coeffs (reverse (poly-coeffs p))]  ; descending for Horner
         [add (ring-add-op R)]
         [mul (ring-mul-op R)])
    (if (null? coeffs)
        (ring-zero R)
        (let loop ([cs (cdr coeffs)] [acc (car coeffs)])
          (if (null? cs)
              acc
              (loop (cdr cs) (add (mul acc x) (car cs))))))))

;;; ============================================================
;;; Derivative
;;; ============================================================

;;; poly-derivative : Polynomial → Polynomial
;;; Formal derivative: d/dx (sum a_k x^k) = sum k*a_k x^{k-1}
(define (poly-derivative p)
  (let* ([R (poly-ring p)]
         [coeffs (poly-coeffs p)]
         [add (ring-add-op R)])
    (if (<= (length coeffs) 1)
        (poly-zero-over R)
        (make-polynomial R
          (let loop ([cs (cdr coeffs)] [k 1] [result '()])
            (if (null? cs)
                (reverse result)
                (loop (cdr cs) (+ k 1)
                      ;; k * a_k (multiply coefficient by power)
                      (cons (poly-scalar-mul-int R (car cs) k) result))))))))

;;; poly-scalar-mul-int : Ring × Coeff × Int → Coeff
;;; Multiply coefficient by integer (repeated addition).
(define (poly-scalar-mul-int R c n)
  (let ([add (ring-add-op R)]
        [zero (ring-zero R)])
    (if (= n 0)
        zero
        (let loop ([i 1] [acc c])
          (if (= i n)
              acc
              (loop (+ i 1) (add acc c)))))))

;;; ============================================================
;;; Factorization
;;; ============================================================

;;; poly-square-free : Polynomial → Polynomial
;;; Square-free part: p / gcd(p, p')
;;; Removes repeated roots.
(define (poly-square-free p)
  (let ([p-prime (poly-derivative p)])
    (if (poly-zero? p-prime)
        p  ; Constant polynomial
        (poly-div p (poly-gcd p p-prime)))))

;;; poly-square-free-factorization : Polynomial → (List (Polynomial × Nat))
;;; Yun's algorithm for square-free factorization.
;;; Returns list of (factor . multiplicity) pairs.
(define (poly-square-free-factorization p)
  (let ([R (poly-ring p)])
    (if (poly-zero? p)
        '()
        (let* ([p-prime (poly-derivative p)]
               [a0 (poly-gcd p p-prime)]
               [b0 (poly-div p a0)]
               [c0 (poly-div p-prime a0)])
          (poly-sqf-loop b0 (poly-sub c0 (poly-derivative b0)) 1 '() R)))))

;;; poly-sqf-loop : Yun's algorithm iteration
(define (poly-sqf-loop b c i result R)
  (if (poly-equal? b (poly-one-over R))
      (reverse result)
      (let* ([a (poly-gcd b c)]
             [b-next (poly-div b a)]
             [c-next (poly-sub (poly-div c a) (poly-derivative b-next))]
             [new-result (if (poly-equal? a (poly-one-over R))
                            result
                            (cons (cons a i) result))])
        (poly-sqf-loop b-next c-next (+ i 1) new-result R))))

;;; ============================================================
;;; Interpolation
;;; ============================================================

;;; poly-lagrange-interpolate : Ring × (List (Coeff × Coeff)) → Polynomial
;;; Lagrange interpolation through points [(x_0, y_0), ..., (x_n, y_n)].
;;; Returns unique polynomial p of degree ≤ n with p(x_i) = y_i.
(define (poly-lagrange-interpolate R points)
  (if (null? points)
      (poly-zero-over R)
      (let ([n (length points)]
            [xs (map car points)]
            [ys (map cdr points)])
        (poly-lagrange-sum R xs ys 0 (poly-zero-over R)))))

;;; poly-lagrange-sum : build interpolation polynomial
(define (poly-lagrange-sum R xs ys i acc)
  (if (= i (length xs))
      acc
      (let* ([xi (list-ref xs i)]
             [yi (list-ref ys i)]
             [Li (poly-lagrange-basis R xs i)]
             [term (poly-scale Li yi)])
        (poly-lagrange-sum R xs ys (+ i 1) (poly-add acc term)))))

;;; poly-lagrange-basis : Ring × (List Coeff) × Nat → Polynomial
;;; Compute i-th Lagrange basis polynomial L_i(x) = prod_{j≠i} (x - x_j)/(x_i - x_j)
(define (poly-lagrange-basis R xs i)
  (let ([xi (list-ref xs i)]
        [n (length xs)])
    (let loop ([j 0] [num (poly-one-over R)] [denom (ring-one R)])
      (if (= j n)
          (poly-scale num (/ 1 denom))
          (if (= j i)
              (loop (+ j 1) num denom)
              (let* ([xj (list-ref xs j)]
                     ;; (x - x_j)
                     [factor (make-polynomial R (list (- xj) (ring-one R)))]
                     ;; (x_i - x_j)
                     [d (- xi xj)])
                (loop (+ j 1)
                      (poly-mul num factor)
                      (* denom d))))))))

;;; poly-newton-interpolate : Ring × (List (Coeff × Coeff)) → Polynomial
;;; Newton interpolation using divided differences.
;;; Often more efficient for incremental point addition.
(define (poly-newton-interpolate R points)
  (if (null? points)
      (poly-zero-over R)
      (let* ([xs (map car points)]
             [ys (map cdr points)]
             [n (length points)]
             [coeffs (poly-divided-differences xs ys)])
        (poly-newton-form R xs coeffs))))

;;; poly-divided-differences : (List Coeff) × (List Coeff) → (List Coeff)
;;; Compute Newton divided difference coefficients.
(define (poly-divided-differences xs ys)
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
                  (vector-set! table i (/ (- fi fim1) (- xi xij))))
                (inner-loop (- i 1))))
            (outer-loop (+ j 1) (cons (vector-ref table j) coeffs)))))))

;;; poly-newton-form : Ring × (List Coeff) × (List Coeff) → Polynomial
;;; Build polynomial from Newton form: sum c_i * prod_{j<i} (x - x_j)
(define (poly-newton-form R xs coeffs)
  (if (null? coeffs)
      (poly-zero-over R)
      (let loop ([cs (reverse (cdr coeffs))]
                 [acc (poly-constant R (car coeffs))]
                 [k (- (length coeffs) 2)])
        (if (null? cs)
            acc
            (let* ([xk (list-ref xs k)]
                   [ck (car cs)]
                   ;; acc = c_k + (x - x_k) * old_acc
                   [factor (make-polynomial R (list (- xk) (ring-one R)))]
                   [new-acc (poly-add (poly-constant R ck)
                                      (poly-mul factor acc))])
              (loop (cdr cs) new-acc (- k 1)))))))

;;; ============================================================
;;; Polynomial Ring as Ring
;;; ============================================================

;;; make-polynomial-ring : Ring → Ring
;;; Construct the polynomial ring R[x] over coefficient ring R.
;;; Note: Elements are polynomials, operations are polynomial operations.
(define (make-polynomial-ring R)
  (make-ring
   '()  ; Elements not enumerable (infinite)
   (lambda (p1 p2) (poly-add p1 p2))    ; Addition
   (lambda (p1 p2) (poly-mul p1 p2))    ; Multiplication
   (poly-zero-over R)                    ; Zero
   (poly-one-over R)                     ; One
   (lambda (p) (poly-neg p))            ; Negation
   (lambda (p1 p2) (poly-equal? p1 p2)) ; Equality
   ))

;;; ============================================================
;;; Display
;;; ============================================================

;;; poly->string : Polynomial × [Symbol] → String
;;; Pretty-print polynomial. Optional variable name (default 'x).
(define (poly->string p . opts)
  (let* ([var (if (null? opts) 'x (car opts))]
         [R (poly-ring p)]
         [coeffs (poly-coeffs p)]
         [n (length coeffs)])
    (if (and (= n 1) ((ring-equal-fn R) (car coeffs) (ring-zero R)))
        "0"
        (let loop ([i (- n 1)] [first? #t] [result ""])
          (if (< i 0)
              result
              (let* ([c (list-ref coeffs i)]
                     [term (poly-term->string c i var first? R)])
                (loop (- i 1)
                      (and first? (string=? term ""))
                      (string-append result term))))))))

;;; poly-term->string : Coeff × Nat × Symbol × Boolean × Ring → String
(define (poly-term->string c power var first? R)
  (let ([zero (ring-zero R)]
        [one (ring-one R)]
        [eq-fn (ring-equal-fn R)])
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

;;; ============================================================
;;; Utility: make-list
;;; ============================================================

(define (make-list n fill)
  (if (<= n 0)
      '()
      (cons fill (make-list (- n 1) fill))))
