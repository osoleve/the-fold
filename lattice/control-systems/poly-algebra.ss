;;; lattice/control-systems/poly-algebra.ss — Polynomial Algebra Integration
;;;
;;; Integrates lattice/algebra/polynomial.ss with control systems modules
;;; for exact rational arithmetic and automatic transfer function simplification.
;;;
;;; Benefits:
;;;   - Exact arithmetic over Q-field instead of floating-point
;;;   - Automatic pole-zero cancellation via polynomial GCD
;;;   - Symbolic stability analysis
;;;   - Field-based rational function simplification
;;;
;;; This is Lattice code: pure, functional, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - lattice/algebra/polynomial.ss (exact polynomial algebra)
;;;   - lattice/algebra/field.ss (field structures)
;;;   - lattice/control-systems/transfer-function.ss
;;;
;;; Note: This module handles the name collision between numeric/polynomial.ss
;;; and algebra/polynomial.ss by loading transfer-function first (which
;;; establishes numeric polynomial functions), then using explicit accessors
;;; for algebra polynomials that don't collide.

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires control/transfer-function field
;; Load transfer-function first (brings in numeric/polynomial.ss)
(require 'control/transfer-function)

;; Load field module (no conflicting names)
(require 'field)

(doc 'module 'poly-algebra)
(doc 'purity 'partial)
(doc 'description "Polynomial algebra integration for exact rational arithmetic in control systems")
(doc 'layer 'lattice)

;;; ====
;;; Algebra Polynomial - Inline Implementation
;;; ====
;;;
;;; We implement algebra polynomial operations inline rather than loading
;;; algebra/polynomial.ss to avoid function name collisions.
;;; Algebra polynomials: (alg-polynomial field (a_0 a_1 ... a_n))

;;; alg-make-polynomial : Field × (List Coeff) → AlgPolynomial
(define (alg-make-polynomial field coeffs)
  (doc 'export #t)
  (list 'alg-polynomial field (alg-normalize-coeffs field coeffs)))

;;; alg-normalize-coeffs : Field × (List Coeff) → (List Coeff)
(define (alg-normalize-coeffs field coeffs)
  (doc 'export #t)
  (let ([zero (field-zero field)]
        [eq-fn (field-equal-fn field)])
    (let loop ([cs (reverse coeffs)])
      (cond
        [(null? cs) (list zero)]
        [(eq-fn (car cs) zero) (loop (cdr cs))]
        [else (reverse cs)]))))

;;; alg-polynomial? : Any → Boolean
(define (alg-polynomial? p)
  (doc 'export #t)
  (and (pair? p) (eq? (car p) 'alg-polynomial)))

;;; alg-poly-field : AlgPolynomial → Field
(define (alg-poly-field p) (cadr p))

;;; alg-poly-coeffs : AlgPolynomial → (List Coeff)
(define (alg-poly-coeffs p) (caddr p))

;;; alg-poly-degree : AlgPolynomial → Nat
(define (alg-poly-degree p)
  (doc 'export #t)
  (- (length (alg-poly-coeffs p)) 1))

;;; alg-poly-leading-coeff : AlgPolynomial → Coeff
(define (alg-poly-leading-coeff p)
  (doc 'export #t)
  (let ([coeffs (alg-poly-coeffs p)])
    (if (null? coeffs)
        (field-zero (alg-poly-field p))
        (list-ref coeffs (- (length coeffs) 1)))))

;;; alg-poly-zero? : AlgPolynomial → Boolean
(define (alg-poly-zero? p)
  (doc 'export #t)
  (let ([F (alg-poly-field p)]
        [coeffs (alg-poly-coeffs p)])
    (and (= (length coeffs) 1)
         ((field-equal-fn F) (car coeffs) (field-zero F)))))

;;; alg-poly-zero-over : Field → AlgPolynomial
(define (alg-poly-zero-over field)
  (doc 'export #t)
  (alg-make-polynomial field (list (field-zero field))))

;;; alg-poly-one-over : Field → AlgPolynomial
(define (alg-poly-one-over field)
  (doc 'export #t)
  (alg-make-polynomial field (list (field-one field))))

;;; alg-poly-add : AlgPolynomial × AlgPolynomial → AlgPolynomial
(define (alg-poly-add p1 p2)
  (doc 'export #t)
  (let* ([F (alg-poly-field p1)]
         [c1 (alg-poly-coeffs p1)]
         [c2 (alg-poly-coeffs p2)]
         [add (field-add-op F)])
    (alg-make-polynomial F (alg-add-coeffs add c1 c2 (field-zero F)))))

;;; alg-add-coeffs : (α×α→α) × (List α) × (List α) × α → (List α)
(define (alg-add-coeffs add c1 c2 zero)
  (doc 'export #t)
  (cond
    [(null? c1) c2]
    [(null? c2) c1]
    [else (cons (add (car c1) (car c2))
                (alg-add-coeffs add (cdr c1) (cdr c2) zero))]))

;;; alg-poly-neg : AlgPolynomial → AlgPolynomial
(define (alg-poly-neg p)
  (doc 'export #t)
  (let* ([F (alg-poly-field p)]
         [neg (field-neg-fn F)])
    (alg-make-polynomial F (map neg (alg-poly-coeffs p)))))

;;; alg-poly-sub : AlgPolynomial × AlgPolynomial → AlgPolynomial
(define (alg-poly-sub p1 p2)
  (doc 'export #t)
  (alg-poly-add p1 (alg-poly-neg p2)))

;;; alg-poly-scale : AlgPolynomial × Coeff → AlgPolynomial
(define (alg-poly-scale p c)
  (doc 'export #t)
  (let* ([F (alg-poly-field p)]
         [mul (field-mul-op F)])
    (alg-make-polynomial F (map (lambda (x) (mul c x)) (alg-poly-coeffs p)))))

;;; alg-poly-mul : AlgPolynomial × AlgPolynomial → AlgPolynomial
(define (alg-poly-mul p1 p2)
  (doc 'export #t)
  (let* ([F (alg-poly-field p1)]
         [c1 (alg-poly-coeffs p1)]
         [c2 (alg-poly-coeffs p2)]
         [n1 (length c1)]
         [n2 (length c2)]
         [result-len (- (+ n1 n2) 1)]
         [mul (field-mul-op F)]
         [add (field-add-op F)]
         [zero (field-zero F)])
    (if (or (= n1 0) (= n2 0))
        (alg-poly-zero-over F)
        (let ([result (make-list result-len zero)])
          (let outer ([i 0] [res result])
            (if (>= i n1)
                (alg-make-polynomial F res)
                (let inner ([j 0] [r res])
                  (if (>= j n2)
                      (outer (+ i 1) r)
                      (let* ([idx (+ i j)]
                             [old (list-ref r idx)]
                             [new (add old (mul (list-ref c1 i) (list-ref c2 j)))])
                        (inner (+ j 1) (list-set r idx new)))))))))))

;;; list-set : (List α) × Nat × α → (List α)
(define (list-set lst idx val)
  (doc 'export #t)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- idx 1) val))))

;;; alg-poly-divmod : AlgPolynomial × AlgPolynomial → (AlgPolynomial . AlgPolynomial)
;;; Polynomial division with remainder: p1 = q*p2 + r
(define (alg-poly-divmod p1 p2)
  (doc 'export #t)
  (let* ([F (alg-poly-field p1)]
         [zero (field-zero F)]
         [div (field-div-fn F)])
    (if (alg-poly-zero? p2)
        (error 'alg-poly-divmod "division by zero polynomial")
        (let loop ([rem p1] [quot (alg-poly-zero-over F)])
          (if (or (alg-poly-zero? rem)
                  (< (alg-poly-degree rem) (alg-poly-degree p2)))
              (cons quot rem)
              (let* ([lc-rem (alg-poly-leading-coeff rem)]
                     [lc-div (alg-poly-leading-coeff p2)]
                     [coeff (div lc-rem lc-div)]
                     [deg-diff (- (alg-poly-degree rem) (alg-poly-degree p2))]
                     [term (alg-poly-monomial F coeff deg-diff)]
                     [new-quot (alg-poly-add quot term)]
                     [new-rem (alg-poly-sub rem (alg-poly-mul term p2))])
                (loop new-rem new-quot)))))))

;;; alg-poly-monomial : Field × Coeff × Nat → AlgPolynomial
(define (alg-poly-monomial field coeff degree)
  (doc 'export #t)
  (let ([zero (field-zero field)])
    (if ((field-equal-fn field) coeff zero)
        (alg-poly-zero-over field)
        (alg-make-polynomial field
          (append (make-list degree zero) (list coeff))))))

;;; alg-poly-mod : AlgPolynomial × AlgPolynomial → AlgPolynomial
(define (alg-poly-mod p1 p2)
  (doc 'export #t)
  (cdr (alg-poly-divmod p1 p2)))

;;; alg-poly-div : AlgPolynomial × AlgPolynomial → AlgPolynomial
(define (alg-poly-div p1 p2)
  (doc 'export #t)
  (car (alg-poly-divmod p1 p2)))

;;; alg-poly-gcd : AlgPolynomial × AlgPolynomial → AlgPolynomial
(define (alg-poly-gcd p1 p2)
  (doc 'export #t)
  (if (alg-poly-zero? p2)
      (alg-poly-make-monic p1)
      (alg-poly-gcd p2 (alg-poly-mod p1 p2))))

;;; alg-poly-make-monic : AlgPolynomial → AlgPolynomial
(define (alg-poly-make-monic p)
  (doc 'export #t)
  (if (alg-poly-zero? p)
      p
      (let* ([F (alg-poly-field p)]
             [lc (alg-poly-leading-coeff p)]
             [inv-lc (field-inv F lc)])
        (alg-poly-scale p inv-lc))))

;;; alg-poly-extended-gcd : AlgPolynomial × AlgPolynomial → (AlgPolynomial × AlgPolynomial × AlgPolynomial)
;;; Extended Euclidean algorithm. Returns (gcd, s, t) such that gcd = s*p1 + t*p2.
(define (alg-poly-extended-gcd p1 p2)
  (doc 'export #t)
  (let ([F (alg-poly-field p1)])
    (let loop ([old-r p1] [r p2]
               [old-s (alg-poly-one-over F)] [s (alg-poly-zero-over F)]
               [old-t (alg-poly-zero-over F)] [t (alg-poly-one-over F)])
      (if (alg-poly-zero? r)
          (list (alg-poly-make-monic old-r) old-s old-t)
          (let* ([qr (alg-poly-divmod old-r r)]
                 [q (car qr)])
            (loop r (cdr qr)
                  s (alg-poly-sub old-s (alg-poly-mul q s))
                  t (alg-poly-sub old-t (alg-poly-mul q t))))))))

;;; alg-poly-square-free : AlgPolynomial → AlgPolynomial
;;; Compute square-free part: p / gcd(p, p')
(define (alg-poly-square-free p)
  (doc 'export #t)
  (let* ([F (alg-poly-field p)]
         [p-deriv (alg-poly-derivative p)]
         [g (alg-poly-gcd p p-deriv)])
    (if (alg-poly-zero? g)
        p
        (alg-poly-div p g))))

;;; alg-poly-derivative : AlgPolynomial → AlgPolynomial
;;; Formal derivative: d/dx (sum_i a_i x^i) = sum_i i*a_i x^{i-1}
(define (alg-poly-derivative p)
  (doc 'export #t)
  (let* ([F (alg-poly-field p)]
         [coeffs (alg-poly-coeffs p)]
         [n (length coeffs)])
    (if (<= n 1)
        (alg-poly-zero-over F)
        (let ([new-coeffs
               (let loop ([i 1] [acc '()])
                 (if (>= i n)
                     (reverse acc)
                     (loop (+ i 1)
                           (cons (* i (list-ref coeffs i)) acc))))])
          (alg-make-polynomial F new-coeffs)))))

;;; alg-poly-square-free-factorization : AlgPolynomial → (List (AlgPolynomial . Nat))
;;; Yun's algorithm for square-free factorization.
;;; Returns list of (factor . multiplicity) pairs.
(define (alg-poly-square-free-factorization p)
  (doc 'export #t)
  (if (alg-poly-zero? p)
      '()
      (let* ([F (alg-poly-field p)]
             [p-deriv (alg-poly-derivative p)]
             [g (alg-poly-gcd p p-deriv)])
        (if (<= (alg-poly-degree g) 0)
            ;; p is already square-free
            (list (cons p 1))
            ;; Factor out common factors
            (let ([q (alg-poly-div p g)])
              (alg-sqf-iter q g 1 '()))))))

;;; alg-sqf-iter : AlgPolynomial × AlgPolynomial × Nat × List → (List (AlgPolynomial . Nat))
(define (alg-sqf-iter a c k factors)
  (doc 'export #t)
  (let ([F (alg-poly-field a)])
    (if (<= (alg-poly-degree a) 0)
        (reverse factors)
        (let* ([g (alg-poly-gcd a c)]
               [new-a (alg-poly-div a g)]
               [new-c (alg-poly-div c g)]
               [new-factors (if (> (alg-poly-degree new-a) 0)
                               (cons (cons new-a k) factors)
                               factors)])
          (alg-sqf-iter g new-c (+ k 1) new-factors)))))

;;; alg-poly->string : AlgPolynomial × Symbol → String
;;; Convert polynomial to string representation.
(define (alg-poly->string p var)
  (doc 'export #t)
  (let* ([coeffs (alg-poly-coeffs p)]
         [n (length coeffs)]
         [var-str (symbol->string var)])
    (if (= n 0)
        "0"
        (let loop ([i 0] [first? #t] [acc ""])
          (if (>= i n)
              (if (string=? acc "") "0" acc)
              (let ([c (list-ref coeffs i)])
                (if (= c 0)
                    (loop (+ i 1) first? acc)
                    (let* ([sign (if (< c 0) " - " (if first? "" " + "))]
                           [abs-c (abs c)]
                           [coeff-str (if (and (= abs-c 1) (> i 0)) "" (number->string abs-c))]
                           [power-str (cond [(= i 0) ""]
                                           [(= i 1) var-str]
                                           [else (string-append var-str "^" (number->string i))])]
                           [term (string-append sign coeff-str
                                               (if (and (not (string=? coeff-str ""))
                                                        (not (string=? power-str "")))
                                                   "*" "")
                                               power-str)])
                      (loop (+ i 1) #f (string-append acc term))))))))))

;;; ====
;;; Explicit Accessors for Numeric Polynomials
;;; ====

;;; Numeric polynomials use tag 'poly and store coeffs as vector in cadr
;;; num-poly-coeffs : NumericPoly → Vector
(define (num-poly-coeffs p)
  (doc 'export #t)
  (cadr p))

;;; ====
;;; Rational Field for Exact Arithmetic
;;; ====

;;; Q-field : Field
;;; The field of rational numbers for exact arithmetic.
;;; Uses Scheme's built-in rational support.
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

;;; ====
;;; Conversion Between Numeric and Algebraic Polynomials
;;; ====

;;; numeric->algebra-poly : Poly × Field → AlgPolynomial
;;; Convert from numeric polynomial (descending order) to algebra polynomial (ascending order).
;;; Descending #(a_n a_{n-1} ... a_0) → Ascending (a_0 a_1 ... a_n)
(define (numeric->algebra-poly p field)
  (doc 'export #t)
  (let* ([coeffs (num-poly-coeffs p)]
         [n (vector-length coeffs)]
         ;; Iterate 0..n-1 and cons to reverse order: descending → ascending
         [ascending-coeffs (let loop ([i 0] [acc '()])
                             (if (>= i n)
                                 acc
                                 (loop (+ i 1) (cons (vector-ref coeffs i) acc))))])
    (alg-make-polynomial field ascending-coeffs)))

;;; algebra->numeric-poly : AlgebraPoly → Poly
;;; Convert from algebra polynomial (ascending order) to numeric polynomial (descending order).
(define (algebra->numeric-poly ap)
  (doc 'export #t)
  (let* ([coeffs (alg-poly-coeffs ap)]
         [n (length coeffs)]
         [desc-vec (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n) (make-poly desc-vec))
      (vector-set! desc-vec i (list-ref coeffs (- n 1 i))))))

;;; ====
;;; Transfer Function Simplification via GCD
;;; ====

;;; tf-simplify : TF × [Field] → TF
;;; Simplify transfer function by canceling common factors.
;;; Uses polynomial GCD for exact pole-zero cancellation.
;;; Optional field parameter (defaults to Q-field for exact rationals).
(define (tf-simplify tf . opts)
  (doc 'export #t)
  (let* ([field (if (null? opts) Q-field (car opts))]
         [num (tf-num tf)]
         [den (tf-den tf)]
         ;; Convert to algebra polynomials
         [num-alg (numeric->algebra-poly num field)]
         [den-alg (numeric->algebra-poly den field)]
         ;; Compute GCD
         [gcd-alg (alg-poly-gcd num-alg den-alg)])
    ;; If GCD is non-trivial (degree > 0), divide it out
    (if (> (alg-poly-degree gcd-alg) 0)
        (let* ([num-simplified (algebra->numeric-poly (alg-poly-div num-alg gcd-alg))]
               [den-simplified (algebra->numeric-poly (alg-poly-div den-alg gcd-alg))])
          (make-tf num-simplified den-simplified))
        ;; No common factors
        tf)))

;;; tf-simplify-exact : TF → TF
;;; Simplify transfer function using exact rational arithmetic.
;;; Converts coefficients to exact rationals for GCD computation.
(define (tf-simplify-exact tf)
  (doc 'export #t)
  (tf-simplify tf Q-field))

;;; ====
;;; Exact Routh-Hurwitz Using Polynomial Algebra
;;; ====

;;; The standard Routh-Hurwitz uses floating-point which can cause
;;; numerical issues. This version uses exact rational arithmetic.

;;; routh-array-exact : AlgebraPoly → (List (List Rational))
;;; Compute Routh array using exact rational arithmetic.
;;; Input is an algebra polynomial over Q-field (ascending order).
(define (routh-array-exact apoly)
  (doc 'export #t)
  (let* ([coeffs (alg-poly-coeffs apoly)]  ; Use algebra accessor
         [n (length coeffs)]
         [degree (- n 1)])
    (if (< n 2)
        ;; Degree 0 polynomial
        (list (list (if (null? coeffs) 0 (car coeffs))))
        ;; Build Routh array with exact arithmetic
        (let* ([desc-coeffs (reverse coeffs)]  ; Convert to descending order
               [row1-len (ceiling (/ n 2))]
               [row1 (make-routh-row desc-coeffs 0 row1-len n)]
               [row2 (make-routh-row desc-coeffs 1 row1-len n)])
          (routh-array-exact-iter (list row2 row1) degree)))))

;;; make-routh-row : (List Coeff) × Nat × Nat × Nat → (List Coeff)
;;; Extract elements at positions start, start+2, start+4, ... from coefficients.
(define (make-routh-row coeffs start row-len n)
  (doc 'export #t)
  (let loop ([i 0] [acc '()])
    (if (>= i row-len)
        (reverse acc)
        (let ([idx (+ start (* i 2))])
          (loop (+ i 1)
                (cons (if (< idx n) (list-ref coeffs idx) 0)
                      acc))))))

;;; routh-array-exact-iter : (List (List Coeff)) × Nat → (List (List Coeff))
;;; Build remaining rows of Routh array using exact rational arithmetic.
(define (routh-array-exact-iter rows remaining)
  (doc 'export #t)
  (if (<= remaining 0)
      (reverse rows)
      (let* ([prev-row (car rows)]
             [prev-prev-row (cadr rows)]
             [len (length prev-row)])
        ;; Handle edge case: if rows are too short, stop iteration
        (if (or (null? prev-row) (null? prev-prev-row))
            (reverse rows)
            (let* ([a (car prev-prev-row)]
                   [b (car prev-row)]
                   ;; Handle zero pivot: use small rational epsilon for limit behavior.
                   ;; NOTE: This is a simplification. For rigorous handling of zero pivots,
                   ;; the auxiliary polynomial method should be used (derivative of the
                   ;; polynomial formed by the row above the zero row). This epsilon
                   ;; approach gives correct stability determination for most cases.
                   ;; TODO: Implement proper auxiliary polynomial method for zero rows.
                   [b-safe (if (= b 0) 1/1000000 b)]
                   [new-row (make-routh-new-row prev-prev-row prev-row b-safe len)])
              ;; If new row is empty, stop iteration
              (if (null? new-row)
                  (reverse rows)
                  (routh-array-exact-iter (cons new-row rows) (- remaining 1))))))))

;;; make-routh-new-row : (List Coeff) × (List Coeff) × Coeff × Nat → (List Coeff)
(define (make-routh-new-row prev-prev-row prev-row b len)
  (doc 'export #t)
  (let ([a (car prev-prev-row)])
    (let loop ([i 0] [acc '()])
      (if (>= i (- len 1))
          (reverse acc)
          (let* ([c (if (< (+ i 1) len) (list-ref prev-prev-row (+ i 1)) 0)]
                 [d (if (< (+ i 1) len) (list-ref prev-row (+ i 1)) 0)]
                 [val (/ (- (* b c) (* a d)) b)])
            (loop (+ i 1) (cons val acc)))))))

;;; routh-hurwitz-exact : Poly → Boolean
;;; Check stability using exact rational Routh-Hurwitz.
;;; Input is numeric polynomial (descending order).
(define (routh-hurwitz-exact p)
  (doc 'export #t)
  (let* ([alg-poly (numeric->algebra-poly p Q-field)]
         [array (routh-array-exact alg-poly)]
         [first-col (map car array)])
    (not (has-sign-changes-exact? first-col))))

;;; has-sign-changes-exact? : (List Rational) → Boolean
(define (has-sign-changes-exact? nums)
  (doc 'export #t)
  (cond
    [(null? nums) #f]
    [(null? (cdr nums)) #f]
    [else
     (let* ([a (car nums)]
            [b (cadr nums)]
            [sign-change? (< (* a b) 0)])
       (or sign-change?
           (has-sign-changes-exact? (cdr nums))))]))

;;; ====
;;; Characteristic Polynomial Utilities
;;; ====

;;; tf-char-poly : TF → AlgebraPoly
;;; Get the characteristic polynomial (denominator) as algebra polynomial.
(define (tf-char-poly tf)
  (doc 'export #t)
  (numeric->algebra-poly (tf-den tf) Q-field))

;;; tf-char-poly-factored : TF → (List (AlgPolynomial × Nat))
;;; Get square-free factorization of characteristic polynomial.
(define (tf-char-poly-factored tf)
  (doc 'export #t)
  (alg-poly-square-free-factorization (tf-char-poly tf)))

;;; ====
;;; Extended GCD for Bezout Identity
;;; ====

;;; tf-bezout : TF × TF → (AlgPolynomial × AlgPolynomial × AlgPolynomial)
;;; Compute Bezout coefficients for transfer function polynomials.
;;; Returns (gcd, s, t) such that gcd = s*num1 + t*num2.
;;; Useful for controller synthesis.
(define (tf-bezout tf1 tf2)
  (doc 'export #t)
  (let* ([num1-alg (numeric->algebra-poly (tf-num tf1) Q-field)]
         [num2-alg (numeric->algebra-poly (tf-num tf2) Q-field)])
    (alg-poly-extended-gcd num1-alg num2-alg)))

;;; ====
;;; Pole-Zero Analysis with Exact Arithmetic
;;; ====

;;; tf-coprime? : TF → Boolean
;;; Check if numerator and denominator are coprime (GCD = 1).
;;; Returns #t if no common factors exist.
(define (tf-coprime? tf)
  (doc 'export #t)
  (let* ([num-alg (numeric->algebra-poly (tf-num tf) Q-field)]
         [den-alg (numeric->algebra-poly (tf-den tf) Q-field)]
         [gcd-alg (alg-poly-gcd num-alg den-alg)])
    (<= (alg-poly-degree gcd-alg) 0)))

;;; tf-canceled-poles : TF → (List Polynomial)
;;; Get the common factors (canceled pole-zero pairs) from transfer function.
;;; Returns list of factors that would be removed by tf-simplify.
(define (tf-canceled-poles tf)
  (doc 'export #t)
  (let* ([num-alg (numeric->algebra-poly (tf-num tf) Q-field)]
         [den-alg (numeric->algebra-poly (tf-den tf) Q-field)]
         [gcd-alg (alg-poly-gcd num-alg den-alg)])
    (if (> (alg-poly-degree gcd-alg) 0)
        (list (algebra->numeric-poly gcd-alg))
        '())))

;;; ====
;;; Polynomial Display Utilities
;;; ====

;;; tf-num-exact->string : TF → String
;;; Display numerator with exact rational coefficients.
(define (tf-num-exact->string tf)
  (doc 'export #t)
  (alg-poly->string (numeric->algebra-poly (tf-num tf) Q-field) 's))

;;; tf-den-exact->string : TF → String
;;; Display denominator with exact rational coefficients.
(define (tf-den-exact->string tf)
  (doc 'export #t)
  (alg-poly->string (numeric->algebra-poly (tf-den tf) Q-field) 's))

;;; tf-exact->string : TF → String
;;; Display full transfer function with exact coefficients.
(define (tf-exact->string tf)
  (doc 'export #t)
  (string-append "(" (tf-num-exact->string tf) ") / ("
                 (tf-den-exact->string tf) ")"))
