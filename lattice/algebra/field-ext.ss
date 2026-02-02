(load "core/base/prelude.ss")
(load "lattice/algebra/field.ss")
(load "lattice/algebra/polynomial.ss")

(doc 'module 'field-ext)
(doc 'description "Algebraic field extensions over infinite fields (Q, R)")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Algebraic field extensions F(α) where α is a root of an irreducible polynomial.")
(doc 'note "Supports:")
(doc 'note "- Simple algebraic extensions: F(α) = F[x]/(m(x))")
(doc 'note "- Quadratic extensions: Q(√d), Q(i)")
(doc 'note "- Minimal polynomial computation")
(doc 'note "- Degree and basis of extension")
(doc 'note "- Field automorphisms and Galois groups (for simple cases)")
(doc 'note "")
(doc 'note "Unlike galois.ss (finite fields), this handles characteristic-0 extensions.")

;;; ====
;;; Section: Algebraic Extension Elements
;;; ====

(doc 'section 'algebraic-elements)

;;; An element of F(α) is represented as a polynomial in α of degree < n
;;; where n = deg(minimal polynomial of α).
;;; Storage: (alg-ext-element coeffs min-poly base-field symbol)

(define (make-alg-ext-element coeffs min-poly base-field symbol)
  (doc 'export #t)
  (doc 'type '(-> (List Coeff) Polynomial Field Symbol AlgExtElement))
  (doc 'description "Create element of algebraic extension F(α)")
  (let* ([n (poly-degree min-poly)]
         [normalized (alg-ext-normalize coeffs n base-field)])
    (list 'alg-ext-element normalized min-poly base-field symbol)))

(define (alg-ext-element? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'alg-ext-element)))

;;; Accessors
(define (alg-ext-coeffs e)
  (doc 'export #t)
  (list-ref e 1))

(define (alg-ext-min-poly e)
  (doc 'export #t)
  (list-ref e 2))

(define (alg-ext-base-field e)
  (doc 'export #t)
  (list-ref e 3))

(define (alg-ext-symbol e)
  (doc 'export #t)
  (list-ref e 4))

;;; Normalize coefficients to length n
;;; INVARIANT: Input coeffs should already be reduced mod min-poly.
;;; This function only pads with zeros or verifies length.
;;; If len > n, this is a logic error (caller should have reduced first).
(define (alg-ext-normalize coeffs n F)
  (let* ([zero (field-zero F)]
         [len (length coeffs)])
    (cond
      [(< len n) (append coeffs (make-list (- n len) zero))]
      [(> len n)
       ;; Caller should have reduced mod min-poly before calling.
       ;; Truncate trailing zeros only, otherwise error.
       (let ([trimmed (trim-trailing-zeros coeffs F)])
         (if (<= (length trimmed) n)
             (alg-ext-normalize trimmed n F)
             (error 'alg-ext-normalize
                    "coeffs exceed degree, should be reduced first")))]
      [else coeffs])))

;;; trim-trailing-zeros : (List Coeff) × Field → (List Coeff)
(define (trim-trailing-zeros coeffs F)
  (let ([eq-fn (field-equal-fn F)]
        [zero (field-zero F)])
    (let loop ([cs (reverse coeffs)])
      (cond
        [(null? cs) (list zero)]
        [(eq-fn (car cs) zero) (loop (cdr cs))]
        [else (reverse cs)]))))

;;; ====
;;; Section: Algebraic Extension Field Construction
;;; ====

(doc 'section 'extension-construction)

;;; make-algebraic-extension : Field × Polynomial × Symbol → Field
;;; Create F(α) where α is a root of the irreducible polynomial min-poly.
;;; Elements are polynomials in α of degree < deg(min-poly).
(define (make-algebraic-extension base-field min-poly symbol)
  (doc 'export #t)
  (doc 'type '(-> Field Polynomial Symbol Field))
  (doc 'description "Create algebraic extension F(α) where α is root of min-poly")
  (doc 'note "min-poly must be irreducible over base-field")
  (let* ([n (poly-degree min-poly)]
         [zero-coeffs (make-list n (field-zero base-field))]
         [one-coeffs (cons (field-one base-field)
                           (make-list (- n 1) (field-zero base-field)))])
    (make-field
     '()  ; Elements not enumerable (infinite extension)
     ;; Addition: coefficient-wise
     (lambda (a b)
       (alg-ext-add a b base-field n min-poly symbol))
     ;; Multiplication: polynomial multiply then reduce
     (lambda (a b)
       (alg-ext-mul a b base-field n min-poly symbol))
     ;; Zero
     (make-alg-ext-element zero-coeffs min-poly base-field symbol)
     ;; One
     (make-alg-ext-element one-coeffs min-poly base-field symbol)
     ;; Negation
     (lambda (a)
       (alg-ext-neg a base-field n min-poly symbol))
     ;; Division
     (lambda (a b)
       (alg-ext-div a b base-field n min-poly symbol))
     ;; Equality
     (lambda (a b)
       (alg-ext-equal? a b base-field)))))

;;; alg-ext-add : element addition
(define (alg-ext-add a b F n min-poly sym)
  (let* ([ca (alg-ext-coeffs a)]
         [cb (alg-ext-coeffs b)]
         [add (field-add-op F)]
         [result (map add ca cb)])
    (make-alg-ext-element result min-poly F sym)))

;;; alg-ext-neg : element negation
(define (alg-ext-neg a F n min-poly sym)
  (let* ([ca (alg-ext-coeffs a)]
         [neg (field-neg-fn F)]
         [result (map neg ca)])
    (make-alg-ext-element result min-poly F sym)))

;;; alg-ext-mul : element multiplication with reduction
(define (alg-ext-mul a b F n min-poly sym)
  (let* ([ca (alg-ext-coeffs a)]
         [cb (alg-ext-coeffs b)]
         [pa (make-polynomial F ca)]
         [pb (make-polynomial F cb)]
         [product (poly-mul pa pb)]
         [reduced (poly-mod product min-poly)]
         [result-coeffs (poly-coeffs reduced)])
    (make-alg-ext-element result-coeffs min-poly F sym)))

;;; alg-ext-inv : element inversion via extended GCD
(define (alg-ext-inv a F n min-poly sym)
  (let* ([ca (alg-ext-coeffs a)]
         [pa (make-polynomial F ca)]
         [result (poly-extended-gcd pa min-poly)]
         [s (cadr result)]  ; Bezout coefficient
         [inv-coeffs (poly-coeffs s)])
    (make-alg-ext-element inv-coeffs min-poly F sym)))

;;; alg-ext-div : element division
(define (alg-ext-div a b F n min-poly sym)
  (alg-ext-mul a (alg-ext-inv b F n min-poly sym) F n min-poly sym))

;;; alg-ext-equal? : element equality
(define (alg-ext-equal? a b F)
  (let* ([ca (alg-ext-coeffs a)]
         [cb (alg-ext-coeffs b)]
         [eq-fn (field-equal-fn F)])
    (and (= (length ca) (length cb))
         (let loop ([as ca] [bs cb])
           (or (null? as)
               (and (eq-fn (car as) (car bs))
                    (loop (cdr as) (cdr bs))))))))

;;; ====
;;; Section: Extension Field Utilities
;;; ====

(doc 'section 'extension-utilities)

;;; ext-degree : Field → Nat
;;; Get the degree of the extension [F(α):F].
(define (ext-degree ext-field)
  (doc 'export #t)
  (doc 'type '(-> Field Nat))
  (doc 'description "Degree of extension [F(α):F]")
  (let ([one (field-one ext-field)])
    (if (alg-ext-element? one)
        (length (alg-ext-coeffs one))
        1)))  ; Base field has degree 1

;;; ext-generator : Field → AlgExtElement
;;; Get the generator α of the extension F(α).
(define (ext-generator ext-field)
  (doc 'export #t)
  (doc 'type '(-> Field AlgExtElement))
  (doc 'description "Get the generator α of F(α)")
  (let* ([one (field-one ext-field)]
         [F (alg-ext-base-field one)]
         [min-poly (alg-ext-min-poly one)]
         [sym (alg-ext-symbol one)]
         [n (poly-degree min-poly)]
         [zero (field-zero F)]
         [one-f (field-one F)]
         ;; α = 0 + 1·α + 0·α² + ...
         [coeffs (cons zero (cons one-f (make-list (- n 2) zero)))])
    (make-alg-ext-element coeffs min-poly F sym)))

;;; ext-embed : Field × Element → AlgExtElement
;;; Embed base field element into extension.
(define (ext-embed ext-field base-elem)
  (doc 'export #t)
  (doc 'type '(-> Field Element AlgExtElement))
  (doc 'description "Embed base field element into extension F(α)")
  (let* ([one (field-one ext-field)]
         [F (alg-ext-base-field one)]
         [min-poly (alg-ext-min-poly one)]
         [sym (alg-ext-symbol one)]
         [n (poly-degree min-poly)]
         [zero (field-zero F)]
         [coeffs (cons base-elem (make-list (- n 1) zero))])
    (make-alg-ext-element coeffs min-poly F sym)))

;;; ====
;;; Section: Common Quadratic Extensions
;;; ====

(doc 'section 'quadratic-extensions)

;;; make-Q-sqrt : Integer → Field
;;; Create Q(√d) where d is a square-free integer.
;;; Elements are a + b√d with a,b ∈ Q.
(define (make-Q-sqrt d)
  (doc 'export #t)
  (doc 'type '(-> Int Field))
  (doc 'description "Create Q(√d), the quadratic extension of rationals")
  (doc 'example "(make-Q-sqrt 2) creates Q(√2)")
  (let* ([F Q-field]
         ;; Minimal polynomial: x² - d
         [min-poly (make-polynomial F (list (- d) 0 1))]
         [sym (string->symbol (string-append "sqrt" (number->string d)))])
    (make-algebraic-extension F min-poly sym)))

;;; make-Q-i : → Field
;;; Create Q(i) = Q(√-1), the Gaussian rationals.
(define (make-Q-i)
  (doc 'export #t)
  (doc 'type '(-> Field))
  (doc 'description "Create Q(i), Gaussian rationals")
  (make-Q-sqrt -1))

;;; Q-sqrt-element : Field × Rational × Rational → AlgExtElement
;;; Create element a + b√d in Q(√d).
(define (Q-sqrt-element ext-field a b)
  (doc 'export #t)
  (doc 'type '(-> Field Rational Rational AlgExtElement))
  (doc 'description "Create a + b√d in quadratic extension")
  (let* ([one (field-one ext-field)]
         [F (alg-ext-base-field one)]
         [min-poly (alg-ext-min-poly one)]
         [sym (alg-ext-symbol one)])
    (make-alg-ext-element (list a b) min-poly F sym)))

;;; Q-sqrt-real-part : AlgExtElement → Rational
;;; Get the rational part of a + b√d.
(define (Q-sqrt-real-part elem)
  (doc 'export #t)
  (car (alg-ext-coeffs elem)))

;;; Q-sqrt-sqrt-part : AlgExtElement → Rational
;;; Get the coefficient of √d in a + b√d.
(define (Q-sqrt-sqrt-part elem)
  (doc 'export #t)
  (cadr (alg-ext-coeffs elem)))

;;; ====
;;; Section: Field Automorphisms
;;; ====

(doc 'section 'automorphisms)

(doc 'note "A field automorphism σ: F → F is a bijection preserving + and ×.")
(doc 'note "For Q(√d), the only non-identity automorphism is conjugation:")
(doc 'note "  σ(a + b√d) = a - b√d")

;;; make-conjugation : Field → (AlgExtElement → AlgExtElement)
;;; Create the conjugation automorphism for a quadratic extension.
;;; For Q(√d): σ(a + b√d) = a - b√d
(define (make-conjugation ext-field)
  (doc 'export #t)
  (doc 'type '(-> Field (-> AlgExtElement AlgExtElement)))
  (doc 'description "Create conjugation automorphism for quadratic extension")
  (let* ([one (field-one ext-field)]
         [F (alg-ext-base-field one)]
         [min-poly (alg-ext-min-poly one)]
         [sym (alg-ext-symbol one)]
         [neg (field-neg-fn F)])
    (lambda (elem)
      (let ([coeffs (alg-ext-coeffs elem)])
        (if (= (length coeffs) 2)
            ;; Quadratic: negate the √d coefficient
            (make-alg-ext-element
             (list (car coeffs) (neg (cadr coeffs)))
             min-poly F sym)
            ;; Higher degree: more complex (not implemented here)
            (error 'make-conjugation "only quadratic extensions supported"))))))

;;; galois-group-quadratic : Field → (List (AlgExtElement → AlgExtElement))
;;; Get the Galois group of a quadratic extension as list of automorphisms.
;;; For Q(√d)/Q, this is {id, conjugation} ≅ Z/2Z.
(define (galois-group-quadratic ext-field)
  (doc 'export #t)
  (doc 'type '(-> Field (List Automorphism)))
  (doc 'description "Galois group of quadratic extension (isomorphic to Z/2Z)")
  (list
   (lambda (x) x)  ; Identity
   (make-conjugation ext-field)))

;;; ====
;;; Section: Norm and Trace for Quadratic Extensions
;;; ====

(doc 'section 'norm-trace-quadratic)

(doc 'note "For α = a + b√d in Q(√d):")
(doc 'note "  Trace(α) = α + σ(α) = 2a")
(doc 'note "  Norm(α) = α × σ(α) = a² - d·b²")

;;; Q-sqrt-trace : AlgExtElement → Rational
;;; Trace of element in quadratic extension.
(define (Q-sqrt-trace elem)
  (doc 'export #t)
  (doc 'type '(-> AlgExtElement Rational))
  (doc 'description "Trace of quadratic extension element: Tr(a+b√d) = 2a")
  (* 2 (Q-sqrt-real-part elem)))

;;; Q-sqrt-norm : AlgExtElement × Integer → Rational
;;; Norm of element in Q(√d).
;;; N(a + b√d) = a² - d·b²
(define (Q-sqrt-norm elem d)
  (doc 'export #t)
  (doc 'type '(-> AlgExtElement Int Rational))
  (doc 'description "Norm of quadratic extension element: N(a+b√d) = a² - d·b²")
  (let ([a (Q-sqrt-real-part elem)]
        [b (Q-sqrt-sqrt-part elem)])
    (- (* a a) (* d b b))))

;;; ====
;;; Section: Splitting Fields
;;; ====

(doc 'section 'splitting-fields)

(doc 'note "The splitting field of a polynomial f over F is the smallest")
(doc 'note "extension of F containing all roots of f.")
(doc 'note "For quadratic f(x) = x² - d, the splitting field is Q(√d).")

;;; splitting-field-quadratic : Field × Polynomial → Field
;;; Compute splitting field for a quadratic polynomial.
;;; For x² + bx + c, splitting field is Q(√(b² - 4c)).
(define (splitting-field-quadratic base-field poly)
  (doc 'export #t)
  (doc 'type '(-> Field Polynomial Field))
  (doc 'description "Splitting field of quadratic polynomial")
  (let* ([coeffs (poly-coeffs poly)]
         [c (list-ref coeffs 0)]
         [b (if (> (length coeffs) 1) (list-ref coeffs 1) 0)]
         [a (if (> (length coeffs) 2) (list-ref coeffs 2) 1)]
         ;; Discriminant Δ = b² - 4ac
         [disc (- (* b b) (* 4 a c))])
    (cond
      [(= disc 0) base-field]  ; Repeated root, no extension needed
      [(and (integer? disc) (exact-sqrt? disc)) base-field]  ; Perfect square
      [else (make-Q-sqrt disc)])))

;;; exact-sqrt? : Integer → Boolean
;;; Check if integer is a perfect square.
(define (exact-sqrt? n)
  (and (>= n 0)
       (let ([s (inexact->exact (floor (sqrt n)))])
         (= (* s s) n))))

;;; ====
;;; Section: Minimal Polynomial Utilities
;;; ====

(doc 'section 'minimal-polynomial-utils)

;;; verify-minimal-poly : Field × AlgExtElement → Boolean
;;; Verify that an element's minimal polynomial is correct.
(define (verify-minimal-poly ext-field elem)
  (doc 'export #t)
  (doc 'type '(-> Field AlgExtElement Boolean))
  (doc 'description "Verify element satisfies its minimal polynomial")
  (let* ([min-poly (alg-ext-min-poly elem)]
         [alpha (ext-generator ext-field)]
         [F (alg-ext-base-field elem)]
         [coeffs (poly-coeffs min-poly)]
         [one (field-one ext-field)]
         [add (field-add-op ext-field)]
         [mul (field-mul-op ext-field)]
         [zero (field-zero ext-field)])
    ;; Evaluate min-poly at α
    (let loop ([cs coeffs] [power one] [sum zero])
      (if (null? cs)
          (alg-ext-equal? sum zero F)
          (loop (cdr cs)
                (mul power alpha)
                (add sum (mul (ext-embed ext-field (car cs)) power)))))))

;;; ====
;;; Section: Display
;;; ====

(doc 'section 'display)

;;; alg-ext->string : AlgExtElement → String
;;; Pretty-print algebraic extension element.
(define (alg-ext->string elem)
  (doc 'export #t)
  (doc 'type '(-> AlgExtElement String))
  (let* ([coeffs (alg-ext-coeffs elem)]
         [sym (symbol->string (alg-ext-symbol elem))]
         [n (length coeffs)])
    (cond
      [(= n 2)
       ;; Quadratic: a + b·sym
       (let ([a (car coeffs)]
             [b (cadr coeffs)])
         (cond
           [(and (= a 0) (= b 0)) "0"]
           [(= b 0) (number->string a)]
           [(= a 0)
            (if (= b 1) sym
                (if (= b -1) (string-append "-" sym)
                    (string-append (number->string b) "·" sym)))]
           [else
            (string-append
             (number->string a)
             (if (< b 0) " - " " + ")
             (let ([abs-b (abs b)])
               (if (= abs-b 1) sym
                   (string-append (number->string abs-b) "·" sym))))]))]
      [else
       ;; General: c₀ + c₁·α + c₂·α² + ...
       (let loop ([cs coeffs] [i 0] [first? #t] [acc ""])
         (if (null? cs)
             (if (string=? acc "") "0" acc)
             (let ([c (car cs)])
               (if (= c 0)
                   (loop (cdr cs) (+ i 1) first? acc)
                   (let* ([sign (if first? "" (if (< c 0) " - " " + "))]
                          [abs-c (abs c)]
                          [coeff-str (if (and (= abs-c 1) (> i 0)) ""
                                         (number->string abs-c))]
                          [var-str (cond
                                     [(= i 0) ""]
                                     [(= i 1) sym]
                                     [else (string-append sym "^" (number->string i))])]
                          [term (string-append coeff-str
                                               (if (and (> (string-length coeff-str) 0)
                                                        (> (string-length var-str) 0))
                                                   "·" "")
                                               var-str)])
                     (loop (cdr cs) (+ i 1) #f
                           (string-append acc sign
                                          (if (and first? (< c 0)) "-" "")
                                          term)))))))])))
