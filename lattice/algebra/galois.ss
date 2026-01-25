(load "core/base/prelude.ss")
(load "lattice/algebra/field.ss")
(load "lattice/algebra/polynomial.ss")
(load "lattice/number-theory/primality.ss")  ; For factorize

(doc 'module 'galois)
(doc 'description "Galois field (finite field) arithmetic: GF(p), GF(p^n), GF(2^n)")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Finite field implementation providing:")
(doc 'note "- GF(p) prime fields (via make-field-zp from field.ss)")
(doc 'note "- GF(p^n) extension fields: polynomials over GF(p) mod irreducible")
(doc 'note "- GF(2^n) binary fields: optimized integer representation")
(doc 'note "- Irreducible polynomial utilities")
(doc 'note "- Standard irreducible polynomials for common field sizes")

;;; ============================================================================
;;; Section 1: GF(p) Prime Fields
;;; ============================================================================

(doc 'section 'gf-prime-fields)

(doc 'note "GF(p) is the field of integers modulo prime p.")
(doc 'note "Already provided by make-field-zp in field.ss.")
(doc 'note "Here we add utility functions for prime field elements.")

;;; gf-prime : Prime → Field
;;; Create GF(p), the prime field of integers mod p.
;;; Alias for make-field-zp with clearer naming.
(define (gf-prime p)
  (doc 'export #t)
  (doc 'type '(-> Int Field))
  (doc 'description "Create GF(p), the finite field of integers mod prime p")
  (make-field-zp p))

;;; gf-order : Field → Nat
;;; Get the order (number of elements) of a finite field.
;;; Uses metadata if available, otherwise falls back to element count.
(define (gf-order F)
  (doc 'export #t)
  (doc 'type '(-> Field Nat))
  (or (field-meta-ref F 'order)
      (let ([elts (field-elements F)])
        (if (null? elts)
            0  ; Unknown/infinite
            (length elts)))))

;;; gf-characteristic : Field → Nat
;;; Get the characteristic of a finite field.
;;; For GF(p^n), the characteristic is p.
;;; Uses metadata if available, otherwise computes by repeated addition.
(define (gf-characteristic F)
  (doc 'export #t)
  (doc 'type '(-> Field Nat))
  (or (field-meta-ref F 'characteristic)
      (let ([elts (field-elements F)])
        (if (null? elts)
            0  ; Infinite field
            (let* ([one (field-one F)]
                   [add (field-add-op F)]
                   [eq-fn (field-equal-fn F)])
              (let loop ([sum one] [n 1])
                (if (eq-fn sum (field-zero F))
                    n
                    (loop (add sum one) (+ n 1)))))))))

;;; ============================================================================
;;; Section 2: GF(p^n) Extension Fields
;;; ============================================================================

(doc 'section 'gf-extension-fields)

(doc 'note "GF(p^n) extension fields are constructed as F[x]/(m(x))")
(doc 'note "where F = GF(p) and m(x) is an irreducible polynomial of degree n.")
(doc 'note "Elements are polynomials of degree < n with coefficients in GF(p).")
(doc 'note "Multiplication is polynomial multiplication mod m(x).")

;;; gf-ext-element : represents an element of GF(p^n)
;;; Stored as (gf-ext-element coeffs modulus base-field)
;;; where coeffs is a list of GF(p) elements [a_0, a_1, ..., a_{n-1}]
;;; representing a_0 + a_1*x + ... + a_{n-1}*x^{n-1}

(define (make-gf-ext-element coeffs modulus base-field)
  (list 'gf-ext-element coeffs modulus base-field))

(define (gf-ext-element? x)
  (and (pair? x) (eq? (car x) 'gf-ext-element)))

(define (gf-ext-coeffs e)
  (list-ref e 1))

(define (gf-ext-modulus e)
  (list-ref e 2))

(define (gf-ext-base-field e)
  (list-ref e 3))

;;; Normalize coefficients to degree < n and remove trailing zeros
(define (gf-ext-normalize coeffs n base-field)
  (let* ([zero (field-zero base-field)]
         [eq-fn (field-equal-fn base-field)]
         ;; Pad or truncate to length n
         [padded (if (< (length coeffs) n)
                     (append coeffs (make-list (- n (length coeffs)) zero))
                     (take coeffs n))])
    ;; Remove trailing zeros but keep at least one
    (let loop ([cs (reverse padded)])
      (cond
        [(null? (cdr cs)) (reverse cs)]
        [(eq-fn (car cs) zero) (loop (cdr cs))]
        [else (reverse cs)]))))

;;; make-gf-extension : Prime × Nat × Polynomial → Field
;;; Create GF(p^n) as GF(p)[x]/(modulus) where modulus is irreducible of degree n.
;;; The modulus polynomial should be over GF(p).
;;; NOTE: For p^n > 10000, use make-gf-extension-large to avoid element enumeration.
(define (make-gf-extension p n modulus)
  (doc 'export #t)
  (doc 'type '(-> Int Int Polynomial Field))
  (doc 'description "Create GF(p^n) as quotient ring GF(p)[x]/(modulus)")
  (let* ([base-field (gf-prime p)]
         [order (expt p n)]
         [zero-coeffs (list (field-zero base-field))]
         [one-coeffs (list (field-one base-field))]
         ;; Only enumerate small fields
         [elements (if (< order 10000)
                       (gf-ext-enumerate p n base-field modulus)
                       '())])
    (make-field*
     elements
     ;; Addition: coefficient-wise
     (lambda (a b)
       (gf-ext-add a b base-field n modulus))
     ;; Multiplication: polynomial multiply then reduce mod modulus
     (lambda (a b)
       (gf-ext-mul a b base-field n modulus))
     ;; Zero
     (make-gf-ext-element zero-coeffs modulus base-field)
     ;; One
     (make-gf-ext-element one-coeffs modulus base-field)
     ;; Negation
     (lambda (a)
       (gf-ext-neg a base-field n modulus))
     ;; Division
     (lambda (a b)
       (gf-ext-div a b base-field n modulus))
     ;; Equality
     (lambda (a b)
       (gf-ext-equal? a b base-field))
     ;; Metadata
     `((order . ,order) (characteristic . ,p) (degree . ,n)))))

;;; make-gf-extension-large : Prime × Nat × Polynomial → Field
;;; Create GF(p^n) without element enumeration. Use for cryptographic field sizes.
(define (make-gf-extension-large p n modulus)
  (doc 'export #t)
  (doc 'type '(-> Int Int Polynomial Field))
  (doc 'description "Create GF(p^n) without enumeration, for large fields")
  (let* ([base-field (make-field-zp-large p)]
         [order (expt p n)]
         [zero-coeffs (list (field-zero base-field))]
         [one-coeffs (list (field-one base-field))])
    (make-field*
     '()  ; No enumeration
     (lambda (a b) (gf-ext-add a b base-field n modulus))
     (lambda (a b) (gf-ext-mul a b base-field n modulus))
     (make-gf-ext-element zero-coeffs modulus base-field)
     (make-gf-ext-element one-coeffs modulus base-field)
     (lambda (a) (gf-ext-neg a base-field n modulus))
     (lambda (a b) (gf-ext-div a b base-field n modulus))
     (lambda (a b) (gf-ext-equal? a b base-field))
     `((order . ,order) (characteristic . ,p) (degree . ,n)))))

;;; Enumerate all elements of GF(p^n)
(define (gf-ext-enumerate p n base-field modulus)
  (let* ([zero (field-zero base-field)]
         [elts (field-elements base-field)])
    (let loop ([coeffs-list (all-coeff-combinations elts n)])
      (map (lambda (coeffs)
             (make-gf-ext-element
              (gf-ext-normalize coeffs n base-field)
              modulus
              base-field))
           coeffs-list))))

;;; Generate all n-tuples of coefficients from a list of elements
(define (all-coeff-combinations elements n)
  (if (= n 0)
      '(())
      (let ([rest (all-coeff-combinations elements (- n 1))])
        (apply append
               (map (lambda (e)
                      (map (lambda (r) (cons e r)) rest))
                    elements)))))

;;; GF(p^n) element addition
(define (gf-ext-add a b base-field n modulus)
  (let* ([ca (gf-ext-coeffs a)]
         [cb (gf-ext-coeffs b)]
         [add-op (field-add-op base-field)]
         [zero (field-zero base-field)])
    (make-gf-ext-element
     (gf-ext-normalize
      (map (lambda (i)
             (add-op (list-ref-or ca i zero)
                     (list-ref-or cb i zero)))
           (iota n))
      n base-field)
     modulus base-field)))

;;; GF(p^n) element negation
(define (gf-ext-neg a base-field n modulus)
  (let ([neg-fn (field-neg-fn base-field)])
    (make-gf-ext-element
     (map neg-fn (gf-ext-coeffs a))
     modulus base-field)))

;;; GF(p^n) element multiplication (with reduction)
(define (gf-ext-mul a b base-field n modulus)
  (let* ([ca (gf-ext-coeffs a)]
         [cb (gf-ext-coeffs b)]
         ;; Convert to polynomials and multiply
         [pa (make-polynomial base-field ca)]
         [pb (make-polynomial base-field cb)]
         [product (poly-mul pa pb)]
         ;; Reduce mod the irreducible
         [reduced (poly-mod product modulus)]
         [result-coeffs (poly-coeffs reduced)])
    (make-gf-ext-element
     (gf-ext-normalize result-coeffs n base-field)
     modulus base-field)))

;;; GF(p^n) element inversion using extended Euclidean algorithm
(define (gf-ext-inv a base-field n modulus)
  (let* ([ca (gf-ext-coeffs a)]
         [pa (make-polynomial base-field ca)]
         ;; Extended GCD: gcd(a, modulus) = s*a + t*modulus
         ;; Since modulus is irreducible and a ≠ 0, gcd = 1, so s*a ≡ 1 (mod modulus)
         [result (poly-extended-gcd pa modulus)]
         [gcd (car result)]
         [s (cadr result)]
         [inv-coeffs (poly-coeffs s)])
    (make-gf-ext-element
     (gf-ext-normalize inv-coeffs n base-field)
     modulus base-field)))

;;; GF(p^n) element division
(define (gf-ext-div a b base-field n modulus)
  (gf-ext-mul a (gf-ext-inv b base-field n modulus) base-field n modulus))

;;; GF(p^n) element equality
;;; O(n) comparison via direct list traversal
(define (gf-ext-equal? a b base-field)
  (let* ([ca (gf-ext-coeffs a)]
         [cb (gf-ext-coeffs b)]
         [eq-fn (field-equal-fn base-field)])
    (and (= (length ca) (length cb))
         (let loop ([as ca] [bs cb])
           (or (null? as)
               (and (eq-fn (car as) (car bs))
                    (loop (cdr as) (cdr bs))))))))

;;; Helper: safe list-ref with default
(define (list-ref-or lst i default)
  (if (< i (length lst))
      (list-ref lst i)
      default))

;;; Helper: iota
(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; Helper: take first n elements
(define (take lst n)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;;; Helper: make-list
(define (make-list n val)
  (if (<= n 0)
      '()
      (cons val (make-list (- n 1) val))))

;;; ============================================================================
;;; Section 3: GF(2^n) Binary Fields (Optimized)
;;; ============================================================================

(doc 'section 'gf2n-binary-fields)

(doc 'note "GF(2^n) binary fields with optimized integer representation.")
(doc 'note "Elements are integers where bit i represents coefficient of x^i.")
(doc 'note "Addition is XOR, multiplication uses shift-and-XOR with reduction.")
(doc 'note "This is significantly faster than generic GF(p^n) for p=2.")

;;; gf2n-element : represents an element of GF(2^n)
;;; Stored as (gf2n-element value n irred) where:
;;; - value is an integer with bits representing polynomial coefficients
;;; - n is the field degree
;;; - irred is the irreducible polynomial as integer

(define (make-gf2n-element value n irred)
  (list 'gf2n-element (bitwise-and value (- (bitwise-arithmetic-shift-left 1 n) 1)) n irred))

(define (gf2n-element? x)
  (and (pair? x) (eq? (car x) 'gf2n-element)))

(define (gf2n-value e)
  (list-ref e 1))

(define (gf2n-degree e)
  (list-ref e 2))

(define (gf2n-irred e)
  (list-ref e 3))

;;; make-gf2n : Nat × Int → Field
;;; Create GF(2^n) with given irreducible polynomial (as integer).
;;; The irreducible should include the leading x^n term.
;;; Example: GF(2^8) with AES polynomial x^8 + x^4 + x^3 + x + 1 = 0x11B
;;; NOTE: For n > 16, elements are not enumerated (too large).
(define (make-gf2n n irred)
  (doc 'export #t)
  (doc 'type '(-> Nat Int Field))
  (doc 'description "Create GF(2^n) with optimized binary arithmetic")
  (doc 'note "irred is the irreducible polynomial including x^n term, as integer")
  (let* ([order (bitwise-arithmetic-shift-left 1 n)]
         ;; Only enumerate for small fields (n <= 16 means 65536 elements max)
         [elements (if (<= n 16)
                       (map (lambda (i) (make-gf2n-element i n irred)) (iota order))
                       '())])
    (make-field*
     elements
     ;; Addition: XOR
     (lambda (a b)
       (make-gf2n-element (bitwise-xor (gf2n-value a) (gf2n-value b)) n irred))
     ;; Multiplication: shift-and-XOR with reduction
     (lambda (a b)
       (make-gf2n-element (gf2n-mul-raw (gf2n-value a) (gf2n-value b) n irred) n irred))
     ;; Zero
     (make-gf2n-element 0 n irred)
     ;; One
     (make-gf2n-element 1 n irred)
     ;; Negation (same as identity in GF(2^n))
     (lambda (a) a)
     ;; Division
     (lambda (a b)
       (make-gf2n-element
        (gf2n-mul-raw (gf2n-value a) (gf2n-inv-raw (gf2n-value b) n irred) n irred)
        n irred))
     ;; Equality
     (lambda (a b)
       (= (gf2n-value a) (gf2n-value b)))
     ;; Metadata
     `((order . ,order) (characteristic . 2) (degree . ,n)))))

;;; make-gf2n-large : Nat × Int → Field
;;; Create GF(2^n) without element enumeration. Use for cryptographic sizes (n >= 128).
(define (make-gf2n-large n irred)
  (doc 'export #t)
  (doc 'type '(-> Nat Int Field))
  (doc 'description "Create GF(2^n) without enumeration, for large n")
  (let ([order (bitwise-arithmetic-shift-left 1 n)])
    (make-field*
     '()  ; No enumeration
     (lambda (a b)
       (make-gf2n-element (bitwise-xor (gf2n-value a) (gf2n-value b)) n irred))
     (lambda (a b)
       (make-gf2n-element (gf2n-mul-raw (gf2n-value a) (gf2n-value b) n irred) n irred))
     (make-gf2n-element 0 n irred)
     (make-gf2n-element 1 n irred)
     (lambda (a) a)
     (lambda (a b)
       (make-gf2n-element
        (gf2n-mul-raw (gf2n-value a) (gf2n-inv-raw (gf2n-value b) n irred) n irred)
        n irred))
     (lambda (a b) (= (gf2n-value a) (gf2n-value b)))
     `((order . ,order) (characteristic . 2) (degree . ,n)))))

;;; Raw GF(2^n) multiplication using Russian peasant algorithm
(define (gf2n-mul-raw a b n irred)
  (let ([high-bit (bitwise-arithmetic-shift-left 1 n)])
    (let loop ([a a] [b b] [result 0])
      (if (= b 0)
          result
          (let* ([new-result (if (odd? b)
                                 (bitwise-xor result a)
                                 result)]
                 [a-shifted (bitwise-arithmetic-shift-left a 1)]
                 [a-reduced (if (>= a-shifted high-bit)
                                (bitwise-xor a-shifted irred)
                                a-shifted)])
            (loop a-reduced (bitwise-arithmetic-shift-right b 1) new-result))))))

;;; Raw GF(2^n) inversion using extended Euclidean algorithm
(define (gf2n-inv-raw a n irred)
  (if (= a 0)
      (error 'gf2n-inv "cannot invert zero")
      (let loop ([r0 irred] [r1 a] [s0 0] [s1 1])
        (if (= r1 0)
            s0  ; This shouldn't happen for valid a
            (if (= r1 1)
                s1
                (let* ([deg-r0 (gf2n-bit-length r0)]
                       [deg-r1 (gf2n-bit-length r1)]
                       [shift (- deg-r0 deg-r1)])
                  (if (< shift 0)
                      ;; Swap
                      (loop r1 r0 s1 s0)
                      (let* ([r1-shifted (bitwise-arithmetic-shift-left r1 shift)]
                             [s1-shifted (bitwise-arithmetic-shift-left s1 shift)]
                             [new-r0 (bitwise-xor r0 r1-shifted)]
                             [new-s0 (bitwise-xor s0 s1-shifted)])
                        (loop new-r0 r1 new-s0 s1)))))))))

;;; Bit length (degree + 1) of a polynomial represented as integer
(define (gf2n-bit-length x)
  (if (= x 0)
      0
      (+ 1 (gf2n-bit-length (bitwise-arithmetic-shift-right x 1)))))

;;; gf2n-from-int : Field × Int → Element
;;; Create a GF(2^n) element from an integer.
(define (gf2n-from-int field val)
  (doc 'export #t)
  (let ([sample (field-one field)])
    (if (gf2n-element? sample)
        (make-gf2n-element val (gf2n-degree sample) (gf2n-irred sample))
        (error 'gf2n-from-int "field is not GF(2^n)"))))

;;; gf2n-to-int : Element → Int
;;; Extract integer value from GF(2^n) element.
(define (gf2n-to-int element)
  (doc 'export #t)
  (if (gf2n-element? element)
      (gf2n-value element)
      (error 'gf2n-to-int "not a GF(2^n) element")))

;;; ============================================================================
;;; Section 4: Irreducible Polynomial Utilities
;;; ============================================================================

(doc 'section 'irreducible-polynomials)

(doc 'note "Utilities for working with irreducible polynomials.")
(doc 'note "An irreducible polynomial cannot be factored into non-trivial factors.")
(doc 'note "Uses Rabin's irreducibility test for O(d² log p) complexity over GF(p).")

;;; poly-power-mod : Polynomial × Int × Polynomial → Polynomial
;;; Compute p^n mod m using repeated squaring with reduction at each step.
;;; Tail-recursive to handle cryptographic-sized exponents without stack overflow.
(define (poly-power-mod p n m)
  (doc 'export #t)
  (doc 'type '(-> Polynomial Int Polynomial Polynomial))
  (doc 'description "Modular polynomial exponentiation: p^n mod m")
  (doc 'complexity "O(log n) multiplications, each followed by reduction mod m")
  (let ([F (poly-field p)]
        [base (poly-mod p m)])
    (if (= n 0)
        (poly-one-over F)
        ;; Tail-recursive square-and-multiply with accumulator
        (let loop ([b base] [e n] [acc (poly-one-over F)])
          (cond
            [(= e 0) acc]
            [(odd? e)
             (loop (poly-mod (poly-mul b b) m)
                   (quotient e 2)
                   (poly-mod (poly-mul acc b) m))]
            [else
             (loop (poly-mod (poly-mul b b) m)
                   (quotient e 2)
                   acc)])))))

;;; unique-prime-divisors : Nat → (List Nat)
;;; Get unique prime divisors of n (uses factorize from primality.ss)
(define (unique-prime-divisors n)
  (if (<= n 1)
      '()
      (let loop ([factors (factorize n)] [seen '()])
        (if (null? factors)
            (reverse seen)
            (let ([p (car factors)])
              (if (member p seen)
                  (loop (cdr factors) seen)
                  (loop (cdr factors) (cons p seen))))))))

;;; poly-frobenius-power : Polynomial × Int × Int × Polynomial → Polynomial
;;; Compute x^(q^k) mod f by iterating Frobenius: x → x^q → (x^q)^q → ...
;;; This avoids materializing the huge integer q^k.
;;; Much more efficient than (poly-power-mod x (expt q k) f) for large q, k.
(define (poly-frobenius-power x q k f)
  (doc 'export #t)
  (doc 'type '(-> Polynomial Int Int Polynomial Polynomial))
  (doc 'description "Compute x^(q^k) mod f via iterated Frobenius map")
  (doc 'complexity "O(k * log(q) * d²) where d = deg(f)")
  (doc 'note "Avoids computing q^k as integer; iterates x → x^q k times")
  (let loop ([result x] [i 0])
    (if (>= i k)
        result
        (loop (poly-power-mod result q f) (+ i 1)))))

;;; rabin-irreducible? : Polynomial × Int → Boolean
;;; Rabin's irreducibility test for polynomial f over finite field F_q.
;;; Much faster than trial division: O(d² log q) vs O(q^(d/2)).
;;;
;;; Algorithm: For f(x) of degree d over F_q, f is irreducible iff:
;;; 1. For each prime p | d: gcd(x^(q^(d/p)) - x, f) = 1
;;; 2. x^(q^d) ≡ x (mod f)
;;;
;;; NOTE: q must be the field ORDER (not characteristic). For GF(p), q=p.
;;; For extension fields GF(p^k), q=p^k.
(define (rabin-irreducible? f q)
  (doc 'export #t)
  (doc 'type '(-> Polynomial Int Boolean))
  (doc 'description "Rabin's fast irreducibility test for polynomials over F_q")
  (doc 'complexity "O(d² * d * log q) field operations where d = deg(f)")
  (doc 'note "q must be field ORDER, not characteristic")
  (let* ([F (poly-field f)]
         [d (poly-degree f)]
         [x (make-polynomial F (list (field-zero F) (field-one F)))]  ; x = [0, 1]
         [neg-x (poly-neg x)]  ; -x for computing x^k - x
         [prime-divs (unique-prime-divisors d)])
    ;; Check condition 1: for each prime p | d, gcd(x^(q^(d/p)) - x, f) = 1
    ;; Use Frobenius iteration to avoid computing q^(d/p) as integer
    (and (let loop ([ps prime-divs])
           (or (null? ps)
               (let* ([p (car ps)]
                      [exp (quotient d p)]       ; d/p
                      ;; x^(q^(d/p)) via Frobenius iteration
                      [x-qk (poly-frobenius-power x q exp f)]
                      [diff (poly-add x-qk neg-x)]    ; x^(q^(d/p)) - x
                      [g (poly-gcd diff f)])
                 (and (= (poly-degree g) 0)      ; gcd = constant (coprime)
                      (loop (cdr ps))))))
         ;; Check condition 2: x^(q^d) ≡ x (mod f)
         ;; Use Frobenius iteration: apply x → x^q exactly d times
         (let ([x-qd (poly-frobenius-power x q d f)])
           (poly-equal? x-qd x)))))

;;; poly-irreducible? : Polynomial → Boolean
;;; Test if a polynomial is irreducible over its coefficient field.
;;; Uses Rabin's test for finite fields (O(d² log q)), falls back to trial
;;; division for infinite fields or when field metadata unavailable.
(define (poly-irreducible? p)
  (doc 'export #t)
  (doc 'type '(-> Polynomial Boolean))
  (doc 'description "Test if polynomial is irreducible over its coefficient field")
  (doc 'note "Uses Rabin's O(d² log q) test for finite fields, trial division otherwise")
  (let* ([F (poly-field p)]
         [d (poly-degree p)])
    (cond
      [(< d 1) #f]  ; Constants are not irreducible
      [(= d 1) #t]  ; Linear polynomials are always irreducible
      [else
       ;; Try to use Rabin's test if we have field order info
       (let ([order (gf-order F)])
         (if (and order (> order 0))
             ;; Finite field with known order - use Rabin's fast test
             (rabin-irreducible? p order)
             ;; Unknown or infinite field - fall back to trial division
             (not (poly-has-factor-up-to-degree? p (quotient d 2) F))))])))

;;; Check if polynomial has a factor of degree 1 to max-deg
(define (poly-has-factor-up-to-degree? p max-deg F)
  (let ([elts (field-elements F)])
    (if (null? elts)
        ;; Infinite field - just check for roots (linear factors)
        (poly-has-root? p F)
        ;; Finite field - try all monic polynomials up to max-deg
        (let loop ([deg 1])
          (and (<= deg max-deg)
               (or (poly-has-monic-factor-of-degree? p deg F elts)
                   (loop (+ deg 1))))))))

;;; Check if polynomial has a root in the field
(define (poly-has-root? p F)
  (let ([elts (field-elements F)])
    (if (null? elts)
        #f  ; Can't enumerate infinite field
        (let loop ([es elts])
          (and (not (null? es))
               (or (field-equal? F (poly-eval p (car es)) (field-zero F))
                   (loop (cdr es))))))))

;;; Check if p has a monic factor of exact degree deg
(define (poly-has-monic-factor-of-degree? p deg F elts)
  (let loop ([coeffs-list (all-coeff-combinations elts deg)])
    (and (not (null? coeffs-list))
         (let* ([coeffs (car coeffs-list)]
                ;; Make monic polynomial of degree deg
                [monic-coeffs (append coeffs (list (field-one F)))]
                [divisor (make-polynomial F monic-coeffs)])
           (or (poly-divides? divisor p)
               (loop (cdr coeffs-list)))))))

;;; ============================================================================
;;; Section 5: Standard Irreducible Polynomials
;;; ============================================================================

(doc 'section 'standard-irreducibles)

(doc 'note "Standard irreducible polynomials for common field sizes.")
(doc 'note "These are well-known and used in cryptographic applications.")

;;; AES polynomial: x^8 + x^4 + x^3 + x + 1 for GF(2^8)
(define gf2-8-aes-irred #x11B)
(doc 'export #t)

;;; Rijndael/AES field GF(2^8)
(define (make-gf2-8-aes)
  (doc 'export #t)
  (doc 'description "Create GF(2^8) used by AES (Rijndael)")
  (make-gf2n 8 gf2-8-aes-irred))

;;; GF(2^4) with x^4 + x + 1 (used in some S-boxes)
(define gf2-4-irred #x13)  ; x^4 + x + 1 = 10011 = 0x13
(doc 'export #t)

(define (make-gf2-4)
  (doc 'export #t)
  (make-gf2n 4 gf2-4-irred))

;;; GF(2^16) with x^16 + x^12 + x^3 + x + 1
(define gf2-16-irred #x1100B)
(doc 'export #t)

(define (make-gf2-16)
  (doc 'export #t)
  (make-gf2n 16 gf2-16-irred))

;;; GF(2^32) with x^32 + x^22 + x^2 + x + 1
(define gf2-32-irred #x100400007)
(doc 'export #t)

(define (make-gf2-32)
  (doc 'export #t)
  (make-gf2n 32 gf2-32-irred))

;;; Standard irreducible for GF(p^2): x^2 + 1 (when -1 is not a square in GF(p))
;;; Works when p ≡ 3 (mod 4)
(define (make-gf-p2 p)
  (doc 'export #t)
  (doc 'description "Create GF(p^2) using x^2 + 1 (requires p ≡ 3 mod 4)")
  (let* ([base (gf-prime p)]
         [one (field-one base)]
         [zero (field-zero base)]
         ;; x^2 + 1
         [modulus (make-polynomial base (list one zero one))])
    (make-gf-extension p 2 modulus)))

;;; ============================================================================
;;; Section 6: Conversion Utilities
;;; ============================================================================

(doc 'section 'conversions)

;;; gf-ext-to-poly : Element → Polynomial
;;; Convert GF(p^n) element to polynomial representation.
(define (gf-ext-to-poly element)
  (doc 'export #t)
  (if (gf-ext-element? element)
      (make-polynomial (gf-ext-base-field element) (gf-ext-coeffs element))
      (error 'gf-ext-to-poly "not a GF(p^n) element")))

;;; gf-ext-from-poly : Polynomial × Field → Element
;;; Create GF(p^n) element from polynomial.
(define (gf-ext-from-poly poly gf-ext-field)
  (doc 'export #t)
  (let* ([sample (field-one gf-ext-field)]
         [n (length (field-elements (gf-ext-base-field sample)))])
    (error 'gf-ext-from-poly "not implemented yet")))  ; TODO

;;; gf2n-to-poly : Element → Polynomial
;;; Convert GF(2^n) element to polynomial over GF(2).
(define (gf2n-to-poly element)
  (doc 'export #t)
  (if (gf2n-element? element)
      (let* ([val (gf2n-value element)]
             [n (gf2n-degree element)]
             [gf2 (gf-prime 2)]
             [coeffs (map (lambda (i)
                            (if (bitwise-bit-set? val i) 1 0))
                          (iota n))])
        (make-polynomial gf2 coeffs))
      (error 'gf2n-to-poly "not a GF(2^n) element")))

;;; ============================================================================
;;; Section 7: Primitive Elements
;;; ============================================================================

(doc 'section 'primitive-elements)

(doc 'note "A primitive element (generator) of GF(q) is an element whose")
(doc 'note "powers generate all non-zero elements. GF(q)* is cyclic of order q-1.")

;;; gf-primitive? : Field × Element → Boolean
;;; Test if element is a primitive element (generator) of the multiplicative group.
(define (gf-primitive? F element)
  (doc 'export #t)
  (doc 'type '(-> Field Element Boolean))
  (let* ([q (gf-order F)]
         [q-1 (- q 1)]
         [one (field-one F)]
         [eq-fn (field-equal-fn F)])
    ;; Element is primitive iff element^d ≠ 1 for all proper divisors d of q-1
    (let ([divisors (proper-divisors q-1)])
      (not (any? (lambda (d)
                   (eq-fn (field-power F element d) one))
                 divisors)))))

;;; Find proper divisors of n (all d where 1 ≤ d < n and d | n)
(define (proper-divisors n)
  (if (< n 2)
      '()
      (filter (lambda (d) (= 0 (modulo n d)))
              (cdr (iota n)))))  ; (1 2 ... n-1)

;;; any? predicate
(define (any? pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (any? pred (cdr lst)))))

;;; filter
(define (filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
    [else (filter pred (cdr lst))]))

;;; gf-find-primitive : Field → Element
;;; Find a primitive element of the field by exhaustive search.
(define (gf-find-primitive F)
  (doc 'export #t)
  (doc 'type '(-> Field Element))
  (let ([elts (field-elements F)]
        [zero (field-zero F)]
        [eq-fn (field-equal-fn F)])
    (let loop ([es elts])
      (cond
        [(null? es) (error 'gf-find-primitive "no primitive element found")]
        [(eq-fn (car es) zero) (loop (cdr es))]
        [(gf-primitive? F (car es)) (car es)]
        [else (loop (cdr es))]))))

;;; ============================================================================
;;; Section 8: Minimal Polynomials
;;; ============================================================================

(doc 'section 'minimal-polynomials)

(doc 'note "The minimal polynomial of an element α in GF(p^n) over GF(p)")
(doc 'note "is the monic polynomial of smallest degree with α as a root.")

;;; gf-minimal-poly : Field × Element × Field → Polynomial
;;; Compute minimal polynomial of element over base field.
;;; element is in extension field, result is polynomial over base field.
(define (gf-minimal-poly ext-field element base-field)
  (doc 'export #t)
  (doc 'type '(-> Field Element Field Polynomial))
  (doc 'description "Compute minimal polynomial of extension field element over base")
  (let* ([p (gf-characteristic base-field)]
         [one (field-one ext-field)]
         [mul (field-mul-op ext-field)]
         [eq-fn (field-equal-fn ext-field)])
    ;; Collect conjugates: α, α^p, α^{p^2}, ... until we cycle
    (let loop ([current element] [conjugates (list element)] [seen (list element)])
      (let ([next (field-power ext-field current p)])
        (if (any? (lambda (x) (eq-fn next x)) seen)
            ;; Found all conjugates, build minimal polynomial
            (gf-poly-from-roots ext-field base-field (reverse conjugates))
            (loop next (cons next conjugates) (cons next seen)))))))

;;; Build polynomial from roots
(define (gf-poly-from-roots ext-field base-field roots)
  ;; Product of (x - r) for each root r
  ;; The result has coefficients in base-field
  (let* ([one-ext (field-one ext-field)]
         [neg-ext (field-neg-fn ext-field)])
    (fold-left
     (lambda (acc root)
       (poly-mul acc (make-polynomial ext-field (list (neg-ext root) one-ext))))
     (poly-one-over ext-field)
     roots)))

;;; ============================================================================
;;; Section 9: Trace and Norm
;;; ============================================================================

(doc 'section 'trace-and-norm)

(doc 'note "Trace and norm are fundamental maps from GF(p^n) to GF(p).")
(doc 'note "Trace(α) = α + α^p + α^{p^2} + ... + α^{p^{n-1}}")
(doc 'note "Norm(α) = α × α^p × α^{p^2} × ... × α^{p^{n-1}}")

;;; gf-trace : Field × Element × Nat → Element
;;; Compute trace of element from GF(p^n) to GF(p).
(define (gf-trace ext-field element n)
  (doc 'export #t)
  (doc 'type '(-> Field Element Nat Element))
  (let* ([p (gf-characteristic ext-field)]
         [add (field-add-op ext-field)]
         [zero (field-zero ext-field)])
    (let loop ([i 0] [current element] [sum zero])
      (if (= i n)
          sum
          (loop (+ i 1)
                (field-power ext-field current p)
                (add sum current))))))

;;; gf-norm : Field × Element × Nat → Element
;;; Compute norm of element from GF(p^n) to GF(p).
(define (gf-norm ext-field element n)
  (doc 'export #t)
  (doc 'type '(-> Field Element Nat Element))
  (let* ([p (gf-characteristic ext-field)]
         [mul (field-mul-op ext-field)]
         [one (field-one ext-field)])
    (let loop ([i 0] [current element] [prod one])
      (if (= i n)
          prod
          (loop (+ i 1)
                (field-power ext-field current p)
                (mul prod current))))))
