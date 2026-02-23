;;; lattice/numeric/polynomial.ss — Numeric Polynomial Operations
;;; @module numeric/polynomial
;;; @requires prelude complex matrix matrix-decomp matrix-eigen hamt iteration

(require 'prelude)
(require 'complex)
(require 'matrix)
(require 'matrix-decomp)
(require 'matrix-eigen)
(require 'hamt)
(require 'iteration)

(doc 'module 'numeric/polynomial)
(doc 'description "Polynomial representation and operations for control theory, signal processing, and numerical analysis")
(doc 'note "Polynomials represented with coefficients in descending power order: p(x) = a_n*x^n + ... + a_0 stored as #(a_n ... a_0)")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Polynomial Representation
;;; ====

;;; Polynomials are stored as vectors of coefficients in descending
;;; power order. The leading coefficient is at index 0.
;;;
;;; Example: 3x^2 + 2x + 1 is stored as #(3 2 1)

;;; poly? : Any → Boolean
;;; Check if value is a polynomial.
(define (poly? p)
  (and (pair? p)
       (eq? (car p) 'poly)
       (vector? (cadr p))
       (> (vector-length (cadr p)) 0)))

;;; make-poly : Vector → Poly
;;; Create polynomial from coefficient vector (descending powers).
;;; Automatically strips leading zeros.
(define (make-poly coeffs)
  (let ([stripped (poly-strip-leading-zeros coeffs)])
       (list 'poly stripped)))

;;; poly-coeffs : Poly → Vector
;;; Get the coefficient vector.
(define (poly-coeffs p)
  (cadr p))

;;; poly-degree : Poly → Nat
;;; Get the degree of the polynomial.
;;; Note: zero polynomial has degree 0 by convention.
(define (poly-degree p)
  (max 0 (- (vector-length (poly-coeffs p)) 1)))

;;; poly-leading : Poly → Number
;;; Get the leading (highest-degree) coefficient.
(define (poly-leading p)
  (vector-ref (poly-coeffs p) 0))

;;; poly-coeff : Poly × Nat → Number
;;; Get coefficient of x^k (power notation, not index).
(define (poly-coeff p k)
  (let* ([coeffs (poly-coeffs p)]
         [n (vector-length coeffs)]
         [idx (- n k 1)])
        (if (or (< idx 0) (>= idx n))
            0
            (vector-ref coeffs idx))))

;;; poly-strip-leading-zeros : Vector → Vector
;;; Remove leading zero coefficients, preserving at least one element.
(define (poly-strip-leading-zeros coeffs)
  (let ([n (vector-length coeffs)])
       (let loop ([i 0])
            (cond
             [(>= i (- n 1)) (vector (vector-ref coeffs (- n 1)))]  ; Keep last
             [(not (zero? (vector-ref coeffs i)))
              (let ([len (- n i)])
                (vec-tabulate len j (vector-ref coeffs (+ i j))))]
             [else (loop (+ i 1))]))))

;;; ====
;;; Polynomial Construction
;;; ====

;;; poly-from-list : (List Number) → Poly
;;; Create polynomial from list of coefficients (descending powers).
(define (poly-from-list lst)
  (make-poly (list->vector lst)))

;;; poly-zero : → Poly
;;; The zero polynomial.
(define (poly-zero)
  (make-poly (vector 0)))

;;; poly-one : → Poly
;;; The constant polynomial 1.
(define (poly-one)
  (make-poly (vector 1)))

;;; poly-monomial : Number × Nat → Poly
;;; Create a monomial c*x^n.
(define (poly-monomial coeff degree)
  (if (zero? coeff)
      (poly-zero)
      (make-poly
        (vec-tabulate (+ degree 1) i
          (if (= i 0) coeff 0)))))

;;; poly-from-roots : (List Complex|Number) × [Number] → Poly
;;; Construct monic polynomial from its roots.
;;; Optional gain k multiplies the result: k*(x-r1)*(x-r2)*...
;;; Roots can be real numbers or complex numbers.
;;; Complex roots with nonzero imaginary parts must come in conjugate pairs.
(define (poly-from-roots roots . opts)
  (let ([gain (if (null? opts) 1 (car opts))])
       (if (null? roots)
           (make-poly (vector gain))
           (poly-scale (poly-from-roots-helper roots) gain))))

;;; poly-from-roots-helper : (List Complex|Number) → Poly
(define (poly-from-roots-helper roots)
  (if (null? roots)
      (make-poly (vector 1))
      (let ([r (car roots)]
            [rest (cdr roots)])
           (cond
            ;; Real number (not our complex type)
            [(and (number? r) (not (pair? r)))
             (poly-mul (make-poly (vector 1 (- r)))
                       (poly-from-roots-helper rest))]
            ;; Complex with zero imaginary part -> treat as real
            [(and (complex? r) (< (abs (complex-imag r)) 1e-10))
             (poly-mul (make-poly (vector 1 (- (complex-real r))))
                       (poly-from-roots-helper rest))]
            ;; Complex - expand (x-r)(x-r*) = x^2 - 2*Re(r)*x + |r|^2
            [(complex? r)
             (let* ([a (complex-real r)]
                    [b (complex-imag r)]
                    [quad (make-poly (vector 1 (* -2 a) (+ (* a a) (* b b))))]
                    ;; Remove conjugate from entire remaining list (not just next)
                    [is-conjugate? (lambda (c)
                                           (and (complex? c)
                                                (< (abs (- a (complex-real c))) 1e-10)
                                                (< (abs (+ b (complex-imag c))) 1e-10)))]
                    [next-rest (filter (lambda (c) (not (is-conjugate? c))) rest)])
                   (poly-mul quad (poly-from-roots-helper next-rest)))]
            [else
             (poly-from-roots-helper rest)]))))

;;; ====
;;; Polynomial Evaluation
;;; ====

;;; poly-eval : Poly × Number → Number
;;; Evaluate polynomial at real value x using Horner's method.
;;; p(x) = a_n*x^n + ... + a_0
;;;      = (...((a_n*x + a_{n-1})*x + a_{n-2})*x + ...) + a_0
(define (poly-eval p x)
  (let ([coeffs (poly-coeffs p)]
        [n (vector-length (poly-coeffs p))])
       (let loop ([i 0] [result 0])
            (if (= i n)
                result
                (loop (+ i 1) (+ (* result x) (vector-ref coeffs i)))))))

;;; poly-eval-complex : Poly × Complex → Complex
;;; Evaluate polynomial at complex value z using Horner's method.
(define (poly-eval-complex p z)
  (let ([coeffs (poly-coeffs p)]
        [n (vector-length (poly-coeffs p))])
       (let loop ([i 0] [result (make-complex 0 0)])
            (if (= i n)
                result
                (let ([c (vector-ref coeffs i)])
                     (loop (+ i 1)
                           (complex-add (complex-mul result z)
                                        (if (complex? c) c (make-complex c 0)))))))))

;;; ====
;;; Polynomial Arithmetic
;;; ====

;;; poly-add : Poly × Poly → Poly
;;; Add two polynomials.
(define (poly-add p1 p2)
  (let* ([c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)]
         [n1 (vector-length c1)]
         [n2 (vector-length c2)]
         [n (max n1 n2)]
         [result (make-vector n 0)])
        ;; Add c1 (right-aligned)
        (do ([i 0 (+ i 1)])
            ((= i n1))
            (vector-set! result (+ i (- n n1)) (vector-ref c1 i)))
        ;; Add c2 (right-aligned)
        (do ([i 0 (+ i 1)])
            ((= i n2))
            (let ([idx (+ i (- n n2))])
                 (vector-set! result idx (+ (vector-ref result idx) (vector-ref c2 i)))))
        (make-poly result)))

;;; poly-sub : Poly × Poly → Poly
;;; Subtract two polynomials.
(define (poly-sub p1 p2)
  (poly-add p1 (poly-scale p2 -1)))

;;; poly-scale : Poly × Number → Poly
;;; Multiply polynomial by scalar.
(define (poly-scale p k)
  (let ([coeffs (poly-coeffs p)])
    (make-poly
      (vec-tabulate (vector-length coeffs) i
        (* k (vector-ref coeffs i))))))

;;; poly-mul : Poly × Poly → Poly
;;; Multiply two polynomials (convolution of coefficients).
(define (poly-mul p1 p2)
  (let* ([c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)]
         [n1 (vector-length c1)]
         [n2 (vector-length c2)]
         [n (+ n1 n2 -1)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n1))
            (do ([j 0 (+ j 1)])
                ((= j n2))
                (let ([idx (+ i j)])
                     (vector-set! result idx
                                  (+ (vector-ref result idx)
                                     (* (vector-ref c1 i) (vector-ref c2 j)))))))
        (make-poly result)))

;;; poly-normalize : Poly → Poly
;;; Normalize to monic form (leading coefficient = 1).
(define (poly-normalize p)
  (let ([lead (poly-leading p)])
       (if (zero? lead)
           p
           (poly-scale p (/ 1 lead)))))

;;; poly-derivative : Poly → Poly
;;; Compute formal derivative of polynomial.
;;; For p(x) = a_n*x^n + ... + a_1*x + a_0,
;;; p'(x) = n*a_n*x^{n-1} + ... + a_1
(define (poly-derivative p)
  (let* ([coeffs (poly-coeffs p)]
         [n (vector-length coeffs)])
        (if (<= n 1)
            (poly-zero)  ; Constant → 0
            (let ([new-len (- n 1)])
                  ;; Derivative: coefficient a_k at x^k becomes k*a_k at x^{k-1}
                  ;; In descending order: coeff at index i is for x^{n-1-i}
                  ;; After derivative: coeff at index i for x^{n-2-i} = (n-1-i)*original[i]
                  (make-poly
                    (vec-tabulate new-len i
                      (let ([power (- n 1 i)])
                        (* power (vector-ref coeffs i)))))))))

;;; ====
;;; Polynomial Root Finding
;;; ====

;;; poly-roots : Poly → (List Complex) | Error
;;; Find roots of polynomial using companion matrix eigenvalues.
;;;
;;; For polynomial p(x) = x^n + a_{n-1}*x^{n-1} + ... + a_1*x + a_0
;;; the companion matrix is:
;;;   [ 0  0  0  ...  -a_0    ]
;;;   [ 1  0  0  ...  -a_1    ]
;;;   [ 0  1  0  ...  -a_2    ]
;;;   [ ...                   ]
;;;   [ 0  0  0  ...  -a_{n-1}]
;;;
;;; The eigenvalues of this matrix are the roots of the polynomial.
(define (poly-roots p)
  (let* ([pn (poly-normalize p)]  ; Make monic
         [coeffs (poly-coeffs pn)]
         [n (- (vector-length coeffs) 1)])  ; Degree
        (cond
         [(= n 0) '()]  ; Constant polynomial, no roots
         [(= n 1)       ; Linear: x + a_0 = 0 → x = -a_0
          (list (make-complex (- (vector-ref coeffs 1)) 0))]
         [(= n 2)       ; Quadratic: use formula
          (poly-roots-quadratic coeffs)]
         [else          ; General case: companion matrix
               (poly-roots-companion pn n)])))

;;; poly-roots-quadratic : Vector → (List Complex)
;;; Solve x^2 + bx + c = 0 (monic form).
(define (poly-roots-quadratic coeffs)
  (let* ([b (vector-ref coeffs 1)]
         [c (vector-ref coeffs 2)]
         [disc (- (* b b) (* 4 c))])
        (if (>= disc 0)
            ;; Real roots
            (let ([sqrt-disc (sqrt disc)])
                 (list (make-complex (/ (+ (- b) sqrt-disc) 2) 0)
                       (make-complex (/ (- (- b) sqrt-disc) 2) 0)))
            ;; Complex roots
            (let ([real-part (/ (- b) 2)]
                  [imag-part (/ (sqrt (- disc)) 2)])
                 (list (make-complex real-part imag-part)
                       (make-complex real-part (- imag-part)))))))

;;; poly-roots-companion : Poly × Nat → (List Complex)
;;; Find roots via companion matrix eigenvalues.
;;;
;;; Frobenius companion matrix for monic polynomial:
;;;   p(x) = x^n + c_{n-1}x^{n-1} + ... + c_1*x + c_0
;;;
;;;   C = | 0   1   0  ...  0       |
;;;       | 0   0   1  ...  0       |
;;;       | ...              ...    |
;;;       | 0   0   0  ...  1       |
;;;       |-c_0 -c_1 -c_2 ... -c_{n-1}|
;;;
;;; The eigenvalues of C are the roots of p(x).
(define (poly-roots-companion p n)
  (let* ([coeffs (poly-coeffs p)]
         ;; Build Frobenius companion matrix
         [C (make-matrix n n 0)])
        ;; Set superdiagonal to 1: C[i][i+1] = 1 for i = 0..n-2
        (do ([i 0 (+ i 1)])
            ((= i (- n 1)))
            (matrix-set! C i (+ i 1) 1))
        ;; Set last row to negative of coefficients (except leading)
        ;; coeffs = #(1 c_{n-1} c_{n-2} ... c_1 c_0) in descending order
        ;; Last row: [-c_0, -c_1, ..., -c_{n-1}]
        (do ([j 0 (+ j 1)])
            ((= j n))
            (let ([coeff-idx (- n j)])  ; Map column j to coefficient index
                 (matrix-set! C (- n 1) j (- (vector-ref coeffs coeff-idx)))))
        ;; Get eigenvalues
        (let ([eig-result (qr-algorithm-shifted C)])
             (cond
              [(and (pair? eig-result) (eq? (car eig-result) 'complex-eigenvalues))
               ;; Complex format: extract eigenvalues properly
               (poly-extract-complex-eigenvalues (cadr eig-result) (caddr eig-result))]
              [(vector? eig-result)
               ;; Real eigenvalues, convert to complex
               (map (lambda (x) (make-complex x 0)) (vector->list eig-result))]
              [else
               ;; Error case
               `(error poly-roots-failed ,eig-result)]))))

;;; poly-extract-complex-eigenvalues : Vector × List → (List Complex)
;;; Extract complex eigenvalues from QR algorithm result.
;;; eig-vec has real parts, complex-info has (index real imag) tuples.
(define (poly-extract-complex-eigenvalues eig-vec complex-info)
  (let* ([n (vector-length eig-vec)]
         ;; Mark indices that are part of complex pairs
         [complex-indices
          (fold-left
           (lambda (acc info)
             (let ([idx (car info)]
                   [real (cadr info)]
                   [imag (caddr info)])
               (hamt-assoc (+ idx 1) (cons real (- imag))
                           (hamt-assoc idx (cons real imag) acc))))
           hamt-empty
           complex-info)])
    ;; Build result list
    (let loop ([i 0] [result '()])
      (if (= i n)
          (reverse result)
          (let ([complex-pair (hamt-lookup i complex-indices)])
            (if complex-pair
                (loop (+ i 1) (cons (make-complex (car complex-pair) (cdr complex-pair)) result))
                (loop (+ i 1) (cons (make-complex (vector-ref eig-vec i) 0) result))))))))

;;; ====
;;; Utility Functions
;;; ====

;;; logspace : Number × Number × Nat → Vector
;;; Generate logarithmically spaced points from 10^start to 10^stop.
;;; Returns n points (inclusive of endpoints).
(define (logspace start stop n)
  (if (< n 2)
      (vector (expt 10 start))
      (let ([step (/ (- stop start) (- n 1))])
        (vec-tabulate n i
          (expt 10 (+ start (* i step)))))))

;;; linspace : Number × Number × Nat → Vector
;;; Generate linearly spaced points from start to stop.
;;; Returns n points (inclusive of endpoints).
(define (linspace start stop n)
  (if (< n 2)
      (vector start)
      (let ([step (/ (- stop start) (- n 1))])
        (vec-tabulate n i
          (+ start (* i step))))))

;;; ====
;;; Display
;;; ====

;;; poly->string : Poly → String
;;; Pretty-print polynomial.
(define (poly->string p)
  (let* ([coeffs (poly-coeffs p)]
         [n (vector-length coeffs)]
         [deg (- n 1)])
        (if (and (= n 1) (zero? (vector-ref coeffs 0)))
            "0"
            (let loop ([i 0] [first? #t] [result ""])
                 (if (= i n)
                     result
                     (let* ([c (vector-ref coeffs i)]
                            [power (- deg i)]
                            [term (poly-term->string c power first?)])
                           (loop (+ i 1)
                                 (and first? (string=? term ""))
                                 (string-append result term))))))))

;;; poly-term->string : Number × Nat × Boolean → String
(define (poly-term->string c power first?)
  (cond
   [(zero? c) ""]
   [(= power 0)
    (if first?
        (number->string c)
        (if (> c 0)
            (string-append " + " (number->string c))
            (string-append " - " (number->string (- c)))))]
   [(= power 1)
    (let ([coeff-str (if (= (abs c) 1) "" (number->string (abs c)))])
         (if first?
             (if (< c 0)
                 (string-append "-" coeff-str "x")
                 (string-append coeff-str "x"))
             (if (> c 0)
                 (string-append " + " coeff-str "x")
                 (string-append " - " coeff-str "x"))))]
   [else
    (let ([coeff-str (if (= (abs c) 1) "" (number->string (abs c)))])
         (if first?
             (if (< c 0)
                 (string-append "-" coeff-str "x^" (number->string power))
                 (string-append coeff-str "x^" (number->string power)))
             (if (> c 0)
                 (string-append " + " coeff-str "x^" (number->string power))
                 (string-append " - " coeff-str "x^" (number->string power)))))]))
