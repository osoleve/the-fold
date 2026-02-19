;;; complex.sexp — Development tasks for lattice/numeric/complex.ss
;;;
;;; Module: complex (tier 0, depends on prelude, transcendental)
;;; 391 lines, ~41 exported functions
;;; Complex numbers in rectangular form (complex real imag).
;;; Arithmetic, polar conversion, transcendental, trigonometric functions.
;;;
;;; Git history:
;;;   710fc341 feat(module): migrate lattice/numeric/ to require
;;;   372ad8e7 docs(lattice): Convert multiple directories to doc forms

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement complex-mul
  ;; ──────────────────────────────────────────────
  ;; FOIL multiplication. Foundation for all higher operations.

  (task
    (id "numeric/complex/mul-impl")
    (module complex)
    (tier 0)
    (type implement)
    (description "Implement complex-mul: multiply two complex numbers. A complex number is (list 'complex real imag). Use the FOIL formula: (a + bi)(c + di) = (ac - bd) + (ad + bc)i. Extract parts with complex-real and complex-imag, construct result with make-complex.")
    (context "Available: prelude, make-complex (creates (list 'complex r i)), complex-real (extracts real part), complex-imag (extracts imaginary part). Standard arithmetic: +, -, *.")
    (before "(define (complex-mul z1 z2)
  (doc 'type '(-> Complex Complex Complex))
  (doc 'description \"Multiply two complex numbers\")
  ;; TODO: (a+bi)(c+di) = (ac-bd) + (ad+bc)i
  z1)")
    (test-file "lattice/numeric/test-complex.ss")
    (test-forms ("(let ([z (complex-mul (make-complex 1 2) (make-complex 3 4))])
  (assert-equal -5 (complex-real z))
  (assert-equal 10 (complex-imag z)))"
                 "(let ([z (complex-mul (make-complex 0 1) (make-complex 0 1))])
  (assert-equal -1 (complex-real z))
  (assert-equal 0 (complex-imag z)))"
                 "(let ([z (complex-mul (make-complex 2 0) (make-complex 3 0))])
  (assert-equal 6 (complex-real z))
  (assert-equal 0 (complex-imag z)))"))
    (available-modules (prelude))
    (hints ("Extract: a = (complex-real z1), b = (complex-imag z1), etc."
            "Real part: ac - bd"
            "Imaginary part: ad + bc"
            "Return (make-complex real-part imag-part)"))
    (reference-solution "(define (complex-mul z1 z2)
  (let ([a (complex-real z1)]
        [b (complex-imag z1)]
        [c (complex-real z2)]
        [d (complex-imag z2)])
       (make-complex (- (* a c) (* b d))
                     (+ (* a d) (* b c)))))")
    (alternatives ())
    (source-commit "710fc341")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement complex-div
  ;; ──────────────────────────────────────────────
  ;; Division by multiplying by conjugate over magnitude squared.

  (task
    (id "numeric/complex/div-impl")
    (module complex)
    (tier 0)
    (type implement)
    (description "Implement complex-div: divide two complex numbers. Formula: (a+bi)/(c+di) = [(ac+bd) + (bc-ad)i] / (c²+d²). Multiply numerator and denominator by the conjugate of the divisor. The denominator becomes c²+d² (a real number). If the denominator is 0, return an error using (error 'complex-div \"division by zero\" z2).")
    (context "Available: prelude (error, +, -, *, /), make-complex, complex-real, complex-imag.")
    (before "(define (complex-div z1 z2)
  (doc 'type '(-> Complex Complex Complex))
  (doc 'description \"Divide two complex numbers\")
  ;; TODO: multiply by conjugate, divide by magnitude squared
  z1)")
    (test-file "lattice/numeric/test-complex.ss")
    (test-forms ("(let ([z (complex-div (make-complex 1 0) (make-complex 0 1))])
  (assert-equal 0 (complex-real z))
  (assert-equal -1 (complex-imag z)))"
                 "(let ([z (complex-div (make-complex 3 4) (make-complex 1 2))])
  (assert-equal 11/5 (complex-real z))
  (assert-equal -2/5 (complex-imag z)))"
                 "(let ([z (complex-div (make-complex 6 0) (make-complex 3 0))])
  (assert-equal 2 (complex-real z))
  (assert-equal 0 (complex-imag z)))"))
    (available-modules (prelude))
    (hints ("Extract a, b from z1 and c, d from z2"
            "Denominator = c² + d²"
            "If denom = 0, raise error"
            "Real part of result = (ac + bd) / denom"
            "Imaginary part = (bc - ad) / denom"))
    (reference-solution "(define (complex-div z1 z2)
  (let ([a (complex-real z1)]
        [b (complex-imag z1)]
        [c (complex-real z2)]
        [d (complex-imag z2)])
       (let ([denom (+ (* c c) (* d d))])
            (if (= denom 0)
                (error 'complex-div \"division by zero\" z2)
                (make-complex (/ (+ (* a c) (* b d)) denom)
                              (/ (- (* b c) (* a d)) denom))))))")
    (alternatives ())
    (source-commit "710fc341")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement complex-exp (Euler's formula)
  ;; ──────────────────────────────────────────────

  (task
    (id "numeric/complex/exp-impl")
    (module complex)
    (tier 0)
    (type implement)
    (description "Implement complex-exp: the complex exponential function. Using Euler's formula: e^(a+bi) = e^a * (cos(b) + i*sin(b)). First compute e^a (the real exponential). Then multiply by cos(b) for the real part and by sin(b) for the imaginary part. The transcendental functions exp, cos, and sin are available from the transcendental module.")
    (context "Available: prelude, transcendental (exp, cos, sin), make-complex, complex-real, complex-imag.")
    (before "(define (complex-exp z)
  (doc 'type '(-> Complex Complex))
  (doc 'description \"Complex exponential: e^(a+bi) = e^a(cos b + i sin b)\")
  ;; TODO: Euler's formula
  z)")
    (test-file "lattice/numeric/test-complex.ss")
    (test-forms (";; e^0 = 1
(let ([z (complex-exp (make-complex 0 0))])
  (assert-true (< (abs (- (complex-real z) 1)) 1e-10))
  (assert-true (< (abs (complex-imag z)) 1e-10)))"
                 ";; e^(i*pi) ≈ -1 (Euler's identity)
(let ([z (complex-exp (make-complex 0 3.141592653589793))])
  (assert-true (< (abs (+ (complex-real z) 1)) 1e-10))
  (assert-true (< (abs (complex-imag z)) 1e-10)))"
                 ";; e^1 = e
(let ([z (complex-exp (make-complex 1 0))])
  (assert-true (< (abs (- (complex-real z) 2.718281828459045)) 1e-10)))"))
    (available-modules (prelude transcendental))
    (hints ("Extract a = real part, b = imaginary part"
            "Compute exp-a = (exp a)"
            "Real part = exp-a * (cos b)"
            "Imaginary part = exp-a * (sin b)"))
    (reference-solution "(define (complex-exp z)
  (let ([a (complex-real z)]
        [b (complex-imag z)])
       (let ([exp-a (exp a)])
            (make-complex (* exp-a (cos b))
                          (* exp-a (sin b))))))")
    (alternatives ())
    (source-commit "710fc341")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement complex-sqrt
  ;; ──────────────────────────────────────────────
  ;; Principal branch square root with case analysis.

  (task
    (id "numeric/complex/sqrt-impl")
    (module complex)
    (tier 0)
    (type implement)
    (description "Implement complex-sqrt: the principal branch square root. Given z = a + bi with magnitude r = sqrt(a² + b²): If b = 0 and a >= 0, return sqrt(a) + 0i. If b = 0 and a < 0, return 0 + sqrt(-a)i. Otherwise: real part = sqrt((r+a)/2), imaginary part = sign(b) * sqrt((r-a)/2) where sign(b) is +1 if b >= 0, else -1. Use complex-magnitude to compute r.")
    (context "Available: prelude, transcendental (sqrt), make-complex, complex-real, complex-imag, complex-magnitude (returns sqrt(a²+b²)).")
    (before "(define (complex-sqrt z)
  (doc 'type '(-> Complex Complex))
  (doc 'description \"Principal branch complex square root\")
  ;; TODO: case analysis on b=0, then general formula
  z)")
    (test-file "lattice/numeric/test-complex.ss")
    (test-forms (";; sqrt(4) = 2
(let ([z (complex-sqrt (make-complex 4 0))])
  (assert-equal 2 (complex-real z))
  (assert-equal 0 (complex-imag z)))"
                 ";; sqrt(-1) = i
(let ([z (complex-sqrt (make-complex -1 0))])
  (assert-true (< (abs (complex-real z)) 1e-10))
  (assert-equal 1 (complex-imag z)))"
                 ";; sqrt(i) ≈ (1+i)/sqrt(2)
(let ([z (complex-sqrt (make-complex 0 1))])
  (assert-true (< (abs (- (complex-real z) 0.7071067811865476)) 1e-10))
  (assert-true (< (abs (- (complex-imag z) 0.7071067811865476)) 1e-10)))"
                 ";; Verify: sqrt(z)^2 = z
(let* ([w (make-complex 3 4)]
       [s (complex-sqrt w)]
       [s2 (complex-mul s s)])
  (assert-true (< (abs (- (complex-real s2) 3)) 1e-10))
  (assert-true (< (abs (- (complex-imag s2) 4)) 1e-10)))"))
    (available-modules (prelude transcendental))
    (hints ("Extract a, b and compute r = (complex-magnitude z)"
            "Special case b = 0: if a >= 0, sqrt is real; if a < 0, sqrt is purely imaginary"
            "General case: re = sqrt((r+a)/2), im = sign(b)*sqrt((r-a)/2)"
            "sign(b) = (if (>= b 0) 1 -1)"))
    (reference-solution "(define (complex-sqrt z)
  (let* ([a (complex-real z)]
         [b (complex-imag z)]
         [r (complex-magnitude z)])
        (if (= b 0)
            (if (>= a 0)
                (make-complex (sqrt a) 0)
                (make-complex 0 (sqrt (- a))))
            (let ([re (sqrt (/ (+ r a) 2))]
                  [im (* (if (>= b 0) 1 -1) (sqrt (/ (- r a) 2)))])
                 (make-complex re im)))))")
    (alternatives ())
    (source-commit "710fc341")
    (difficulty 5))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Write tests — complex number identities
  ;; ──────────────────────────────────────────────

  (task
    (id "numeric/complex/write-tests")
    (module complex)
    (tier 0)
    (type test)
    (description "Write tests that verify fundamental complex number identities: (1) i² = -1, (2) conjugate(z1*z2) = conjugate(z1)*conjugate(z2), (3) |z1*z2| = |z1|*|z2| (magnitude is multiplicative), (4) z * conjugate(z) = |z|² (real number), (5) Euler's identity: e^(i*pi) + 1 = 0 (approximately). Use small concrete examples with make-complex.")
    (context "Available: full complex module (loaded via require). Use assert-equal for exact checks and assert-true with (< (abs ...) 1e-10) for floating-point comparisons. Do NOT use define-test, test-group, or run-all-tests. Constructors: make-complex, complex-real, complex-imag. Operations: complex-mul, complex-conjugate, complex-magnitude, complex-exp, complex-add.")
    (before ";; Write tests for complex number identities.
;; No identity tests exist yet.")
    (test-file #f)
    (test-forms ())
    (available-modules (prelude transcendental complex))
    (hints ("i² = -1: (complex-mul (make-complex 0 1) (make-complex 0 1)) should give real=-1, imag=0"
            "Conjugate distributes: conjugate(z1*z2) vs conjugate(z1)*conjugate(z2)"
            "z * conj(z) is real with value |z|²"
            "Euler: e^(iπ) = (complex-exp (make-complex 0 pi)) ≈ -1 + 0i"))
    (reference-solution ";; i² = -1
(let ([z (complex-mul (make-complex 0 1) (make-complex 0 1))])
  (assert-equal -1 (complex-real z))
  (assert-equal 0 (complex-imag z)))

;; Conjugate distributes over multiplication
(let* ([z1 (make-complex 2 3)]
       [z2 (make-complex 4 -1)]
       [lhs (complex-conjugate (complex-mul z1 z2))]
       [rhs (complex-mul (complex-conjugate z1) (complex-conjugate z2))])
  (assert-equal (complex-real lhs) (complex-real rhs))
  (assert-equal (complex-imag lhs) (complex-imag rhs)))

;; |z1 * z2| = |z1| * |z2|
(let* ([z1 (make-complex 3 4)]
       [z2 (make-complex 1 2)])
  (assert-true (< (abs (- (complex-magnitude (complex-mul z1 z2))
                           (* (complex-magnitude z1) (complex-magnitude z2))))
                   1e-10)))

;; z * conj(z) = |z|² (real number)
(let* ([z (make-complex 3 4)]
       [prod (complex-mul z (complex-conjugate z))])
  (assert-equal 25 (complex-real prod))
  (assert-equal 0 (complex-imag prod)))

;; Euler's identity: e^(iπ) + 1 ≈ 0
(let ([z (complex-exp (make-complex 0 3.141592653589793))])
  (assert-true (< (abs (+ (complex-real z) 1)) 1e-10))
  (assert-true (< (abs (complex-imag z)) 1e-10)))")
    (alternatives ())
    (source-commit #f)
    (difficulty 3))

) ;; end tasks
