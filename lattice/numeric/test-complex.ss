;;; fabric/stitches/test-complex.ss — Tests for Complex Numbers

(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")
(load "lattice/numeric/complex.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

(define (test-approx name expected actual tolerance)
  (let ([diff (if (complex? actual)
                  (complex-magnitude (complex-sub expected actual))
                  (abs (- expected actual)))])
       (if (< diff tolerance)
           (begin
            (set! tests-passed (+ tests-passed 1))
            (printf "  [PASS] ~a\n" name))
           (begin
            (set! tests-failed (+ tests-failed 1))
            (printf "  [FAIL] ~a\n" name)
            (printf "      Expected: ~s (±~a)\n" expected tolerance)
            (printf "      Got:      ~s\n" actual)
            (printf "      Diff:     ~a\n" diff)))))

(printf "\n=== Complex Number Tests ===\n\n")

;;; ============================================================
;;; Construction and Accessors
;;; ============================================================

(printf "--- Construction and Accessors ---\n")

(let ([z (make-complex 3 4)])
     (test "make-complex creates complex" #t (complex? z))
     (test "complex-real extracts real" 3 (complex-real z))
     (test "complex-imag extracts imag" 4 (complex-imag z)))

(test "real-num->complex lifts real"
      (make-complex 5 0)
      (real-num->complex 5))

(test "i-value creates i"
      (make-complex 0 1)
      (i-value))

;;; ============================================================
;;; Polar Form
;;; ============================================================

(printf "\n--- Polar Form ---\n")

(let ([z (make-complex 1 1)])
     (test-approx "magnitude of 1+i"
                  (sqrt 2)
                  (complex-magnitude z)
                  1e-10)
     (test-approx "angle of 1+i"
                  (/ (pi-value) 4)
                  (complex-angle z)
                  1e-10))

(test-approx "make-polar creates complex"
             (make-complex 1 1)
             (make-polar (sqrt 2) (/ (pi-value) 4))
             1e-10)

(let ([z (make-complex 3 4)])
     (test "magnitude of 3+4i" 5 (complex-magnitude z)))

;;; ============================================================
;;; Basic Arithmetic
;;; ============================================================

(printf "\n--- Basic Arithmetic ---\n")

(test "addition: (1+2i) + (3+4i)"
      (make-complex 4 6)
      (complex-add (make-complex 1 2) (make-complex 3 4)))

(test "subtraction: (5+7i) - (2+3i)"
      (make-complex 3 4)
      (complex-sub (make-complex 5 7) (make-complex 2 3)))

(test "multiplication: (1+i) * (1-i)"
      (make-complex 2 0)
      (complex-mul (make-complex 1 1) (make-complex 1 -1)))

(test "multiplication: (2+3i) * (4+5i)"
      (make-complex -7 22)
      (complex-mul (make-complex 2 3) (make-complex 4 5)))

(test-approx "division: (8+6i) / (3+i)"
             (make-complex 3 1)
             (complex-div (make-complex 8 6) (make-complex 3 1))
             1e-10)

(test "negation: -(3+4i)"
      (make-complex -3 -4)
      (complex-negate (make-complex 3 4)))

(test "conjugate: conj(3+4i)"
      (make-complex 3 -4)
      (complex-conjugate (make-complex 3 4)))

;;; ============================================================
;;; Properties
;;; ============================================================

(printf "\n--- Arithmetic Properties ---\n")

(let ([z (make-complex 3 4)])
     (test-approx "z * conj(z) = |z|²"
                  (make-complex 25 0)
                  (complex-mul z (complex-conjugate z))
                  1e-10))

(let ([z1 (make-complex 1 2)]
      [z2 (make-complex 3 4)])
     (test "commutativity: z1 + z2 = z2 + z1"
           (complex-add z1 z2)
           (complex-add z2 z1))
     (test "commutativity: z1 * z2 = z2 * z1"
           (complex-mul z1 z2)
           (complex-mul z2 z1)))

(let ([z (make-complex 3 4)])
     (test "additive identity: z + 0 = z"
           z
           (complex-add z (complex-zero)))
     (test "multiplicative identity: z * 1 = z"
           z
           (complex-mul z (complex-one))))

;;; ============================================================
;;; Comparison
;;; ============================================================

(printf "\n--- Comparison ---\n")

(test "equal: (3+4i) = (3+4i)"
      #t
      (complex-equal? (make-complex 3 4) (make-complex 3 4)))

(test "not equal: (3+4i) ≠ (3+5i)"
      #f
      (complex-equal? (make-complex 3 4) (make-complex 3 5)))

(test "zero?: 0+0i is zero"
      #t
      (complex-zero? (make-complex 0 0)))

(test "zero?: 1+0i is not zero"
      #f
      (complex-zero? (make-complex 1 0)))

(test "real?: 5+0i is real"
      #t
      (complex-real? (make-complex 5 0)))

(test "real?: 5+1i is not real"
      #f
      (complex-real? (make-complex 5 1)))

;;; ============================================================
;;; Exponential and Logarithm
;;; ============================================================

(printf "\n--- Exponential and Logarithm ---\n")

(test-approx "exp(iπ) = -1 (Euler's identity)"
             (make-complex -1 0)
             (complex-exp (make-complex 0 (pi-value)))
             1e-10)

(test-approx "exp(0) = 1"
             (make-complex 1 0)
             (complex-exp (complex-zero))
             1e-10)

(test-approx "log(1) = 0"
             (make-complex 0 0)
             (complex-log (complex-one))
             1e-10)

(test-approx "log(e) = 1"
             (make-complex 1 0)
             (complex-log (complex-e))
             1e-10)

(let ([z (make-complex 2 3)])
     (test-approx "log(exp(z)) = z"
                  z
                  (complex-log (complex-exp z))
                  1e-10))

;;; ============================================================
;;; Powers and Roots
;;; ============================================================

(printf "\n--- Powers and Roots ---\n")

(test-approx "sqrt(i) in first quadrant"
             (make-complex (sqrt 0.5) (sqrt 0.5))
             (complex-sqrt (make-complex 0 1))
             1e-10)

(test-approx "sqrt(4) = 2"
             (make-complex 2 0)
             (complex-sqrt (make-complex 4 0))
             1e-10)

(test-approx "sqrt(-1) = i"
             (make-complex 0 1)
             (complex-sqrt (make-complex -1 0))
             1e-10)

(test-approx "i² = -1"
             (make-complex -1 0)
             (complex-pow (complex-i) (make-complex 2 0))
             1e-10)

(test-approx "2³ = 8"
             (make-complex 8 0)
             (complex-pow (make-complex 2 0) (make-complex 3 0))
             1e-10)

;;; Edge cases for 0^w
(printf "\n--- Powers of Zero (Edge Cases) ---\n")

;; 0^0 should error
(let ([caught #f])
     (guard (ex [else (set! caught #t)])
            (complex-pow (complex-zero) (complex-zero)))
     (test "0^0 raises error" #t caught))

;; 0^(-1) should error
(let ([caught #f])
     (guard (ex [else (set! caught #t)])
            (complex-pow (complex-zero) (make-complex -1 0)))
     (test "0^(-1) raises error" #t caught))

;; 0^(-2) should error
(let ([caught #f])
     (guard (ex [else (set! caught #t)])
            (complex-pow (complex-zero) (make-complex -2 0)))
     (test "0^(-2) raises error" #t caught))

;; 0^(-1+i) should error (negative real part)
(let ([caught #f])
     (guard (ex [else (set! caught #t)])
            (complex-pow (complex-zero) (make-complex -1 1)))
     (test "0^(-1+i) raises error" #t caught))

;; 0^(-0.5) should error
(let ([caught #f])
     (guard (ex [else (set! caught #t)])
            (complex-pow (complex-zero) (make-complex -0.5 0)))
     (test "0^(-0.5) raises error" #t caught))

;; 0^1 should return 0
(test "0^1 = 0"
      (complex-zero)
      (complex-pow (complex-zero) (make-complex 1 0)))

;; 0^2 should return 0
(test "0^2 = 0"
      (complex-zero)
      (complex-pow (complex-zero) (make-complex 2 0)))

;; 0^i is undefined (oscillates on unit circle); returns NaN
(let ([result (complex-pow (complex-zero) (make-complex 0 1))])
     (test "0^i is undefined (NaN)"
           #t
           (and (nan? (complex-real result))
                (nan? (complex-imag result)))))

;; 0^(1+i) should return 0 (real part is positive)
(test "0^(1+i) = 0"
      (complex-zero)
      (complex-pow (complex-zero) (make-complex 1 1)))

;;; ============================================================
;;; Trigonometric Functions
;;; ============================================================

(printf "\n--- Trigonometric Functions ---\n")

(test-approx "sin(0) = 0"
             (complex-zero)
             (complex-sin (complex-zero))
             1e-10)

(test-approx "cos(0) = 1"
             (complex-one)
             (complex-cos (complex-zero))
             1e-10)

(test-approx "sin(π/2) = 1"
             (make-complex 1 0)
             (complex-sin (make-complex (/ (pi-value) 2) 0))
             1e-10)

(let ([z (make-complex 1 2)])
     (test-approx "sin²(z) + cos²(z) = 1 (Pythagorean identity)"
                  (complex-one)
                  (complex-add (complex-pow (complex-sin z) (make-complex 2 0))
                               (complex-pow (complex-cos z) (make-complex 2 0)))
                  1e-8))

;;; ============================================================
;;; Hyperbolic Functions
;;; ============================================================

(printf "\n--- Hyperbolic Functions ---\n")

(test-approx "sinh(0) = 0"
             (complex-zero)
             (complex-sinh (complex-zero))
             1e-10)

(test-approx "cosh(0) = 1"
             (complex-one)
             (complex-cosh (complex-zero))
             1e-10)

(let ([z (make-complex 1 2)])
     (test-approx "cosh²(z) - sinh²(z) = 1 (hyperbolic identity)"
                  (complex-one)
                  (complex-sub (complex-pow (complex-cosh z) (make-complex 2 0))
                               (complex-pow (complex-sinh z) (make-complex 2 0)))
                  1e-8))

;;; ============================================================
;;; Euler's Relations
;;; ============================================================

(printf "\n--- Euler's Relations ---\n")

;; Test relation between sin and exp using Euler's formula
;; sin(z) = (e^(iz) - e^(-iz)) / (2i)
(let* ([z (make-complex 1 0)]  ; Use real number for simpler test
       [iz (complex-mul (complex-i) z)]
       [neg-iz (complex-negate iz)]
       [exp-iz (complex-exp iz)]
       [exp-neg-iz (complex-exp neg-iz)]
       [numerator (complex-sub exp-iz exp-neg-iz)]
       [denominator (make-complex 0 2)]
       [result (complex-div numerator denominator)])
      (test-approx "sin(1) via Euler's formula"
                   (complex-sin z)
                   result
                   1e-10))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(printf "\n--- Utility Functions ---\n")

(test "complex->string: 3+4i"
      "3+4i"
      (complex->string (make-complex 3 4)))

(test "complex->string: 3-4i"
      "3-4i"
      (complex->string (make-complex 3 -4)))

(test "complex->string: 5+0i"
      "5"
      (complex->string (make-complex 5 0)))

(test "complex->string: 0+3i"
      "3i"
      (complex->string (make-complex 0 3)))

(test "complex-scale: 2 * (3+4i)"
      (make-complex 6 8)
      (complex-scale 2 (make-complex 3 4)))

(test "hermitian product: conj(1+2i) * (3+4i) = (1-2i) * (3+4i) = 11-2i"
      (make-complex 11 -2)
      (complex-hermitian-product (make-complex 1 2) (make-complex 3 4)))

;;; ============================================================
;;; Constants
;;; ============================================================

(printf "\n--- Constants ---\n")

(test "complex-zero" (make-complex 0 0) (complex-zero))
(test "complex-one" (make-complex 1 0) (complex-one))
(test "complex-i" (make-complex 0 1) (complex-i))
(test-approx "complex-pi" (make-complex (pi-value) 0) (complex-pi) 1e-10)
(test-approx "complex-e" (make-complex (e-value) 0) (complex-e) 1e-10)

;;; ============================================================
;;; Results
;;; ============================================================

(printf "\n================================================================\n")
(printf "                    TEST RESULTS\n")
(printf "================================================================\n\n")
(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n[SUCCESS] All complex number tests passed.\n")
    (printf "\n[FAILURE] Some tests failed.\n"))
