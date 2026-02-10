;;; fabric/stitches/fp/test-transcendental.ss — Tests for Transcendental Functions

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~a (±~a)\n" expected tolerance)
       (printf "      Got:      ~a\n" actual)
       (printf "      Diff:     ~a\n" (abs (- expected actual))))))

(printf "\n=== Transcendental Functions Tests ===\n\n")

;;; ====
;;; Exponential Tests
;;; ====

(printf "--- Exponential Function ---\n")

(test "exp(0) = 1" 1.0 (exp-num 0) 1e-10)
(test "exp(1) ≈ e" 2.71828 (exp-num 1) 0.001)
(test "exp(2) ≈ 7.389" 7.389 (exp-num 2) 0.01)
(test "exp(-1) ≈ 0.36788" 0.36788 (exp-num -1) 0.001)

;;; ====
;;; Logarithm Tests
;;; ====

(printf "\n--- Natural Logarithm ---\n")

(test "log(1) = 0" 0.0 (log-num 1) 1e-10)
(test "log(e) ≈ 1" 1.0 (log-num 2.71828) 0.001)
(test "log(2) ≈ 0.6931" 0.6931 (log-num 2) 0.001)
(test "log(10) ≈ 2.3026" 2.3026 (log-num 10) 0.001)
(test "log2(8) = 3" 3.0 (log2 8) 1e-10)
(test "log10(100) = 2" 2.0 (log10 100) 1e-10)

;;; ====
;;; Inverse Property Tests
;;; ====

(printf "\n--- Inverse Properties ---\n")

(test "log(exp(1)) = 1" 1.0 (log-num (exp-num 1)) 1e-10)
(test "exp(log(5)) = 5" 5.0 (exp-num (log-num 5)) 1e-10)

;;; ====
;;; Trigonometric Tests
;;; ====

(printf "\n--- Trigonometric Functions ---\n")

(test "sin(0) = 0" 0.0 (sin-num 0) 1e-10)
(test "cos(0) = 1" 1.0 (cos-num 0) 1e-10)
(test "tan(0) = 0" 0.0 (tan-num 0) 1e-10)
(test "sin(π/2) ≈ 1" 1.0 (sin-num (/ (pi-value) 2)) 1e-10)
(test "cos(π) ≈ -1" -1.0 (cos-num (pi-value)) 1e-10)
(test "tan(π/4) ≈ 1" 1.0 (tan-num (/ (pi-value) 4)) 1e-10)

;;; ====
;;; Inverse Trigonometric Tests
;;; ====

(printf "\n--- Inverse Trigonometric Functions ---\n")

(test "asin(0) = 0" 0.0 (asin-num 0) 1e-10)
(test "acos(1) = 0" 0.0 (acos-num 1) 1e-10)
(test "atan(0) = 0" 0.0 (atan-num 0) 1e-10)
(test "asin(1) ≈ π/2" (/ (pi-value) 2) (asin-num 1) 1e-10)
(test "acos(0) ≈ π/2" (/ (pi-value) 2) (acos-num 0) 1e-10)
(test "atan(1) ≈ π/4" (/ (pi-value) 4) (atan-num 1) 1e-10)

;;; ====
;;; Pythagorean Identity
;;; ====

(printf "\n--- Pythagorean Identity ---\n")

(let* ([x 0.5]
       [s (sin-num x)]
       [c (cos-num x)])
      (test "sin²(0.5) + cos²(0.5) = 1" 1.0 (+ (* s s) (* c c)) 1e-10))

(let* ([x 1.0]
       [s (sin-num x)]
       [c (cos-num x)])
      (test "sin²(1.0) + cos²(1.0) = 1" 1.0 (+ (* s s) (* c c)) 1e-10))

;;; ====
;;; Hyperbolic Functions
;;; ====

(printf "\n--- Hyperbolic Functions ---\n")

(test "sinh(0) = 0" 0.0 (sinh-num 0) 1e-10)
(test "cosh(0) = 1" 1.0 (cosh-num 0) 1e-10)
(test "tanh(0) = 0" 0.0 (tanh-num 0) 1e-10)
(test "sinh(1) ≈ 1.1752" 1.1752 (sinh-num 1) 0.001)
(test "cosh(1) ≈ 1.5431" 1.5431 (cosh-num 1) 0.001)
(test "tanh(1) ≈ 0.7616" 0.7616 (tanh-num 1) 0.001)

;;; ====
;;; Inverse Hyperbolic Functions
;;; ====

(printf "\n--- Inverse Hyperbolic Functions ---\n")

(test "asinh(0) = 0" 0.0 (asinh-num 0) 1e-10)
(test "acosh(1) = 0" 0.0 (acosh-num 1) 1e-10)
(test "atanh(0) = 0" 0.0 (atanh-num 0) 1e-10)
(test "asinh(sinh(1)) = 1" 1.0 (asinh-num (sinh-num 1)) 1e-10)
(test "acosh(cosh(1)) = 1" 1.0 (acosh-num (cosh-num 1)) 1e-10)

;;; ====
;;; Hyperbolic Identity
;;; ====

(printf "\n--- Hyperbolic Identity ---\n")

(let* ([x 0.5]
       [s (sinh-num x)]
       [c (cosh-num x)])
      (test "cosh²(0.5) - sinh²(0.5) = 1" 1.0 (- (* c c) (* s s)) 1e-10))

;;; ====
;;; Mathematical Constants
;;; ====

(printf "\n--- Mathematical Constants ---\n")

(test "π ≈ 3.14159" 3.14159 (pi-value) 0.00001)
(test "e ≈ 2.71828" 2.71828 (e-value) 0.00001)
(test "τ ≈ 6.28318" 6.28318 (tau-value) 0.00001)
(test "φ ≈ 1.61803" 1.61803 (phi-value) 0.00001)

;;; ====
;;; Degree/Radian Conversion
;;; ====

(printf "\n--- Degree/Radian Conversion ---\n")

(test "180° = π rad" (pi-value) (deg->rad 180) 1e-10)
(test "90° = π/2 rad" (/ (pi-value) 2) (deg->rad 90) 1e-10)
(test "π rad = 180°" 180.0 (rad->deg (pi-value)) 1e-10)

;;; ====
;;; Compound Functions
;;; ====

(printf "\n--- Compound Functions ---\n")

(test "sec(0) = 1" 1.0 (sec-num 0) 1e-10)
(test "csc(π/2) = 1" 1.0 (csc-num (/ (pi-value) 2)) 1e-10)
(test "cot(π/4) ≈ 1" 1.0 (cot-num (/ (pi-value) 4)) 1e-10)
(test "sech(0) = 1" 1.0 (sech-num 0) 1e-10)

;;; ====
;;; Special Functions
;;; ====

(printf "\n--- Special Functions ---\n")

(test "sinc(0) = 1" 1.0 (sinc-num 0) 1e-10)
(test "sinc(1) ≈ 0" 0.0 (sinc-num 1) 0.01)
(test "expm1(0) = 0" 0.0 (expm1-num 0) 1e-10)
(test "log1p(0) = 0" 0.0 (log1p-num 0) 1e-10)
(test "expm1(1e-6) ≈ 1e-6" 1e-6 (expm1-num 1e-6) 1e-9)

;;; ====
;;; Power and Root Functions
;;; ====

(printf "\n--- Power and Root Functions ---\n")

(test "sqrt(4) = 2" 2.0 (sqrt-num 4) 1e-10)
(test "sqrt(2)² ≈ 2" 2.0 (pow-num (sqrt-num 2) 2) 1e-10)
(test "cbrt(8) = 2" 2.0 (cbrt-num 8) 1e-10)
(test "cbrt(-8) = -2" -2.0 (cbrt-num -8) 1e-10)
(test "2³ = 8" 8.0 (pow-num 2 3) 1e-10)

;;; ====
;;; Results
;;; ====

(printf "\n====\n")
(printf "                    TEST RESULTS\n")
(printf "====\n\n")
(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n[SUCCESS] All transcendental function tests passed.\n")
    (printf "\n[FAILURE] Some tests failed.\n"))
