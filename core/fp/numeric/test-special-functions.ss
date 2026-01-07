;;; core/fp/numeric/test-special-functions.ss — Tests for Special Functions
;;;
;;; Test suite for error functions, gamma functions, and related
;;; special mathematical functions.

(load "core/base/prelude.ss")
(load "core/fp/numeric/special-functions.ss")

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

(define (test-equal name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~a\n" expected)
       (printf "      Got:      ~a\n" actual))))

(printf "\n=== Special Functions Tests ===\n\n")

;;; ============================================================
;;; Error Function Tests
;;; ============================================================

(printf "--- Error Functions ---\n")

(test "erf(0) = 0" 0.0 (erf 0) 1e-8)
(test "erf(1) ≈ 0.8427" 0.8427007929497149 (erf 1) 1e-4)
(test "erf(2) ≈ 0.9953" 0.9953222650189527 (erf 2) 1e-4)
(test "erf(-1) = -erf(1)" (- (erf 1)) (erf -1) 1e-10)
(test "erf(5) ≈ 1" 1.0 (erf 5) 1e-6)

(printf "\n--- Complementary Error Function ---\n")

(test "erfc(0) = 1" 1.0 (erfc 0) 1e-8)
(test "erfc(1) ≈ 0.1573" 0.15729920705028513 (erfc 1) 1e-4)
(test "erf(x) + erfc(x) = 1" 1.0 (+ (erf 1.5) (erfc 1.5)) 1e-10)
(test "erf(x) + erfc(x) = 1 (x=2)" 1.0 (+ (erf 2) (erfc 2)) 1e-10)

(printf "\n--- Inverse Error Function ---\n")

(test "erfinv(0) = 0" 0.0 (erfinv 0) 1e-10)
(test "erf(erfinv(0.5)) = 0.5" 0.5 (erf (erfinv 0.5)) 1e-4)
(test "erf(erfinv(0.9)) ≈ 0.9" 0.9 (erf (erfinv 0.9)) 1e-3)
(test "erfinv(erf(0.5)) = 0.5" 0.5 (erfinv (erf 0.5)) 1e-4)

;;; ============================================================
;;; Gamma Function Tests
;;; ============================================================

(printf "\n--- Gamma Function ---\n")

(test "gamma(1) = 1" 1.0 (gamma 1) 1e-10)
(test "gamma(2) = 1" 1.0 (gamma 2) 1e-10)
(test "gamma(3) = 2 (2!)" 2.0 (gamma 3) 1e-10)
(test "gamma(4) = 6 (3!)" 6.0 (gamma 4) 1e-10)
(test "gamma(5) = 24 (4!)" 24.0 (gamma 5) 1e-8)
(test "gamma(6) = 120 (5!)" 120.0 (gamma 6) 1e-6)
(test "gamma(0.5) = √π" *sqrt-pi* (gamma 0.5) 1e-8)
(test "gamma(1.5) = √π/2" (/ *sqrt-pi* 2) (gamma 1.5) 1e-8)

(printf "\n--- Gamma Recurrence ---\n")

(test "gamma(x+1) = x*gamma(x)" (* 2.5 (gamma 2.5)) (gamma 3.5) 1e-8)
(test "gamma(4.7) = 3.7*gamma(3.7)" (* 3.7 (gamma 3.7)) (gamma 4.7) 1e-6)

(printf "\n--- Gamma Reflection ---\n")

(let* ((x 0.3)
       (lhs (* (gamma x) (gamma (- 1 x))))
       (rhs (/ 3.141592653589793 (sin (* 3.141592653589793 x)))))
      (test "gamma(x)*gamma(1-x) = π/sin(πx)" rhs lhs 1e-6))

;;; ============================================================
;;; Log-Gamma Tests
;;; ============================================================

(printf "\n--- Log-Gamma Function ---\n")

(test "lgamma(1) = 0" 0.0 (lgamma 1) 1e-10)
(test "lgamma(2) = 0" 0.0 (lgamma 2) 1e-10)
(test "lgamma(5) = log(24)" (log 24) (lgamma 5) 1e-8)
(test "lgamma(6) = log(120)" (log 120) (lgamma 6) 1e-8)
(test "lgamma consistent with gamma" (log (gamma 3.5)) (lgamma 3.5) 1e-8)
(test "lgamma consistent (5.7)" (log (gamma 5.7)) (lgamma 5.7) 1e-8)

;;; ============================================================
;;; Factorial Tests
;;; ============================================================

(printf "\n--- Factorial ---\n")

(test-equal "0! = 1" 1 (factorial 0))
(test-equal "1! = 1" 1 (factorial 1))
(test-equal "2! = 2" 2 (factorial 2))
(test-equal "3! = 6" 6 (factorial 3))
(test-equal "4! = 24" 24 (factorial 4))
(test-equal "5! = 120" 120 (factorial 5))
(test-equal "10! = 3628800" 3628800 (factorial 10))

;;; ============================================================
;;; Digamma Tests
;;; ============================================================

(printf "\n--- Digamma Function ---\n")

(test "digamma(1) = -γ" (- *euler-gamma*) (digamma 1) 1e-6)
(test "digamma recurrence" (+ (digamma 2) 0.5) (digamma 3) 1e-8)
(test "digamma recurrence (5)" (+ (digamma 4) 0.25) (digamma 5) 1e-8)

(let ((expected (- (- *euler-gamma*) (* 2 (log 2)))))
     (test "digamma(0.5) = -γ - 2*log(2)" expected (digamma 0.5) 1e-6))

;;; ============================================================
;;; Beta Function Tests
;;; ============================================================

(printf "\n--- Beta Function ---\n")

(test "beta(1,1) = 1" 1.0 (beta 1 1) 1e-10)
(test "beta symmetry: B(2,3) = B(3,2)" (beta 3 2) (beta 2 3) 1e-10)
(test "beta symmetry (1.5,2.5)" (beta 2.5 1.5) (beta 1.5 2.5) 1e-10)

(let* ((a 2.5)
       (b 3.5)
       (from-gamma (/ (* (gamma a) (gamma b)) (gamma (+ a b))))
       (from-beta (beta a b)))
      (test "beta(a,b) = Γ(a)Γ(b)/Γ(a+b)" from-gamma from-beta 1e-8))

(test "beta(2,1) = 1/2" 0.5 (beta 2 1) 1e-10)
(test "beta(3,1) = 1/3" (/ 1 3) (beta 3 1) 1e-10)
(test "beta(4,1) = 1/4" 0.25 (beta 4 1) 1e-10)
(test "beta(0.5,0.5) = π" 3.141592653589793 (beta 0.5 0.5) 1e-6)

;;; ============================================================
;;; Incomplete Gamma Tests
;;; ============================================================

(printf "\n--- Incomplete Gamma Functions ---\n")

(test "gammainc-lower(2,0) = 0" 0.0 (gammainc-lower 2 0) 1e-10)

(let ((a 3.0)
      (x 2.0))
     (test "lower + upper = gamma" (gamma a)
           (+ (gammainc-lower a x) (gammainc-upper a x)) 1e-6))

(let ((result (gammainc-reg 2 1)))
     (if (and (>= result 0) (<= result 1))
         (begin
          (set! tests-passed (+ tests-passed 1))
          (printf "  [PASS] gammainc-reg in [0,1]\n"))
         (begin
          (set! tests-failed (+ tests-failed 1))
          (printf "  [FAIL] gammainc-reg in [0,1]\n")
          (printf "      Got: ~a\n" result))))

(test "gammainc-reg(1,x) = 1 - e^(-x)" (- 1 (exp -2)) (gammainc-reg 1 2) 1e-6)

;;; ============================================================
;;; Incomplete Beta Tests
;;; ============================================================

(printf "\n--- Incomplete Beta Function ---\n")

(test "betainc(2,3,0) = 0" 0.0 (betainc 2 3 0) 1e-10)
(test "betainc(2,3,1) = 1" 1.0 (betainc 2 3 1) 1e-10)

(let ((a 2.0)
      (b 3.0)
      (x 0.4))
     (test "betainc symmetry" (- 1 (betainc b a (- 1 x))) (betainc a b x) 1e-6))

(test "betainc(1,1,x) = x (0.3)" 0.3 (betainc 1 1 0.3) 1e-6)
(test "betainc(1,1,x) = x (0.7)" 0.7 (betainc 1 1 0.7) 1e-6)

;;; ============================================================
;;; Binomial Coefficient Tests
;;; ============================================================

(printf "\n--- Binomial Coefficient ---\n")

(test-equal "C(5,0) = 1" 1 (binomial 5 0))
(test-equal "C(10,0) = 1" 1 (binomial 10 0))
(test-equal "C(5,5) = 1" 1 (binomial 5 5))
(test-equal "C(5,1) = 5" 5 (binomial 5 1))
(test-equal "C(10,1) = 10" 10 (binomial 10 1))
(test-equal "C(4,2) = 6" 6 (binomial 4 2))
(test-equal "C(5,2) = 10" 10 (binomial 5 2))
(test-equal "C(5,3) = 10" 10 (binomial 5 3))
(test-equal "C(6,3) = 20" 20 (binomial 6 3))
(test-equal "C(10,5) = 252" 252 (binomial 10 5))
(test-equal "C(n,k) = C(n,n-k)" (binomial 7 5) (binomial 7 2))

;;; ============================================================
;;; Pochhammer Symbol Tests
;;; ============================================================

(printf "\n--- Pochhammer Symbol ---\n")

(test "pochhammer(x,0) = 1" 1.0 (pochhammer 5 0) 1e-10)
(test "pochhammer(x,1) = x" 5.0 (pochhammer 5 1) 1e-10)
(test "pochhammer(1,n) = n!" 120.0 (pochhammer 1 5) 1e-8)
(test "pochhammer recurrence" (* (pochhammer 3 3) 6) (pochhammer 3 4) 1e-8)

;;; ============================================================
;;; Double Factorial Tests
;;; ============================================================

(printf "\n--- Double Factorial ---\n")

(test-equal "0!! = 1" 1 (double-factorial 0))
(test-equal "1!! = 1" 1 (double-factorial 1))
(test-equal "7!! = 105" 105 (double-factorial 7))
(test-equal "8!! = 384" 384 (double-factorial 8))
(test-equal "6!! = 48" 48 (double-factorial 6))

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "\n=== Test Summary ===\n")
(printf "Passed: ~a\n" tests-passed)
(printf "Failed: ~a\n" tests-failed)
(printf "Total:  ~a\n\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "All tests passed!\n\n")
    (printf "Some tests failed.\n\n"))
