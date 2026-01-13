;;; lattice/number-theory/test-primality.ss — Tests for Primality and Factorization
;;;
;;; Comprehensive tests for primality testing and integer factorization.

(load "lattice/number-theory/primality.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  [PASS] ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "  [FAIL] ")
       (display name)
       (newline)
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (run-tests)
  (display "================================================================\n")
  (display "           PRIMALITY AND FACTORIZATION TEST SUITE\n")
  (display "================================================================\n\n")

  ;;; ============================================================
  ;;; Primality Testing: prime?
  ;;; ============================================================

  (display "--- Primality Testing (prime?) ---\n")

  ;; Edge cases
  (test-false "prime? 0 is false" (prime? 0))
  (test-false "prime? 1 is false" (prime? 1))
  (test-true "prime? 2 is true" (prime? 2))
  (test-true "prime? 3 is true" (prime? 3))
  (test-false "prime? 4 is false" (prime? 4))

  ;; Small primes
  (test-true "prime? 5" (prime? 5))
  (test-true "prime? 7" (prime? 7))
  (test-true "prime? 11" (prime? 11))
  (test-true "prime? 13" (prime? 13))
  (test-true "prime? 17" (prime? 17))
  (test-true "prime? 19" (prime? 19))
  (test-true "prime? 23" (prime? 23))
  (test-true "prime? 29" (prime? 29))
  (test-true "prime? 31" (prime? 31))

  ;; Small composites
  (test-false "prime? 6 (2×3)" (prime? 6))
  (test-false "prime? 9 (3²)" (prime? 9))
  (test-false "prime? 15 (3×5)" (prime? 15))
  (test-false "prime? 21 (3×7)" (prime? 21))
  (test-false "prime? 25 (5²)" (prime? 25))
  (test-false "prime? 27 (3³)" (prime? 27))

  ;; Larger primes
  (test-true "prime? 97" (prime? 97))
  (test-true "prime? 101" (prime? 101))
  (test-true "prime? 997" (prime? 997))
  (test-true "prime? 1009" (prime? 1009))

  ;; Larger composites
  (test-false "prime? 100 (2²×5²)" (prime? 100))
  (test-false "prime? 1001 (7×11×13)" (prime? 1001))
  (test-false "prime? 1024 (2¹⁰)" (prime? 1024))

  ;;; ============================================================
  ;;; Miller-Rabin Primality Testing
  ;;; ============================================================

  (display "\n--- Miller-Rabin Primality Testing ---\n")

  ;; Edge cases
  (test-false "miller-rabin? 0" (miller-rabin? 0))
  (test-false "miller-rabin? 1" (miller-rabin? 1))
  (test-true "miller-rabin? 2" (miller-rabin? 2))
  (test-true "miller-rabin? 3" (miller-rabin? 3))

  ;; Primes of various sizes
  (test-true "miller-rabin? 104729 (10000th prime)" (miller-rabin? 104729))
  (test-true "miller-rabin? 1299709 (100000th prime)" (miller-rabin? 1299709))

  ;; Composites
  (test-false "miller-rabin? 104730" (miller-rabin? 104730))
  (test-false "miller-rabin? 561 (Carmichael number)" (miller-rabin? 561))
  (test-false "miller-rabin? 1105 (Carmichael number)" (miller-rabin? 1105))
  (test-false "miller-rabin? 1729 (Carmichael/taxi number)" (miller-rabin? 1729))

  ;; Agreement with prime?
  (test "miller-rabin? agrees with prime? on 997" (prime? 997) (miller-rabin? 997))
  (test "miller-rabin? agrees with prime? on 1000" (prime? 1000) (miller-rabin? 1000))

  ;;; ============================================================
  ;;; Trial Division Factorization
  ;;; ============================================================

  (display "\n--- Trial Division Factorization ---\n")

  (test "trial-division 1" '() (trial-division 1))
  (test "trial-division 2" '(2) (trial-division 2))
  (test "trial-division 6" '(2 3) (trial-division 6))
  (test "trial-division 12" '(2 2 3) (trial-division 12))
  (test "trial-division 100" '(2 2 5 5) (trial-division 100))
  (test "trial-division 360" '(2 2 2 3 3 5) (trial-division 360))
  (test "trial-division 1001" '(7 11 13) (trial-division 1001))

  ;;; ============================================================
  ;;; General Factorization
  ;;; ============================================================

  (display "\n--- General Factorization ---\n")

  (test "factorize 1" '() (factorize 1))
  (test "factorize 2" '(2) (factorize 2))
  (test "factorize 12" '(2 2 3) (factorize 12))
  (test "factorize 100" '(2 2 5 5) (factorize 100))
  (test "factorize 1001" '(7 11 13) (factorize 1001))
  (test "factorize 997 (prime)" '(997) (factorize 997))

  ;; Verify products
  (test "product of factors of 360" 360 (apply * (factorize 360)))
  (test "product of factors of 1000" 1000 (apply * (factorize 1000)))
  (test "product of factors of 12345" 12345 (apply * (factorize 12345)))

  ;;; ============================================================
  ;;; Prime Factorization (exponent form)
  ;;; ============================================================

  (display "\n--- Prime Factorization (exponent form) ---\n")

  (test "prime-factorization 1" '() (prime-factorization 1))
  (test "prime-factorization 8" '((2 . 3)) (prime-factorization 8))
  (test "prime-factorization 12" '((2 . 2) (3 . 1)) (prime-factorization 12))
  (test "prime-factorization 360" '((2 . 3) (3 . 2) (5 . 1)) (prime-factorization 360))
  (test "prime-factorization 1000" '((2 . 3) (5 . 3)) (prime-factorization 1000))

  ;;; ============================================================
  ;;; Divisors
  ;;; ============================================================

  (display "\n--- Divisors ---\n")

  (test "divisors 1" '(1) (divisors 1))
  (test "divisors 2" '(1 2) (divisors 2))
  (test "divisors 6" '(1 2 3 6) (divisors 6))
  (test "divisors 12" '(1 2 3 4 6 12) (divisors 12))
  (test "divisors 28 (perfect)" '(1 2 4 7 14 28) (divisors 28))
  (test "divisors 36" '(1 2 3 4 6 9 12 18 36) (divisors 36))

  ;; Verify divisor count
  (test "divisors count of 12" 6 (length (divisors 12)))
  (test "divisors count of 360" 24 (length (divisors 360)))

  ;;; ============================================================
  ;;; LCM and GCD
  ;;; ============================================================

  (display "\n--- LCM and GCD ---\n")

  (test "gcd 12 18" 6 (gcd 12 18))
  (test "gcd 17 19 (coprime)" 1 (gcd 17 19))
  (test "gcd 48 18" 6 (gcd 48 18))
  (test "gcd 0 5" 5 (gcd 0 5))

  (test "lcm 12 18" 36 (lcm 12 18))
  (test "lcm 4 6" 12 (lcm 4 6))
  (test "lcm 5 7 (coprime)" 35 (lcm 5 7))
  (test "lcm 0 5" 0 (lcm 0 5))

  (test "gcd* (12 18 24)" 6 (gcd* '(12 18 24)))
  (test "lcm* (4 6 8)" 24 (lcm* '(4 6 8)))

  ;; lcm × gcd = a × b
  (test "lcm×gcd = 12×18" (* 12 18) (* (lcm 12 18) (gcd 12 18)))

  ;;; ============================================================
  ;;; Euler's Totient Function
  ;;; ============================================================

  (display "\n--- Euler's Totient Function ---\n")

  (test "euler-totient 1" 1 (euler-totient 1))
  (test "euler-totient 2" 1 (euler-totient 2))
  (test "euler-totient 6" 2 (euler-totient 6))
  (test "euler-totient 10" 4 (euler-totient 10))
  (test "euler-totient 12" 4 (euler-totient 12))
  (test "euler-totient 36" 12 (euler-totient 36))

  ;; φ(p) = p-1 for prime p
  (test "euler-totient 7 (prime)" 6 (euler-totient 7))
  (test "euler-totient 13 (prime)" 12 (euler-totient 13))

  ;; φ(p^k) = p^(k-1) × (p-1)
  (test "euler-totient 8 (2³)" 4 (euler-totient 8))
  (test "euler-totient 27 (3³)" 18 (euler-totient 27))

  ;;; ============================================================
  ;;; Carmichael Lambda Function
  ;;; ============================================================

  (display "\n--- Carmichael Lambda Function ---\n")

  (test "carmichael-lambda 1" 1 (carmichael-lambda 1))
  (test "carmichael-lambda 2" 1 (carmichael-lambda 2))
  (test "carmichael-lambda 4" 2 (carmichael-lambda 4))
  (test "carmichael-lambda 8" 2 (carmichael-lambda 8))
  (test "carmichael-lambda 12" 2 (carmichael-lambda 12))

  ;; λ(p) = p-1 for odd prime p
  (test "carmichael-lambda 7 (prime)" 6 (carmichael-lambda 7))
  (test "carmichael-lambda 13 (prime)" 12 (carmichael-lambda 13))

  ;;; ============================================================
  ;;; Möbius Function
  ;;; ============================================================

  (display "\n--- Möbius Function ---\n")

  (test "mobius 1" 1 (mobius 1))
  (test "mobius 2 (prime)" -1 (mobius 2))
  (test "mobius 3 (prime)" -1 (mobius 3))
  (test "mobius 4 (2²)" 0 (mobius 4))
  (test "mobius 6 (2×3)" 1 (mobius 6))
  (test "mobius 8 (2³)" 0 (mobius 8))
  (test "mobius 30 (2×3×5)" -1 (mobius 30))

  ;;; ============================================================
  ;;; Radical
  ;;; ============================================================

  (display "\n--- Radical ---\n")

  (test "radical 1" 1 (radical 1))
  (test "radical 2" 2 (radical 2))
  (test "radical 8" 2 (radical 8))
  (test "radical 12" 6 (radical 12))
  (test "radical 360" 30 (radical 360))

  ;;; ============================================================
  ;;; Prime Navigation
  ;;; ============================================================

  (display "\n--- Prime Navigation ---\n")

  (test "next-prime 0" 2 (next-prime 0))
  (test "next-prime 2" 3 (next-prime 2))
  (test "next-prime 10" 11 (next-prime 10))
  (test "next-prime 100" 101 (next-prime 100))

  (test "prev-prime 3" 2 (prev-prime 3))
  (test "prev-prime 12" 11 (prev-prime 12))
  (test "prev-prime 100" 97 (prev-prime 100))
  (test "prev-prime 2" #f (prev-prime 2))

  (test "nth-prime 1" 2 (nth-prime 1))
  (test "nth-prime 2" 3 (nth-prime 2))
  (test "nth-prime 10" 29 (nth-prime 10))
  (test "nth-prime 25" 97 (nth-prime 25))

  ;;; ============================================================
  ;;; Primes Up To (Sieve)
  ;;; ============================================================

  (display "\n--- Primes Up To (Sieve) ---\n")

  (test "primes-up-to 1" '() (primes-up-to 1))
  (test "primes-up-to 10" '(2 3 5 7) (primes-up-to 10))
  (test "primes-up-to 30" '(2 3 5 7 11 13 17 19 23 29) (primes-up-to 30))
  (test "prime-pi 30" 10 (prime-pi 30))
  (test "prime-pi 100" 25 (prime-pi 100))

  ;;; ============================================================
  ;;; Coprimality
  ;;; ============================================================

  (display "\n--- Coprimality ---\n")

  (test-true "coprime? 15 28" (coprime? 15 28))
  (test-false "coprime? 12 18" (coprime? 12 18))
  (test-true "coprime? 1 anything" (coprime? 1 100))
  (test-true "coprime? primes" (coprime? 7 13))

  ;;; ============================================================
  ;;; Perfect Powers and Roots
  ;;; ============================================================

  (display "\n--- Perfect Powers and Roots ---\n")

  (test "isqrt 0" 0 (isqrt 0))
  (test "isqrt 1" 1 (isqrt 1))
  (test "isqrt 4" 2 (isqrt 4))
  (test "isqrt 10" 3 (isqrt 10))
  (test "isqrt 100" 10 (isqrt 100))
  (test "isqrt 101" 10 (isqrt 101))

  (test-true "is-perfect-square? 0" (is-perfect-square? 0))
  (test-true "is-perfect-square? 1" (is-perfect-square? 1))
  (test-true "is-perfect-square? 4" (is-perfect-square? 4))
  (test-true "is-perfect-square? 144" (is-perfect-square? 144))
  (test-false "is-perfect-square? 5" (is-perfect-square? 5))
  (test-false "is-perfect-square? 143" (is-perfect-square? 143))

  (test "is-perfect-power? 8" '(2 . 3) (is-perfect-power? 8))
  (test "is-perfect-power? 27" '(3 . 3) (is-perfect-power? 27))
  ;; 16 = 4² = 2⁴, algorithm finds 4² first (smallest exponent)
  (test "is-perfect-power? 16" '(4 . 2) (is-perfect-power? 16))
  (test "is-perfect-power? 7" #f (is-perfect-power? 7))

  ;;; ============================================================
  ;;; Jacobi Symbol
  ;;; ============================================================

  (display "\n--- Jacobi Symbol ---\n")

  ;; Basic cases
  (test "jacobi-symbol 1 n" 1 (jacobi-symbol 1 15))
  (test "jacobi-symbol 0 n" 0 (jacobi-symbol 0 15))

  ;; Known values
  ;; (2/15) = (2/3)(2/5) = (-1)(−1) = 1
  (test "jacobi-symbol 2 15" 1 (jacobi-symbol 2 15))
  ;; (7/15) = (7/3)(7/5) = (1/3)(2/5) = 1×(-1) = -1
  (test "jacobi-symbol 7 15" -1 (jacobi-symbol 7 15))
  ;; (11/15) = (11/3)(11/5) = (2/3)(1/5) = (-1)×1 = -1
  (test "jacobi-symbol 11 15" -1 (jacobi-symbol 11 15))

  ;; For prime p, equals Legendre symbol
  (test "jacobi(2/7) = legendre(2/7)" (legendre-symbol 2 7) (jacobi-symbol 2 7))
  (test "jacobi(3/11) = legendre(3/11)" (legendre-symbol 3 11) (jacobi-symbol 3 11))

  ;;; ============================================================
  ;;; Summary
  ;;; ============================================================

  (display "\n================================================================\n")
  (display "                    TEST RESULTS\n")
  (display "================================================================\n\n")
  (display (format "Tests passed: ~a\n" *pass-count*))
  (display (format "Tests failed: ~a\n" *fail-count*))
  (display (format "Total tests:  ~a\n\n" *test-count*))

  (if (= *fail-count* 0)
      (display "[SUCCESS] All primality tests passed.\n")
      (display "[FAILURE] Some tests failed!\n")))

;; Run the tests
(run-tests)
