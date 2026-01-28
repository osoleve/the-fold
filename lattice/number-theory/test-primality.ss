;;; lattice/number-theory/test-primality.ss — Tests for Primality and Factorization
;;;
;;; Comprehensive tests for primality testing and integer factorization.

(load "core/testing/test-framework.ss")
(load "lattice/number-theory/primality.ss")

;;; ====
;;; Primality Testing: prime?
;;; ====

(test-group "Primality Testing (prime?)"
  ;; Edge cases
  (define-test "prime? 0 is false"
    (assert-false (prime? 0)))
  (define-test "prime? 1 is false"
    (assert-false (prime? 1)))
  (define-test "prime? 2 is true"
    (assert-true (prime? 2)))
  (define-test "prime? 3 is true"
    (assert-true (prime? 3)))
  (define-test "prime? 4 is false"
    (assert-false (prime? 4)))

  ;; Small primes
  (define-test "prime? 5"
    (assert-true (prime? 5)))
  (define-test "prime? 7"
    (assert-true (prime? 7)))
  (define-test "prime? 11"
    (assert-true (prime? 11)))
  (define-test "prime? 13"
    (assert-true (prime? 13)))
  (define-test "prime? 17"
    (assert-true (prime? 17)))
  (define-test "prime? 19"
    (assert-true (prime? 19)))
  (define-test "prime? 23"
    (assert-true (prime? 23)))
  (define-test "prime? 29"
    (assert-true (prime? 29)))
  (define-test "prime? 31"
    (assert-true (prime? 31)))

  ;; Small composites
  (define-test "prime? 6 (2×3)"
    (assert-false (prime? 6)))
  (define-test "prime? 9 (3²)"
    (assert-false (prime? 9)))
  (define-test "prime? 15 (3×5)"
    (assert-false (prime? 15)))
  (define-test "prime? 21 (3×7)"
    (assert-false (prime? 21)))
  (define-test "prime? 25 (5²)"
    (assert-false (prime? 25)))
  (define-test "prime? 27 (3³)"
    (assert-false (prime? 27)))

  ;; Larger primes
  (define-test "prime? 97"
    (assert-true (prime? 97)))
  (define-test "prime? 101"
    (assert-true (prime? 101)))
  (define-test "prime? 997"
    (assert-true (prime? 997)))
  (define-test "prime? 1009"
    (assert-true (prime? 1009)))

  ;; Larger composites
  (define-test "prime? 100 (2²×5²)"
    (assert-false (prime? 100)))
  (define-test "prime? 1001 (7×11×13)"
    (assert-false (prime? 1001)))
  (define-test "prime? 1024 (2¹⁰)"
    (assert-false (prime? 1024))))

;;; ====
;;; Miller-Rabin Primality Testing
;;; ====

(test-group "Miller-Rabin Primality Testing"
  ;; Edge cases
  (define-test "miller-rabin? 0"
    (assert-false (miller-rabin? 0)))
  (define-test "miller-rabin? 1"
    (assert-false (miller-rabin? 1)))
  (define-test "miller-rabin? 2"
    (assert-true (miller-rabin? 2)))
  (define-test "miller-rabin? 3"
    (assert-true (miller-rabin? 3)))

  ;; Primes of various sizes
  (define-test "miller-rabin? 104729 (10000th prime)"
    (assert-true (miller-rabin? 104729)))
  (define-test "miller-rabin? 1299709 (100000th prime)"
    (assert-true (miller-rabin? 1299709)))

  ;; Composites
  (define-test "miller-rabin? 104730"
    (assert-false (miller-rabin? 104730)))
  (define-test "miller-rabin? 561 (Carmichael number)"
    (assert-false (miller-rabin? 561)))
  (define-test "miller-rabin? 1105 (Carmichael number)"
    (assert-false (miller-rabin? 1105)))
  (define-test "miller-rabin? 1729 (Carmichael/taxi number)"
    (assert-false (miller-rabin? 1729)))

  ;; Agreement with prime?
  (define-test "miller-rabin? agrees with prime? on 997"
    (assert-equal (prime? 997) (miller-rabin? 997)))
  (define-test "miller-rabin? agrees with prime? on 1000"
    (assert-equal (prime? 1000) (miller-rabin? 1000))))

;;; ====
;;; Trial Division Factorization
;;; ====

(test-group "Trial Division Factorization"
  (define-test "trial-division 1"
    (assert-equal '() (trial-division 1)))
  (define-test "trial-division 2"
    (assert-equal '(2) (trial-division 2)))
  (define-test "trial-division 6"
    (assert-equal '(2 3) (trial-division 6)))
  (define-test "trial-division 12"
    (assert-equal '(2 2 3) (trial-division 12)))
  (define-test "trial-division 100"
    (assert-equal '(2 2 5 5) (trial-division 100)))
  (define-test "trial-division 360"
    (assert-equal '(2 2 2 3 3 5) (trial-division 360)))
  (define-test "trial-division 1001"
    (assert-equal '(7 11 13) (trial-division 1001))))

;;; ====
;;; General Factorization
;;; ====

(test-group "General Factorization"
  (define-test "factorize 1"
    (assert-equal '() (factorize 1)))
  (define-test "factorize 2"
    (assert-equal '(2) (factorize 2)))
  (define-test "factorize 12"
    (assert-equal '(2 2 3) (factorize 12)))
  (define-test "factorize 100"
    (assert-equal '(2 2 5 5) (factorize 100)))
  (define-test "factorize 1001"
    (assert-equal '(7 11 13) (factorize 1001)))
  (define-test "factorize 997 (prime)"
    (assert-equal '(997) (factorize 997)))

  ;; Verify products
  (define-test "product of factors of 360"
    (assert-equal 360 (apply * (factorize 360))))
  (define-test "product of factors of 1000"
    (assert-equal 1000 (apply * (factorize 1000))))
  (define-test "product of factors of 12345"
    (assert-equal 12345 (apply * (factorize 12345)))))

;;; ====
;;; Prime Factorization (exponent form)
;;; ====

(test-group "Prime Factorization (exponent form)"
  (define-test "prime-factorization 1"
    (assert-equal '() (prime-factorization 1)))
  (define-test "prime-factorization 8"
    (assert-equal '((2 . 3)) (prime-factorization 8)))
  (define-test "prime-factorization 12"
    (assert-equal '((2 . 2) (3 . 1)) (prime-factorization 12)))
  (define-test "prime-factorization 360"
    (assert-equal '((2 . 3) (3 . 2) (5 . 1)) (prime-factorization 360)))
  (define-test "prime-factorization 1000"
    (assert-equal '((2 . 3) (5 . 3)) (prime-factorization 1000))))

;;; ====
;;; Divisors
;;; ====

(test-group "Divisors"
  (define-test "divisors 1"
    (assert-equal '(1) (divisors 1)))
  (define-test "divisors 2"
    (assert-equal '(1 2) (divisors 2)))
  (define-test "divisors 6"
    (assert-equal '(1 2 3 6) (divisors 6)))
  (define-test "divisors 12"
    (assert-equal '(1 2 3 4 6 12) (divisors 12)))
  (define-test "divisors 28 (perfect)"
    (assert-equal '(1 2 4 7 14 28) (divisors 28)))
  (define-test "divisors 36"
    (assert-equal '(1 2 3 4 6 9 12 18 36) (divisors 36)))

  ;; Verify divisor count
  (define-test "divisors count of 12"
    (assert-equal 6 (length (divisors 12))))
  (define-test "divisors count of 360"
    (assert-equal 24 (length (divisors 360)))))

;;; ====
;;; LCM and GCD
;;; ====

(test-group "LCM and GCD"
  (define-test "gcd 12 18"
    (assert-equal 6 (gcd 12 18)))
  (define-test "gcd 17 19 (coprime)"
    (assert-equal 1 (gcd 17 19)))
  (define-test "gcd 48 18"
    (assert-equal 6 (gcd 48 18)))
  (define-test "gcd 0 5"
    (assert-equal 5 (gcd 0 5)))

  (define-test "lcm 12 18"
    (assert-equal 36 (lcm 12 18)))
  (define-test "lcm 4 6"
    (assert-equal 12 (lcm 4 6)))
  (define-test "lcm 5 7 (coprime)"
    (assert-equal 35 (lcm 5 7)))
  (define-test "lcm 0 5"
    (assert-equal 0 (lcm 0 5)))

  (define-test "gcd* (12 18 24)"
    (assert-equal 6 (gcd* '(12 18 24))))
  (define-test "lcm* (4 6 8)"
    (assert-equal 24 (lcm* '(4 6 8))))

  ;; lcm × gcd = a × b
  (define-test "lcm×gcd = 12×18"
    (assert-equal (* 12 18) (* (lcm 12 18) (gcd 12 18)))))

;;; ====
;;; Euler's Totient Function
;;; ====

(test-group "Euler's Totient Function"
  (define-test "euler-totient 1"
    (assert-equal 1 (euler-totient 1)))
  (define-test "euler-totient 2"
    (assert-equal 1 (euler-totient 2)))
  (define-test "euler-totient 6"
    (assert-equal 2 (euler-totient 6)))
  (define-test "euler-totient 10"
    (assert-equal 4 (euler-totient 10)))
  (define-test "euler-totient 12"
    (assert-equal 4 (euler-totient 12)))
  (define-test "euler-totient 36"
    (assert-equal 12 (euler-totient 36)))

  ;; φ(p) = p-1 for prime p
  (define-test "euler-totient 7 (prime)"
    (assert-equal 6 (euler-totient 7)))
  (define-test "euler-totient 13 (prime)"
    (assert-equal 12 (euler-totient 13)))

  ;; φ(p^k) = p^(k-1) × (p-1)
  (define-test "euler-totient 8 (2³)"
    (assert-equal 4 (euler-totient 8)))
  (define-test "euler-totient 27 (3³)"
    (assert-equal 18 (euler-totient 27))))

;;; ====
;;; Carmichael Lambda Function
;;; ====

(test-group "Carmichael Lambda Function"
  (define-test "carmichael-lambda 1"
    (assert-equal 1 (carmichael-lambda 1)))
  (define-test "carmichael-lambda 2"
    (assert-equal 1 (carmichael-lambda 2)))
  (define-test "carmichael-lambda 4"
    (assert-equal 2 (carmichael-lambda 4)))
  (define-test "carmichael-lambda 8"
    (assert-equal 2 (carmichael-lambda 8)))
  (define-test "carmichael-lambda 12"
    (assert-equal 2 (carmichael-lambda 12)))

  ;; λ(p) = p-1 for odd prime p
  (define-test "carmichael-lambda 7 (prime)"
    (assert-equal 6 (carmichael-lambda 7)))
  (define-test "carmichael-lambda 13 (prime)"
    (assert-equal 12 (carmichael-lambda 13))))

;;; ====
;;; Möbius Function
;;; ====

(test-group "Möbius Function"
  (define-test "mobius 1"
    (assert-equal 1 (mobius 1)))
  (define-test "mobius 2 (prime)"
    (assert-equal -1 (mobius 2)))
  (define-test "mobius 3 (prime)"
    (assert-equal -1 (mobius 3)))
  (define-test "mobius 4 (2²)"
    (assert-equal 0 (mobius 4)))
  (define-test "mobius 6 (2×3)"
    (assert-equal 1 (mobius 6)))
  (define-test "mobius 8 (2³)"
    (assert-equal 0 (mobius 8)))
  (define-test "mobius 30 (2×3×5)"
    (assert-equal -1 (mobius 30))))

;;; ====
;;; Radical
;;; ====

(test-group "Radical"
  (define-test "radical 1"
    (assert-equal 1 (radical 1)))
  (define-test "radical 2"
    (assert-equal 2 (radical 2)))
  (define-test "radical 8"
    (assert-equal 2 (radical 8)))
  (define-test "radical 12"
    (assert-equal 6 (radical 12)))
  (define-test "radical 360"
    (assert-equal 30 (radical 360))))

;;; ====
;;; Prime Navigation
;;; ====

(test-group "Prime Navigation"
  (define-test "next-prime 0"
    (assert-equal 2 (next-prime 0)))
  (define-test "next-prime 2"
    (assert-equal 3 (next-prime 2)))
  (define-test "next-prime 10"
    (assert-equal 11 (next-prime 10)))
  (define-test "next-prime 100"
    (assert-equal 101 (next-prime 100)))

  (define-test "prev-prime 3"
    (assert-equal 2 (prev-prime 3)))
  (define-test "prev-prime 12"
    (assert-equal 11 (prev-prime 12)))
  (define-test "prev-prime 100"
    (assert-equal 97 (prev-prime 100)))
  (define-test "prev-prime 2"
    (assert-equal #f (prev-prime 2)))

  (define-test "nth-prime 1"
    (assert-equal 2 (nth-prime 1)))
  (define-test "nth-prime 2"
    (assert-equal 3 (nth-prime 2)))
  (define-test "nth-prime 10"
    (assert-equal 29 (nth-prime 10)))
  (define-test "nth-prime 25"
    (assert-equal 97 (nth-prime 25))))

;;; ====
;;; Primes Up To (Sieve)
;;; ====

(test-group "Primes Up To (Sieve)"
  (define-test "primes-up-to 1"
    (assert-equal '() (primes-up-to 1)))
  (define-test "primes-up-to 10"
    (assert-equal '(2 3 5 7) (primes-up-to 10)))
  (define-test "primes-up-to 30"
    (assert-equal '(2 3 5 7 11 13 17 19 23 29) (primes-up-to 30)))
  (define-test "prime-pi 30"
    (assert-equal 10 (prime-pi 30)))
  (define-test "prime-pi 100"
    (assert-equal 25 (prime-pi 100))))

;;; ====
;;; Coprimality
;;; ====

(test-group "Coprimality"
  (define-test "coprime? 15 28"
    (assert-true (coprime? 15 28)))
  (define-test "coprime? 12 18"
    (assert-false (coprime? 12 18)))
  (define-test "coprime? 1 anything"
    (assert-true (coprime? 1 100)))
  (define-test "coprime? primes"
    (assert-true (coprime? 7 13))))

;;; ====
;;; Perfect Powers and Roots
;;; ====

(test-group "Perfect Powers and Roots"
  (define-test "isqrt 0"
    (assert-equal 0 (isqrt 0)))
  (define-test "isqrt 1"
    (assert-equal 1 (isqrt 1)))
  (define-test "isqrt 4"
    (assert-equal 2 (isqrt 4)))
  (define-test "isqrt 10"
    (assert-equal 3 (isqrt 10)))
  (define-test "isqrt 100"
    (assert-equal 10 (isqrt 100)))
  (define-test "isqrt 101"
    (assert-equal 10 (isqrt 101)))

  (define-test "is-perfect-square? 0"
    (assert-true (is-perfect-square? 0)))
  (define-test "is-perfect-square? 1"
    (assert-true (is-perfect-square? 1)))
  (define-test "is-perfect-square? 4"
    (assert-true (is-perfect-square? 4)))
  (define-test "is-perfect-square? 144"
    (assert-true (is-perfect-square? 144)))
  (define-test "is-perfect-square? 5"
    (assert-false (is-perfect-square? 5)))
  (define-test "is-perfect-square? 143"
    (assert-false (is-perfect-square? 143)))

  (define-test "is-perfect-power? 8"
    (assert-equal '(2 . 3) (is-perfect-power? 8)))
  (define-test "is-perfect-power? 27"
    (assert-equal '(3 . 3) (is-perfect-power? 27)))
  ;; 16 = 4² = 2⁴, algorithm finds 4² first (smallest exponent)
  (define-test "is-perfect-power? 16"
    (assert-equal '(4 . 2) (is-perfect-power? 16)))
  (define-test "is-perfect-power? 7"
    (assert-equal #f (is-perfect-power? 7))))

;;; ====
;;; Jacobi Symbol
;;; ====

(test-group "Jacobi Symbol"
  ;; Basic cases
  (define-test "jacobi-symbol 1 n"
    (assert-equal 1 (jacobi-symbol 1 15)))
  (define-test "jacobi-symbol 0 n"
    (assert-equal 0 (jacobi-symbol 0 15)))

  ;; Known values
  ;; (2/15) = (2/3)(2/5) = (-1)(−1) = 1
  (define-test "jacobi-symbol 2 15"
    (assert-equal 1 (jacobi-symbol 2 15)))
  ;; (7/15) = (7/3)(7/5) = (1/3)(2/5) = 1×(-1) = -1
  (define-test "jacobi-symbol 7 15"
    (assert-equal -1 (jacobi-symbol 7 15)))
  ;; (11/15) = (11/3)(11/5) = (2/3)(1/5) = (-1)×1 = -1
  (define-test "jacobi-symbol 11 15"
    (assert-equal -1 (jacobi-symbol 11 15)))

  ;; For prime p, equals Legendre symbol
  (define-test "jacobi(2/7) = legendre(2/7)"
    (assert-equal (legendre-symbol 2 7) (jacobi-symbol 2 7)))
  (define-test "jacobi(3/11) = legendre(3/11)"
    (assert-equal (legendre-symbol 3 11) (jacobi-symbol 3 11))))

;; Run all tests
(run-all-tests)
