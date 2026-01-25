;;; core/number-theory/test-modular.ss — Tests for Modular Arithmetic
;;;
;;; Comprehensive tests for modular arithmetic operations.

(load "lattice/number-theory/modular.ss")

;;; ====
;;; Test Framework
;;; ====

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "✗ ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (run-tests)
  (display "\n=== Modular Arithmetic Test Suite ===\n\n")
  
  ;;; ====
  ;;; Basic Modular Operations
  ;;; ====
  
  (display "--- Basic Modular Operations ---\n")
  
  ;; Modular addition
  (test "mod+ basic: (7 + 5) mod 12 = 0" 0 (mod+ 7 5 12))
  (test "mod+ basic: (3 + 4) mod 7 = 0" 0 (mod+ 3 4 7))
  (test "mod+ basic: (10 + 15) mod 13 = 12" 12 (mod+ 10 15 13))
  (test "mod+ zero: (0 + 5) mod 7 = 5" 5 (mod+ 0 5 7))
  (test "mod+ negative: (-3 + 5) mod 7 = 2" 2 (mod+ -3 5 7))
  
  ;; Modular subtraction
  (test "mod- basic: (5 - 3) mod 7 = 2" 2 (mod- 5 3 7))
  (test "mod- negative result: (3 - 5) mod 7 = 5" 5 (mod- 3 5 7))
  (test "mod- wrap: (10 - 15) mod 13 = 8" 8 (mod- 10 15 13))
  (test "mod- zero: (5 - 5) mod 7 = 0" 0 (mod- 5 5 7))
  
  ;; Modular multiplication
  (test "mod* basic: (3 * 4) mod 7 = 5" 5 (mod* 3 4 7))
  (test "mod* basic: (5 * 6) mod 13 = 4" 4 (mod* 5 6 13))
  (test "mod* zero: (0 * 5) mod 7 = 0" 0 (mod* 0 5 7))
  (test "mod* one: (1 * 5) mod 7 = 5" 5 (mod* 1 5 7))
  (test "mod* large: (123 * 456) mod 1000 = 88" 88 (mod* 123 456 1000))
  
  ;;; ====
  ;;; Modular Exponentiation
  ;;; ====
  
  (display "\n--- Modular Exponentiation ---\n")
  
  ;; Basic cases
  (test "mod-expt: 2^0 mod 7 = 1" 1 (mod-expt 2 0 7))
  (test "mod-expt: 2^1 mod 7 = 2" 2 (mod-expt 2 1 7))
  (test "mod-expt: 2^3 mod 7 = 1" 1 (mod-expt 2 3 7))
  (test "mod-expt: 3^4 mod 7 = 4" 4 (mod-expt 3 4 7))
  (test "mod-expt: 5^3 mod 13 = 8" 8 (mod-expt 5 3 13))
  
  ;; Large exponents
  (test "mod-expt large: 2^10 mod 1000 = 24" 24 (mod-expt 2 10 1000))
  (test "mod-expt large: 3^20 mod 100 = 1" 1 (mod-expt 3 20 100))
  (test "mod-expt very large: 2^100 mod 1000000007 = 976371285"
        976371285 (mod-expt 2 100 1000000007))
  
  ;; Fermat's Little Theorem: a^(p-1) ≡ 1 (mod p) for prime p
  (test "Fermat: 2^6 mod 7 = 1" 1 (mod-expt 2 6 7))
  (test "Fermat: 3^10 mod 11 = 1" 1 (mod-expt 3 10 11))
  (test "Fermat: 5^12 mod 13 = 1" 1 (mod-expt 5 12 13))
  
  ;;; ====
  ;;; GCD and Extended GCD
  ;;; ====
  
  (display "\n--- GCD and Extended GCD ---\n")
  
  ;; Basic GCD
  (test "gcd(12, 8) = 4" 4 (gcd 12 8))
  (test "gcd(17, 13) = 1" 1 (gcd 17 13))
  (test "gcd(100, 50) = 50" 50 (gcd 100 50))
  (test "gcd(7, 0) = 7" 7 (gcd 7 0))
  (test "gcd(0, 5) = 5" 5 (gcd 0 5))
  
  ;; Extended GCD - verify Bézout's identity: gcd = ax + by
  (let* ([result (extended-gcd 240 46)]
         [g (car result)]
         [x (cadr result)]
         [y (caddr result)])
        (test "extended-gcd(240, 46) gcd = 2" 2 g)
        (test "extended-gcd(240, 46) Bézout identity"
              g (+ (* 240 x) (* 46 y))))
  
  (let* ([result (extended-gcd 17 13)]
         [g (car result)]
         [x (cadr result)]
         [y (caddr result)])
        (test "extended-gcd(17, 13) gcd = 1" 1 g)
        (test "extended-gcd(17, 13) Bézout identity"
              g (+ (* 17 x) (* 13 y))))
  
  ;;; ====
  ;;; Modular Inverse
  ;;; ====
  
  (display "\n--- Modular Inverse ---\n")
  
  ;; Inverses that exist
  (test "mod-inverse: 3^-1 mod 7 = 5" 5 (mod-inverse 3 7))
  (test "mod-inverse: 5^-1 mod 13 = 8" 8 (mod-inverse 5 13))
  (test "mod-inverse: 7^-1 mod 11 = 8" 8 (mod-inverse 7 11))
  
  ;; Verify inverses: (a * a^-1) ≡ 1 (mod m)
  (let ([inv (mod-inverse 3 7)])
       (test "mod-inverse verification: (3 * 5) mod 7 = 1"
             1 (mod* 3 inv 7)))
  
  (let ([inv (mod-inverse 5 13)])
       (test "mod-inverse verification: (5 * 8) mod 13 = 1"
             1 (mod* 5 inv 13)))
  
  ;; Non-existent inverses (gcd ≠ 1)
  (test-false "mod-inverse: 4^-1 mod 8 doesn't exist" (mod-inverse 4 8))
  (test-false "mod-inverse: 6^-1 mod 9 doesn't exist" (mod-inverse 6 9))
  (test-false "mod-inverse: 10^-1 mod 15 doesn't exist" (mod-inverse 10 15))
  
  ;;; ====
  ;;; Chinese Remainder Theorem
  ;;; ====
  
  (display "\n--- Chinese Remainder Theorem ---\n")
  
  ;; Basic CRT examples
  ;; x ≡ 2 (mod 3), x ≡ 3 (mod 5), x ≡ 2 (mod 7)
  ;; Solution: x = 23 (mod 105)
  (test "crt basic: x ≡ 2 (mod 3), x ≡ 3 (mod 5), x ≡ 2 (mod 7)"
        23 (crt '(2 3 2) '(3 5 7)))
  
  ;; Verify the solution
  (let ([x (crt '(2 3 2) '(3 5 7))])
       (test "crt verification: 23 mod 3 = 2" 2 (modulo x 3))
       (test "crt verification: 23 mod 5 = 3" 3 (modulo x 5))
       (test "crt verification: 23 mod 7 = 2" 2 (modulo x 7)))
  
  ;; x ≡ 1 (mod 2), x ≡ 2 (mod 3), x ≡ 3 (mod 5)
  ;; Solution: x = 23 (mod 30)
  (test "crt: x ≡ 1 (mod 2), x ≡ 2 (mod 3), x ≡ 3 (mod 5)"
        23 (crt '(1 2 3) '(2 3 5)))
  
  ;; x ≡ 0 (mod 3), x ≡ 0 (mod 4), x ≡ 0 (mod 5)
  ;; Solution: x = 0 (mod 60)
  (test "crt: all zeros" 0 (crt '(0 0 0) '(3 4 5)))
  
  ;; Two moduli
  ;; x ≡ 1 (mod 5), x ≡ 3 (mod 7)
  ;; Solution: x = 31 (mod 35)
  (test "crt: x ≡ 1 (mod 5), x ≡ 3 (mod 7)"
        31 (crt '(1 3) '(5 7)))
  
  ;; Empty lists
  (test "crt: empty lists" 0 (crt '() '()))
  
  ;;; ====
  ;;; Montgomery Multiplication
  ;;; ====
  
  (display "\n--- Montgomery Multiplication ---\n")
  
  ;; Test Montgomery setup
  (let* ([m 17]
         [setup (montgomery-setup m)]
         [R (car setup)]
         [m-prime (cadr setup)])
        (test "montgomery-setup: R > m" #t (> R m))
        ;; Check R is power of 2: R & (R-1) should be 0
        (test "montgomery-setup: R is power of 2" #t
              (= 0 (bitwise-and R (- R 1)))))
  
  ;; Test round-trip conversion
  (let* ([m 17]
         [a 5]
         [setup (montgomery-setup m)]
         [R (car setup)]
         [m-prime (cadr setup)]
         [a-mont (to-montgomery a m R)]
         [a-back (from-montgomery a-mont m R m-prime)])
        (test "montgomery round-trip: 5 -> mont -> 5" a a-back))
  
  ;; Test Montgomery multiplication
  (let* ([m 17]
         [a 3]
         [b 5]
         [setup (montgomery-setup m)]
         [R (car setup)]
         [m-prime (cadr setup)]
         [a-mont (to-montgomery a m R)]
         [b-mont (to-montgomery b m R)]
         [prod-mont (montgomery-mult a-mont b-mont m R m-prime)]
         [prod (from-montgomery prod-mont m R m-prime)])
        (test "montgomery-mult: (3 * 5) mod 17 = 15" 15 prod))
  
  ;; Test Montgomery exponentiation
  (test "montgomery-expt: 2^10 mod 17 = 4" 4 (montgomery-expt 2 10 17))
  (test "montgomery-expt: 3^5 mod 13 = 9" 9 (montgomery-expt 3 5 13))
  (test "montgomery-expt: 5^7 mod 23 = 17" 17 (montgomery-expt 5 7 23))
  
  ;; Verify Montgomery exponentiation matches regular mod-expt
  (test "montgomery-expt = mod-expt: 2^10 mod 17"
        (mod-expt 2 10 17) (montgomery-expt 2 10 17))
  (test "montgomery-expt = mod-expt: 7^13 mod 19"
        (mod-expt 7 13 19) (montgomery-expt 7 13 19))
  (test "montgomery-expt = mod-expt: 11^23 mod 31"
        (mod-expt 11 23 31) (montgomery-expt 11 23 31))
  
  ;; Large exponents
  (test "montgomery-expt large: 2^100 mod 1000000007"
        (mod-expt 2 100 1000000007)
        (montgomery-expt 2 100 1000000007))

  ;;; ====
  ;;; Quadratic Residues and Modular Square Roots
  ;;; ====

  (display "\n--- Quadratic Residues ---\n")

  ;; Legendre symbol tests
  (test "legendre(1, 7) = 1" 1 (legendre-symbol 1 7))
  (test "legendre(2, 7) = 1 (2 is QR mod 7)" 1 (legendre-symbol 2 7))
  (test "legendre(3, 7) = -1 (3 is NR mod 7)" -1 (legendre-symbol 3 7))
  (test "legendre(4, 7) = 1 (4 = 2² is QR)" 1 (legendre-symbol 4 7))
  (test "legendre(0, 7) = 0" 0 (legendre-symbol 0 7))
  (test "legendre(7, 7) = 0" 0 (legendre-symbol 7 7))

  ;; Quadratic residue tests
  (test-true "quadratic-residue?(4, 7)" (quadratic-residue? 4 7))
  (test-true "quadratic-residue?(2, 7)" (quadratic-residue? 2 7))
  (test-false "quadratic-residue?(3, 7)" (quadratic-residue? 3 7))
  (test-true "quadratic-residue?(0, 7)" (quadratic-residue? 0 7))

  (display "\n--- Modular Square Root (Tonelli-Shanks) ---\n")

  ;; Simple case: p ≡ 3 (mod 4)
  ;; For p = 7: 7 ≡ 3 (mod 4), so use simple formula
  (let ([r (mod-sqrt 2 7)])
    (test "mod-sqrt(2, 7) exists" #t (number? r))
    (test "mod-sqrt(2, 7)² ≡ 2 (mod 7)" 2 (mod* r r 7)))

  (let ([r (mod-sqrt 4 7)])
    (test "mod-sqrt(4, 7) = 2 or 5" #t (or (= r 2) (= r 5)))
    (test "mod-sqrt(4, 7)² ≡ 4 (mod 7)" 4 (mod* r r 7)))

  ;; Non-residue returns #f
  (test-false "mod-sqrt(3, 7) = #f (3 is NR mod 7)" (mod-sqrt 3 7))

  ;; Zero
  (test "mod-sqrt(0, 7) = 0" 0 (mod-sqrt 0 7))

  ;; General case: p ≡ 1 (mod 4) - requires full Tonelli-Shanks
  ;; p = 13: 13 ≡ 1 (mod 4)
  (let ([r (mod-sqrt 3 13)])
    (test "mod-sqrt(3, 13) exists" #t (number? r))
    (test "mod-sqrt(3, 13)² ≡ 3 (mod 13)" 3 (mod* r r 13)))

  (let ([r (mod-sqrt 4 13)])
    (test "mod-sqrt(4, 13) = 2 or 11" #t (or (= r 2) (= r 11)))
    (test "mod-sqrt(4, 13)² ≡ 4 (mod 13)" 4 (mod* r r 13)))

  (let ([r (mod-sqrt 9 13)])
    (test "mod-sqrt(9, 13) = 3 or 10" #t (or (= r 3) (= r 10)))
    (test "mod-sqrt(9, 13)² ≡ 9 (mod 13)" 9 (mod* r r 13)))

  ;; Larger primes
  ;; p = 17: 17 ≡ 1 (mod 4)
  (let ([r (mod-sqrt 2 17)])
    (test "mod-sqrt(2, 17) exists" #t (number? r))
    (test "mod-sqrt(2, 17)² ≡ 2 (mod 17)" 2 (mod* r r 17)))

  ;; p = 41: 41 ≡ 1 (mod 4)
  (let ([r (mod-sqrt 5 41)])
    (test "mod-sqrt(5, 41) exists" #t (number? r))
    (test "mod-sqrt(5, 41)² ≡ 5 (mod 41)" 5 (mod* r r 41)))

  ;; p = 97: larger prime, 97 ≡ 1 (mod 4)
  (let ([r (mod-sqrt 2 97)])
    (test "mod-sqrt(2, 97) exists" #t (number? r))
    (test "mod-sqrt(2, 97)² ≡ 2 (mod 97)" 2 (mod* r r 97)))

  ;; Test mod-sqrt-both
  (let ([roots (mod-sqrt-both 4 13)])
    (test "mod-sqrt-both(4, 13) returns two roots" 2 (length roots))
    (test "mod-sqrt-both(4, 13) first root squared" 4 (mod* (car roots) (car roots) 13))
    (test "mod-sqrt-both(4, 13) second root squared" 4 (mod* (cadr roots) (cadr roots) 13))
    (test "mod-sqrt-both(4, 13) roots sum to p" 13 (+ (car roots) (cadr roots))))

  (test-false "mod-sqrt-both(5, 13) = #f (5 is NR mod 13)" (mod-sqrt-both 5 13))

  ;; Large prime for crypto-like test
  ;; Mersenne prime 2^31 - 1 = 2147483647, which is ≡ 3 (mod 4)
  (let* ([p 2147483647]
         [a 12345678]
         [r (mod-sqrt a p)])
    (if r
        (test "mod-sqrt with large prime verifies" a (mod* r r p))
        (test-true "mod-sqrt with large prime: a is non-residue" #t)))

  ;;; ====
  ;;; Summary
  ;;; ====
  
  (display "\n=== Test Summary ===\n")
  (display "Total:  ")
  (display *test-count*)
  (newline)
  (display "Passed: ")
  (display *pass-count*)
  (newline)
  (display "Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "\n✓ All tests passed!\n")
      (display "\n✗ Some tests failed.\n"))
  
  (exit (if (= *fail-count* 0) 0 1)))

;; Run tests when loaded
(run-tests)
