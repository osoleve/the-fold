;;; core/number-theory/test-modular.ss — Tests for Modular Arithmetic
;;;
;;; Comprehensive tests for modular arithmetic operations.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(require 'modular)

(define (run-tests)
  (test-group "Basic Modular Operations"
    ;; Modular addition
    (define-test "mod+ basic: (7 + 5) mod 12 = 0"
      (assert-equal 0 (mod+ 7 5 12)))
    (define-test "mod+ basic: (3 + 4) mod 7 = 0"
      (assert-equal 0 (mod+ 3 4 7)))
    (define-test "mod+ basic: (10 + 15) mod 13 = 12"
      (assert-equal 12 (mod+ 10 15 13)))
    (define-test "mod+ zero: (0 + 5) mod 7 = 5"
      (assert-equal 5 (mod+ 0 5 7)))
    (define-test "mod+ negative: (-3 + 5) mod 7 = 2"
      (assert-equal 2 (mod+ -3 5 7)))

    ;; Modular subtraction
    (define-test "mod- basic: (5 - 3) mod 7 = 2"
      (assert-equal 2 (mod- 5 3 7)))
    (define-test "mod- negative result: (3 - 5) mod 7 = 5"
      (assert-equal 5 (mod- 3 5 7)))
    (define-test "mod- wrap: (10 - 15) mod 13 = 8"
      (assert-equal 8 (mod- 10 15 13)))
    (define-test "mod- zero: (5 - 5) mod 7 = 0"
      (assert-equal 0 (mod- 5 5 7)))

    ;; Modular multiplication
    (define-test "mod* basic: (3 * 4) mod 7 = 5"
      (assert-equal 5 (mod* 3 4 7)))
    (define-test "mod* basic: (5 * 6) mod 13 = 4"
      (assert-equal 4 (mod* 5 6 13)))
    (define-test "mod* zero: (0 * 5) mod 7 = 0"
      (assert-equal 0 (mod* 0 5 7)))
    (define-test "mod* one: (1 * 5) mod 7 = 5"
      (assert-equal 5 (mod* 1 5 7)))
    (define-test "mod* large: (123 * 456) mod 1000 = 88"
      (assert-equal 88 (mod* 123 456 1000))))
  
  (test-group "Modular Exponentiation"
    ;; Basic cases
    (define-test "mod-expt: 2^0 mod 7 = 1"
      (assert-equal 1 (mod-expt 2 0 7)))
    (define-test "mod-expt: 2^1 mod 7 = 2"
      (assert-equal 2 (mod-expt 2 1 7)))
    (define-test "mod-expt: 2^3 mod 7 = 1"
      (assert-equal 1 (mod-expt 2 3 7)))
    (define-test "mod-expt: 3^4 mod 7 = 4"
      (assert-equal 4 (mod-expt 3 4 7)))
    (define-test "mod-expt: 5^3 mod 13 = 8"
      (assert-equal 8 (mod-expt 5 3 13)))

    ;; Large exponents
    (define-test "mod-expt large: 2^10 mod 1000 = 24"
      (assert-equal 24 (mod-expt 2 10 1000)))
    (define-test "mod-expt large: 3^20 mod 100 = 1"
      (assert-equal 1 (mod-expt 3 20 100)))
    (define-test "mod-expt very large: 2^100 mod 1000000007 = 976371285"
      (assert-equal 976371285 (mod-expt 2 100 1000000007)))

    ;; Fermat's Little Theorem: a^(p-1) ≡ 1 (mod p) for prime p
    (define-test "Fermat: 2^6 mod 7 = 1"
      (assert-equal 1 (mod-expt 2 6 7)))
    (define-test "Fermat: 3^10 mod 11 = 1"
      (assert-equal 1 (mod-expt 3 10 11)))
    (define-test "Fermat: 5^12 mod 13 = 1"
      (assert-equal 1 (mod-expt 5 12 13))))
  
  (test-group "GCD and Extended GCD"
    ;; Basic GCD
    (define-test "gcd(12, 8) = 4"
      (assert-equal 4 (gcd 12 8)))
    (define-test "gcd(17, 13) = 1"
      (assert-equal 1 (gcd 17 13)))
    (define-test "gcd(100, 50) = 50"
      (assert-equal 50 (gcd 100 50)))
    (define-test "gcd(7, 0) = 7"
      (assert-equal 7 (gcd 7 0)))
    (define-test "gcd(0, 5) = 5"
      (assert-equal 5 (gcd 0 5)))

    ;; Extended GCD - verify Bézout's identity: gcd = ax + by
    (define-test "extended-gcd(240, 46) gcd = 2"
      (let* ([result (extended-gcd 240 46)]
             [g (car result)])
        (assert-equal 2 g)))
    (define-test "extended-gcd(240, 46) Bézout identity"
      (let* ([result (extended-gcd 240 46)]
             [g (car result)]
             [x (cadr result)]
             [y (caddr result)])
        (assert-equal g (+ (* 240 x) (* 46 y)))))

    (define-test "extended-gcd(17, 13) gcd = 1"
      (let* ([result (extended-gcd 17 13)]
             [g (car result)])
        (assert-equal 1 g)))
    (define-test "extended-gcd(17, 13) Bézout identity"
      (let* ([result (extended-gcd 17 13)]
             [g (car result)]
             [x (cadr result)]
             [y (caddr result)])
        (assert-equal g (+ (* 17 x) (* 13 y))))))
  
  (test-group "Modular Inverse"
    ;; Inverses that exist
    (define-test "mod-inverse: 3^-1 mod 7 = 5"
      (assert-equal 5 (mod-inverse 3 7)))
    (define-test "mod-inverse: 5^-1 mod 13 = 8"
      (assert-equal 8 (mod-inverse 5 13)))
    (define-test "mod-inverse: 7^-1 mod 11 = 8"
      (assert-equal 8 (mod-inverse 7 11)))

    ;; Verify inverses: (a * a^-1) ≡ 1 (mod m)
    (define-test "mod-inverse verification: (3 * 5) mod 7 = 1"
      (let ([inv (mod-inverse 3 7)])
        (assert-equal 1 (mod* 3 inv 7))))

    (define-test "mod-inverse verification: (5 * 8) mod 13 = 1"
      (let ([inv (mod-inverse 5 13)])
        (assert-equal 1 (mod* 5 inv 13))))

    ;; Non-existent inverses (gcd ≠ 1)
    (define-test "mod-inverse: 4^-1 mod 8 doesn't exist"
      (assert-false (mod-inverse 4 8)))
    (define-test "mod-inverse: 6^-1 mod 9 doesn't exist"
      (assert-false (mod-inverse 6 9)))
    (define-test "mod-inverse: 10^-1 mod 15 doesn't exist"
      (assert-false (mod-inverse 10 15))))
  
  (test-group "Chinese Remainder Theorem"
    ;; Basic CRT examples
    ;; x ≡ 2 (mod 3), x ≡ 3 (mod 5), x ≡ 2 (mod 7)
    ;; Solution: x = 23 (mod 105)
    (define-test "crt basic: x ≡ 2 (mod 3), x ≡ 3 (mod 5), x ≡ 2 (mod 7)"
      (assert-equal 23 (crt '(2 3 2) '(3 5 7))))

    ;; Verify the solution
    (define-test "crt verification: 23 mod 3 = 2"
      (let ([x (crt '(2 3 2) '(3 5 7))])
        (assert-equal 2 (modulo x 3))))
    (define-test "crt verification: 23 mod 5 = 3"
      (let ([x (crt '(2 3 2) '(3 5 7))])
        (assert-equal 3 (modulo x 5))))
    (define-test "crt verification: 23 mod 7 = 2"
      (let ([x (crt '(2 3 2) '(3 5 7))])
        (assert-equal 2 (modulo x 7))))

    ;; x ≡ 1 (mod 2), x ≡ 2 (mod 3), x ≡ 3 (mod 5)
    ;; Solution: x = 23 (mod 30)
    (define-test "crt: x ≡ 1 (mod 2), x ≡ 2 (mod 3), x ≡ 3 (mod 5)"
      (assert-equal 23 (crt '(1 2 3) '(2 3 5))))

    ;; x ≡ 0 (mod 3), x ≡ 0 (mod 4), x ≡ 0 (mod 5)
    ;; Solution: x = 0 (mod 60)
    (define-test "crt: all zeros"
      (assert-equal 0 (crt '(0 0 0) '(3 4 5))))

    ;; Two moduli
    ;; x ≡ 1 (mod 5), x ≡ 3 (mod 7)
    ;; Solution: x = 31 (mod 35)
    (define-test "crt: x ≡ 1 (mod 5), x ≡ 3 (mod 7)"
      (assert-equal 31 (crt '(1 3) '(5 7))))

    ;; Empty lists
    (define-test "crt: empty lists"
      (assert-equal 0 (crt '() '()))))
  
  (test-group "Montgomery Multiplication"
    ;; Test Montgomery setup
    (define-test "montgomery-setup: R > m"
      (let* ([m 17]
             [setup (montgomery-setup m)]
             [R (car setup)])
        (assert-true (> R m))))
    (define-test "montgomery-setup: R is power of 2"
      (let* ([m 17]
             [setup (montgomery-setup m)]
             [R (car setup)])
        ;; Check R is power of 2: R & (R-1) should be 0
        (assert-true (= 0 (bitwise-and R (- R 1))))))

    ;; Test round-trip conversion
    (define-test "montgomery round-trip: 5 -> mont -> 5"
      (let* ([m 17]
             [a 5]
             [setup (montgomery-setup m)]
             [R (car setup)]
             [m-prime (cadr setup)]
             [a-mont (to-montgomery a m R)]
             [a-back (from-montgomery a-mont m R m-prime)])
        (assert-equal a a-back)))

    ;; Test Montgomery multiplication
    (define-test "montgomery-mult: (3 * 5) mod 17 = 15"
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
        (assert-equal 15 prod)))

    ;; Test Montgomery exponentiation
    (define-test "montgomery-expt: 2^10 mod 17 = 4"
      (assert-equal 4 (montgomery-expt 2 10 17)))
    (define-test "montgomery-expt: 3^5 mod 13 = 9"
      (assert-equal 9 (montgomery-expt 3 5 13)))
    (define-test "montgomery-expt: 5^7 mod 23 = 17"
      (assert-equal 17 (montgomery-expt 5 7 23)))

    ;; Verify Montgomery exponentiation matches regular mod-expt
    (define-test "montgomery-expt = mod-expt: 2^10 mod 17"
      (assert-equal (mod-expt 2 10 17) (montgomery-expt 2 10 17)))
    (define-test "montgomery-expt = mod-expt: 7^13 mod 19"
      (assert-equal (mod-expt 7 13 19) (montgomery-expt 7 13 19)))
    (define-test "montgomery-expt = mod-expt: 11^23 mod 31"
      (assert-equal (mod-expt 11 23 31) (montgomery-expt 11 23 31)))

    ;; Large exponents
    (define-test "montgomery-expt large: 2^100 mod 1000000007"
      (assert-equal (mod-expt 2 100 1000000007)
                    (montgomery-expt 2 100 1000000007))))

  (test-group "Quadratic Residues"
    ;; Legendre symbol tests
    (define-test "legendre(1, 7) = 1"
      (assert-equal 1 (legendre-symbol 1 7)))
    (define-test "legendre(2, 7) = 1 (2 is QR mod 7)"
      (assert-equal 1 (legendre-symbol 2 7)))
    (define-test "legendre(3, 7) = -1 (3 is NR mod 7)"
      (assert-equal -1 (legendre-symbol 3 7)))
    (define-test "legendre(4, 7) = 1 (4 = 2² is QR)"
      (assert-equal 1 (legendre-symbol 4 7)))
    (define-test "legendre(0, 7) = 0"
      (assert-equal 0 (legendre-symbol 0 7)))
    (define-test "legendre(7, 7) = 0"
      (assert-equal 0 (legendre-symbol 7 7)))

    ;; Quadratic residue tests
    (define-test "quadratic-residue?(4, 7)"
      (assert-true (quadratic-residue? 4 7)))
    (define-test "quadratic-residue?(2, 7)"
      (assert-true (quadratic-residue? 2 7)))
    (define-test "quadratic-residue?(3, 7)"
      (assert-false (quadratic-residue? 3 7)))
    (define-test "quadratic-residue?(0, 7)"
      (assert-true (quadratic-residue? 0 7))))

  (test-group "Modular Square Root (Tonelli-Shanks)"
    ;; Simple case: p ≡ 3 (mod 4)
    ;; For p = 7: 7 ≡ 3 (mod 4), so use simple formula
    (define-test "mod-sqrt(2, 7) exists"
      (let ([r (mod-sqrt 2 7)])
        (assert-true (number? r))))
    (define-test "mod-sqrt(2, 7)² ≡ 2 (mod 7)"
      (let ([r (mod-sqrt 2 7)])
        (assert-equal 2 (mod* r r 7))))

    (define-test "mod-sqrt(4, 7) = 2 or 5"
      (let ([r (mod-sqrt 4 7)])
        (assert-true (or (= r 2) (= r 5)))))
    (define-test "mod-sqrt(4, 7)² ≡ 4 (mod 7)"
      (let ([r (mod-sqrt 4 7)])
        (assert-equal 4 (mod* r r 7))))

    ;; Non-residue returns #f
    (define-test "mod-sqrt(3, 7) = #f (3 is NR mod 7)"
      (assert-false (mod-sqrt 3 7)))

    ;; Zero
    (define-test "mod-sqrt(0, 7) = 0"
      (assert-equal 0 (mod-sqrt 0 7)))

    ;; Special case: p = 2 (GF(2) where sqrt(a) = a)
    (define-test "mod-sqrt(0, 2) = 0"
      (assert-equal 0 (mod-sqrt 0 2)))
    (define-test "mod-sqrt(1, 2) = 1"
      (assert-equal 1 (mod-sqrt 1 2)))
    (define-test "mod-sqrt(0, 2)² ≡ 0 (mod 2)"
      (assert-equal 0 (mod* (mod-sqrt 0 2) (mod-sqrt 0 2) 2)))
    (define-test "mod-sqrt(1, 2)² ≡ 1 (mod 2)"
      (assert-equal 1 (mod* (mod-sqrt 1 2) (mod-sqrt 1 2) 2)))
    (define-test "mod-sqrt(4, 2) = 0 (4 mod 2 = 0)"
      (assert-equal 0 (mod-sqrt 4 2)))
    (define-test "mod-sqrt(5, 2) = 1 (5 mod 2 = 1)"
      (assert-equal 1 (mod-sqrt 5 2)))

    ;; General case: p ≡ 1 (mod 4) - requires full Tonelli-Shanks
    ;; p = 13: 13 ≡ 1 (mod 4)
    (define-test "mod-sqrt(3, 13) exists"
      (let ([r (mod-sqrt 3 13)])
        (assert-true (number? r))))
    (define-test "mod-sqrt(3, 13)² ≡ 3 (mod 13)"
      (let ([r (mod-sqrt 3 13)])
        (assert-equal 3 (mod* r r 13))))

    (define-test "mod-sqrt(4, 13) = 2 or 11"
      (let ([r (mod-sqrt 4 13)])
        (assert-true (or (= r 2) (= r 11)))))
    (define-test "mod-sqrt(4, 13)² ≡ 4 (mod 13)"
      (let ([r (mod-sqrt 4 13)])
        (assert-equal 4 (mod* r r 13))))

    (define-test "mod-sqrt(9, 13) = 3 or 10"
      (let ([r (mod-sqrt 9 13)])
        (assert-true (or (= r 3) (= r 10)))))
    (define-test "mod-sqrt(9, 13)² ≡ 9 (mod 13)"
      (let ([r (mod-sqrt 9 13)])
        (assert-equal 9 (mod* r r 13))))

    ;; Larger primes
    ;; p = 17: 17 ≡ 1 (mod 4)
    (define-test "mod-sqrt(2, 17) exists"
      (let ([r (mod-sqrt 2 17)])
        (assert-true (number? r))))
    (define-test "mod-sqrt(2, 17)² ≡ 2 (mod 17)"
      (let ([r (mod-sqrt 2 17)])
        (assert-equal 2 (mod* r r 17))))

    ;; p = 41: 41 ≡ 1 (mod 4)
    (define-test "mod-sqrt(5, 41) exists"
      (let ([r (mod-sqrt 5 41)])
        (assert-true (number? r))))
    (define-test "mod-sqrt(5, 41)² ≡ 5 (mod 41)"
      (let ([r (mod-sqrt 5 41)])
        (assert-equal 5 (mod* r r 41))))

    ;; p = 97: larger prime, 97 ≡ 1 (mod 4)
    (define-test "mod-sqrt(2, 97) exists"
      (let ([r (mod-sqrt 2 97)])
        (assert-true (number? r))))
    (define-test "mod-sqrt(2, 97)² ≡ 2 (mod 97)"
      (let ([r (mod-sqrt 2 97)])
        (assert-equal 2 (mod* r r 97))))

    ;; Test mod-sqrt-both
    (define-test "mod-sqrt-both(4, 13) returns two roots"
      (let ([roots (mod-sqrt-both 4 13)])
        (assert-equal 2 (length roots))))
    (define-test "mod-sqrt-both(4, 13) first root squared"
      (let ([roots (mod-sqrt-both 4 13)])
        (assert-equal 4 (mod* (car roots) (car roots) 13))))
    (define-test "mod-sqrt-both(4, 13) second root squared"
      (let ([roots (mod-sqrt-both 4 13)])
        (assert-equal 4 (mod* (cadr roots) (cadr roots) 13))))
    (define-test "mod-sqrt-both(4, 13) roots sum to p"
      (let ([roots (mod-sqrt-both 4 13)])
        (assert-equal 13 (+ (car roots) (cadr roots)))))

    (define-test "mod-sqrt-both(5, 13) = #f (5 is NR mod 13)"
      (assert-false (mod-sqrt-both 5 13)))

    ;; Edge case: mod-sqrt-both(0, p) should return (0) not (0, p)
    (define-test "mod-sqrt-both(0, 7) returns single root"
      (assert-equal 1 (length (mod-sqrt-both 0 7))))
    (define-test "mod-sqrt-both(0, 7) root is 0"
      (assert-equal 0 (car (mod-sqrt-both 0 7))))

    ;; Edge case: p=2 has single roots
    (define-test "mod-sqrt-both(0, 2) returns single root"
      (assert-equal 1 (length (mod-sqrt-both 0 2))))
    (define-test "mod-sqrt-both(1, 2) returns single root"
      (assert-equal 1 (length (mod-sqrt-both 1 2))))

    ;; Large prime for crypto-like test
    ;; Mersenne prime 2^31 - 1 = 2147483647, which is ≡ 3 (mod 4)
    (define-test "mod-sqrt with large prime verifies"
      (let* ([p 2147483647]
             [a 12345678]
             [r (mod-sqrt a p)])
        (if r
            (assert-equal a (mod* r r p))
            (assert-true #t)))))

  (run-all-tests))

;; Run tests when loaded
(run-tests)
