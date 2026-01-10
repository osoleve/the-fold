((name "number-theory")
 (purpose "Number-theoretic foundations for cryptography")
 (description "Modular arithmetic and number theory algorithms.
Provides the mathematical foundation for cryptographic primitives,
primality testing, and finite field arithmetic.")
 (modules
  ((modular.ss "Modular arithmetic - 61 tests")
   (test-modular.ss "Comprehensive test suite")))
 (dependencies (base))
 (exports
  ;; Basic modular operations
  (mod+ "Modular addition: (a + b) mod m")
  (mod- "Modular subtraction: (a - b) mod m")
  (mod* "Modular multiplication: (a * b) mod m")

  ;; Modular exponentiation
  (mod-expt "Fast modular exponentiation using square-and-multiply")

  ;; GCD algorithms
  (gcd "Greatest common divisor")
  (extended-gcd "Extended Euclidean algorithm with Bézout coefficients")

  ;; Modular inverse
  (mod-inverse "Modular multiplicative inverse (returns #f if none exists)")

  ;; Chinese Remainder Theorem
  (crt "CRT solver for systems of congruences")

  ;; Montgomery multiplication (optimization)
  (montgomery-setup "Initialize Montgomery multiplication parameters")
  (montgomery-mult "Montgomery multiplication")
  (montgomery-expt "Optimized modular exponentiation using Montgomery")
  (to-montgomery "Convert to Montgomery representation")
  (from-montgomery "Convert from Montgomery representation"))

 (features
  "✓ Modular arithmetic operations (add, sub, mul)"
  "✓ Fast modular exponentiation (O(log n) square-and-multiply)"
  "✓ Extended Euclidean algorithm with Bézout identity"
  "✓ Modular multiplicative inverse"
  "✓ Chinese Remainder Theorem solver"
  "✓ Montgomery multiplication optimization"
  "✓ 61 comprehensive tests with edge cases")

 (usage-examples
  ((basic-ops
    "(mod+ 7 5 12)           ; => 0"
    "(mod* 3 4 7)            ; => 5"
    "(mod-expt 2 10 1000)    ; => 24")

   (inverses
    "(mod-inverse 3 7)       ; => 5 (since 3*5 ≡ 1 mod 7)"
    "(mod-inverse 4 8)       ; => #f (no inverse, gcd(4,8)=4≠1)")

   (crt
    ";; Solve: x ≡ 2 (mod 3), x ≡ 3 (mod 5), x ≡ 2 (mod 7)"
    "(crt '(2 3 2) '(3 5 7)) ; => 23")

   (montgomery
    ";; Optimized for many operations with same modulus"
    "(montgomery-expt 2 100 1000000007) ; => 976371285")))

 (performance
  "mod-expt: O(log exp) time, O(1) space"
  "extended-gcd: O(log min(a,b)) time"
  "crt: O(n log M) where M is product of moduli"
  "montgomery-expt: ~30% faster than mod-expt for large exponents")

 (notes
  "All functions are pure and total (assume perfect input)"
  "Functions assume modulus > 0 and exponents >= 0"
  "Montgomery multiplication trades setup cost for faster repeated operations"
  "Implements Bézout's identity: gcd(a,b) = ax + by"))
