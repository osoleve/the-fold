((name "number-theory")
 (purpose "Number-theoretic foundations for cryptography and mathematics")
 (description "Modular arithmetic, primality testing, and integer factorization.
Provides the mathematical foundation for cryptographic primitives,
primality testing, finite field arithmetic, and number-theoretic functions.")
 (modules
  ((modular.ss "Modular arithmetic - 61 tests")
   (primality.ss "Primality testing and factorization - 152 tests")
   (test-modular.ss "Modular arithmetic test suite")
   (test-primality.ss "Primality and factorization test suite")))
 (dependencies (base))
 (exports
  ;; === modular.ss ===
  ;; Basic modular operations
  (mod+ "Modular addition: (a + b) mod m")
  (mod- "Modular subtraction: (a - b) mod m")
  (mod* "Modular multiplication: (a * b) mod m")
  (mod-expt "Fast modular exponentiation using square-and-multiply")
  (gcd "Greatest common divisor")
  (extended-gcd "Extended Euclidean algorithm with Bezout coefficients")
  (mod-inverse "Modular multiplicative inverse (returns #f if none exists)")
  (crt "Chinese Remainder Theorem solver")
  (montgomery-setup "Initialize Montgomery multiplication parameters")
  (montgomery-mult "Montgomery multiplication")
  (montgomery-expt "Optimized modular exponentiation using Montgomery")
  (to-montgomery "Convert to Montgomery representation")
  (from-montgomery "Convert from Montgomery representation")

  ;; === primality.ss ===
  ;; Primality testing
  (prime? "Deterministic primality test via trial division O(sqrt n)")
  (composite? "Test if n is composite")
  (miller-rabin? "Probabilistic Miller-Rabin primality test")

  ;; Integer factorization
  (trial-division "Factor via trial division")
  (factorize "General factorization (trial division + Pollard rho)")
  (prime-factorization "Factorization as (prime . exponent) pairs")
  (divisors "All positive divisors in sorted order")

  ;; GCD/LCM
  (lcm "Least common multiple")
  (gcd* "GCD of a list")
  (lcm* "LCM of a list")

  ;; Number-theoretic functions
  (euler-totient "Euler's totient function phi(n)")
  (carmichael-lambda "Carmichael's lambda function")
  (mobius "Mobius function mu(n)")
  (radical "Radical of n: product of distinct prime factors")

  ;; Prime navigation
  (next-prime "Smallest prime > n")
  (prev-prime "Largest prime < n")
  (nth-prime "The nth prime (1-indexed)")
  (primes-up-to "Sieve of Eratosthenes")
  (prime-pi "Prime counting function pi(n)")
  (coprime? "Test if gcd(a,b) = 1")

  ;; Perfect powers
  (isqrt "Integer square root")
  (is-perfect-square? "Test if n is a perfect square")
  (is-perfect-power? "Test if n = a^b, return (a . b) or #f")
  (integer-root "Compute floor(n^(1/k))")

  ;; Legendre/Jacobi symbols
  (legendre-symbol "Legendre symbol (a/p) for prime p")
  (jacobi-symbol "Jacobi symbol (a/n) for odd n"))

 (features
  "=== Modular Arithmetic ==="
  "  Modular arithmetic operations (add, sub, mul)"
  "  Fast modular exponentiation (O(log n) square-and-multiply)"
  "  Extended Euclidean algorithm with Bezout identity"
  "  Modular multiplicative inverse"
  "  Chinese Remainder Theorem solver"
  "  Montgomery multiplication optimization"
  ""
  "=== Primality Testing ==="
  "  Trial division O(sqrt n)"
  "  Miller-Rabin with deterministic witnesses for n < 3.3e24"
  "  Correctly handles Carmichael numbers"
  ""
  "=== Factorization ==="
  "  Trial division for small factors"
  "  Pollard's rho O(n^(1/4)) expected"
  "  Prime factorization with exponents"
  "  Divisor enumeration"
  ""
  "=== Number-Theoretic Functions ==="
  "  Euler's totient, Carmichael's lambda"
  "  Mobius function, radical"
  "  Legendre and Jacobi symbols"
  ""
  "=== Prime Utilities ==="
  "  Sieve of Eratosthenes"
  "  Prime navigation (next, prev, nth)"
  "  Prime counting function"
  ""
  "  213 comprehensive tests total")

 (usage-examples
  ((primality
    "(prime? 997)            ; => #t"
    "(prime? 1001)           ; => #f (7 x 11 x 13)"
    "(miller-rabin? 104729)  ; => #t (10000th prime)")

   (factorization
    "(factorize 360)         ; => (2 2 2 3 3 5)"
    "(prime-factorization 360) ; => ((2 . 3) (3 . 2) (5 . 1))"
    "(divisors 12)           ; => (1 2 3 4 6 12)")

   (number-theory
    "(euler-totient 12)      ; => 4"
    "(mobius 30)             ; => -1 (squarefree, 3 factors)"
    "(jacobi-symbol 2 15)    ; => 1")

   (primes
    "(primes-up-to 20)       ; => (2 3 5 7 11 13 17 19)"
    "(next-prime 100)        ; => 101"
    "(nth-prime 10)          ; => 29")

   (modular
    "(mod-expt 2 10 1000)    ; => 24"
    "(mod-inverse 3 7)       ; => 5"
    "(crt '(2 3 2) '(3 5 7)) ; => 23")))

 (performance
  "prime?: O(sqrt n)"
  "miller-rabin?: O(k log^3 n) for k rounds"
  "factorize: O(n^(1/4)) expected via Pollard's rho"
  "primes-up-to: O(n log log n) sieve"
  "euler-totient: O(sqrt n) via factorization"
  "mod-expt: O(log exp)"
  "crt: O(n log M) where M is product of moduli")

 (notes
  "All functions are pure and total (assume perfect input)"
  "Miller-Rabin uses deterministic witnesses for n < 3.3 x 10^24"
  "Pollard's rho may fall back to trial division for difficult composites"
  "Montgomery multiplication trades setup cost for faster repeated operations"))
