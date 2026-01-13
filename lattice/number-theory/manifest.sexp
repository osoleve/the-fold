(skill number-theory
  (version "0.2.0")
  (tier 0)
  (path "lattice/number-theory")
  (purity total)
  (stability stable)
  (fuel-bound "O(log n) for modular exponentiation, O(√n) for primality, O(n^(1/4)) for factorization")
  (deps ())

  (description
   "Number theory primitives for modular arithmetic, primality testing, and
    integer factorization. Provides modular operations, Chinese Remainder Theorem,
    Montgomery multiplication, Miller-Rabin primality testing, Pollard's rho
    factorization, and number-theoretic functions (Euler's totient, Carmichael's
    lambda, Möbius, Jacobi symbol).")

  (keywords (number-theory modular-arithmetic cryptography gcd extended-gcd
             chinese-remainder-theorem montgomery-multiplication mod-inverse
             primality miller-rabin factorization euler-totient jacobi-symbol
             divisors primes sieve))
  (aliases (modular mod-arith nt primes))

  (exports
   (modular
    mod+ mod- mod* mod-expt
    extended-gcd gcd mod-inverse crt
    montgomery-reduce montgomery-setup montgomery-mult
    to-montgomery from-montgomery montgomery-expt)

   (primality
    ;; Primality testing
    prime? composite? miller-rabin?
    ;; Factorization
    trial-division factorize prime-factorization
    divisors
    ;; GCD/LCM
    lcm gcd* lcm*
    ;; Number-theoretic functions
    euler-totient carmichael-lambda mobius radical
    ;; Prime navigation
    next-prime prev-prime nth-prime primes-up-to prime-pi
    coprime?
    ;; Perfect powers
    isqrt is-perfect-square? is-perfect-power? integer-root
    ;; Legendre/Jacobi symbols
    legendre-symbol jacobi-symbol))

  (modules
   (modular "modular.ss"
    "Modular arithmetic operations: addition, subtraction, multiplication,
     exponentiation. Extended GCD, modular inverse, CRT. Montgomery form
     for fast repeated modular multiplication.")

   (primality "primality.ss"
    "Primality testing (trial division, Miller-Rabin), integer factorization
     (trial division, Pollard's rho), number-theoretic functions (Euler's
     totient, Carmichael's lambda, Möbius, radical), prime navigation and
     sieve of Eratosthenes, Legendre and Jacobi symbols.")))
