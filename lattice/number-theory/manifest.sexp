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
    Montgomery multiplication, quadratic residues with Tonelli-Shanks modular
    square root algorithm, Miller-Rabin primality testing, Pollard's rho
    factorization, and number-theoretic functions (Euler's totient, Carmichael's
    lambda, Möbius, Jacobi symbol).")

  (keywords (number-theory modular-arithmetic cryptography gcd extended-gcd
             chinese-remainder-theorem montgomery-multiplication mod-inverse
             primality miller-rabin factorization euler-totient jacobi-symbol
             divisors primes sieve quadratic-residue mod-sqrt tonelli-shanks
             legendre-symbol elliptic-curve))
  (aliases (modular mod-arith nt primes))

  (exports
   (modular
    mod+ mod- mod* mod-expt
    extended-gcd gcd mod-inverse crt
    montgomery-reduce montgomery-setup montgomery-mult
    to-montgomery from-montgomery montgomery-expt
    ;; Quadratic residues and modular square roots
    quadratic-residue? legendre-symbol
    mod-sqrt tonelli-shanks mod-sqrt-both)

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
     for fast repeated modular multiplication. Quadratic residue testing
     and Tonelli-Shanks algorithm for modular square roots (essential for
     elliptic curve point decompression).")

   (primality "primality.ss"
    "Primality testing (trial division, Miller-Rabin), integer factorization
     (trial division, Pollard's rho), number-theoretic functions (Euler's
     totient, Carmichael's lambda, Möbius, radical), prime navigation and
     sieve of Eratosthenes, Legendre and Jacobi symbols.")))
