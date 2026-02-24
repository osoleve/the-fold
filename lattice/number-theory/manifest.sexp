(skill number-theory
  (version "0.3.0")
  (tier 0)
  (path "lattice/number-theory")
  (purity total)
  (stability stable)
  (fuel-bound "O(log n) for modular exponentiation, O(√n) for primality, O(n^(1/4)) for factorization, O(n^1.585) for Karatsuba, O(n^1.465) for Toom-3")
  (deps ())

  (description
   "Number theory primitives for modular arithmetic, primality testing,
    integer factorization, and fast multiplication algorithms. Provides modular
    operations, Chinese Remainder Theorem, Montgomery multiplication, quadratic
    residues with Tonelli-Shanks modular square root algorithm, Miller-Rabin
    primality testing, Pollard's rho factorization, number-theoretic functions
    (Euler's totient, Carmichael's lambda, Möbius, Jacobi symbol), and
    asymptotically fast multiplication (Karatsuba, Toom-Cook) for large integers.")

  (keywords (number-theory modular-arithmetic cryptography gcd extended-gcd
             chinese-remainder-theorem montgomery-multiplication mod-inverse
             primality miller-rabin factorization euler-totient jacobi-symbol
             divisors primes sieve quadratic-residue mod-sqrt tonelli-shanks
             legendre-symbol elliptic-curve karatsuba toom-cook toom-3
             fast-multiply bignum limb schoolbook))
  (aliases (modular mod-arith nt primes fast-mult))

  (concepts
    (concept number-theory
      (description "Properties of integers: primality, modular arithmetic, factorization, and number-theoretic functions.")
      (parent mathematics)
      (synonyms nt))
    (concept modular-arithmetic
      (description "Arithmetic modulo an integer, including Chinese Remainder Theorem, Montgomery multiplication, and quadratic residues.")
      (parent number-theory)
      (synonyms modular mod-arith))
    (concept primality
      (description "Primality testing (Miller-Rabin), integer factorization (Pollard rho), Euler totient, and prime navigation.")
      (parent number-theory)
      (synonyms primes))
    (concept fast-arithmetic
      (description "Sub-quadratic multiplication algorithms: Karatsuba and Toom-Cook for large integer arithmetic.")
      (parent number-theory)
      (synonyms fast-mult)))

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
    legendre-symbol jacobi-symbol)

   (fast-multiply
    ;; Limb representation
    integer->limbs limbs->integer limbs-normalize limbs-pad-to limbs-split
    ;; Limb arithmetic
    limbs-add limbs-sub limbs-shift limb-scale
    ;; Multiplication algorithms
    limbs-multiply-schoolbook limbs-multiply-karatsuba limbs-multiply-toom3
    limbs-multiply
    ;; Integer interface
    fast-multiply karatsuba-multiply toom3-multiply fast-square
    ;; Configuration
    set-karatsuba-threshold! set-toom3-threshold! get-multiply-thresholds))

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
     sieve of Eratosthenes, Legendre and Jacobi symbols.")

   (fast-multiply "fast-multiply.ss"
    "Fast multiplication algorithms for large integers: schoolbook O(n²),
     Karatsuba O(n^1.585), and Toom-Cook/Toom-3 O(n^1.465). Operates on
     limb lists (base-B digit representation). Includes hybrid multiplier
     that automatically selects optimal algorithm based on input size.
     Foundation for arbitrary precision arithmetic.")))
