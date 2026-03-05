;;; skills/number-theory.sexp — Skill curation file

(skill number-theory
  (description
    "Number theory primitives for modular arithmetic, primality testing,\n    integer factorization, and fast multiplication algorithms. Provides modular\n    operations, Chinese Remainder Theorem, Montgomery multiplication, quadratic\n    residues with Tonelli-Shanks modular square root algorithm, Miller-Rabin\n    primality testing, Pollard's rho factorization, number-theoretic functions\n    (Euler's totient, Carmichael's lambda, Möbius, Jacobi symbol), and\n    asymptotically fast multiplication (Karatsuba, Toom-Cook) for large integers.")

  (keywords (number-theory modular-arithmetic cryptography gcd extended-gcd chinese-remainder-theorem montgomery-multiplication mod-inverse primality miller-rabin factorization euler-totient jacobi-symbol divisors primes sieve quadratic-residue mod-sqrt tonelli-shanks legendre-symbol elliptic-curve karatsuba toom-cook toom-3 fast-multiply bignum limb schoolbook))

  (aliases (modular mod-arith nt primes fast-mult))

  (concepts (number-theory modular-arithmetic primality fast-arithmetic))

  (modules
    modular
    primality
    fast-multiply
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
