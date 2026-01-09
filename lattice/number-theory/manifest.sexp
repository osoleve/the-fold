(skill number-theory
  (version "0.1.0")
  (tier 0)
  (path "lattice/number-theory")
  (purity total)
  (stability stable)
  (fuel-bound "O(log n) for modular exponentiation, O(log(min(a,b))) for gcd")
  (deps ())

  (description
   "Number theory primitives for modular arithmetic and cryptographic operations.
    Provides modular addition, subtraction, multiplication, and exponentiation.
    Includes extended Euclidean algorithm, modular inverse, Chinese Remainder
    Theorem, and Montgomery multiplication for efficient modular arithmetic.")

  (keywords (number-theory modular-arithmetic cryptography gcd extended-gcd
             chinese-remainder-theorem montgomery-multiplication mod-inverse))
  (aliases (modular mod-arith nt))

  (exports
   (modular
    mod+ mod- mod* mod-expt
    extended-gcd gcd mod-inverse crt
    montgomery-reduce montgomery-setup montgomery-mult
    to-montgomery from-montgomery montgomery-expt))

  (modules
   (modular "modular.ss"
    "Modular arithmetic operations: addition, subtraction, multiplication,
     exponentiation. Extended GCD, modular inverse, CRT. Montgomery form
     for fast repeated modular multiplication.")))
