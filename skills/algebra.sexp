;;; skills/algebra.sexp — Skill curation file

(skill algebra
  (description
    "Pure functional abstract algebra library implementing groups, rings, fields,\n    polynomial rings, Galois fields (finite fields), and their associated structures.\n    Provides algebraic operations, axiom verification, homomorphisms, ideals,\n    polynomial arithmetic, GCD, factorization, interpolation, multivariate polynomials,\n    Gröbner basis computation, GF(p) prime fields, GF(p^n) extension fields,\n    GF(2^n) optimized binary fields, and irreducible polynomial utilities.\n    Polynomials require Field coefficients to enable exact division operations.\n    Includes e-graph bridge for polynomial identity proving via Gröbner bases,\n    enabling combined structural and algebraic optimization.")

  (keywords (algebra group ring field ideal homomorphism cyclic-group symmetric-group dihedral-group subgroup kernel image quotient axiom polynomial gcd factorization interpolation multivariate monomial-ordering groebner-basis galois-field finite-field gf2n binary-field irreducible primitive-element aes-field tropical semiring min-plus max-plus newton-polygon))

  (aliases (abstract-algebra group-theory ring-theory polynomial-algebra galois-fields))

  (concepts (abstract-algebra group-theory ring-theory field-theory polynomial-algebra finite-fields tropical-algebra))

  (modules
    group
    ring
    field
    polynomial
    multivariate
    groebner
    poly-bridge
    galois
    tropical
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
