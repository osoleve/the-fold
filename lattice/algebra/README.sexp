((name "algebra")
 (purpose "Algebraic structures, polynomial algebra, and Gröbner bases")
 (description
  "Pure implementation of fundamental algebraic structures.
   Provides groups, rings, polynomial rings over arbitrary coefficients,
   multivariate polynomials with configurable monomial orderings,
   and Gröbner basis computation. Enables generic programming over
   algebraic types for symbolic computation and computer algebra.")
 (modules
  ((group.ss "Group theory: cyclic, symmetric, dihedral groups and homomorphisms")
   (ring.ss "Ring theory: rings, ideals, homomorphisms, and standard rings Z_n")
   (polynomial.ss "Univariate polynomials: arithmetic, division, GCD, factorization, interpolation")
   (multivariate.ss "Multivariate polynomials: sparse representation, lex/grlex/grevlex orderings")
   (groebner.ss "Gröbner bases: Buchberger's algorithm, ideal membership, reduction")))
 (tests
  ((test-group.ss "Group theory tests")
   (test-polynomial.ss "Polynomial algebra tests: univariate, multivariate, Gröbner")))
 (dependencies (base))
 (load-order "group.ss -> ring.ss -> polynomial.ss -> multivariate.ss -> groebner.ss"))
