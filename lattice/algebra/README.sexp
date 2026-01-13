((name "algebra")
 (purpose "Algebraic structures, polynomial algebra, and Gröbner bases")
 (description
  "Pure implementation of fundamental algebraic structures.
   Provides groups, rings, fields, polynomial rings over arbitrary fields,
   multivariate polynomials with configurable monomial orderings,
   and Gröbner basis computation. Enables generic programming over
   algebraic types for symbolic computation and computer algebra.

   Polynomials require Field coefficients (not just Rings) to enable
   exact division operations. Standard fields provided: Q (rationals),
   R (reals), and Z_p (finite fields for prime p).")
 (modules
  ((group.ss "Group theory: cyclic, symmetric, dihedral groups and homomorphisms")
   (ring.ss "Ring theory: rings, ideals, homomorphisms, and standard rings Z_n")
   (field.ss "Field theory: fields with division, Q-field, R-field, make-field-zp")
   (polynomial.ss "Univariate polynomials over fields: arithmetic, division, GCD, interpolation")
   (multivariate.ss "Multivariate polynomials over fields: sparse representation, orderings")
   (groebner.ss "Gröbner bases: Buchberger's algorithm, ideal membership, reduction")
   (poly-bridge.ss "Bridge between numeric and algebra polynomial representations")))
 (tests
  ((test-group.ss "Group theory tests")
   (test-polynomial.ss "Polynomial algebra tests: univariate, multivariate, Gröbner")
   (test-poly-bridge.ss "Bridge module tests: conversion, prefixed ops, bridge ops")))
 (dependencies (base))
 (load-order "group.ss -> ring.ss -> field.ss -> polynomial.ss -> multivariate.ss -> groebner.ss")
 (poly-bridge-load-order "IMPORTANT: Load numeric/polynomial.ss BEFORE poly-bridge.ss to avoid name collisions")
 (usage-notes
  "Polynomials use Fields (not Rings) because polynomial division requires
   coefficient division. Use Q-field for exact rational arithmetic:

   (define p (make-polynomial Q-field '(1 2 1)))  ; 1 + 2x + x^2
   (poly-divmod p1 p2)  ; exact division with remainder

   For modular arithmetic, use finite fields:
   (define F7 (make-field-zp 7))
   (define p (make-polynomial F7 '(1 2 3))))")
 (poly-bridge-usage
  "The poly-bridge module unifies numeric (descending, vector) and algebra
   (ascending, list) polynomial representations.

   LOAD ORDER: Load numeric/polynomial.ss BEFORE poly-bridge.ss

   ;; Convert between representations
   (numeric->algebra num-poly)        ; descending vec -> ascending list
   (algebra->numeric alg-poly)        ; ascending list -> descending vec

   ;; Use prefixed algebra ops alongside numeric/polynomial.ss
   (alg-gcd p1 p2)                    ; GCD without name collision
   (alg-divmod p1 p2)                 ; division with remainder

   ;; Bridge operations work on numeric polynomials via algebra
   (bridge-gcd num-p1 num-p2)         ; GCD of numeric polys
   (bridge-simplify num den)          ; cancel common factors
   (bridge-coprime? num-p1 num-p2)    ; coprimality check

   ;; Factory functions for convenience
   (make-numeric-from-roots '(1 -1))  ; x^2 - 1 as numeric
   (make-algebra-from-descending Q-field '(1 0 -1))  ; as algebra"))
