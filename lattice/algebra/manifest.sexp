;;; lattice/algebra/manifest.sexp — Abstract Algebra Skill Manifest

(skill algebra
  (version "0.2.0")
  (tier 0)
  (path "lattice/algebra")
  (purity total)
  (stability stable)
  (fuel-bound "O(n^3) for axiom verification, O(n^2) for group/polynomial operations, exponential for Gröbner bases")
  (deps ())  ; Tier 0 - no lattice dependencies

  (description
   "Pure functional abstract algebra library implementing groups, rings,
    polynomial rings, and their associated structures. Provides algebraic
    operations, axiom verification, homomorphisms, ideals, polynomial
    arithmetic, GCD, factorization, interpolation, multivariate polynomials,
    and Gröbner basis computation.")

  (keywords (algebra group ring field ideal homomorphism
             cyclic-group symmetric-group dihedral-group
             subgroup kernel image quotient axiom
             polynomial gcd factorization interpolation
             multivariate monomial-ordering groebner-basis))
  (aliases (abstract-algebra group-theory ring-theory polynomial-algebra))

  (exports
   (group make-group group? group-elements group-op group-identity
          group-inverse-fn group-equal-fn group-order group-compose
          group-inverse group-power group-equal? element-order
          verify-closure verify-associativity verify-identity
          verify-inverses verify-group-axioms
          make-cyclic-group Z cyclic-generator
          permutation-apply permutation-compose permutation-inverse
          permutation-identity permutation-equal? all-permutations
          make-symmetric-group S cycle-to-permutation transposition
          permutation-parity make-dihedral-group D
          is-subgroup? generate-subgroup make-homomorphism
          homomorphism? homomorphism-source homomorphism-target
          homomorphism-mapping homomorphism-apply verify-homomorphism
          is-isomorphism? kernel image cayley-table)
   (ring make-ring ring? ring-elements ring-add-op ring-mul-op
         ring-zero ring-one ring-neg-fn ring-equal-fn ring-order
         ring-add ring-mul ring-neg ring-sub ring-equal?
         ring-power ring-sum ring-product
         is-commutative-ring? is-zero-divisor? has-zero-divisors?
         is-integral-domain? is-unit? ring-units
         make-ring-zn make-ring-z
         make-ring-homomorphism ring-hom? ring-hom-source
         ring-hom-target ring-hom-phi ring-hom-apply
         is-valid-ring-homomorphism? ring-hom-kernel ring-hom-image
         make-ideal ideal? ideal-ring ideal-elements is-valid-ideal?
         make-principal-ideal ideal-sum ideal-product
         is-prime-ideal? is-maximal-ideal?)
   (polynomial make-polynomial polynomial? poly-ring poly-coeffs
               poly-degree poly-leading-coeff poly-coeff-at poly-zero?
               poly-zero-over poly-one-over poly-constant poly-monomial poly-x
               poly-add poly-neg poly-sub poly-scale poly-mul poly-power
               poly-equal? poly-divmod poly-div poly-mod poly-divides?
               poly-gcd poly-make-monic poly-extended-gcd poly-lcm
               poly-eval poly-derivative poly-square-free
               poly-square-free-factorization
               poly-lagrange-interpolate poly-newton-interpolate
               make-polynomial-ring poly->string)
   (multivariate make-monomial mono-one mono-var mono-power
                 mono-degree mono-degree-in mono-vars
                 mono-mul mono-divides? mono-div mono-lcm mono-gcd mono-equal?
                 mono-compare-lex mono-compare-grlex mono-compare-grevlex
                 make-ordering
                 make-mpoly mpoly? mpoly-ring mpoly-vars mpoly-ordering mpoly-terms
                 mpoly-zero mpoly-one mpoly-constant mpoly-var mpoly-from-terms
                 mpoly-zero? mpoly-degree mpoly-degree-in
                 mpoly-leading-term mpoly-leading-coeff mpoly-leading-mono
                 mpoly-add mpoly-neg mpoly-sub mpoly-scale mpoly-mul-term
                 mpoly-mul mpoly-power mpoly-equal? mpoly-divmod
                 mpoly-eval mpoly->string)
   (groebner s-polynomial reduce-poly reduce-poly-full
             buchberger reduce-basis mpoly-make-monic
             minimize-basis interreduce
             ideal-membership? solve-system normal-form
             is-groebner-basis? groebner->string))

  (modules
   (group "group.ss" "Group theory: cyclic, symmetric, dihedral groups and homomorphisms")
   (ring "ring.ss" "Ring theory: rings, ideals, homomorphisms, and standard rings")
   (polynomial "polynomial.ss" "Univariate polynomial rings: arithmetic, division, GCD, factorization, interpolation")
   (multivariate "multivariate.ss" "Multivariate polynomials: sparse representation, monomial orderings, division")
   (groebner "groebner.ss" "Gröbner bases: Buchberger's algorithm, ideal membership, reduction")))
