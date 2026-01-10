;;; lattice/algebra/manifest.sexp — Abstract Algebra Skill Manifest

(skill algebra
  (version "0.1.0")
  (tier 0)
  (path "lattice/algebra")
  (purity total)
  (stability stable)
  (fuel-bound "O(n^3) for axiom verification, O(n^2) for group operations")
  (deps ())  ; Tier 0 - no lattice dependencies

  (description
   "Pure functional abstract algebra library implementing groups, rings,
    and their associated structures. Provides algebraic operations,
    axiom verification, homomorphisms, ideals, and standard constructions
    like cyclic groups Z_n, symmetric groups S_n, and dihedral groups D_n.")

  (keywords (algebra group ring field ideal homomorphism
             cyclic-group symmetric-group dihedral-group
             subgroup kernel image quotient axiom))
  (aliases (abstract-algebra group-theory ring-theory))

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
         is-prime-ideal? is-maximal-ideal?))

  (modules
   (group "group.ss" "Group theory: cyclic, symmetric, dihedral groups and homomorphisms")
   (ring "ring.ss" "Ring theory: rings, ideals, homomorphisms, and standard rings")))
