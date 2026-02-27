;;; lattice/category/manifest.sexp — Category Theory Skill Manifest
;;;
;;; Adjunctions, natural transformations, Kan extensions, comonads,
;;; abstract interpretation, and inter-category framework.
;;; Extracted from fp skill (fold-zy24).

(skill category
  (version "0.1.0")
  (path "lattice/category")
  (purity total)
  (stability stable)
  (fuel-bound "O(n) for natural transformations, O(n^2) for adjunction verification")

  (deps (fp rewrite))  ; fp for combinators/templates/effects; rewrite for rule/engine

  (description
   "Category theory foundations applied to programming. Natural transformations,
adjunctions with triangle identities, Kan extensions, comonads, monad derivation
from adjunctions, abstract interpretation via Galois connections, and an
inter-category framework with first-class categories and indexed transforms.
Free algebras bridge to the rewrite system for operational semantics.")

  (keywords (category-theory adjunction natural-transformation kan-extension
             comonad abstract-interpretation galois-connection monad-derivation
             free-algebra inter-category))

  (aliases (category cat-theory))

  (concepts
    (concept category-theory
      (description "Abstract mathematical framework: functors, natural transformations, adjunctions, and Kan extensions applied to programming.")
      (parent programming-paradigms)
      (synonyms category abstract-algebra categorical-semantics)))

  (exports
   (natural-transform
     make-natural-transform natural-transform?
     nt-source nt-target nt-component
     nt-compose nt-id vertical-compose horizontal-compose)

   (adjunction
     make-adjunction adjunction?
     adj-left adj-right adj-unit adj-counit
     verify-triangle-identities)

   (monad-derivation
     adjunction->monad monad-return monad-bind monad-join)

   (kan-extension
     make-lan make-ran lan? ran?
     lan-compute ran-compute)

   (comonad
     make-comonad comonad?
     comonad-extract comonad-duplicate comonad-extend)

   (free-algebra
     make-free-algebra free-algebra?
     free-inject free-fold free-forgetful-adjunction)

   (abstract-interp
     make-galois-connection galois-connection?
     gc-abstract gc-concrete gc-compose
     abstract-eval abstract-fixpoint)

   (multi-category
     make-category category? cat-objects cat-morphisms cat-compose cat-id
     make-functor-general functor-general?))

  (modules
   (natural-transform "natural-transform.ss" "Natural transformations with composition")
   (adjunction "adjunction.ss" "Adjunctions with triangle identities")
   (monad-derivation "monad-derivation.ss" "Monads from adjunctions")
   (kleisli "kleisli.ss" "Kleisli categories from monads")
   (common-monads "common-monads.ss" "Standard monad derivations")
   (comonad "comonad.ss" "Comonads (Store, Env, Traced, Stream)")
   (free-algebra "free-algebra.ss" "Free algebras and Free-Forgetful adjunction")
   (effect-category "effect-category.ss" "Categorical foundations of algebraic effects")
   (kan-extension "kan-extension.ss" "Left and right Kan extensions")
   (limits "limits.ss" "Categorical limits and colimits")
   (logic-adjunction "logic-adjunction.ss" "Logic adjunctions (Galois connections)")
   (state-store-adjunction "state-store-adjunction.ss" "State-Store adjunction")
   (abstract-interp "abstract-interp.ss" "Abstract interpretation via Galois connections"))

  (submodules (
    ((subdir "multi")
     (description "Inter-category framework: first-class categories, indexed transforms")
     (modules
      (multi-category "category.ss" "Categories as first-class values")
      (functor-general "functor-general.ss" "Inter-category functors F : C -> D")
      (nat-transform-indexed "nat-transform-indexed.ss" "Indexed natural transformations")
      (adjunction-inter "adjunction-inter.ss" "Inter-category adjunctions")
      (effect-adjunction "effect-adjunction.ss" "Effect adjunctions with correct counit"))))))
