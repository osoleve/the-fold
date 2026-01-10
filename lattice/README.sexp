;;; lattice/README.sexp — The Verified Skill Lattice
;;;
;;; The lattice is a DAG of verified programs that build on each other
;;; compositionally. Every node is content-addressed, type-checked, and
;;; carries verification badges (totality, fuel bounds, purity).

(readme
 (name "The Lattice")
 (version "0.1.0")

 (description
  "The lattice is The Fold's standard library and skill tree unified.
   'Standard library' is simply the foundational nodes — skills with
   no lattice dependencies, only core. Everything else grows from there.

   The lattice enables:
   - Compositional verification (if deps verify, so can you)
   - Curriculum learning (topological sort = learning order)
   - Catastrophic forgetting detection (re-test prerequisites)
   - Hash-verifiable edits (content-addressed identity)")

 (structure
  (tiers
   (tier-0 "Foundational — no lattice deps, only core"
           (modules linalg data algebra text random))
   (tier-1 "Intermediate — builds on tier 0"
           (modules numeric geometry autodiff fp query dsl info))
   (tier-2+ "Advanced — multiple dependencies"
           (modules physics tiles sim automata number-theory pipeline))))

 (purity-levels
  (total "Pure and guaranteed to terminate (fuel-bounded)")
  (partial "Pure but may require fuel to bound execution")
  (effect "Has side effects or depends on outside world"))

 (manifests
  "Each skill has a manifest.sexp declaring:
   - version: Semantic version
   - purity: total | partial | effect
   - fuel-bound: Big-O complexity estimate
   - deps: List of lattice dependencies
   - exports: Public API
   - modules: Files in this skill

   The root index.sexp is computed from manifests.")

 (imports
  "To use a lattice skill:"
  (example "(load \"lattice/linalg/vec.ss\")")
  (example "(import (lattice linalg vec))  ; planned module syntax"))

 (contributing
  "The lattice grows through verified contributions:
   1. Create skill in user/ or lattice/contrib/
   2. Add manifest.sexp with deps and purity
   3. Verification badges computed automatically
   4. Mature skills may be promoted up tiers")

 (see-also
  (core "Language kernel — what The Fold IS")
  (shell "Impure boundary — how it touches the world")
  (user "Playground for experiments")))
