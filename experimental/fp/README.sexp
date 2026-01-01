;;; experimental/fp/README.sexp
;;; Relocated FP toolkit modules
;;;
;;; These modules were moved from core/fp/ during the tech debt cleanup
;;; on 2026-01-01. They are not currently used by the core system but
;;; are preserved here for potential future use.
;;;
;;; To use a module from here, load it with the experimental path:
;;;   (load "experimental/fp/data/heap.ss")
;;;
;;; If a module proves useful, it can be promoted back to core/fp/
;;; after proper vetting and integration.

((status . experimental)
 (moved-from . "core/fp/")
 (moved-date . "2026-01-01")
 (reason . "tech-debt-cleanup")

 (categories
  ((control . "Monad transformers, reader, writer, validation, alternative")
   (data . "Persistent data structures: heap, trie, rope, zipper, finger-tree, etc.")
   (functors . "Bifunctor, profunctor, comonad, arrow, recursion-schemes, traversable")
   (instances . "Type class instances for graphs and matrices")
   (meta . "Logic programming utilities")
   (numeric . "Bignum, intervals, units, differentiable, semiring")
   (optics . "Lenses, prisms, profunctor optics")
   (parsing . "Regex, FSM, parser DSL extensions")
   (testing . "QuickCheck-style property testing")
   (typeclasses . "Type class definitions, algebraic structures, laws")))

 (note . "These modules may have internal load paths that need updating if used.
          Check for (load \"core/fp/...\") and update to (load \"experimental/fp/...\")
          if you want to use these modules.")

 (promotion-criteria
  ("Module is actually used by non-test code"
   "Module has comprehensive tests"
   "Module follows core purity principles"
   "Clear value proposition for the system")))
