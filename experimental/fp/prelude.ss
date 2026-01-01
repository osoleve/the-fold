;;; fabric/stitches/fp/prelude.ss — FP Toolkit Prelude
;;;
;;; Load this file to get the core FP abstractions.
;;; Modules are loaded in dependency order.
;;;
;;; Usage:
;;;   (load "core/fp/prelude.ss")
;;;
;;; This loads:
;;;   - Core combinators (id, compose, curry, etc.)
;;;   - Algebraic structures (Semigroup, Monoid, Group)
;;;   - Functor variants (Bifunctor, Contravariant, Profunctor)
;;;   - Traversable and Foldable
;;;   - Monad ecosystem (Reader, Writer, State, Continuation, Free)
;;;   - Monad transformers
;;;   - Validation
;;;   - Optics (Lens, Prism, Iso, etc.)
;;;   - Comonad
;;;   - Arrow
;;;   - Recursion schemes
;;;   - Alternative
;;;
;;; For specialized modules (parser, stream, data structures),
;;; load them individually as needed.

(display "Loading FP Prelude...
")

;;; ============================================================
;;; Foundation
;;; ============================================================

;; Base combinators and utilities
(load "core/fp/meta/combinators.ss")
(display "  ✓ combinators
")

;; Algebraic structures
(load "core/fp/typeclasses/algebraic.ss")
(display "  ✓ algebraic (Semigroup, Monoid, Group)
")

;; Unified typeclass hierarchy
(load "core/fp/typeclasses/typeclasses.ss")
(display "  ✓ typeclasses (Functor, Applicative, Monad, Comonad)
")

;;; ============================================================
;;; Functor Variants
;;; ============================================================

;; Bifunctor (Either, Pair, Validation, These)
(load "core/fp/functors/bifunctor.ss")
(display "  ✓ bifunctor
")

;; Contravariant (Predicate, Comparison, Equivalence)
(load "core/fp/functors/contravariant.ss")
(display "  ✓ contravariant
")

;; Profunctor (for advanced optics)
(load "core/fp/functors/profunctor.ss")
(display "  ✓ profunctor
")

;;; ============================================================
;;; Traversable & Foldable
;;; ============================================================

;; Already loads algebraic.ss
(load "core/fp/functors/traversable.ss")
(display "  ✓ traversable (Foldable, Traversable)
")

;;; ============================================================
;;; Monad Ecosystem
;;; ============================================================

;; Reader monad
(load "core/fp/control/reader.ss")
(display "  ✓ reader
")

;; Writer monad
(load "core/fp/control/writer.ss")
(display "  ✓ writer
")

;; State monad
(load "core/fp/control/state.ss")
(display "  ✓ state
")

;; Continuation monad
(load "core/fp/control/continuation.ss")
(display "  ✓ continuation
")

;; Free monad
(load "core/fp/control/free.ss")
(display "  ✓ free
")

;; Monad transformers
(load "core/fp/control/transformers.ss")
(display "  ✓ transformers
")

;; Validation (Applicative error accumulation)
(load "core/fp/control/validation.ss")
(display "  ✓ validation
")

;;; ============================================================
;;; Optics
;;; ============================================================

;; Simple lenses
(load "core/fp/optics/lens.ss")
(display "  ✓ lens (simple)
")

;; van Laarhoven optics
(load "core/fp/optics/optics.ss")
(display "  ✓ optics (van Laarhoven)
")

;;; ============================================================
;;; Comonad
;;; ============================================================

(load "core/fp/functors/comonad.ss")
(display "  ✓ comonad
")

;;; ============================================================
;;; Arrow
;;; ============================================================

(load "core/fp/functors/arrow.ss")
(display "  ✓ arrow
")

;;; ============================================================
;;; Recursion Schemes
;;; ============================================================

(load "core/fp/functors/recursion-schemes.ss")
(display "  ✓ recursion-schemes
")

;;; ============================================================
;;; Alternative
;;; ============================================================

(load "core/fp/control/alternative.ss")
(display "  ✓ alternative
")

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "FP Prelude loaded. Core abstractions available:
")
(display "  • Combinators: id, const, compose, pipe, curry, flip
")
(display "  • Algebraic: Semigroup, Monoid, Group (+ instances)
")
(display "  • Typeclasses: Functor, Applicative, Monad, Comonad (unified)
")
(display "  • Functors: Bifunctor, Contravariant, Profunctor
")
(display "  • Traversable: fold-map, traverse, sequence
")
(display "  • Monads: Reader, Writer, State, Continuation, Free
")
(display "  • Transformers: MaybeT, EitherT, ReaderT, StateT, WriterT
")
(display "  • Validation: error accumulation
")
(display "  • Optics: Lens, Prism, Iso, Affine, Traversal
")
(display "  • Comonad: Env, Store, Stream, Zipper, Traced
")
(display "  • Arrow: arr, >>>, first, ArrowChoice, ArrowLoop
")
(display "  • Recursion: cata, ana, hylo, para, apo, histo, futu
")
(display "  • Alternative: empty, <|>, some, many
")
(display "
")
(display "For additional modules, load individually:
")
(display "  • fp/stream.ss      — Lazy streams
")
(display "  • fp/parser.ss      — Parser combinators
")
(display "  • fp/effects.ss     — Algebraic effects
")
(display "  • fp/semiring.ss    — Semirings
")
(display "  • fp/dlist.ss       — Difference lists
")
(display "  • fp/finger-tree.ss — Finger trees
")
(display "  • fp/heap.ss        — Heaps
")
(display "  • fp/trie.ss        — Tries
")
(display "  • fp/graph.ss       — Graphs
")
(display "  • fp/zipper.ss      — Zippers
")
(display "  • fp/quickcheck.ss  — Property testing
")
(display "
")
