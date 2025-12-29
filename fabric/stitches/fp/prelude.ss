;;; fabric/stitches/fp/prelude.ss — FP Toolkit Prelude
;;;
;;; Load this file to get the core FP abstractions.
;;; Modules are loaded in dependency order.
;;;
;;; Usage:
;;;   (load "fabric/stitches/fp/prelude.ss")
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
(load "fabric/stitches/fp/combinators.ss")
(display "  ✓ combinators
")

;; Algebraic structures
(load "fabric/stitches/fp/algebraic.ss")
(display "  ✓ algebraic (Semigroup, Monoid, Group)
")

;;; ============================================================
;;; Functor Variants
;;; ============================================================

;; Bifunctor (Either, Pair, Validation, These)
(load "fabric/stitches/fp/bifunctor.ss")
(display "  ✓ bifunctor
")

;; Contravariant (Predicate, Comparison, Equivalence)
(load "fabric/stitches/fp/contravariant.ss")
(display "  ✓ contravariant
")

;; Profunctor (for advanced optics)
(load "fabric/stitches/fp/profunctor.ss")
(display "  ✓ profunctor
")

;;; ============================================================
;;; Traversable & Foldable
;;; ============================================================

;; Already loads algebraic.ss
(load "fabric/stitches/fp/traversable.ss")
(display "  ✓ traversable (Foldable, Traversable)
")

;;; ============================================================
;;; Monad Ecosystem
;;; ============================================================

;; Reader monad
(load "fabric/stitches/fp/reader.ss")
(display "  ✓ reader
")

;; Writer monad
(load "fabric/stitches/fp/writer.ss")
(display "  ✓ writer
")

;; State monad
(load "fabric/stitches/fp/state.ss")
(display "  ✓ state
")

;; Continuation monad
(load "fabric/stitches/fp/continuation.ss")
(display "  ✓ continuation
")

;; Free monad
(load "fabric/stitches/fp/free.ss")
(display "  ✓ free
")

;; Monad transformers
(load "fabric/stitches/fp/transformers.ss")
(display "  ✓ transformers
")

;; Validation (Applicative error accumulation)
(load "fabric/stitches/fp/validation.ss")
(display "  ✓ validation
")

;;; ============================================================
;;; Optics
;;; ============================================================

;; Simple lenses
(load "fabric/stitches/fp/lens.ss")
(display "  ✓ lens (simple)
")

;; van Laarhoven optics
(load "fabric/stitches/fp/optics.ss")
(display "  ✓ optics (van Laarhoven)
")

;;; ============================================================
;;; Comonad
;;; ============================================================

(load "fabric/stitches/fp/comonad.ss")
(display "  ✓ comonad
")

;;; ============================================================
;;; Arrow
;;; ============================================================

(load "fabric/stitches/fp/arrow.ss")
(display "  ✓ arrow
")

;;; ============================================================
;;; Recursion Schemes
;;; ============================================================

(load "fabric/stitches/fp/recursion-schemes.ss")
(display "  ✓ recursion-schemes
")

;;; ============================================================
;;; Alternative
;;; ============================================================

(load "fabric/stitches/fp/alternative.ss")
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
