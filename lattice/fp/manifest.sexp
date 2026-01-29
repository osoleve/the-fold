;;; lattice/fp/manifest.sexp — Functional Programming Toolkit Manifest
;;;
;;; Comprehensive FP infrastructure for The Fold implementing Haskell-style
;;; type classes and abstractions using dictionary-passing style.

(skill fp
  (version "0.1.0")
  (tier 1)
  (path "lattice/fp")
  (purity mixed)  ; Most modules pure, some have debug output
  (stability stable)
  (fuel-bound 10000)

  (deps (algebra))  ; Depends on algebraic structures (field.ss, polynomial.ss)

  (description
   "A comprehensive functional programming library implementing Haskell-style
type classes and abstractions in Scheme. Uses dictionary-passing style
for type class polymorphism, maintaining purity and explicit dependencies.

Key design principles:
- Type classes as first-class values (dictionaries)
- Explicit dictionary passing (no global instances)
- Composability through higher-order functions
- Lawful abstractions verified via property-based testing")

  (keywords (functional-programming type-classes monads parsers
             streams logic-programming game-theory symbolic-computation
             units-of-measure control-systems term-rewriting))

  (aliases (fp functional))

  ;;; ====
  ;;; Exports by Submodule
  ;;; ====


  (exports
   ;; FP is an umbrella skill. Import from sub-skills directly:
   ;; - fp/optics: Complete optics tower (371 exports)
   ;; - fp/game: Game theory (110 exports)
   ;; - fp/clp: Constraint logic programming (103 exports)
   ;;
   ;; Source-annotated exports from fp root modules are minimal.
   ;; See lattice/fp/*/manifest.sexp for detailed exports.
   )


  ;;; ====
  ;;; Modules by Subdirectory
  ;;; ====

  (modules (
    ((subdir "control")
     (description "Monads, algebraic effects, continuations, and free monads")
     (files (
       "state.ss"        ; State monad with get/put/modify
       "effects.ss"      ; Algebraic effects with handlers
       "free.ss"         ; Free monad for DSL interpretation
       "continuation.ss" ; Continuation monad with shift/reset
       "reader.ss"       ; Reader monad (environment passing)
       "writer.ss"       ; Writer monad (accumulation)
       "transformers.ss" ; Monad transformers (StateT, ReaderT, etc.)
       "alternative.ss"  ; Alternative and MonadPlus
       "comonad.ss")))   ; Comonad (dual of monad)

    ((subdir "numeric")
     (description "Transcendental and mathematical functions")
     (files (
       "transcendental.ss"   ; exp, log, sin, cos, tan, hyperbolic, etc.
       "bignum.ss"           ; Arbitrary precision integers and rationals
       "differentiable.ss"   ; Differentiable type class for AD
       "vec-matrix-instances.ss"))) ; Type class instances for linalg

    ((subdir "parsing")
     (description "Monadic parser combinators with packrat memoization")
     (files (
       "parser.ss"           ; Core combinator framework
       "parser-dsl.ss"       ; DSL for parser construction
       "parser-examples.ss"  ; JSON, S-expr, arithmetic parsers
       "regex.ss")))         ; Regular expression matching

    ((subdir "meta")
     (description "FP combinators, DSL utilities, logic programming, Result type")
     (files (
       "combinators.ss"  ; id, const, compose, Maybe, Either, do-monad
       "result.ss"       ; Result type utilities, validation, error handling
       "dsl.ss"          ; DSL builder with tagless final support
       "logic.ss"        ; miniKanren-style logic programming
       "strategies.ss"   ; Rewriting strategies
       "fsm.ss")))       ; Finite state machines

    ((subdir "data")
     (description "Lazy streams and functional data structures")
     (files (
       "stream.ss"       ; Lazy infinite streams with memoization
       "dlist.ss"        ; Difference lists (O(1) append)
       "finger-tree.ss"  ; 2-3 finger trees
       "heap.ss"         ; Priority heaps
       "nonempty.ss"     ; Non-empty lists
       "ring-buffer.ss"  ; Circular buffers
       "rope.ss"         ; Efficient string manipulation
       "zipper.ss"        ; Functional cursor/zipper (lists)
       "tree-zipper.ss"   ; Rose tree zipper with navigation
       "generic-zipper.ss" ; Type-theoretic zipper derivation
       "zipper-lens.ss"))) ; Zipper-lens integration (lenses, affines, comonad connection)

    ((subdir "optics")
     (description "Complete optics tower: Iso, Lens, Prism, Affine, Traversal, Fold, Getter, Setter")
     (files (
       "optics.ss")))     ; Unified optics with composition and operators

    ((subdir "game")
     (description "Game theory: normal form, cooperative games, matching, voting, fair division")
     (files (
       "normal-form.ss"   ; Normal form games, Nash equilibrium, IESDS
       "coop-games.ss"    ; Cooperative games, Shapley value, core, bargaining
       "matching.ss"      ; Two-sided matching, Gale-Shapley, assignment games
       "voting.ss"        ; Social choice: plurality, Borda, Condorcet, Schulze
       "voting-games.ss"  ; Bridge: voting rules to simple games, power indices
       "fair-division.ss"))) ; Cake cutting, adjusted winner, EF1, maximin share

    ((subdir "symbolic")
     (description "Symbolic computation and computer algebra")
     (files (
       "expr.ss"           ; Symbolic expression representation
       "diff.ss"           ; Symbolic differentiation
       "simplify.ss"       ; Algebraic simplification
       "integrate.ss"      ; Symbolic integration
       "solve.ss"          ; Symbolic equation solving
       "poly-canonical.ss"))) ; Polynomial canonical form conversion

    ((subdir "measure")
     (description "Units of measure with dimensional analysis")
     (files (
       "units.ss")))       ; SI units, prefixes, dimension checking

    ((subdir "control-systems")
     (description "Control theory and dynamical systems")
     (files (
       "state-space.ss"       ; LTI state space models, controllability
       "kalman.ss"            ; Kalman filter for state estimation
       "discrete-control.ss"  ; Continuous-to-discrete conversion (c2d-zoh, c2d-tustin)
       "stability.ss"         ; Stability analysis (Routh-Hurwitz, Lyapunov, Nyquist)
       "z-transform.ss"       ; Discrete transfer functions (Z-domain)
       "transfer-function.ss" ; Continuous transfer functions (S-domain)
       "controller-design.ss" ; Controller synthesis (LQR, pole placement, PID tuning)
       "digital-pid.ss"       ; Discrete PID with anti-windup and tuning rules
       "tf-convert.ss"        ; State-space <-> transfer function conversion
       "poly-algebra.ss")))   ; Polynomial algebra (GCD, simplification, coprimality)

    ((subdir "analysis")
     (description "Cost analysis and parallelization heuristics")
     (files (
       "cost-analysis.ss"))) ; Work estimation, hotspot detection

    ((subdir "rewrite")
     (description "Term rewriting systems and proof tactics")
     (files (
       "rule.ss"         ; Rule data structures and accessors
       "engine.ss"       ; Pattern matching and rewriting engine
       "trace.ss"        ; Rewrite trace visualization
       "laws.ss"         ; Algebraic laws (monoid, functor, monad)
       "fusion-rules.ss" ; Fusion optimization rules
       "verify.ss"       ; Rule verification
       "goals.ss"        ; Goal-directed rewriting
       "proof-tactics.ss" ; Proof tactics for equational reasoning
       "sketch.ss")))    ; Program sketching with holes

    ((subdir "category")
     (description "Category theory foundations: adjunctions, natural transformations, Kan extensions, abstract interpretation")
     (files (
       "natural-transform.ss"    ; Natural transformations with composition
       "adjunction.ss"           ; Adjunctions with triangle identities
       "abstract-interp.ss"      ; Abstract interpretation via Galois connections
       "free-algebra.ss"         ; Free algebras and Free ⊣ Forgetful
       "monad-derivation.ss"     ; Monads from adjunctions
       "kan-extension.ss"        ; Left and right Kan extensions
       "comonad.ss"              ; Comonads (Store, Env, Traced, Stream)
       "state-store-adjunction.ss" ; State-Store adjunction
       "logic-adjunction.ss"     ; Logic adjunctions (Galois connections)
       "effect-category.ss"))    ; Categorical foundations of algebraic effects
     ;; Submodule: multi-category framework
     (submodules (
       ((subdir "multi")
        (description "Inter-category framework: first-class categories, indexed transforms, correct counit")
        (files (
          "category.ss"              ; Categories as first-class values
          "functor-general.ss"       ; Inter-category functors F : C → D
          "nat-transform-indexed.ss" ; Indexed natural transformations
          "adjunction-inter.ss"      ; Inter-category adjunctions
          "effect-adjunction.ss"))))))  ; Effect adjunctions with correct counit

    ;; Root-level modules
    ((subdir "")
     (description "Core FP infrastructure at lattice/fp root")
     (files (
       "protocol.ss"        ; Open protocol system for extensible dispatch
       "protocol-bundle.ss" ; Protocol bundles for reduced boilerplate
       "templates.ss")))    ; Lens infrastructure
  ))

  ;;; ====
  ;;; Design Patterns
  ;;; ====

  (patterns (
    ((pattern "Dictionary-Passing Style")
     (description "Type classes as explicit dictionary values passed as arguments")
     (rationale "Maintains purity, enables multiple instances, supports local overriding")
     (example "(define (sum-list num-dict xs) (foldl (num-add num-dict) (num-zero num-dict) xs))"))

    ((pattern "Newtype Pattern")
     (description "Wrap types to provide different type class instances")
     (example "(define-newtype (Sum n) n)  ; Sum newtype for + Monoid"))

    ((pattern "Tagless Final")
     (description "DSLs as type classes; interpreters as instances")
     (example "Parser combinator framework uses this for multiple interpretations"))

    ((pattern "Free Monads for DSLs")
     (description "Build ASTs as free monads; separate syntax from semantics")
     (benefits "Testable, composable, multiple interpreters"))

    ((pattern "Algebraic Effects")
     (description "Effects as first-class values with composable handlers")
     (benefits "Modular effect handling, effect polymorphism"))))

  ;;; ====
  ;;; Testing
  ;;; ====

  (testing (
    ((framework "Property-Based Testing")
     (description "QuickCheck-style property testing for verifying laws")
     (location "lattice/fp/meta/quickcheck.ss"))

    ((framework "Law Checking")
     (description "Automated verification of type class laws")
     (location "lattice/fp/rewrite/laws.ss"))))

  ;;; ====
  ;;; Future Work
  ;;; ====

  (future-work (
    "High-performance BigNum in fold-rs (Rust FFI)"
    "Compile-time instance resolution and specialization"
    "Effect system integration with algebraic effects"
    "Dependent types integration (Pi, Sigma)"
    "Linear types for resource management"
    "Refinement types for precondition checking"
    "Automatic differentiation integration"
    "Probabilistic programming primitives"))

  (see-also (
    "lattice/fp/README.sexp"
    "core/types/types.ss"
    "lattice/data/manifest.sexp"
    "CLAUDE.md")))
