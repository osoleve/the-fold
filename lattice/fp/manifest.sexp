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
             streams logic-programming symbolic-computation
             units-of-measure term-rewriting))

  (aliases (fp functional))

  ;;; ====
  ;;; Exports by Submodule
  ;;; ====


  (exports
   ;; Root-level re-exports from core prelude for discoverability
   (prelude ok? error? result-map result-bind result-sequence
            unwrap-ok unwrap-error)

   ;; ---- Root-level modules ----

   (protocol
     register-protocol-impl! get-protocol-impl protocol-implementations
     get-type-tag protocol-dispatch protocol-dispatch/default
     implement-protocol! protocol-exists? type-implements? list-protocols)

   (protocol-bundle
     make-protocol-bundle bundle? bundle-name bundle-slots
     make-bundle-slot slot? slot-getter slot-setter slot-label
     register-bundle! get-bundle list-bundles
     derive-bundle-runtime! implement-bundle-runtime!
     bundle-types bundle-protocols bundle-slot-count
     implements-bundle? compose-bundles assert-bundle! missing-protocols)

   (protocol-introspect
     make-protocol-doc protocol-doc? protocol-doc-name
     protocol-doc-docstring protocol-doc-signature protocol-doc-module
     register-protocol-doc! get-protocol-doc protocol-docstring
     protocols-for-type all-type-tags
     protocol-describe type-describe protocol-matrix protocol-graph
     protocol-stats print-protocol-stats pi ti pm)

   (templates
     make-monoid monoid? monoid-mempty monoid-mappend
     monoid-sum monoid-product monoid-list monoid-string
     monoid-all monoid-any monoid-first monoid-last monoid-max monoid-min monoid-endo
     mconcat mtimes verify-monoid-laws
     make-foldable foldable? foldable-fold-map foldable-foldr foldable-foldl
     foldable-list foldable-maybe
     fold-with to-list null-foldable? length-foldable elem-foldable
     make-traversable traversable? traversable-traverse
     make-functor make-named-functor functor? functor-fmap functor-name
     functor-list functor-maybe functor-either functor-id
     fmap-with replace-with void-with
     make-applicative applicative? applicative-pure applicative-ap
     applicative-list applicative-maybe applicative-either
     ap-with lift-a2 sequence-a traverse-a
     make-lens lens? lens-getter lens-setter
     view set-lens over lens-compose
     lens-fst lens-snd lens-head lens-tail lens-nth lens-key
     make-prism prism? prism-match prism-build preview review
     prism-just prism-left prism-right)

   ;; ---- control/ subdir ----

   (state
     make-state state? state-fn run-state eval-state exec-state
     state-get state-put state-modify state-gets
     state-pure state-bind state-then state-map state-ap
     state-sequence state-map-m state-for-each
     state-when state-unless
     state-view state-set state-over
     state-get-key state-put-key)

   (effects
     make-effect-sig effect-sig? effect-sig-name effect-sig-operations
     make-operation operation? operation-name operation-param-type operation-result-type
     lookup-operation
     sig-State sig-Reader sig-Writer sig-Exception sig-NonDet sig-Console sig-Async
     make-effect-row effect-row? effect-row-effects empty-row
     row-contains? row-add row-remove row-union)

   (free
     pure-free free pure-free? free-suspended? from-pure-free from-free
     free-map free-bind free-then free-ap
     lift-free fold-free iter-free run-free)

   (continuation
     make-cont cont? run-cont eval-cont
     cont-return cont-bind cont-map cont-ap cont-join
     callCC cont-abort cont-shift cont-reset
     with-escape cont-throw
     cont-loop cont-for-each cont-fold
     with-early-return cont-when cont-unless
     fail choose amb)

   ;; ---- meta/ subdir ----

   (combinators
     id identity const flip apply-fn
     compose2 compose pipe2 pipe >>> <<<
     curry2 uncurry2 curry3 uncurry3
     partial partial2 rpartial on both
     pair-first pair-second bimap complement
     conjoin)

   (result
     ok err
     result-fold result-default result-or result-and
     result-map-error result-bimap
     result-catch result-catch/handler result-try
     assert-ok! assert-ok/msg!
     result->maybe maybe->result result->either either->result
     result-ap result-lift2 result-traverse result-filter
     validation-ok validation-err validation-errs validation-ok?)

   (logic
     make-lvar lvar? lvar-name lvar-id lvar=?
     extend-subst lookup-subst walk walk* unify
     succeed fail == =/= conj disj conj* disj* conde
     call/fresh fresh1 fresh2 fresh3)

   (dsl
     make-instruction instruction-tag instruction-payload instruction-cont
     dsl-fmap dsl-pure dsl-pure? dsl-suspended?
     dsl-pure-value dsl-instruction dsl-bind dsl-map dsl-emit dsl-request
     make-interpreter interpreter? interpreter-handler run-dsl
     dsl-trace layered-interpreter composed-interpreter)

   ;; ---- parsing/ subdir ----

   (parser
     make-pos pos? pos-line pos-col pos-offset
     parser-initial-pos advance-pos
     parser-make-state parser-state? parser-state-input
     parser-state-index parser-state-pos parser-state-remaining
     parser-state-at-end? parser-state-current-char parser-initial-state
     make-parse-error parse-error? error-pos error-message error-expected
     merge-errors format-error make-parser)

   (regex
     regex-lit regex-lit? regex-lit-char
     regex-dot regex-dot?
     regex-class regex-class? regex-class-chars regex-class-negated?
     regex-seq regex-seq? regex-seq-exprs
     regex-alt regex-alt? regex-alt-exprs
     regex-star regex-star? regex-star-expr
     regex-plus regex-plus? regex-plus-expr
     regex-opt regex-opt? regex-opt-expr
     regex-group regex-group? regex-group-expr
     regex-empty regex-empty?)

   (fsm
     make-fsm fsm? fsm-states fsm-alphabet fsm-transitions
     fsm-start fsm-accepting fsm-epsilon fsm-assertions
     fsm-deterministic? dfa nfa epsilon-nfa
     fsm-delta epsilon-closure epsilon-closure-set
     fsm-move fsm-run fsm-accepts?
     fsm-reachable)

   (parser-compile
     dfa->parser regex-ast->parser
     regex->parser regex->combinator-parser
     compiled-regex? compiled-regex-pattern compiled-regex-ast
     compiled-regex-dfa compiled-regex-parser
     compile-regex compiled-regex-matches? compiled-regex-parse)

   ;; ---- data/ subdir ----

   (stream
     stream-nil stream-nil? stream-cons stream-cons?
     stream-head stream-tail
     list->stream stream->list
     stream-iterate stream-repeat stream-cycle stream-from stream-range
     naturals stream-unfold
     stream-map stream-filter stream-take stream-drop)

   (zipper
     make-zipper zipper? zipper-left zipper-focus-maybe zipper-right
     zipper-empty zipper-empty? zipper-singleton
     list->zipper zipper->list zipper-from-position
     zipper-has-focus? zipper-can-go-left? zipper-can-go-right?
     zipper-left! zipper-right! zipper-start zipper-end zipper-goto)

   ;; ---- symbolic/ subdir ----

   (expr
     num var
     make-sum make-product make-diff make-neg make-div make-pow make-app
     sum product difference division power
     num? var? sum? product? difference? quotient?)

   (diff
     deriv partial gradient jacobian hessian
     deriv-n directional-derivative curl divergence laplacian)

   ;; ---- measure/ subdir ----

   (units
     make-dim dim-length dim-time dim-mass dim-current
     dim-temperature dim-amount dim-luminosity dim?  dim=?
     dim-one dim-length-base dim-time-base dim-mass-base
     dim-current-base dim-temperature-base dim-amount-base dim-luminosity-base
     dim* dim/)

   ;; ---- rewrite/ subdir ----

   (rule
     make-rule rule? rule-name rule-lhs rule-rhs
     rule-category rule-direction rule-conditions
     metavar? metavar-name metavar-constraint
     pattern-vars pattern-vars-unique rule->string valid-rule?)

   (engine
     match-pattern substitute-template
     apply-rule apply-rule-name apply-rule-at apply-rule-anywhere
     find-all-positions
     id-strategy fail-strategy seq choice try repeat repeat-n map-children))


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
       "regex.ss"            ; Regular expression matching
       "parser-compile.ss")))  ; DFA-backed parsers, compiled regex

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
       "protocol.ss"           ; Open protocol system for extensible dispatch
       "protocol-bundle.ss"    ; Protocol bundles for reduced boilerplate
       "protocol-introspect.ss" ; Protocol introspection and visualization
       "templates.ss")))       ; Lens infrastructure
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
     (description "QuickCheck-style property testing with generators, shrinkers, and test framework integration")
     (location "lattice/quickcheck/quickcheck.ss"))

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
