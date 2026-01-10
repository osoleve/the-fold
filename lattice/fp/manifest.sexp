;;; lattice/fp/manifest.sexp — Functional Programming Toolkit Manifest
;;;
;;; Comprehensive FP infrastructure for The Fold implementing Haskell-style
;;; type classes and abstractions using dictionary-passing style.

((skill . fp)
 (version . "0.1.0")
 (tier . 1)
 (path . "lattice/fp")
 (purity . mixed)  ; Most modules pure, some have debug output
 (stability . stable)
 (fuel-bound . 10000)

 (deps . (data))  ; Depends on data structures

 (description . "
A comprehensive functional programming library implementing Haskell-style
type classes and abstractions in Scheme. Uses dictionary-passing style
for type class polymorphism, maintaining purity and explicit dependencies.

Key design principles:
- Type classes as first-class values (dictionaries)
- Explicit dictionary passing (no global instances)
- Composability through higher-order functions
- Lawful abstractions verified via property-based testing
")

 (keywords . (functional-programming type-classes monads parsers
              streams logic-programming game-theory symbolic-computation
              units-of-measure control-systems term-rewriting))

 (aliases . (fp functional))

 ;;; ============================================================
 ;;; Exports by Submodule
 ;;; ============================================================

 (exports . (
   ;; control/ — Monads, Effects, Continuations
   ;; state.ss
   make-state run-state eval-state exec-state
   state-get state-put state-modify state-gets
   state-return state-bind state-map state-ap
   state-sequence state-traverse
   stack-push stack-pop counter-inc counter-get

   ;; effects.ss
   make-effect effect-name effect-handler effect-continuation
   perform handle with-handler
   sig-State sig-Reader sig-Writer sig-Exception sig-NonDet
   handler-State handler-Reader handler-Writer handler-Exception handler-NonDet
   run-state-eff run-reader run-writer run-exception run-nondet
   with-state with-reader with-writer with-exception with-nondet

   ;; free.ss
   pure-free free free? pure-free? impure-free?
   free-return free-bind free-map free-ap
   fold-free iter-free
   ;; KV Store DSL
   kv-get kv-put kv-delete run-kv-store
   ;; Console DSL
   console-print console-read run-console

   ;; continuation.ss
   make-cont run-cont cont-return cont-bind cont-map
   callCC cont-shift cont-reset
   make-trampoline trampoline-done trampoline-bounce run-trampoline

   ;; numeric/ — Transcendental Functions
   ;; transcendental.ss
   pi-value e-value
   exp-num log-num pow-num sqrt-num
   sin-num cos-num tan-num
   asin-num acos-num atan-num atan2-num
   sinh-num cosh-num tanh-num
   asinh-num acosh-num atanh-num
   deg->rad rad->deg
   log10 log2 expm1 log1p
   hypot sinc gamma-approx factorial-approx
   floor-num ceiling-num truncate-num round-num
   frac-part int-part
   clamp lerp smoothstep
   sign-num copysign-num

   ;; parsing/ — Parser Combinators
   ;; parser.ss
   make-parser run-parser
   parser-return parser-fail parser-bind parser-map parser-ap
   parser-get-pos parser-set-pos parser-get-input
   parser-satisfy parser-char parser-string parser-eof
   parser-any-char parser-one-of parser-none-of
   parser-digit parser-letter parser-alphanum parser-space parser-newline
   parser-choice parser-try parser-lookahead parser-not-followed-by
   parser-many parser-many1 parser-sep-by parser-sep-by1
   parser-between parser-optional parser-skip parser-skip-many
   parser-chainl parser-chainl1 parser-chainr parser-chainr1
   parser-label parser-expected
   ;; Packrat memoization
   make-packrat-table packrat-lookup packrat-store
   make-bounded-packrat bounded-packrat-lookup bounded-packrat-store

   ;; meta/ — DSL Utilities, Logic, Combinators
   ;; combinators.ss
   id const flip compose compose* pipe pipe*
   curry2 curry3 uncurry2 uncurry3
   partial partial-right
   juxt fan-out
   ;; Maybe
   just nothing maybe? just? nothing? from-just maybe-default
   maybe-map maybe-bind maybe-ap maybe-filter
   maybe-to-list list-to-maybe cat-maybes map-maybe
   ;; Either
   left right either? left? right? from-left from-right
   either-map either-bimap either-bind either-ap
   partition-eithers lefts rights
   ;; Monadic utilities
   do-monad sequence-m map-m filter-m fold-m
   lift-m lift-m2 join-m
   ;; Function combinators
   memoize on both first-of second-of
   when-pred unless-pred if-then-else

   ;; dsl.ss
   make-instruction instruction? instruction-tag instruction-args
   make-interpreter run-dsl dsl-return dsl-bind dsl-do
   ;; Expression DSL
   dsl-lit dsl-var dsl-add dsl-mul dsl-if
   eval-expr-dsl
   ;; Statement DSL
   dsl-assign dsl-seq dsl-while dsl-print
   run-stmt-dsl
   ;; Middleware
   make-middleware compose-middleware with-logging with-timing

   ;; logic.ss
   make-lvar lvar? lvar-name lvar-id lvar=?
   empty-subst extend-subst lookup-subst walk walk*
   unify unify-check
   == =/= ==/check
   succeed fail conj disj conj* disj* conde
   call/fresh fresh1 fresh2 fresh3 fresh4
   run-goal run*
   reify reify-name
   ;; List relations
   conso nullo pairo caro cdro
   appendo membero lengtho reverseo
   ;; Peano arithmetic
   zeroo succo pluso minuso lesso
   nat->peano peano->nat
   ;; Constraint store
   make-constraint-store cstore-subst cstore-constraints
   occurs?

   ;; data/ — Lazy Streams
   ;; stream.ss
   stream-nil stream-nil?
   stream-cons stream-cons? stream-car stream-cdr
   stream-null? stream-pair?
   stream-ref stream-take stream-drop stream-take-while stream-drop-while
   stream-map stream-filter stream-append stream-flatmap
   stream-zip stream-zip-with stream-interleave
   stream-fold stream-unfold
   stream->list stream-iterate stream-repeat stream-cycle
   stream-from stream-range
   ;; Classic sequences
   naturals fibonacci primes
   ;; Type class instances
   stream-functor stream-applicative stream-monad stream-monoid
   ;; Memoized streams
   make-memo-stream memo-stream-ref

   ;; game/ — Game Theory
   ;; normal-form.ss
   make-game game? game-players game-strategies game-payoff-fn
   game-payoff strategy-profile
   ;; Classic games
   prisoners-dilemma chicken stag-hunt battle-of-sexes matching-pennies coordination
   ;; Analysis
   best-response-p1 best-response-p2
   is-nash-equilibrium? find-pure-nash
   find-dominated-strategies strictly-dominated? weakly-dominated?
   iesds
   ;; Mixed strategies
   make-mixed-strategy expected-payoff
   solve-2x2-nash

   ;; symbolic/ — Symbolic Expressions
   ;; expr.ss
   num var sum product power
   num? var? sum? product? power?
   sum-terms product-factors power-base power-exponent
   expr? atomic-expr? compound-expr?
   make-sum make-product make-power
   expr-equal? expr-hash
   free-vars bound-vars subst
   expr->string string->expr
   ;; Simplification
   simplify-expr collect-like-terms

   ;; measure/ — Units of Measure
   ;; units.ss
   make-dimension dimension? dimension-length dimension-time dimension-mass
   dimension-current dimension-temperature dimension-amount dimension-intensity
   dimensionless dimension=? dimension-mul dimension-div dimension-pow
   ;; SI base units
   meter second kilogram ampere kelvin mole candela
   ;; SI derived units
   hertz newton pascal joule watt coulomb volt farad ohm siemens
   weber tesla henry lumen lux becquerel gray sievert katal
   ;; Prefixes
   yotta zetta exa peta tera giga mega kilo hecto deca
   deci centi milli micro nano pico femto atto zepto yocto
   ;; Quantity operations
   make-quantity quantity? quantity-value quantity-unit
   q+ q- q* q/ q-scale q-convert
   ;; Dimension checking
   check-dimension compatible-dimensions?

   ;; control-systems/ — Control Theory
   ;; state-space.ss
   make-ss ss? ss-A ss-B ss-C ss-D
   ss-state-dim ss-input-dim ss-output-dim
   ss-state-eq ss-output-eq
   ss-step ss-simulate
   ss-transfer-function
   ;; Controllability/Observability
   controllability-matrix observability-matrix
   controllability-gramian observability-gramian
   is-controllable? is-observable?
   ;; Transformations
   ss-series ss-parallel ss-feedback
   ss-transform
   ;; kalman.ss
   make-kalman-filter kalman? kalman-mean kalman-variance kalman-Q kalman-R
   kalman-predict kalman-update kalman-estimate kalman-gain kalman-batch
   kalman-residual kalman-mahalanobis kalman-summary
   kalman-with-Q kalman-boost-Q
   ;; Log-space Kalman
   make-log-kalman-filter log-kalman-update log-kalman-estimate
   log-kalman-mean log-kalman-predict-cost log-kalman-confidence-interval
   log-kalman-summary

   ;; analysis/ — Cost Analysis
   ;; cost-analysis.ss
   estimate-work work-analysis
   parallel-beneficial? parallelization-speedup
   op-cost op-costs
   complexity-class estimate-complexity
   find-hotspots critical-path-cost
   work-span-ratio

   ;; rewrite/ — Term Rewriting
   ;; rule.ss
   make-rule rule? rule-name rule-lhs rule-rhs
   rule-category rule-direction rule-conditions
   metavar? metavar-name metavar-constraint

   ;; engine.ss
   match-pattern substitute-template
   apply-rule apply-rule-name
   get-at-position rewrite-at apply-rule-anywhere
   ;; Strategies
   seq choice try-strategy repeat-strategy
   topdown bottomup innermost outermost
   some-strategy all-strategy one-strategy
   ;; Strategy combinators
   fix-strategy normalize
   ;; Tracing
   make-rewrite-trace trace-step show-trace
 ))

 ;;; ============================================================
 ;;; Modules by Subdirectory
 ;;; ============================================================

 (modules . (
   ((subdir . "control")
    (description . "Monads, algebraic effects, continuations, and free monads")
    (files . (
      "state.ss"        ; State monad with get/put/modify
      "effects.ss"      ; Algebraic effects with handlers
      "free.ss"         ; Free monad for DSL interpretation
      "continuation.ss" ; Continuation monad with shift/reset
      "reader.ss"       ; Reader monad (environment passing)
      "writer.ss"       ; Writer monad (accumulation)
      "transformers.ss" ; Monad transformers (StateT, ReaderT, etc.)
      "alternative.ss"  ; Alternative and MonadPlus
      "comonad.ss")))   ; Comonad (dual of monad)

   ((subdir . "numeric")
    (description . "Transcendental and mathematical functions")
    (files . (
      "transcendental.ss"   ; exp, log, sin, cos, tan, hyperbolic, etc.
      "bignum.ss"           ; Arbitrary precision integers and rationals
      "differentiable.ss"   ; Differentiable type class for AD
      "vec-matrix-instances.ss"))) ; Type class instances for linalg

   ((subdir . "parsing")
    (description . "Monadic parser combinators with packrat memoization")
    (files . (
      "parser.ss"           ; Core combinator framework
      "parser-dsl.ss"       ; DSL for parser construction
      "parser-examples.ss"  ; JSON, S-expr, arithmetic parsers
      "regex.ss")))         ; Regular expression matching

   ((subdir . "meta")
    (description . "FP combinators, DSL utilities, logic programming")
    (files . (
      "combinators.ss"  ; id, const, compose, Maybe, Either, do-monad
      "dsl.ss"          ; DSL builder with tagless final support
      "logic.ss"        ; miniKanren-style logic programming
      "strategies.ss"   ; Rewriting strategies
      "fsm.ss")))       ; Finite state machines

   ((subdir . "data")
    (description . "Lazy streams and functional data structures")
    (files . (
      "stream.ss"       ; Lazy infinite streams with memoization
      "dlist.ss"        ; Difference lists (O(1) append)
      "finger-tree.ss"  ; 2-3 finger trees
      "heap.ss"         ; Priority heaps
      "nonempty.ss"     ; Non-empty lists
      "ring-buffer.ss"  ; Circular buffers
      "rope.ss"         ; Efficient string manipulation
      "zipper.ss")))    ; Functional cursor/zipper

   ((subdir . "game")
    (description . "Game theory utilities")
    (files . (
      "normal-form.ss"))) ; Normal form games, Nash equilibrium, IESDS

   ((subdir . "symbolic")
    (description . "Symbolic computation and computer algebra")
    (files . (
      "expr.ss")))        ; Symbolic expression representation

   ((subdir . "measure")
    (description . "Units of measure with dimensional analysis")
    (files . (
      "units.ss")))       ; SI units, prefixes, dimension checking

   ((subdir . "control-systems")
    (description . "Control theory and dynamical systems")
    (files . (
      "state-space.ss"   ; LTI state space models, controllability
      "kalman.ss")))     ; Kalman filter for state estimation

   ((subdir . "analysis")
    (description . "Cost analysis and parallelization heuristics")
    (files . (
      "cost-analysis.ss"))) ; Work estimation, hotspot detection

   ((subdir . "rewrite")
    (description . "Term rewriting systems and proof tactics")
    (files . (
      "rule.ss"         ; Rule data structures and accessors
      "engine.ss"       ; Pattern matching and rewriting engine
      "trace.ss"        ; Rewrite trace visualization
      "laws.ss"         ; Algebraic laws (monoid, functor, monad)
      "fusion-rules.ss" ; Fusion optimization rules
      "verify.ss"       ; Rule verification
      "goals.ss"        ; Goal-directed rewriting
      "proof-tactics.ss" ; Proof tactics for equational reasoning
      "sketch.ss")))    ; Program sketching with holes
 ))

 ;;; ============================================================
 ;;; Design Patterns
 ;;; ============================================================

 (patterns . (
   ((pattern . "Dictionary-Passing Style")
    (description . "Type classes as explicit dictionary values passed as arguments")
    (rationale . "Maintains purity, enables multiple instances, supports local overriding")
    (example . "(define (sum-list num-dict xs) (foldl (num-add num-dict) (num-zero num-dict) xs))"))

   ((pattern . "Newtype Pattern")
    (description . "Wrap types to provide different type class instances")
    (example . "(define-newtype (Sum n) n)  ; Sum newtype for + Monoid"))

   ((pattern . "Tagless Final")
    (description . "DSLs as type classes; interpreters as instances")
    (example . "Parser combinator framework uses this for multiple interpretations"))

   ((pattern . "Free Monads for DSLs")
    (description . "Build ASTs as free monads; separate syntax from semantics")
    (benefits . "Testable, composable, multiple interpreters"))

   ((pattern . "Algebraic Effects")
    (description . "Effects as first-class values with composable handlers")
    (benefits . "Modular effect handling, effect polymorphism"))))

 ;;; ============================================================
 ;;; Testing
 ;;; ============================================================

 (testing . (
   ((framework . "Property-Based Testing")
    (description . "QuickCheck-style property testing for verifying laws")
    (location . "lattice/fp/meta/quickcheck.ss"))

   ((framework . "Law Checking")
    (description . "Automated verification of type class laws")
    (location . "lattice/fp/rewrite/laws.ss"))))

 ;;; ============================================================
 ;;; Future Work
 ;;; ============================================================

 (future-work . (
   "High-performance BigNum in fold-rs (Rust FFI)"
   "Compile-time instance resolution and specialization"
   "Effect system integration with algebraic effects"
   "Dependent types integration (Pi, Sigma)"
   "Linear types for resource management"
   "Refinement types for precondition checking"
   "Automatic differentiation integration"
   "Probabilistic programming primitives"))

 (see-also . (
   "lattice/fp/README.sexp"
   "core/types/types.ss"
   "lattice/data/manifest.sexp"
   "CLAUDE.md")))
