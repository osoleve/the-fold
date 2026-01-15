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

  (deps (data algebra))  ; Depends on data structures and algebraic structures

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

  (exports (
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

    ;; zipper.ss — Functional Zippers
    zipper? zipper-empty zipper-empty? zipper-singleton
    list->zipper zipper->list zipper-from-position
    ;; Navigation
    zipper-has-focus? zipper-can-go-left? zipper-can-go-right?
    zipper-left! zipper-right! zipper-start zipper-end
    zipper-goto zipper-position zipper-length
    ;; Focus operations
    zipper-focus zipper-focus-maybe zipper-focus-or
    zipper-set zipper-modify
    ;; Insertion and deletion
    zipper-insert-left zipper-insert-right zipper-insert-focus
    zipper-delete zipper-delete-left zipper-delete-right
    ;; Bulk operations
    zipper-map zipper-filter zipper-fold-left zipper-fold-right
    zipper-find zipper-find-index
    ;; Context operations
    zipper-context zipper-window zipper-split
    zipper-append zipper-prepend
    zipper-take-left zipper-take-right
    ;; Type class instances
    zipper-functor zipper-fmap
    zipper-extract zipper-extend zipper-duplicate zipper-comonad
    ;; Equality and display
    zipper-equal? zipper->string

    ;; tree-zipper.ss — Tree Zipper for Rose Trees
    ;; Rose tree type
    tree? tree-value tree-children tree-leaf tree-leaf? tree-node
    make-tree tree-size tree-depth tree-map tree-fold tree-flatten
    ;; Tree zipper type
    tree-zipper? tree->zipper zipper->tree
    tree-zipper-at-root? tree-zipper-at-leaf? tree-zipper-depth
    ;; Navigation
    tree-zipper-up tree-zipper-down tree-zipper-down-left tree-zipper-down-right
    tree-zipper-left tree-zipper-right
    tree-zipper-leftmost tree-zipper-rightmost tree-zipper-root
    tree-zipper-nth-child
    ;; Navigation predicates
    tree-zipper-can-go-up? tree-zipper-can-go-down?
    tree-zipper-can-go-left? tree-zipper-can-go-right?
    ;; Focus operations
    tree-zipper-get tree-zipper-set tree-zipper-modify
    tree-zipper-set-tree tree-zipper-modify-tree
    tree-zipper-focus tree-zipper-crumbs tree-zipper-children-count
    ;; Insertion
    tree-zipper-insert-child tree-zipper-insert-child-left tree-zipper-insert-child-right
    tree-zipper-insert-left tree-zipper-insert-right
    ;; Deletion
    tree-zipper-delete tree-zipper-delete-children
    ;; Traversal
    tree-zipper-next-preorder tree-zipper-prev-preorder tree-zipper-preorder
    ;; Path operations
    tree-zipper-path tree-zipper-sibling-index
    ;; Functor and Comonad
    tree-zipper-map tree-zipper-functor
    tree-zipper-extract tree-zipper-extend tree-zipper-duplicate tree-zipper-comonad
    ;; Equality and display
    tree-equal? tree-zipper-equal? tree->string tree-zipper->string

    ;; generic-zipper.ss — Type-Theoretic Zipper Derivation
    ;; Type constructors
    type-zero type-one type-zero? type-one?
    type-var type-var? type-var-name
    type-sum type-sum? type-sum-components
    type-prod type-prod? type-prod-components
    type-rec type-rec? type-rec-var type-rec-body
    ;; Common type patterns
    type-list type-maybe type-pair type-either
    type-rose-tree type-binary-tree
    ;; Type derivative (core algorithm)
    type-deriv type-subst type-simplify
    ;; Zipper type construction
    make-zipper-type context-type
    ;; Generic zipper runtime
    make-generic-zipper generic-zipper?
    generic-zipper-focus generic-zipper-contexts
    generic-zipper-at-root? generic-zipper-depth
    generic-zipper-set generic-zipper-modify
    ;; Context frames
    make-context-frame context-frame?
    context-frame-kind context-frame-position context-frame-siblings
    ;; Display
    type->string

    ;; zipper-lens.ss — Zipper-Lens Integration
    ;; Lenses into list zippers
    zipper-focus-lens zipper-focus-maybe-lens
    zipper-left-lens zipper-right-lens zipper-position-lens
    ;; Lenses into tree zippers
    tree-zipper-focus-lens tree-zipper-value-lens
    tree-zipper-children-lens tree-zipper-crumbs-lens tree-zipper-depth-lens
    ;; Affine (partial lens) operations
    make-affine affine? affine-getter affine-setter
    preview-affine set-affine over-affine
    ;; List zipper navigation affines
    zipper-left-affine zipper-right-affine zipper-nth-affine
    ;; Tree zipper navigation affines
    tree-up-affine tree-down-affine tree-left-affine tree-right-affine
    tree-nth-child-affine tree-child-value-affine
    ;; Zipper-to-lens conversion
    follow-path zipper-to-lens tree-path-to-lens
    ;; Comonad-lens integration
    extend-with-lens map-with-context neighbor-lens window-lens
    ;; Traversal utilities
    zipper-each zipper-all? zipper-any? zipper-collect
    ;; Composed lens paths
    at-focus at-left at-right

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
    ;; coop-games.ss — Cooperative Games
    ;; Coalition operations
    coalition-empty coalition-singleton coalition-member?
    coalition-union coalition-intersection coalition-difference coalition-complement
    coalition-size coalition->list list->coalition all-coalitions coalition-subset?
    ;; Game types
    make-coop-game coop-game? coop-game-players coop-game-value coop-game-grand-coalition
    ;; Allocations
    allocation-total allocation-coalition-total imputation?
    ;; Solution concepts
    shapley-value allocation-in-core? core-excess max-excess nucleolus
    bargaining-set nash-bargaining kalai-smorodinsky
    ;; Game constructors
    make-additive-game make-unanimity-game make-weighted-voting-game
    make-airport-game make-bankruptcy-game make-gloves-game
    ;; Properties
    coop-game-superadditive? coop-game-convex? coop-game-monotonic? coop-game-simple?
    ;; Power indices
    is-winning? is-blocking? is-pivotal? banzhaf-index
    ;; matching.ss — Matching Theory
    ;; Market construction
    make-matching-market matching-market?
    market-proposers market-receivers market-num-proposers market-num-receivers
    market-proposer-prefs market-receiver-prefs
    ;; Stable matching (Gale-Shapley)
    stable-match stable-match-receiver-optimal
    matching-stable? same-matched-agents?
    ;; Assignment games
    make-assignment-game optimal-assignment
    ;; Classic market constructors
    make-medical-residency-market make-school-choice-market

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
    ;; poly-canonical.ss — Polynomial canonical forms
    expr->polynomial polynomial->expr simplify-rational
    expr-poly-gcd expr-degree polynomial-expr?
    ;; solve.ss — Equation solving
    make-equation equation? equation-lhs equation-rhs equation->expr
    solve solve-for solve-linear solve-quadratic solve-cubic solve-polynomial
    solve-linear-system
    is-polynomial? polynomial-degree extract-poly-coefficients
    contains-var? collect-linear-coeffs

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

    ;; discrete-control.ss — Continuous-to-discrete conversion
    make-dss dss? dss-Ts dss-A dss-B dss-C dss-D dss-order
    c2d-zoh c2d-tustin c2d-tustin-prewarp d2c-tustin
    dss-step dss-output dss-simulate
    dss-impulse-response dss-step-response dss-stable?
    recommend-sample-rate

    ;; stability.ss — Stability analysis
    ss-poles ss-stable? tf-stable? ss-marginally-stable? discrete-stable?
    routh-array routh-hurwitz-stable? routh-hurwitz-count-rhp
    lyapunov-solve-continuous lyapunov-solve-discrete lyapunov-stable?
    matrix-positive-definite? matrix-frobenius-norm
    root-locus-points root-locus-breakaway root-locus-asymptotes
    nyquist-plot-points nyquist-stable?
    gain-margin phase-margin stability-margins
    bibo-stable? bibo-stable-discrete?
    controllable? observable?
    stability-report

    ;; z-transform.ss — Discrete transfer functions
    dtf? make-dtf dtf-from-coeffs dtf-from-lists
    dtf-num dtf-den dtf-Ts dtf-order
    dtf-relative-degree dtf-proper?
    dtf-poles dtf-zeros dtf-gain dtf-from-poles-zeros
    dtf-eval dtf-eval-real
    dtf-freq-response dtf-magnitude dtf-magnitude-db
    dtf-phase dtf-phase-deg
    dtf-series dtf-parallel dtf-feedback dtf-unity-feedback
    dtf-stable? dtf-pole-magnitudes
    dss->dtf dtf->dss dtf->string

    ;; transfer-function.ss — Continuous transfer functions
    tf? make-tf tf-from-coeffs tf-from-lists
    tf-num tf-den tf-order
    tf-relative-degree tf-proper? tf-strictly-proper?
    tf-poles tf-zeros tf-gain tf-from-poles-zeros
    tf-eval tf-eval-real tf-dc-gain
    tf-freq-response tf-magnitude tf-magnitude-db
    tf-phase tf-phase-deg tf-bode-data
    tf-series tf-parallel tf-feedback tf-unity-feedback
    tf-stable? tf-pole-real-parts tf->string

    ;; controller-design.ss — Controller synthesis
    pid-tf
    pid-design-zn-open-loop pid-design-zn-closed-loop
    pid-design-imc pid-design-lambda
    pole-placement-ackermann pole-placement-bass-gura
    observer-design-ackermann observer-design-place
    lqr dlqr solve-care solve-dare lqg
    hinf-norm hinf-bounded?
    closed-loop-tf sensitivity-tf complementary-sensitivity-tf
    lead-compensator lag-compensator lead-lag-compensator
    controller-info

    ;; poly-algebra.ss — Polynomial algebra for transfer functions
    tf-simplify tf-coprime? routh-hurwitz-exact
    tf-char-poly tf-bezout alg-poly-degree alg-poly-gcd

    ;; digital-pid.ss — Discrete PID controllers
    make-digital-pid digital-pid?
    pid-Kp pid-Ki pid-Kd pid-Ts
    make-pid-state pid-state? pid-integral pid-prev-error pid-prev-output
    pid-step pid-step-antiwindup clamp
    make-filtered-pid-state filtered-pid-state? pid-step-filtered
    pid-reset pid-reset-to
    ziegler-nichols-tune tyreus-luyben-tune cohen-coon-tune
    make-velocity-pid-state velocity-pid-state? pid-step-velocity
    pid-simulate pid-simulate-antiwindup pid->string

    ;; tf-convert.ss — State-space/transfer function conversion
    ss->tf ss->tf-leverrier tf->ss

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
     (description "FP combinators, DSL utilities, logic programming")
     (files (
       "combinators.ss"  ; id, const, compose, Maybe, Either, do-monad
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

    ((subdir "game")
     (description "Game theory: normal form, cooperative games, and matching")
     (files (
       "normal-form.ss"   ; Normal form games, Nash equilibrium, IESDS
       "coop-games.ss"    ; Cooperative games, Shapley value, core, bargaining
       "matching.ss")))   ; Two-sided matching, Gale-Shapley, assignment games

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
