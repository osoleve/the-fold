((title . "Functional Programming Toolkit") (location . "lattice/fp/") (tier-access . shepherd)
  (purity
    .
    "All fp/ code is pure, typed, and follows dictionary-passing style")
  (description
    .
    "\nA comprehensive functional programming library implementing Haskell-style\ntype classes and abstractions in Scheme. Uses dictionary-passing style\nfor type class polymorphism, maintaining purity and explicit dependencies.\n\nDesign Philosophy:\n- Type classes as first-class values (dictionaries)\n- Explicit dictionary passing (no global instances)\n- Composability through higher-order functions\n- Lawful abstractions verified via property-based testing\n- Zero-cost abstractions where possible (compile-time resolution)\n")
  (organization
    ((category . "Type Class Infrastructure")
      (description
        .
        "Core type class system and foundational abstractions")
      (modules "typeclasses.ss - Type class definition framework"
        "algebraic.ss   - Semigroup, Monoid, Group, Ring"
        "numeric.ss     - Numeric type class tower (Num, Integral, Fractional, etc.)"
        "semiring.ss    - Semiring abstraction for generalized arithmetic"
        "laws.ss        - Type class law verification framework"))
    ((category . "Functorial Structures")
      (description
        .
        "Functor-like abstractions and transformations")
      (modules
        "bifunctor.ss    - Bifunctor (maps over two type parameters)"
        "contravariant.ss - Contravariant functors"
        "profunctor.ss   - Profunctors (input contravariant, output covariant)"
        "representable.ss - Representable functors (isomorphic to function types)"
        "traversable.ss  - Traversable and Foldable type classes"))
    ((category . "Monadic Structures")
      (description . "Monads and related abstractions")
      (modules "alternative.ss - Alternative (choice) and MonadPlus"
        "comonad.ss     - Comonad (dual of monad)"
        "transformers.ss - Monad transformers (StateT, ReaderT, etc.)"
        "continuation.ss - Continuation monad"
        "reader.ss      - Reader monad (environment passing)"
        "state.ss       - State monad"
        "writer.ss      - Writer monad (accumulation)"
        "free.ss        - Free monads (AST interpretation)"
        "effects.ss     - Algebraic effects and handlers"))
    ((category . "Advanced Abstractions")
      (description
        .
        "Higher-order abstractions and design patterns")
      (modules
        "arrow.ss             - Arrow type class (generalized functions)"
        "recursion-schemes.ss - Catamorphisms, anamorphisms, hylomorphisms"
        "optics.ss            - Lenses, prisms, traversals"
        "lens.ss              - Lens utilities and combinators"
        "validation.ss        - Validation applicative (accumulating errors)"))
    ((category . "Data Structures")
      (description . "Functional data structures")
      (collections "dlist.ss       - Difference lists (O(1) append)"
        "finger-tree.ss - 2-3 finger trees (sequences with fast access)"
        "heap.ss        - Priority heaps"
        "map.ss         - Functional maps (association lists with type class interface)"
        "nonempty.ss    - Non-empty lists (safe head/tail)"
        "ring-buffer.ss - Circular buffers"
        "rope.ss        - Rope data structure (efficient string manipulation)"
        "set.ss         - Functional sets"
        "stream.ss      - Lazy infinite streams"
        "trie.ss        - Prefix trees"
        "zipper.ss      - List zipper (functional cursor with Comonad)"
        "tree-zipper.ss - Rose tree zipper with navigation"
        "generic-zipper.ss - Type-theoretic zipper derivation"
        "zipper-lens.ss - Zipper-lens integration (lenses, affines, comonad connection)")
      (specialized
        "interval.ss      - Interval arithmetic"
        "interval-elem.ss - Interval elements"
        "graph.ss         - Graph algorithms and representations"))
    ((category . "Mathematical Computing")
      (description . "Numerical and mathematical abstractions")
      (modules
        "bignum.ss          - Arbitrary precision integers (BigInt) and rationals (BigRational)"
        "                     • Base 2^30 representation for efficiency"
        "                     • Automatic normalization and reduction"
        "                     • 35 comprehensive tests"
        "transcendental.ss  - Transcendental functions (exp, log, sin, cos, trig, hyperbolic)"
        "                     • 59 functions with full test coverage"
        "                     • Domain checking and error handling"
        "                     • Foundation for high-precision computation"
        "differentiable.ss  - Differentiable type class for automatic differentiation"
        "units.ss           - Physical units and dimensional analysis"
        "vec-matrix-instances.ss - Type class instances for linear algebra types"))
    ((category . "Parsing & DSLs")
      (description
        .
        "Parser combinators and domain-specific languages")
      (modules
        "parser.ss         - Packrat parser combinator framework with memoization"
        "parser-dsl.ss     - DSL for parser construction"
        "parser-examples.ss - Example parsers (JSON, S-expressions, etc.)"
        "regex.ss          - Regular expression matching"
        "dsl.ss            - DSL construction utilities"
        "fsm.ss            - Finite state machines"
        "logic.ss          - Logic programming utilities"))
    ((category . "Testing & Verification")
      (description . "Property-based testing and verification")
      (modules
        "property.ss   - Property definition framework"
        "quickcheck.ss - QuickCheck-style property-based testing"
        "laws.ss       - Type class law verification"))
    ((category . "Game Theory")
      (description . "Strategic and cooperative game theory")
      (modules
        "normal-form.ss - Normal form games, Nash equilibrium, IESDS"
        "coop-games.ss  - Cooperative games, Shapley value, nucleolus, core"
        "matching.ss    - Two-sided matching, Gale-Shapley, assignment games, ILP matching"))
    ((category . "Utilities")
      (description . "General-purpose functional utilities")
      (modules
        "combinators.ss - Standard combinators (id, const, compose, flip, etc.)"
        "strategies.ss  - Rewriting strategies and term rewriting"
        "prelude.ss     - FP prelude with common utilities"
        "templates.ss   - FP code templates: monoids, foldables, functors, applicatives, lenses, prisms")))
  (usage-examples
    ((title . "Type Classes: Monoid Example")
      (code
        .
        "\n; Define a Monoid instance for lists\n(define list-monoid\n  (make-monoid\n   (lambda () '())           ; mempty\n   (lambda (a b) (append a b))))  ; mappend\n\n; Use the instance\n(monoid-mconcat list-monoid '((1 2) (3 4) (5)))\n; => (1 2 3 4 5)\n"))
    ((title . "Parser Combinators")
      (code
        .
        "\n; Parse a simple arithmetic expression\n(define number-parser\n  (parser-map string->number\n              (parser-many1 parser-digit)))\n\n(define expr-parser\n  (parser-bind number-parser\n               (lambda (n)\n                 (parser-choice\n                  (parser-sequence\n                   (parser-char #\\+)\n                   (parser-map (lambda (m) (+ n m)) number-parser))\n                  (parser-return n)))))\n"))
    ((title . "Lens Operations")
      (code
        .
        "\n; Create a lens for accessing nested data\n(define name-lens (lens-compose person-lens person-name-lens))\n\n; View through the lens\n(lens-view name-lens data)\n\n; Update through the lens\n(lens-set name-lens \"Alice\" data)\n"))
    ((title . "Free Monad DSL")
      (code
        .
        "\n; Define a DSL for turtle graphics\n(define-instruction (forward! dist) 'forward)\n(define-instruction (right! angle) 'right)\n(define-request (get-pos) 'getpos)\n\n; Compose operations monadically\n(define square\n  (dsl-bind (forward! 100)\n    (lambda (_)\n      (dsl-bind (right! 90)\n        (lambda (_)\n           ...)))))\n\n; Run with an interpreter\n(run-dsl turtle-interpreter square)\n"))
    ((title . "FP Templates: Monoids and Lenses")
      (code
        .
        "\n; Use pre-defined monoids\n(mconcat monoid-sum '(1 2 3 4 5))        ; => 15\n(mconcat monoid-product '(1 2 3 4 5))    ; => 120\n(mconcat monoid-list '((a b) (c d)))     ; => (a b c d)\n\n; Verify monoid laws\n(verify-monoid-laws monoid-sum '(1 2 3)) ; => #t\n\n; Use lenses for nested data access\n(view lens-fst '(1 . 2))                 ; => 1\n(set-lens lens-fst 10 '(1 . 2))          ; => (10 . 2)\n(over lens-fst (lambda (x) (* x 2)) '(5 . 3)) ; => (10 . 3)\n\n; Compose lenses for deep access\n(define deep-lens (lens-compose lens-fst lens-fst))\n(view deep-lens '((a . b) . c))          ; => a\n"))
    ((title . "Zippers and Zipper-Lens Integration")
      (code
        .
        "\n; Create a list zipper and navigate\n(define z (list->zipper '(1 2 3 4 5)))\n(define z2 (from-just (zipper-right! z)))  ; Focus on 2\n(zipper-focus z2)                          ; => 2\n\n; Use lenses on zippers\n(view zipper-focus-lens z2)                ; => 2\n(set-lens zipper-focus-lens 99 z2)         ; Focus becomes 99\n\n; Navigation as affines (partial lenses)\n(preview-affine zipper-right-affine z)     ; => Just(zipper at 2)\n(preview-affine zipper-left-affine z)      ; => Nothing (at start)\n\n; Comonad: extend operations across all positions\n(zipper-extend (lambda (z) (* (zipper-focus z) 10))\n               (list->zipper '(1 2 3)))    ; => [10 20 30]\n\n; Convert zipper position to lens for list access\n(define pos-lens (zipper-to-lens z2))      ; Lens for position 1\n(view pos-lens '(a b c d e))               ; => b\n")))
  (design-patterns
    ((pattern . "Dictionary-Passing Style")
      (description
        .
        "Type classes are explicit dictionary values passed as arguments")
      (rationale
        .
        "Maintains purity, enables multiple instances, supports local overriding")
      (example
        .
        "(define (sum-list num-dict xs) (foldl (num-add num-dict) (num-zero num-dict) xs))"))
    ((pattern . "Newtype Pattern")
      (description
        .
        "Wrap types to provide different type class instances")
      (example
        .
        "(define-newtype (Sum n) n)  ; Sum newtype for + Monoid"))
    ((pattern . "Tagless Final")
      (description
        .
        "DSLs as type classes; interpreters as instances")
      (example
        .
        "Parser combinator framework uses this for multiple interpretations"))
    ((pattern . "Free Monads for DSLs")
      (description
        .
        "Build ASTs as free monads; separate syntax from semantics")
      (benefits . "Testable, composable, multiple interpreters")))
  (testing
    ((framework . "Property-Based Testing")
      (description
        .
        "QuickCheck-style property testing for verifying laws")
      (example
        .
        "\n(define-property \"Monoid associativity\"\n  (lambda (monoid gen)\n    (forall ([a gen] [b gen] [c gen])\n      (equal? (mappend monoid a (mappend monoid b c))\n              (mappend monoid (mappend monoid a b) c)))))\n"))
    ((framework . "Law Checking")
      (description . "Automated verification of type class laws")
      (example
        .
        "(check-monoid-laws list-monoid list-generator)")))
  (performance
    ((optimization . "Compile-time Instance Resolution")
      (description
        .
        "Instances resolved at compile time where possible")
      (status . "Planned - requires whole-program analysis"))
    ((optimization . "Inlining & Specialization")
      (description
        .
        "Aggressive inlining of dictionary accessors")
      (status
        .
        "Enabled via (declare (inline ...)) in Chez Scheme"))
    ((bottleneck . "BigNum Operations")
      (description
        .
        "Scheme BigNum too slow for high-precision transcendentals")
      (solution . "Implement in fold-rs (Rust) with FFI")))
  (dependencies
    "prelude.ss - Required by all fp/ modules"
    "Most modules are self-contained or depend only on type class definitions"
    "Parser modules depend on parser.ss combinator framework"
    "Mathematical modules may depend on bignum.ss or numeric.ss")
  (future-work "High-performance BigNum in fold-rs"
    "Compile-time instance resolution and specialization"
    "Effect system integration with algebraic effects"
    "Dependent types integration (Pi, Sigma)"
    "Linear types for resource management"
    "Refinement types for precondition checking")
  (see-also
    "fabric/README.sexp       - Overall fabric architecture"
    "core/types.ss - The type system"
    "CLAUDE.md                - Development guidelines"))

