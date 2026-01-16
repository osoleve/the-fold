;;; core/README.sexp — The Core of The Fold
;;;
;;; This directory contains the load-bearing core of The Fold.

((title . "The System Core")
 (tier-access . shepherd)
 (purity . "All core code is pure, typed, and total")
 (description . "
Core is the foundation of The Fold. It defines the language,
the type system, the evaluation model, and the block primitives.

Everything in core/ is:
- Functionally pure (no side effects)
- Type-checked (bidirectional inference)
- Total (terminates with fuel limits)
- Call-by-value semantics
")
 (structure . (
   ((name . "core/")
    (purpose . "Pure, typed, load-bearing core modules")
    (description . "Individual threads (functions, modules) that weave together
                    to form the computational fabric. Each 'stitch' is a
                    self-contained module with minimal dependencies.")
    (core-modules . (
      "Core Language & Evaluation:"
      "  block.ss       - Block construction and serialization"
      "  normalize.ss   - S-expr → canonical form (de Bruijn indices)"
      "  expand.ss      - Canonical form → S-expr with symbols"
      "  cas.ss         - Content-addressed store (pure, in-memory)"
      "  sha256.ss      - SHA-256 hashing"
      "  prim.ss        - Pure primitive dispatcher"
      "  eval.ss        - Pure evaluator with fuel"
      "  typed-eval.ss  - Type-directed evaluation"
      "  compile.ss     - Compilation pipeline"
      "  module.ss      - Module system"
      "  nbe.ss         - Normalization by evaluation"
      ""
      "Type System:"
      "  types.ss       - The type system"
      "  kinds.ss       - Kind system (higher-kinded types)"
      "  infer.ss       - Bidirectional type inference"
      "  dep-infer.ss   - Dependent type inference"
      "  dep-types.ss   - Dependent types (Pi, Sigma)"
      "  resolve.ss     - Type class instance resolution"
      "  annotate.ss    - AST annotation with inferred types"
      ""
      "Parsing & Pretty Printing:"
      "  parse.ss       - Parser combinators"
      "  fold-parse.ss  - Fold-specific parser"
      "  pretty.ss      - Wadler-Lindig pretty printing"
      ""
      "Linear Algebra:"
      "  vec.ss         - Vector operations (55 tests)"
      "  vec2.ss        - 2D vector graphics"
      "  matrix.ss      - Matrix operations (50 tests)"
      "  matrix-decomp.ss - LU, QR, Cholesky decompositions (22 tests)"
      ""
      "Automatic Differentiation:"
      "  comp-graph.ss      - Computational graph for autodiff"
      "  reverse-diff.ss    - Reverse-mode AD (backpropagation)"
      ""
      "Complex Numbers & Signal Processing:"
      "  complex.ss     - Complex number arithmetic (56 tests)"
      "  dft.ss         - DFT/FFT algorithms (46 tests)"
      ""
      "Utilities:"
      "  prelude.ss     - Pure utility functions"
      "  error.ss       - Error types and formatting"
      "  span.ss        - Source location tracking"
      "  state.ss       - State monad utilities"
      "  index.ss       - Module index"
      "  help.ss        - Help system"
      "  debug.ss       - Debugging utilities"
      ""
      "Testing & Benchmarking:"
      "  test-*.ss      - Unit tests (one per module)"
      "  run-tests.ss   - Core test runner"
      "  bench-core.ss  - Core benchmarks"
      "  bench-prim.ss  - Primitive benchmarks")))
   ((name . "lattice/fp/")
    (purpose . "Functional programming abstractions and type classes")
    (description . "Comprehensive FP toolkit implementing Haskell-style type classes
                    and functional abstractions. All code is pure and follows
                    dictionary-passing style for type class polymorphism.")
    (type-classes . (
      "Core Type Classes:"
      "  typeclasses.ss - Base type class infrastructure"
      "  algebraic.ss   - Semigroup, Monoid, Group, Ring"
      "  numeric.ss     - Num, Integral, Fractional, Real, Floating"
      "  semiring.ss    - Semiring abstraction"
      ""
      "Functorial Structures:"
      "  bifunctor.ss    - Bifunctor type class"
      "  contravariant.ss - Contravariant functors"
      "  profunctor.ss   - Profunctors"
      "  representable.ss - Representable functors"
      ""
      "Monadic Structures:"
      "  alternative.ss - Alternative and MonadPlus"
      "  comonad.ss     - Comonad type class"
      "  transformers.ss - Monad transformers"
      "  continuation.ss - Continuation monad"
      "  reader.ss      - Reader monad"
      "  state.ss       - State monad"
      "  writer.ss      - Writer monad"
      ""
      "Advanced Abstractions:"
      "  arrow.ss       - Arrow type class"
      "  traversable.ss - Traversable and Foldable"
      "  free.ss        - Free monads"
      "  recursion-schemes.ss - Catamorphisms, anamorphisms"
      "  optics.ss      - Lenses and optics"
      "  lens.ss        - Lens utilities"))
    (data-structures . (
      "Collections:"
      "  dlist.ss       - Difference lists"
      "  finger-tree.ss - Finger trees"
      "  heap.ss        - Priority heaps"
      "  map.ss         - Functional maps"
      "  nonempty.ss    - Non-empty lists"
      "  ring-buffer.ss - Ring buffers"
      "  rope.ss        - Rope data structure"
      "  set.ss         - Functional sets"
      "  stream.ss      - Lazy streams"
      "  trie.ss        - Prefix trees"
      "  zipper.ss      - Zipper data structure"
      ""
      "Specialized:"
      "  interval.ss      - Interval arithmetic"
      "  interval-elem.ss - Interval elements"
      "  graph.ss         - Graph algorithms"))
    (mathematical . (
      "Numeric Computing:"
      "  bignum.ss        - Arbitrary precision integers and rationals (35 tests)"
      "  transcendental.ss - exp, log, sin, cos, etc. (59 tests)"
      "  differentiable.ss - Differentiable type class"
      "  units.ss         - Physical units and dimensions"
      ""
      "Computational:"
      "  vec-matrix-instances.ss - Type class instances for vectors/matrices"
      ""
      "Signal Processing (in core/):"
      "  complex.ss - Complex number arithmetic for Fourier analysis"
      "  dft.ss     - DFT/FFT algorithms (naive O(N²) and radix-2 O(N log N))"))
    (parsing-dsl . (
      "Parser Infrastructure:"
      "  parser.ss      - Parser combinator framework"
      "  parser-dsl.ss  - Parser DSL"
      "  parser-examples.ss - Example parsers"
      "  regex.ss       - Regular expressions"
      ""
      "DSL Tools:"
      "  dsl.ss         - DSL construction utilities"
      "  fsm.ss         - Finite state machines"
      "  logic.ss       - Logic programming"))
    (testing-verification . (
      "Property-Based Testing:"
      "  property.ss    - Property definitions"
      "  quickcheck.ss  - QuickCheck-style testing"
      "  laws.ss        - Type class law verification"
      ""
      "Effect Systems:"
      "  effects.ss     - Algebraic effects"
      "  validation.ss  - Validation applicative"))
    (utilities . (
      "Combinators & Strategies:"
      "  combinators.ss - Standard combinators (id, const, compose, etc.)"
      "  strategies.ss  - Rewriting strategies"
      "  prelude.ss     - FP prelude")))
   ((name . "core/")
    (purpose . "Canonicalized abstractions and reusable components")
    (description . "Higher-level patterns built from stitches.
                    Reusable components that don't fit in the core.")
    (contents . (
      "parse.ss      - Meta-parsing utilities"
      "query.ss      - Query combinators")))
   ((name . "core/wrinkles/")
    (purpose . "Low-level extensions and exceptional cases")
    (description . "Primitives that cannot be implemented in pure Scheme.
                    Foreign function interfaces, system calls, unsafe operations.
                    Currently empty - reserved for future use.")
    (contents . "(empty)"))))
 (philosophy . "
Core is the machine. It assumes perfect input. It never fails
(except by returning an error value). It computes answers.

Shell wraps Core, providing defensive validation and IO.
Users interact with Shell. Shell calls Core.
")
 (rules . (
   "Pure functions only - no (set!), no IO, no mutation (except local gensym counters)"
   "Total functions - always terminate with fuel parameter"
   "Type-checked - all exports have type signatures"
   "Defensive-free - assume inputs are valid (shell validates)"
   "Core assumes perfect input - errors are values, not exceptions"
   "De Bruijn indices - names are presentation, not semantics"))
 (for-builders . "
Builders may READ core/ to understand the system.
Builders must NOT MODIFY core/ (Shepherd-only).

To understand a module:
1. Read the header comment (purpose, dependencies)
2. Read the test file (test-<module>.ss)
3. Trace through an example in the REPL

To request a feature:
Post to forum/requests/ with your use case.
")
 (for-shepherds . "
Shepherds maintain core/. Evolution is conservative.

Before modifying:
1. Check MODULES.md for dependency graph
2. Run tests: scheme --script core/run-tests.ss
3. Document decision in docs/decisions/
4. Update MODULES.md if dependencies change

Refactoring is holy. Complexity is sin.
")
 (see-also . (
   "core/MODULES.md"
   "core/TEST-FRAMEWORK.md"
   "CLAUDE.md"
   "docs/decisions/")))
