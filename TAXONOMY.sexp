;;; TAXONOMY.sexp — The Fold Module Taxonomy
;;;
;;; Authoritative classification of all modules in the system.
;;; This file defines the hierarchical structure, naming conventions,
;;; and categorization rules for the entire codebase.
;;;
;;; Machine-readable for tooling (linters, navigation, documentation).

((version . "2.0")
 (date . "2026-01-09")
 (status . canonical)

 ;;; ====
 ;;; THREE-LAYER ARCHITECTURE
 ;;; ====
 ;;;
 ;;; core/    — Language kernel (minimal, axiomatic)
 ;;; lattice/ — Skill DAG (stdlib = tier 0, grows organically)
 ;;; shell/   — Impure boundary (IO, validation, capabilities)

 ;;; ====
 ;;; DOMAIN HIERARCHY
 ;;; ====
 ;;;
 ;;; Top-level conceptual domains organizing the codebase.

 (domains

  ((domain . foundation)
   (description . "Core language primitives and block machine")
   (purity . pure)
   (tier . shepherd)
   (modules . (core/base core/blocks)))

  ((domain . types)
   (description . "Type system, inference, and dependent types")
   (purity . pure)
   (tier . shepherd)
   (modules . (core/types)))

  ((domain . language)
   (description . "Parsing, evaluation, compilation, modules")
   (purity . pure)
   (tier . shepherd)
   (modules . (core/lang core/dsl)))

  ((domain . mathematics)
   (description . "Pure mathematical computation")
   (purity . pure)
   (tier . shepherd)
   (subdomains . (
     ((name . algebra)
      (modules . (core/algebra core/number-theory)))
     ((name . linear-algebra)
      (modules . (core/linalg)))
     ((name . analysis)
      (modules . (core/numeric core/fp/numeric)))
     ((name . probability)
      (modules . (core/random core/info-theory)))
     ((name . topology)
      (modules . (lattice/topology))))))

  ((domain . computation)
   (description . "Computational abstractions and algorithms")
   (purity . pure)
   (tier . shepherd)
   (subdomains . (
     ((name . autodiff)
      (modules . (core/autodiff)))
     ((name . physics)
      (modules . (core/diff-physics core/diff-physics-3d core/dynamics)))
     ((name . geometry)
      (modules . (core/geometry core/sim)))
     ((name . data-structures)
      (modules . (core/data core/query)))
     ((name . automata)
      (modules . (core/automata))))))

  ((domain . functional-programming)
   (description . "Type classes, effects, and FP abstractions")
   (purity . pure)
   (tier . shepherd)
   (modules . (core/fp))
   (subdomains . (
     ((name . control)
      (description . "Monads, effects, continuations")
      (modules . (core/fp/control)))
     ((name . data)
      (description . "Streams, lazy structures")
      (modules . (core/fp/data)))
     ((name . parsing)
      (description . "Parser combinators")
      (modules . (core/fp/parsing)))
     ((name . numeric)
      (description . "Transcendental functions")
      (modules . (core/fp/numeric)))
     ((name . symbolic)
      (description . "Symbolic computation")
      (modules . (core/fp/symbolic)))
     ((name . rewrite)
      (description . "Term rewriting, fusion")
      (modules . (core/fp/rewrite)))
     ((name . analysis)
      (description . "Static program analysis")
      (modules . (core/fp/analysis)))
     ((name . game)
      (description . "Game theory")
      (modules . (core/fp/game)))
     ((name . measure)
      (description . "Units of measure")
      (modules . (core/fp/measure)))
     ((name . meta)
      (description . "DSL utilities")
      (modules . (core/fp/meta)))
     ((name . control-systems)
      (description . "Control theory")
      (modules . (core/fp/control-systems))))))

  ((domain . infrastructure)
   (description . "Testing, benchmarking, utilities")
   (purity . pure)
   (tier . shepherd)
   (modules . (core/testing core/benchmarks core/util core/pipeline core/lsp)))

  ((domain . shell)
   (description . "IO layer, validation, and user interface")
   (purity . impure)
   (tier . builder)
   (subdomains . (
     ((name . repl)
      (description . "Interactive environment and sessions")
      (modules . (shell/repl)))
     ((name . storage)
      (description . "Persistence and CAS")
      (modules . (shell/storage)))
     ((name . diagnostics)
      (description . "Profiling, debugging, analysis")
      (modules . (shell/diagnostics shell/debug shell/introspect)))
     ((name . visualization)
      (description . "Graphics, UI, rendering")
      (modules . (shell/ui)))
     ((name . integration)
      (description . "External systems: git, lsp, discord, mcp")
      (modules . (shell/git shell/lsp shell/discord shell/mcp-server shell/pipeline)))
     ((name . assistants)
      (description . "AI agents and helpers")
      (modules . (shell/assistants)))
     ((name . tools)
      (description . "Developer utilities")
      (modules . (shell/tools shell/lens)))
     ((name . io)
      (description . "Filesystem, JSON, validation")
      (modules . (shell/io)))))))

 ;;; ====
 ;;; NAMING CONVENTIONS
 ;;; ====

 (naming-conventions

  (file-naming
   ((pattern . "module-name.ss")
    (description . "Hyphenated lowercase for all module files")
    (examples . (block-navigator.ss fuel-profile.ss type-inspect.ss)))

   ((pattern . "test-module-name.ss")
    (description . "Test files prefixed with test-")
    (examples . (test-block.ss test-eval.ss test-matrix.ss))
    (deprecated . "*-test.ss"))

   ((pattern . "README.sexp")
    (description . "S-expression documentation in every directory")))

  (directory-naming
   ((pattern . "lowercase-hyphenated")
    (examples . (diff-physics info-theory number-theory control-systems))
    (avoid . (camelCase underscores single_letters abbreviations))))

  (function-naming
   ;; Reference the detailed FP conventions
   (reference . "lattice/fp/NAMING-CONVENTIONS.sexp")

   ;; Summary of key patterns
   (patterns . (
     ((pattern . "type-operation")
      (description . "Type-specific operations")
      (examples . (maybe-bind state-get matrix-multiply)))

     ((pattern . "make-type")
      (description . "Constructors for complex types")
      (examples . (make-functor make-applicative make-block)))

     ((pattern . "type?")
      (description . "Predicates")
      (examples . (maybe? block? valid-hash?)))

     ((pattern . "type-field")
      (description . "Accessors")
      (examples . (block-tag block-payload functor-fmap)))))))

 ;;; ====
 ;;; CLASSIFICATION RULES
 ;;; ====
 ;;;
 ;;; Rules for determining where new code belongs.

 (classification-rules

  ((rule . "Pure computation without IO -> core/")
   (rationale . "Maintains the Fabric/Thimble separation")
   (test . "Does this function call any OS/network/file operations?"))

  ((rule . "IO, validation, or effects -> shell/")
   (rationale . "Defensive code lives in the shell")
   (test . "Does this touch the outside world?"))

  ((rule . "Type class or FP abstraction -> core/fp/")
   (rationale . "FP toolkit is the home for abstract patterns")
   (test . "Is this a reusable functional abstraction?"))

  ((rule . "Mathematical computation -> core/{domain}/")
   (rationale . "Use domain-specific directories")
   (test . "Is this math? Which branch?"))

  ((rule . "Developer tooling -> shell/tools/")
   (rationale . "Keep tools consolidated")
   (test . "Is this for developer productivity?"))

  ((rule . "Test infrastructure -> core/testing/")
   (rationale . "Test framework is pure and foundational")
   (test . "Is this about running or defining tests?"))

  ((rule . "Performance measurement -> core/benchmarks/")
   (rationale . "Benchmarks are reproducible computation")
   (test . "Is this for measuring performance?")))

 ;;; ====
 ;;; TIER ACCESS
 ;;; ====

 (tiers
  ((tier . player)
   (description . "Can use shell/ functions, cannot modify")
   (access . (read shell)))

  ((tier . builder)
   (description . "Can modify shell/, add utilities")
   (access . (read core shell) (write shell)))

  ((tier . shepherd)
   (description . "Can modify core/, architectural decisions")
   (access . (read core shell) (write core shell))))

 ;;; ====
 ;;; DIRECTORY STRUCTURE
 ;;; ====

 (structure
  ;; CORE — Language Kernel (minimal, axiomatic)
  (core
   (base "Foundation: prelude, sha256, error")
   (blocks "Block system and CAS")
   (types "Type system")
   (lang "Language core: eval, compile, module, nbe")
   (util "Core utilities: debug, pretty, cost-model")
   (testing "Test framework and runners")
   (benchmarks "Performance benchmarks"))

  ;; LATTICE — Skill DAG (stdlib = tier 0)
  (lattice
   ;; Tier 0 — Foundational (no lattice deps)
   (linalg "Linear algebra: vec, matrix, decomp, solvers")
   (data "Data structures: graphs, collections")
   (algebra "Abstract algebra: groups, rings, fields")
   (random "PRNG, distributions, monte-carlo")
   ;; Tier 1 — Intermediate
   (numeric "Complex, DFT, signal processing")
   (geometry "Shapes, transforms, SDF, raymarching")
   (autodiff "Automatic differentiation")
   (fp "FP toolkit (11 subdirectories)")
   (query "Query DSL, SQL parser, patterns")
   (dsl "Tagless final, chronicle, staging")
   (info "Information theory")
   (number-theory "Primes, modular arithmetic")
   (statistics "Statistical modeling: regression, GLM, time series, hypothesis tests")
   (topology "Simplicial complexes, homology, TDA")
   ;; Tier 2+ — Advanced
   (physics/diff "Differentiable 2D physics")
   (physics/diff3d "Differentiable 3D physics")
   (physics/classical "Classical 2D physics")
   (physics/classical3d "Classical 3D physics")
   (tiles "Board game SDK (hex, square, triangle)")
   (sim "Simulation, dynamics")
   (automata "State machines, DFA/NFA")
   (pipeline "Agent workflows"))

  (shell
   (tests "Test suite")
   (tools "Developer tools")
   (ui "Graphics and visualization")
   (lens "Code navigation")
   (git "Version control")
   (pipeline "Effect interpretation")
   (lsp "Language server protocol")
   (discord "Discord integration")
   (mcp-server "MCP integration")
   (benchmarks "Shell benchmarks")
   (introspect "Code analysis")
   ;; Future directories (shell reorg)
   ;; (repl "REPL and sessions")
   ;; (storage "Persistence")
   ;; (diagnostics "Profiling and analysis")
   ;; (assistants "AI agents")
   ;; (io "Core IO")
   ;; (debug "Debugging tools")
   ))

 (see-also . (
   "lattice/fp/NAMING-CONVENTIONS.sexp"
   "core/README.sexp"
   "shell/README.sexp"
   "CLAUDE.md"
   "docs/language-reference.md")))
