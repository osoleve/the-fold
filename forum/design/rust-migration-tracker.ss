;;; forum/design/rust-migration-tracker.ss
;;; Project Tracker: Rust Migration
;;;
;;; This tracks the migration from Chez Scheme to Kernel (Rust VM)
;;; Status codes: todo, in-progress, blocked, review, done
;;;
;;; Author: Conductor (Opus)
;;; Date: 2025-12-26

'(project-tracker
  (name "Rust Migration: Chez → Core/Mantle/Crust")
  (start-date "2025-12-26")
  (target-date "2026-01-30")
  (owner opus)

  (milestones
    ((name "M0: Spec Fixes - BLOCKING")
     (target "2026-01-02")
     (description "Resolve 4 blocking specification issues from external review"))

    ((name "M1: Core Foundation")
     (target "2026-01-09")
     (description "Rust VM with 8 core forms, fuel-based totality"))

    ((name "M2: Mantle - Standard Primitives")
     (target "2026-01-16")
     (description "ANSI Scheme compatibility layer"))

    ((name "M3: Mantle - FoldLang Extensions")
     (target "2026-01-23")
     (description "Blocks, CAS, normalization, content-addressing"))

    ((name "M4: Parser & Integration")
     (target "2026-01-30")
     (description "Complete system with parser, REPL, tests"))

    ((name "M5: Production Ready")
     (target "2026-02-06")
     (description "Performance, validation, Crust cutover")))

  ;; ================================================================
  ;; PHASE 0: SPEC FIXES (3-5 days) - BLOCKING
  ;; ================================================================
  ;; External review identified 4 critical issues that MUST be resolved
  ;; before any Rust implementation begins.
  ;; See: forum/design/rust-migration-review-findings.ss

  (phase
    (name "Phase 0: Specification Fixes")
    (milestone "M0")
    (estimate "3-5 days")
    (blocking "ALL subsequent phases depend on Phase 0 completion")

    (task
      (id "P0-001")
      (title "Resolve fuel specification ambiguity")
      (status todo)
      (estimate "1 day")
      (assignee opus)
      (deliverable "Updated core/eval.ss with precise fuel semantics")
      (notes "Decision: 1 fuel = 1 eval call, primitives = 0 fuel
              Suspension at eval boundaries only, deterministic
              Add tests validating fuel consumption"))

    (task
      (id "P0-002")
      (title "Document CAS persistence boundary")
      (status todo)
      (estimate "0.5 days")
      (assignee opus)
      (deliverable "Updated core/cas.ss and shell/fs.ss documentation")
      (notes "Core CAS: In-memory only (HashMap<Hash, Block>)
              shell/fs.ss: Optional persistence layer in Shell tier
              Clarify boundary in architecture docs"))

    (task
      (id "P0-003")
      (title "Decide NFC normalization strategy")
      (status todo)
      (estimate "1 day")
      (assignee opus)
      (deliverable "Decision documented + spec updated + tests")
      (notes "Option A: Keep NFC, add unicode-normalization crate
              Option B: Relax to raw UTF-8 bytes (breaks hash stability)
              Must decide BEFORE any production hashes exist
              If keep NFC: update approved crates list
              If relax: update core/block.ss serialization spec"))

    (task
      (id "P0-004")
      (title "Add versioned address format to spec")
      (status todo)
      (estimate "1-2 days")
      (assignee opus)
      (deliverable "Updated core/block.ss and core/cas.ss with versioned addresses")
      (notes "Address = version:u8 || hash:bytes[32] (33 bytes total)
              Version 0: Current spec (NFC/raw UTF-8, little-endian, length-prefixed)
              Update hash-block to return 33 bytes
              Update CAS store keys to 33-byte addresses
              Add tests validating version byte handling")))

  ;; ================================================================
  ;; PHASE 1: CORE (Week 1-2)
  ;; ================================================================

  (phase
    (name "Phase 1: Core Foundation")
    (milestone "M1")
    (estimate "10-15 days")

    (task
      (id "C001")
      (title "Project setup: Cargo.toml, directory structure")
      (status todo)
      (estimate "1 hour")
      (assignee opus)
      (depends-on "P0-001" "P0-002" "P0-003" "P0-004")
      (deliverable "fold-rs/ with src/, tests/, benches/")
      (notes "⚠️ BLOCKED until Phase 0 complete
              cargo new fold-rs --lib
              Directory: core/, mantle/, tools/"))

    (task
      (id "C002")
      (title "Define Value enum (src/value.rs)")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "C001")
      (deliverable "Value with Number, String, Symbol, Bool, Bytevector, Closure, Block, Nil")
      (notes "Implement Display, Debug, PartialEq for Value"))

    (task
      (id "C003")
      (title "Define Expr AST (src/expr.rs)")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "C001")
      (deliverable "Expr enum for all 8 core forms")
      (notes "Quote, Fn, Call, Let, If, Fix, Case, Prim"))

    (task
      (id "C004")
      (title "Environment implementation (src/env.rs)")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "C002")
      (deliverable "Env with lookup, extend, extend*")
      (notes "HashMap<Symbol, Value> with parent chain"))

    (task
      (id "C005")
      (title "Closure type (src/closure.rs)")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "C002" "K004")
      (deliverable "Closure struct with params, body, env")
      (notes "Captures environment at creation time"))

    (task
      (id "C006")
      (title "TRAMPOLINE evaluator: quote, fn, if (src/eval.rs)")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "C002" "C003" "C004" "C005")
      (deliverable "Trampoline loop with eval_step handling quote, fn, if")
      (notes "⚠️ CRITICAL: Implement trampoline from day 1 (NOT naive tree-walker)
              enum Trampoline { Done(Value), Continue(Expr, Env, usize) }
              fn eval_loop() with loop + match
              Prevents stack overflow on deep recursion
              Fuel tracking: 1 fuel = 1 eval call"))

    (task
      (id "C007")
      (title "Evaluator: call (function application)")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "C006")
      (deliverable "eval_call() with argument evaluation")
      (notes "Evaluate function and arguments, apply closure"))

    (task
      (id "C008")
      (title "Evaluator: let (bindings)")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "C006")
      (deliverable "eval_let() with sequential bindings")
      (notes "Evaluate each binding, extend environment"))

    (task
      (id "C009")
      (title "Evaluator: fix (recursion with fuel)")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "C006")
      (deliverable "eval_fix() with self-reference")
      (notes "Translate from core/eval.ss:141-146"))

    (task
      (id "C010")
      (title "Evaluator: case (pattern matching)")
      (status todo)
      (estimate "5 hours")
      (assignee opus)
      (depends-on "C006")
      (deliverable "eval_case() on Block tags")
      (notes "Destructure blocks, bind pattern variables"))

    (task
      (id "C011")
      (title "Evaluator: prim (primitive dispatch stub)")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "C006")
      (deliverable "eval_prim() calling primitives module")
      (notes "Just stub for now, real primitives in Phase 2"))

    (task
      (id "C012")
      (title "Fuel management and suspension")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "C006")
      (deliverable "Fuel tracking, (suspended expr env) return")
      (notes "Decrement fuel each step, suspend at 0"))

    (task
      (id "C013")
      (title "Error handling: Result type")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "C006")
      (deliverable "EvalResult enum: Ok, Suspended, Error")
      (notes "Proper error propagation with context"))

    (task
      (id "C014")
      (title "Unit tests for Kernel evaluator")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "C006" "K007" "K008" "K009" "K010")
      (deliverable "tests/kernel_eval.rs with 20+ tests")
      (notes "Test each form, fuel exhaustion, errors"))

    (task
      (id "C015")
      (title "Minimal REPL for Kernel testing")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "C014")
      (deliverable "examples/kernel_repl.rs")
      (notes "Read-eval-print loop for manual testing")))

  ;; ================================================================
  ;; PHASE 2: MANTLE - STANDARD PRIMITIVES (Week 2)
  ;; ================================================================

  (phase
    (name "Phase 2: Mantle - Standard Primitives")
    (milestone "M2")
    (estimate "5-7 days")

    (task
      (id "M001")
      (title "Primitive dispatcher framework (src/primitives/mod.rs)")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "C011")
      (deliverable "dispatch_primitive(op, args) → Result<Value>")
      (notes "Match on Symbol, route to implementations"))

    (task
      (id "M002")
      (title "List primitives: cons, car, cdr, pair?, null?")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "C011")
      (deliverable "src/primitives/lists.rs")
      (notes "Use Value::Cons(Box<Value>, Box<Value>)"))

    (task
      (id "M003")
      (title "Arithmetic primitives: +, -, *, /, quotient, modulo")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "C011")
      (deliverable "src/primitives/arithmetic.rs")
      (notes "Handle div-by-zero, overflow"))

    (task
      (id "M004")
      (title "Comparison primitives: =, <, <=, >, >=")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "C011")
      (deliverable "src/primitives/comparison.rs")
      (notes "Generic comparison using PartialOrd"))

    (task
      (id "M005")
      (title "Boolean primitives: and, or, not")
      (status todo)
      (estimate "1 hour")
      (assignee opus)
      (depends-on "C011")
      (deliverable "Add to arithmetic.rs or comparison.rs")
      (notes "Simple boolean logic"))

    (task
      (id "M006")
      (title "Vector primitives: vector, make-vector, vector-ref, vector-set!")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "C011")
      (deliverable "src/primitives/vectors.rs")
      (notes "Use Value::Vector(Vec<Value>)"))

    (task
      (id "M007")
      (title "String primitives: string-length, string-ref, string-append")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "C011")
      (deliverable "src/primitives/strings.rs")
      (notes "UTF-8 handling, bounds checking"))

    (task
      (id "M008")
      (title "Character primitives: char->integer, char-alphabetic?")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "C011")
      (deliverable "Add to strings.rs")
      (notes "Rust char is Unicode scalar"))

    (task
      (id "M009")
      (title "Type predicates: number?, symbol?, string?, pair?, etc.")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "C011")
      (deliverable "src/primitives/predicates.rs")
      (notes "Pattern match on Value enum"))

    (task
      (id "M010")
      (title "Higher-order: map, filter, fold (foldl)")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "M001")
      (deliverable "src/primitives/higher_order.rs")
      (notes "Implement in Rust, not as FoldLang source"))

    (task
      (id "M011")
      (title "Tests: ANSI Scheme compliance")
      (status todo)
      (estimate "8 hours")
      (assignee opus)
      (depends-on "M001" "A003" "A004" "A006" "A007" "A010")
      (deliverable "tests/ansi_scheme.rs with 50+ tests")
      (notes "Test against R6RS/R7RS spec")))

  ;; ================================================================
  ;; PHASE 3: MANTLE - FOLDLANG EXTENSIONS (Week 3)
  ;; ================================================================

  (phase
    (name "Phase 3: Mantle - FoldLang Extensions")
    (milestone "M3")
    (estimate "7-10 days")

    (task
      (id "M101")
      (title "Block struct (src/block.rs)")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "C002")
      (deliverable "Block { tag: Symbol, payload: Vec<u8>, refs: Vec<Hash> }")
      (notes "Translate from core/block.ss"))

    (task
      (id "M102")
      (title "Block serialization: block->bytes")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "M101")
      (deliverable "Canonical serialization (deterministic)")
      (notes "Little-endian, NFC normalization, length-prefixed"))

    (task
      (id "M103")
      (title "Block deserialization: bytes->block")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "M102")
      (deliverable "Round-trip: block == bytes->block(block->bytes(block))")
      (notes "Error handling for invalid bytes"))

    (task
      (id "M104")
      (title "SHA-256 hashing with versioned addresses (src/hash.rs)")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "M102" "P0-004")
      (deliverable "hash_block(block) → Address (33 bytes: version || hash)")
      (notes "⚠️ CRITICAL: Use sha2 crate - NEVER roll your own crypto
              Address = version:u8 || sha256(canonical_bytes):bytes[32]
              Version 0: Current spec (per P0-003 NFC decision)
              Depends on P0-004 versioned address spec"))

    (task
      (id "M105")
      (title "Content-addressed store - in-memory only (src/cas.rs)")
      (status todo)
      (estimate "5 hours")
      (assignee opus)
      (depends-on "M104" "P0-002")
      (deliverable "CAS with store!, fetch, pin!, stored?")
      (notes "Core CAS: In-memory ONLY (HashMap<Address, Block>)
              shell/fs.ss handles OPTIONAL persistence (Shell tier)
              Depends on P0-002 CAS boundary clarification"))

    (task
      (id "M106")
      (title "Block primitives: make-block, block-tag, block-payload, block-refs")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "M101" "A001")
      (deliverable "src/primitives/fold_blocks.rs")
      (notes "Wire into primitive dispatcher"))

    (task
      (id "M107")
      (title "CAS primitives: hash-block, store!, fetch")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "M105" "F006")
      (deliverable "Add to fold_blocks.rs")
      (notes "Thread CAS through evaluator context"))

    (task
      (id "M108")
      (title "Normalization: α-equivalence (src/normalize.rs)")
      (status todo)
      (estimate "8 hours")
      (assignee opus)
      (depends-on "C003")
      (deliverable "normalize(expr) → canonical form")
      (notes "Translate core/normalize.ss - de Bruijn indices"))

    (task
      (id "M109")
      (title "Expansion: canonical → named (src/expand.rs)")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "M108")
      (deliverable "expand(canonical, symbols) → expr")
      (notes "Translate core/expand.ss - capture avoidance"))

    (task
      (id "M110")
      (title "Normalization primitives: normalize, expand")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "M108" "F009")
      (deliverable "Add to fold_blocks.rs")
      (notes "Wire into primitive dispatcher"))

    (task
      (id "M111")
      (title "Bytevector primitives: bv-make, bv-ref, bv-length, bv-concat")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "C011")
      (deliverable "src/primitives/bytevectors.rs")
      (notes "Use Value::Bytevector(Vec<u8>)"))

    (task
      (id "M112")
      (title "Tests: Block substrate")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "M101" "F002" "F003" "F004" "F005")
      (deliverable "tests/blocks.rs - port core/test-block.ss")
      (notes "Validate serialization, hashing, CAS"))

    (task
      (id "M113")
      (title "Tests: CAS operations")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "M105" "F007")
      (deliverable "tests/cas.rs - port core/test-cas.ss")
      (notes "All 8 original CAS tests"))

    (task
      (id "M114")
      (title "Tests: Normalization")
      (status todo)
      (estimate "5 hours")
      (assignee opus)
      (depends-on "M108" "F009" "F010")
      (deliverable "tests/normalize.rs - port core/test-normalize.ss")
      (notes "α-equivalence, round-trip, capture avoidance")))

  ;; ================================================================
  ;; PHASE 4: PARSER & INTEGRATION (Week 4)
  ;; ================================================================

  (phase
    (name "Phase 4: Parser & Integration")
    (milestone "M4")
    (estimate "7 days")

    (task
      (id "P001")
      (title "Hand-written S-expression lexer (src/parser/lexer.rs)")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (deliverable "Tokenize Scheme source → Token stream")
      (notes "Hand-write lexer (NOT nom/pest crate)
              Handle strings, symbols, numbers, parens, comments
              ~200-300 LOC total for lexer+parser"))

    (task
      (id "P002")
      (title "Hand-written S-expression parser (src/parser/sexpr.rs)")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "P001")
      (deliverable "Token stream → SExpr (atoms, lists)")
      (notes "Hand-write recursive descent parser (NOT nom/pest)
              Reviewer consensus: hand-written is simpler for S-exprs
              ~200-500 LOC total for full parser"))

    (task
      (id "P003")
      (title "SExpr → Expr conversion (src/parser/to_expr.rs)")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "P002" "K003")
      (deliverable "SExpr → Expr (recognize core forms)")
      (notes "Validate syntax, report errors"))

    (task
      (id "P004")
      (title "File loading: load function")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "P003")
      (deliverable "load(path) → Vec<Expr>")
      (notes "Read file, parse, return expressions"))

    (task
      (id "P005")
      (title "REPL: Read-Eval-Print Loop (src/repl.rs)")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "P003")
      (deliverable "Interactive REPL with history, fuel setting")
      (notes "Use rustyline crate for line editing"))

    (task
      (id "P006")
      (title "CLI: Command-line interface (src/main.rs)")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "P004" "P005")
      (deliverable "fold --repl, fold file.ss, fold --eval expr")
      (notes "Use clap crate for argument parsing"))

    (task
      (id "P007")
      (title "Pretty printer: Value → String")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "C002")
      (deliverable "Display trait for Value (readable output)")
      (notes "Proper quoting for strings, lists"))

    (task
      (id "P008")
      (title "Error messages: user-friendly reporting")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "C013")
      (deliverable "Error with source location, context")
      (notes "Include file, line, column in errors"))

    (task
      (id "P009")
      (title "Tests: Parser round-trip")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "P002" "P003")
      (deliverable "tests/parser.rs - parse → print → parse")
      (notes "Validate all core forms parse correctly"))

    (task
      (id "P010")
      (title "Integration test: Load and run core/test-block.ss")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "P004" "F012")
      (deliverable "Can execute original Scheme test files")
      (notes "First real end-to-end test"))

    (task
      (id "P011")
      (title "Documentation: README, API docs, examples")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "P006")
      (deliverable "docs/ with usage, architecture, API")
      (notes "Rustdoc comments for all public APIs")))

  ;; ================================================================
  ;; PHASE 5: VALIDATION & CUTOVER (Week 5)
  ;; ================================================================

  (phase
    (name "Phase 5: Production Readiness")
    (milestone "M5")
    (estimate "7 days")

    (task
      (id "V001")
      (title "Port all core tests to Rust")
      (status todo)
      (estimate "12 hours")
      (assignee opus)
      (depends-on "P010")
      (deliverable "All 13 test-*.ss files run in Rust")
      (notes "test-block, test-cas, test-normalize, test-sha256, etc."))

    (task
      (id "V002")
      (title "Cross-validation: Rust vs Chez outputs")
      (status todo)
      (estimate "8 hours")
      (assignee opus)
      (depends-on "V001")
      (deliverable "Validation suite comparing outputs")
      (notes "Run same inputs, assert same results"))

    (task
      (id "V003")
      (title "Performance benchmarking")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "V001")
      (deliverable "benches/ with criterion benchmarks")
      (notes "Compare Rust vs Chez on eval, primitives, hashing"))

    (task
      (id "V004")
      (title "Memory profiling and optimization")
      (status todo)
      (estimate "8 hours")
      (assignee opus)
      (depends-on "V003")
      (deliverable "Identify and fix memory leaks, excessive allocations")
      (notes "Use valgrind, heaptrack, or Rust profilers"))

    (task
      (id "V005")
      (title "Shell script adaptation: Use Kernel instead of Chez")
      (status todo)
      (estimate "6 hours")
      (assignee sonnet)
      (depends-on "V002")
      (deliverable "shell/*.ss run with fold instead of scheme")
      (notes "Update daemon.sh, fold.sh, start.sh"))

    (task
      (id "V006")
      (title "REPL daemon: Port to Kernel")
      (status todo)
      (estimate "8 hours")
      (assignee sonnet)
      (depends-on "P005" "V005")
      (deliverable "shell/repl-daemon.ss works with Kernel")
      (notes "File-based IPC still functional"))

    (task
      (id "V007")
      (title "Forum tools: Validate on Kernel")
      (status todo)
      (estimate "4 hours")
      (assignee haiku)
      (depends-on "V005")
      (deliverable "Can post to forum using Kernel REPL")
      (notes "Test (hi), (chat), (msg) commands"))

    (task
      (id "V008")
      (title "Graphics engine: Validate on Kernel")
      (status todo)
      (estimate "4 hours")
      (assignee sonnet)
      (depends-on "V005")
      (deliverable "shell/graphics.ss works with Kernel")
      (notes "Test rendering, block storage"))

    (task
      (id "V009")
      (title "Regression testing: Full system")
      (status todo)
      (estimate "6 hours")
      (assignee opus)
      (depends-on "V005" "V006" "V007" "V008")
      (deliverable "All existing functionality works")
      (notes "Automated test suite for entire Scaffold layer"))

    (task
      (id "V010")
      (title "Documentation: Migration guide")
      (status todo)
      (estimate "4 hours")
      (assignee opus)
      (depends-on "V009")
      (deliverable "docs/migration.md - how to switch")
      (notes "Differences, gotchas, performance notes"))

    (task
      (id "V011")
      (title "Release: Kernel v0.1.0")
      (status todo)
      (estimate "3 hours")
      (assignee opus)
      (depends-on "V009" "V010")
      (deliverable "Tagged release, binary artifacts")
      (notes "cargo build --release, publish?"))

    (task
      (id "V012")
      (title "Cutover: Switch default to Kernel")
      (status todo)
      (estimate "2 hours")
      (assignee opus)
      (depends-on "V011")
      (deliverable "./daemon.sh uses fold instead of scheme")
      (notes "Keep Chez as fallback for one release")))

  ;; ================================================================
  ;; ONGOING / FUTURE
  ;; ================================================================

  (phase
    (name "Future Enhancements")
    (milestone "Future")
    (estimate "TBD")

    (task
      (id "E001")
      (title "Bytecode compilation")
      (status todo)
      (estimate "20 hours")
      (assignee opus)
      (deliverable "Compile Expr → Bytecode, interpret bytecode")
      (notes "Performance optimization"))

    (task
      (id "E002")
      (title "JIT compilation (cranelift?)")
      (status todo)
      (estimate "40 hours")
      (assignee opus)
      (deliverable "Hot path compilation to native code")
      (notes "Major performance boost"))

    (task
      (id "E003")
      (title "Incremental GC for blocks")
      (status todo)
      (estimate "30 hours")
      (assignee opus)
      (deliverable "Garbage collect unpinned blocks")
      (notes "Prevent CAS bloat"))

    (task
      (id "E004")
      (title "Persistent CAS: Filesystem backend")
      (status todo)
      (estimate "12 hours")
      (assignee sonnet)
      (depends-on "M105")
      (deliverable "Store blocks on disk, not just memory")
      (notes "Already spec'd in shell/fs.ss"))

    (task
      (id "E005")
      (title "Type checker: Rust implementation")
      (status todo)
      (estimate "25 hours")
      (assignee opus)
      (deliverable "Type inference, kind checking")
      (notes "Translate core/types.ss, core/infer.ss"))

    (task
      (id "E006")
      (title "Debugger: Step, breakpoints, inspect")
      (status todo)
      (estimate "20 hours")
      (assignee opus)
      (deliverable "Interactive debugger for FoldLang")
      (notes "Leverage Rust tooling"))

    (task
      (id "E007")
      (title "LSP server: Language Server Protocol")
      (status todo)
      (estimate "30 hours")
      (assignee sonnet)
      (deliverable "FoldLang support in editors (VSCode, etc.)")
      (notes "Completion, goto-definition, diagnostics")))

  ;; ================================================================
  ;; DEPENDENCIES GRAPH (for visualization)
  ;; ================================================================

  (dependencies
    ;; Kernel phase
    (K002 K001)
    (K003 K001)
    (K004 K002)
    (K005 K002 K004)
    (K006 K002 K003 K004 K005)
    (K007 K006)
    (K008 K006)
    (K009 K006)
    (K010 K006)
    (K011 K006)
    (K012 K006)
    (K013 K006)
    (K014 K006 K007 K008 K009 K010)
    (K015 K014)

    ;; ANSI Scheme phase
    (A001 K011)
    (A002 A001)
    (A003 A001)
    (A004 A001)
    (A005 A001)
    (A006 A001)
    (A007 A001)
    (A008 A001)
    (A009 A001)
    (A010 A002)
    (A011 A002 A003 A004 A006 A007 A010)

    ;; FoldLang phase
    (F001 K002)
    (F002 F001)
    (F003 F002)
    (F004 F002)
    (F005 F004)
    (F006 F001 A001)
    (F007 F005 F006)
    (F008 K003)
    (F009 F008)
    (F010 F008 F009)
    (F011 A001)
    (F012 F001 F002 F003 F004 F005)
    (F013 F005 F007)
    (F014 F008 F009 F010)

    ;; Parser phase
    (P002 P001)
    (P003 P002 K003)
    (P004 P003)
    (P005 P003)
    (P006 P004 P005)
    (P007 K002)
    (P008 K013)
    (P009 P002 P003)
    (P010 P004 F012)
    (P011 P006)

    ;; Validation phase
    (V001 P010)
    (V002 V001)
    (V003 V001)
    (V004 V003)
    (V005 V002)
    (V006 P005 V005)
    (V007 V005)
    (V008 V005)
    (V009 V005 V006 V007 V008)
    (V010 V009)
    (V011 V009 V010)
    (V012 V011)

    ;; Future enhancements
    (E002 E001)
    (E004 F005)
    (E005 F014))

  ;; ================================================================
  ;; SUMMARY STATISTICS
  ;; ================================================================

  (summary
    (total-tasks 81)
    (total-estimate "~420 hours")
    (target-duration "8-10 weeks realistic (5 weeks aggressive)")
    (critical-path "P0-001→P0-002→P0-003→P0-004→C001→C002→C004→C005→C006→C014→M001→M101→M104→M105→P001→P002→P003→P010→V001→V009→V012")
    (staffing "1 Opus (primary), Sonnet + Haiku for validation")
    (blocking-requirements
      "⚠️ Phase 0 MUST complete before Rust implementation
       External review identified 4 critical spec issues")
    (risks
      ((risk "Blocking spec issues not resolved")
       (mitigation "Phase 0 must complete first - no Rust code until specs fixed"))
      ((risk "Rust learning curve")
       (mitigation "Start simple, iterate, reference existing Scheme code"))
      ((risk "Performance worse than Chez")
       (mitigation "Benchmark early, optimize hotspots, consider bytecode/JIT"))
      ((risk "Semantic bugs in translation")
       (mitigation "Cross-validation suite, comprehensive tests, differential testing"))
      ((risk "Scope creep")
       (mitigation "Defer type system, debugger, LSP to future")))))
