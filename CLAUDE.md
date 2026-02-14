# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Identity

**The Fold** is a content-addressable homoiconic universe built on Chez Scheme.

The Fold is being built as a cognitive substrate for AI — an environment where intelligence and tooling converge. The homoiconic design means abstractions are free from a context-window perspective: once a skill is verified in the lattice, an AI agent doesn't need to reason about its internals, it just reaches for a tool. The lattice is a hierarchy of verified cognitive shortcuts of increasing abstraction and sophistication. Every design decision optimizes for composability and cognitive efficiency — the more composable things are, the more an LLM can accomplish with fewer parameters.

The long-term vision: a smaller, Fold-native model that's as capable as frontier models within this substrate, because the lattice has already done most of the thinking for it. The lines between AI and ecosystem blur because the tools are symbiotic with its intelligence.

---

## Interacting with The Fold

### Using ./fold (Recommended)

```bash
./fold "(+ 1 2)"                   # Evaluate an expression
./fold "(bye)"                     # Single-token commands
./fold -s dev "(define x 10)"      # Named session with -s flag
./fold -s dev "(begin x)"          # Retrieve variable value from session
./fold --status                    # Check if daemon is running
./fold --sessions                  # List active worker sessions
```

**Key features:**
- **Auto-starts daemon** if not running (disable with `--no-auto-start`)
- Code is passed as-is — use explicit parentheses
- **Sessions persist state** across invocations - variables, functions, and loaded modules are retained

Returns result on stdout, errors to stderr with exit codes: 0=success, 1=error, 2=timeout.

### Session Cleanup

```bash
./fold -s dev "(bye)"              # Logout and clean up session files
./fold -s dev "(who)"              # Show current session info
```

### Essential Commands

```scheme
(help)                           ; Show all commands
(blocks)                         ; CAS statistics
(explore-block hash)             ; Explore a block
(search "query")                 ; Search blocks
(commit! "message")              ; Git commit
(push!)                          ; Git push

;; Session management
(who)                            ; Show current session info
(bye)                            ; Logout and clean up session files
```

### Debugging

The time-travel debugger lives in `boundary/debug/debug-repl.ss`:

```scheme
(load "boundary/debug/debug-repl.ss")
(debug expr)                     ; Start debugging
(step)                           ; Single step
(next)                           ; Step over
(continue)                       ; Run to breakpoint/completion
(break 'fn)                      ; Set breakpoint
(inspect)                        ; Show environment
(fuel)                           ; Show fuel status
(trace)                          ; Show call stack
```

---

## Running Tests

```bash
# Full test suite
scheme --script test-all.ss

# Test subsets
scheme --script test-all.ss quick     # Skip slow tests
scheme --script test-all.ss core      # Core tests only
scheme --script test-all.ss boundary  # Boundary tests only

# Individual modules and their tests are colocated, e.g.
scheme --script lattice/linalg/test-vec.ss
```

Test framework: `core/testing/test-framework.ss` provides unified API across all tests.

### Writing Tests

```scheme
(load "core/testing/test-framework.ss")

(define-test "descriptive name"
  (assert-equal expected actual)
  (assert-true expr)
  (assert-false expr)
  (assert-error expr) ; Verify expr throws
  (assert-ok expr))   ; Verify (ok ...) result

(test-group "group name"
  (define-test "test 1" ...)
  (define-test "test 2" ...))

(run-all-tests)          ; Run all registered tests
(run-tests 'group-name)  ; Run specific group
```

**Note:** `assert-true` checks `(eq? #t expr)`, not just truthiness. Use `(assert-true (pair? x))` instead of `(assert-true x)` when x might be a truthy non-boolean like a pair from `assq`.

**Performance tip:** When tests need expensive initialization (building indices, parsing manifests), use an "ensure" pattern: check if already initialized before building. See `kg-ensure!` and `lattice-ensure!` in `lattice/meta/` for examples. This reduced test-meta.ss runtime from 20s to 2s.

---

## Architecture Overview

### The Block Machine

Everything is a **Block**: `{tag, payload, refs[]}`

- `tag`: Interned symbol identifying block type
- `payload`: Raw bytes (literals, encoded S-expressions)
- `refs`: Ordered vector of hashes pointing to other blocks

All content is **content-addressed** — the cryptographic hash IS the identity.

### Directory Structure

| Directory | Purpose |
|----|----|
| `core/` | Language kernel — pure, minimal, axiomatic |
| `lattice/` | Skill lattice — verified library DAG (includes "stdlib") |
| `boundary/` | Impure boundary — IO, validation, capabilities |
| `user/` | Playground — experiments and demos |
| `docs/` | Documentation and policy |

**Note on Technical Report:** `docs/technical-report.md` is **generated** from chapter files in `docs/technical-report/`. Edit the chapter files (e.g., `00-abstract.md`, `06-the-module-system.md`), then run `scheme --script docs/technical-report/assemble.ss` to rebuild. Chapter order is defined in `docs/technical-report/manifest.sexp`.

### Core (Language Kernel)

Core defines what The Fold IS — minimal, axiomatic, changes are breaking:

| Directory | Purpose | Key Modules |
|----|----|----|
| `base/` | Foundation (no deps) | prelude.ss, sha256.ss, error.ss |
| `blocks/` | Block system & CAS | block.ss, cas.ss, normalize.ss |
| `types/` | Type system | types.ss, dep-types.ss, infer.ss, kinds.ss |
| `lang/` | Evaluation & compilation | eval.ss, compile.ss, module.ss, nbe.ss |
| `util/` | Core utilities | debug.ss, pretty.ss, pretty-class.ss, cost-model.ss |
| `testing/` | Test infrastructure | test-framework.ss |
| `benchmarks/` | Performance benchmarks | bench-core.ss, bench-prim.ss |

**Type Classes (`core/types/kinds.ss`, `core/types/resolve.ss`):**

| Class | Kind | Methods | Purpose |
|-------|------|---------|---------|
| `Eq` | `* → Constraint` | `==`, `/=` | Equality comparison |
| `Ord` | `* → Constraint` | `<`, `<=`, `>`, `>=`, `compare` | Ordering |
| `Show` | `* → Constraint` | `show : a → String` | String conversion |
| `Pretty` | `* → Constraint` | `pretty : a → Doc`, `pretty-prec : Int → a → Doc` | Width-aware pretty-printing |
| `Functor` | `(* → *) → Constraint` | `fmap` | Mappable containers |
| `Monad` | `(* → *) → Constraint` | `>>=`, `return` | Sequencing with context |

### Lattice (Skill DAG)

The lattice is a DAG of verified skills.

| Directory | Purpose |
|----|----|
| `linalg/` | Vectors, matrices, decomposition, solvers |
| `data/` | Data structures, graphs, collections, community detection |
| `algebra/` | Groups, rings, polynomial algebra, Gröbner bases |
| `random/` | PRNG, distributions |
| `numeric/` | Complex numbers, DFT, signal processing |
| `geometry/` | Shapes, transforms, raymarching, SDFs, mesh topology |
| `diffgeo/` | Charts, atlases, Lie groups, Riemannian curvature |
| `autodiff/` | Reverse-mode AD, computational graphs, interval gradients, optics-based gradient |
| `fp/` | Monads, parsers, streams, protocols, game theory, control systems |
| `optics/` | Complete optics tower (Iso, Lens, Prism, Affine, Traversal, Fold, Getter, Setter) |
| `fp/clp/` | Constraint logic programming (cKanren-style CLP(FD)) |
| `query/` | Query DSL, SQL parser, patterns |
| `dsl/` | Tagless final, chronicle, staging, template DSL |
| `info/` | Entropy, coding, information theory |
| `number-theory/` | Primes, modular arithmetic |
| `meta/` | Lattice navigation, search, introspection |
| `topology/` | Simplicial complexes, homology, Betti numbers |
| `crypto/` | SHA-512, BLAKE2b, HMAC |
| `optimization/` | LP, ILP, gradient descent, Newton, L-BFGS, interval global, constraint contractors |
| `statistics/` | Regression, GLM, time series, hypothesis testing |
| `physics/lenses/` | Physics lenses integrated with optics tower (bodies, particles, worlds) |
| `physics/diff/` | Differentiable 2D physics |
| `physics/diff3d/` | Differentiable 3D physics |
| `physics/classical/` | Classical 2D physics |
| `physics/classical3d/` | Classical 3D physics |
| `tiles/` | Board game SDK (hex, square, triangle) |
| `sim/` | Simulation, dynamics |
| `egraph/` | Equality saturation, cost-based extraction |
| `ui/` | Layout combinators (pure) |
| `ipc/` | Wire protocol (pure) |
| `automata/` | State machines, DFA/NFA |
| `pipeline/` | Agent workflows, RLM types, council |

**Key Subsystems:** FP (monads, parsers, game theory, control systems), Optics, SAT/MaxSAT, CLP(FD), Statistics. Use `/lattice-api` skill for detailed API reference, or `(li 'skill)` / `(le 'skill)` for quick inspection.

**Module System (`core/lang/module.ss`):**

```scheme
(require 'charts)              ; Simple (first-match-wins)
(require 'diffgeo/charts)      ; Namespaced (unambiguous)
(require 'algebra/polynomial)  ; Avoids collision with numeric/polynomial

(modules)                      ; List all modules
(module-info 'charts)          ; Show module details
(module-collisions)            ; List name collisions
```

Use namespaced form (`'dir/module`) when module names collide. The system warns on collision during simple require.

**`(require ...)` vs `(load ...)`:**

| Use Case | Mechanism | Notes |
|----------|-----------|-------|
| **Library code** | `(require 'module)` | Preferred. Handles dependencies, avoids double-loading, collision detection |
| **Test scripts** | `(load "path.ss")` | OK for standalone scripts run via `scheme --script` |
| **Boundary modules** | `(require 'boundary/bbs)` | Boundary code can use require too |
| **REPL exploration** | Either | `require` is cleaner; `load` works for quick experiments |

When creating new modules, prefer `(require ...)` chains over `(load ...)`. The module system tracks what's loaded and prevents redundant evaluation.

**New module header pattern:**

```scheme
(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module my-module
;;; @requires prelude vec2 rigid-body
(require 'prelude)
(require 'vec2)
(require 'rigid-body)

(doc 'module 'my-module)
```

The guarded bootstrap ensures `module.ss` loads exactly once. The `@module`/`@requires` annotations (space-separated, no commas) enable the reload system. All lattice modules follow this pattern — register new modules in `core/lang/module.ss` via `(register-module-path! 'name "path.ss")`. Use namespaced names (`'tiles/core`, `'physics/optimize`) when bare names collide.

**Module Reloading:**

```scheme
(reload! 'parse)              ; Reload single module
(reload-with-dependents! 'parse)  ; Reload module + all dependents
(rel! 'parse)                 ; Alias for reload!
(rel+! 'parse)                ; Alias for reload-with-dependents!

(module-dependents 'parse)    ; Show direct dependents
(all-dependents 'parse)       ; Show transitive dependents
```

Reloading requires modules to use `@requires` annotations in their headers. The reload respects topological order (dependencies before dependents).

**Meta-Tooling:** Use `/lattice-search` skill. Quick: `(lf "query")` for search, `(li 'skill)` for info, `(le 'skill)` for exports.

### Boundary Subsystems

Boundary is organized into functional subdirectories. Root-level files are entry points only:
- `commands.ss` — REPL command registry
- `toolkit.ss` — Development toolkit index
- `run-tests.ss` — Boundary test runner

| Directory | Purpose | Key Modules |
|----|----|----|
| `repl/` | REPL & session management | repl.ss, repl-daemon.ss, session-manager.ss, history.ss |
| `blocks/` | Block system tools | block-explorer.ss, block-navigator.ss, block-query.ss |
| `debug/` | Debugging & errors | debug-repl.ss, error-fmt.ss, type-inspect.ss, xref.ss |
| `diagnostics/` | Profiling & analysis | profiler-unified.ss, fuel-analysis.ss, profile-*.ss |
| `storage/` | Persistence & identity | store-api.ss, cas-persist.ss, identity.ss |
| `io/` | Low-level IO utilities | fs.ss, json.ss, process.ss |
| `git/` | Git operations | git.ss, git-workflow.ss |
| `tools/` | Developer utilities | edit.ss, refactor-toolkit.ss, autodoc.ss, capability-lens.ss |
| `ui/` | Graphics & display | graphics.ss, color.ss, layers.ss, turtle.ss |
| `lens/` | Code navigation lenses | call-graph.ss, navigator.ss, jump.ss |
| `introspect/` | System introspection | complexity.ss, exports.ss, memory.ss, timing.ss |
| `pipeline/` | Agent pipelines | workflow integration |
| `provenance/` | Optic provenance tracking | provenance.ss, traced-optics.ss, query.ss |
| `reactive/` | Reactive derivations | reactive.ss (optic dependency tracking) |
| `bbs/` | Issue tracker | bbs.ss, ops.ss, index.ss |
| `migrations/` | Schema migrations | runner.ss (CAS tree migration), registry.ss (version graph) |
| `ffi/` | Rust FFI & acceleration | ffi-core.ss, bvh-ffi.ss, socket-ffi.ss, rust-accel/ |
| `ipc/` | Socket IPC client | socket-client.ss |
| `geometry/` | Geometry I/O wrappers | bvh-accel.ss, raymarch-accel.ss, obj-io.ss |
| `meta/` | Lattice meta I/O orchestrators | exports-io.ss, xref-io.ss, docs-io.ss, persist-io.ss |
| `lsp/` | Language server protocol | lsp-server.ss, protocol.ss |
| `examples/` | Demo scripts | demo-turtle.ss, core-playground.ss |
| `tests/` | Boundary test suite | test-*.ss files |

**Developer Tools:** Protocols (`lattice/fp/protocol.ss`), protocol bundles, refactoring toolkit (`boundary/tools/refactor-toolkit.ss`), and template DSL. Use `/dev-tools` skill for detailed API.

---

## Core Principles

### The Core Is Pure

- Core code in `core/` is functionally pure and type-checked
- Core code assumes perfect input — no defensive code
- Core functions are total (enforced via **fuel** parameter)
- Evaluation strategy is **call-by-value**
- Purity is a **cognitive optimization**: referentially transparent functions are safe to treat as black boxes. An AI agent can trust composed results without re-verifying internals. Trust is transitive through pure code.

### The Lattice Is Transparent

- Lattice code must be **decomposable to Fold primitives** (cons, car, cdr, arithmetic, comparisons, eq?, pair?, null?)
- **Never replace lattice implementations with opaque Scheme built-ins** (e.g., don't replace a local `list-sort` with Chez's built-in) — even if the built-in is faster
- Rationale: fuel instrumentation, Rust codegen, and BSL containment all require the compiler to see through every operation
- Fuel semantics are **physics**, not optimization hints — they're the planned containment mechanism for Fold-native models. A Fold-native AI is constitutively bounded by fuel; it can reason about its own computational budget the way we reason about energy
- Boundary code is exempt: it's already outside the fuel model and can freely use Scheme built-ins
- See `fold-zxum` for the epic tracking Scheme primitive surface area reduction

### The Boundary Is Impure

- `boundary/` handles all IO
- Contains all defensive logic
- Validates before passing to Core
- Mints capability tokens

### Everything Is S-expressions

Assets, logs, knowledge base — all valid S-expressions. The system can introspect everything. Use `README.sexp` for directory documentation.

### Doc Forms

Use `(doc 'tag value)` for typed comments: `'type`, `'description`, `'todo`, `'deprecated`, etc. Authoritative for type inference. Use `/doc-forms` skill for full reference.

### Normalization and Content Addressing

S-expressions are normalized before hashing:

```scheme
(lambda (x) (+ x 1))
(lambda (y) (+ 1 (* 1 y))
;; These produce the SAME hash
```

### No Third-Party Dependencies

Everything is built in-house. Exceptions require approval from Andy. This is an **epistemological constraint**: third-party code is opaque to introspection. Lattice code decomposes to Fold primitives, giving AI agents total transparency when needed and free abstraction when not. Dependencies would break that unity.

### Naming Conventions

**Files:** Hyphenated lowercase (`block-navigator.ss`, `fuel-profile.ss`)
**Tests:** Prefixed with `test-` (`test-block.ss`, `test-eval.ss`)
**Documentation:** Use `README.sexp` for machine-readable directory docs

**Functions:**
- `type-operation` — Type-specific ops (`maybe-bind`, `matrix-multiply`)
- `make-type` — Constructors (`make-block`, `make-functor`)
- `type?` — Predicates (`block?`, `valid-hash?`)
- `type-field` — Accessors (`block-tag`, `functor-fmap`)

---

## Key Architectural Pipelines

### Optics → Autodiff → E-Graphs → Codegen

These subsystems compose into a differentiable programming pipeline:

1. **Optics** (`lattice/optics/`) specify *what* to access and differentiate. The full tower: Iso, Lens, Prism, Affine, Traversal, Fold, Getter, Setter.
2. **Traced optics** (`lattice/autodiff/traced-optics.ss`) integrate optics with autodiff. `lift-at-optic` traces only the optic's focus (the "pair of traced" pattern), enabling efficient gradient computation through optic-focused paths.
3. **Autodiff** (`core/autodiff/`) provides forward-mode (dual numbers), reverse-mode (tape-based), and second-order (hyperdual numbers). The `Differentiable` type class unifies these.
4. **E-graphs** (`lattice/egraph/`) enable equality saturation: the same computation can be extracted as different optimal forms depending on the cost model (`cuda-cost`, `cpu-cost`, `code-size-cost`). The same function yields different kernels for different hardware.
5. **CUDA codegen** (planned, `docs/cuda-codegen-design.md`) — traced computations normalize to S-expressions, hash to content addresses, and compile to CUDA kernels cached in the CAS. Optics compile to GPU memory access patterns (Lens→direct index, Traversal→parallel map, Fold→parallel reduction).

### Hardware Context

Development runs on **2x NVIDIA DGX Spark** (GB10 Grace Blackwell, ARM Cortex-X925/A725) linked over **200Gbps ConnectX-7 with NCCL**. 256GB unified memory total. Native FP8 tensor core support. vLLM serves local models for development and experimentation.

---

## Agent Operations

For agents operating within The Fold, see [`docs/agent-operating-manual.md`](docs/agent-operating-manual.md) — algorithmic procedures for:

- **Capability discovery** — Lattice search (`lf`, `li`, `le`) and boundary capability scanning
- **Trust/verification** — Capability audits, static analysis, mint-only-in-boundary rule
- **Fuel prediction** — Cost estimation, budgeting, parallel hazards
- **Boundary layer** — Request/response protocol, validation checklist
- **Provenance** — Normalization, hashing, storing with refs, pinning

---

## BBS

Issue tracker and changelog system. Use `/bbs` skill for full documentation. Quick: `(bbs-list)`, `(bbs-ready)`, `(bbs-create "Title")`.

---

## Troubleshooting

Use `/troubleshooting` skill for file locations, daemon issues, and common problems. Quick fix: `./fold --status` to check daemon, `./daemon.sh stop && ./fold` to restart.

---

## Critical Reminders

1. **Use `./fold` for REPL interaction** — It auto-starts the daemon and handles sessions
2. **Load from project root** — All `(load ...)` paths are relative to the project root
3. **Land the Plane** — A session is NOT complete until work is committed and pushed. See protocol below.
4. **Maintain The Fold** — Before you write a helper, ensure it doesn't already exist. Seek opportunities to simplify.

---

## Agent Notes

**MCP Tooling:** Prefer `fold_eval` via MCP over shelling out to `./fold` for REPL interactions. Requires `fold_login` first (use tier `opus`/`sonnet`/`haiku` based on your model). The LSP tools (`fold_lsp_*`) work well for code exploration—`fold_lsp_symbols` finds definitions, `fold_lsp_lookup` combines hover/definition/references. Use these instead of grep for finding Scheme symbols.

**BBS Cache Staleness:** After reloading modules with `(load ...)`, the BBS in-memory state may be stale. Run `(bbs-init!)` to refresh from disk if issues show incorrect status.

**Atomic Writes:** Use `boundary/io/atomic.ss` for durable file writes—it provides fdatasync when FFI is available. Don't roll custom atomic-write functions.

**Gemini QA Reviews:** Use `gemini -m gemini-3-pro-preview --include-directories <dir> "QA review..."` for complex code reviews. Flash model is cost-effective for bulk QA. Create BBS issues from findings (`bbs-create`), fix, then close (`bbs-close`).

---

## Land the Plane Protocol

**A session is not complete until code is committed and pushed to remote.**

```bash
# 1. Verify tests pass
scheme --script <relevant-test-file>.ss

# 2. Stage and commit
git status && git diff
git add <specific-files>
git commit -m "feat(module): Brief description

Co-Authored-By: Claude <noreply@anthropic.com>"

# 3. Push and verify
git push
git status  # Should show "up to date with 'origin/main'"
```

**If blocked:** Merge conflict → resolve locally. Push rejected → `git pull --rebase && git push`.
