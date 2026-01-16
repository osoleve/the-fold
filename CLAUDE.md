# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Identity

**The Fold** is a content-addressable homoiconic universe built on Chez Scheme. This server (`debian-8gb-ash-1`) is the first production deployment.

Repository: `git@github.com:osoleve/the-fold`

---

## First Step: Start the REPL Daemon

**Before doing ANYTHING else:**

```bash
./daemon.sh status   # Verify running
./daemon.sh start    # Start persistent REPL if needed
```

The daemon is **essential** — state is lost between Bash invocations without it.

---

## Interacting with The Fold

### Using ./fold (Recommended)

```bash
./fold "+ 1 2"                     # Implicit parens: becomes (+ 1 2)
./fold "bye"                       # Single-token commands work: becomes (bye)
./fold -s dev "define x 10"        # Named session with -s flag
./fold -s dev "x"                  # Retrieve value from session
./fold script.ss                   # Run script file
```

**Key features:**
- Implicit outer parentheses: `"+ 1 2"` becomes `(+ 1 2)` automatically
- Single-token symbols auto-wrap: `"bye"` becomes `(bye)`
- Literals stay unwrapped: `"42"` stays `42`, `"'(a b)"` stays `'(a b)`
- Short session flag: `-s` instead of `--session`
- Auto-sessions: Omit `-s` for ephemeral sessions

Returns JSON output with status, result, output, and any errors.

### Session Cleanup

```bash
./fold -s dev "bye"                # Logout and clean up session files
./fold -s dev "who"                # Show current session info
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

The time-travel debugger lives in `shell/debug/debug-repl.ss`:

```scheme
(load "shell/debug/debug-repl.ss")
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

# Core tests only
scheme --script core/run-tests.ss

# Lattice tests
scheme --script lattice/linalg/test-vec.ss
scheme --script lattice/info/test-entropy.ss
scheme --script lattice/physics/diff/test-rollout.ss
scheme --script lattice/meta/test-meta.ss
scheme --script lattice/fp/game/test-voting-games.ss
scheme --script lattice/fp/game/test-coop-games.ss

# Shell tests
scheme --script shell/tests/test-string-utils.ss
```

Test framework: `core/testing/test-framework.ss` provides unified API across all tests.
Tests are co-located with their modules (e.g., `test-vec.ss` next to `vec.ss`).

### Writing Tests

```scheme
(load "core/testing/test-framework.ss")

(define-test "descriptive name"
  (assert-equal expected actual)
  (assert-true expr)
  (assert-false expr)
  (assert-error expr)            ; Verify expr throws
  (assert-ok expr))              ; Verify (ok ...) result

(test-group "group name"
  (define-test "test 1" ...)
  (define-test "test 2" ...))

(run-all-tests)                  ; Run all registered tests
(run-tests 'group-name)          ; Run specific group
```

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
| `shell/` | Impure boundary — IO, validation, capabilities |
| `user/` | Playground — experiments and demos |
| `agents/` | Multi-agent ecosystem |
| `ops/` | Operational deployment (systemd, scripts) |
| `docs/` | Documentation and policy |
| `archives/` | Historical exports |

### Three-Layer Architecture

```
┌─────────────────────────────────────┐
│              user/                  │  Applications, experiments
├─────────────────────────────────────┤
│              shell/                 │  ALL impure code lives here
├─────────────────────────────────────┤
│              lattice/               │  Verified skill DAG (pure)
├─────────────────────────────────────┤
│              core/                  │  Language kernel (pure)
└─────────────────────────────────────┘
```

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

The type class system provides ad-hoc polymorphism. Key classes:

| Class | Kind | Methods | Purpose |
|-------|------|---------|---------|
| `Eq` | `* → Constraint` | `==`, `/=` | Equality comparison |
| `Ord` | `* → Constraint` | `<`, `<=`, `>`, `>=`, `compare` | Ordering |
| `Show` | `* → Constraint` | `show : a → String` | String conversion |
| `Pretty` | `* → Constraint` | `pretty : a → Doc`, `pretty-prec : Int → a → Doc` | Width-aware pretty-printing |
| `Functor` | `(* → *) → Constraint` | `fmap` | Mappable containers |
| `Monad` | `(* → *) → Constraint` | `>>=`, `return` | Sequencing with context |

**Pretty Type Class** (`core/util/pretty-class.ss`):

Unlike `Show` (returns flat `String`), `Pretty` returns `Doc` for composable, width-aware layout using the Wadler-Lindig algorithm.

```scheme
(load "core/util/pretty-class.ss")

;; Render Doc to string
(pretty-render (nat-pretty 42))           ; => "42"
(pretty-render-width 40 doc)              ; Custom width

;; Precedence-aware expression printing
(parens-if (> outer-prec inner-prec) doc) ; Add parens when needed
(pretty-binop prec op-prec left "+" right) ; Binary operators

;; Precedence levels (higher = tighter binding)
prec-atom  ; 10 - atoms (tightest)
prec-mul   ; 7  - multiplication
prec-add   ; 6  - addition
prec-top   ; 0  - top level (loosest)
```

Lattice extensions (`lattice/fp/pretty-instances.ss`):

```scheme
(load "lattice/fp/pretty-instances.ss")

(pretty-render (vec2-pretty '(vec2 3 4)))       ; => "[3, 4]"
(pretty-render (complex-pretty '(complex 3 4))) ; => "3 + 4i"
(pretty-render (expr-pretty '(+ (num 2) (* (num 3) (var x)))))
                                                ; => "2 + 3 * x"
```

### Lattice (Skill DAG)

The lattice is a DAG of verified skills. "Stdlib" = tier 0 (foundational nodes).

**Tier 0 — Foundational (no lattice deps):**
| Directory | Purpose |
|----|----|
| `linalg/` | Vectors, matrices, decomposition, solvers |
| `data/` | Data structures, graphs, collections |
| `algebra/` | Groups, rings, polynomial algebra, Gröbner bases |
| `random/` | PRNG, distributions |

**Tier 1 — Intermediate:**
| Directory | Purpose |
|----|----|
| `numeric/` | Complex numbers, DFT, signal processing |
| `geometry/` | Shapes, transforms, raymarching, SDFs |
| `diffgeo/` | Coordinate charts, atlases, Jacobians, manifold foundations |
| `autodiff/` | Reverse-mode AD, computational graphs |
| `fp/` | Monads, parsers, streams, rewriting |
| `query/` | Query DSL, SQL parser, patterns |
| `dsl/` | Tagless final, chronicle, staging, template DSL |
| `info/` | Entropy, coding, information theory |
| `number-theory/` | Primes, modular arithmetic |
| `meta/` | Lattice navigation, search, introspection |
| `topology/` | Simplicial complexes, boundary operators, TDA |
| `crypto/` | SHA-512, BLAKE2b, HMAC |
| `optimization/` | LP, ILP, gradient descent, Newton, L-BFGS |

**Tier 2+ — Advanced:**
| Directory | Purpose |
|----|----|
| `physics/diff/` | Differentiable 2D physics |
| `physics/diff3d/` | Differentiable 3D physics |
| `physics/classical/` | Classical 2D physics |
| `physics/classical3d/` | Classical 3D physics |
| `tiles/` | Board game SDK (hex, square, triangle) |
| `sim/` | Simulation, dynamics |
| `automata/` | State machines, DFA/NFA |
| `pipeline/` | Agent workflows, council |

**FP Toolkit (`lattice/fp/`):** Monads, parsers, streams, zippers, game theory (cooperative games, matching theory, Nash equilibrium, voting theory with power indices, fair division with cake cutting and adjusted winner), symbolic math, control systems, rewriting. Use `(li 'fp)` and `(le 'fp)` for details.

Each lattice skill has a `manifest.sexp` declaring metadata:

```scheme
(skill <name>
  (version "x.y.z")
  (tier 0-2)                       ; 0=foundational, 1=intermediate, 2+=advanced
  (path "lattice/<name>")
  (purity total|partial)           ; total=pure, partial=may have effects
  (stability stable|experimental)
  (fuel-bound "O(...)")            ; Complexity bound
  (deps (<skill> ...))             ; Skill-level dependencies
  (description "...")
  (keywords (<keyword> ...))       ; For search
  (aliases (<alias> ...))          ; Alternative names
  (exports (<module> <symbol> ...) ...)
  (modules (<name> "<file>" "<desc>") ...))
```

**Module System (`core/lang/module.ss`):**

Load modules with dependency resolution:

```scheme
(require 'charts)              ; Simple (first-match-wins)
(require 'diffgeo/charts)      ; Namespaced (unambiguous)
(require 'algebra/polynomial)  ; Avoids collision with numeric/polynomial

(modules)                      ; List all modules
(module-info 'charts)          ; Show module details
(module-collisions)            ; List name collisions
```

Use namespaced form (`'dir/module`) when module names collide. The system warns on collision during simple require.

**Meta-Tooling (`lattice/meta/`):**

Use `/lattice-search` skill for full documentation. Quick reference:

```scheme
;; Search
(lf "query")        ; Full-text search (BM25)
(lfe 'symbol)       ; Exact lookup

;; Inspection
(li 'skill)         ; Skill description
(le 'skill)         ; List exports
(ld 'skill)         ; Dependencies
(lu 'skill)         ; Dependents

;; Testing
(lt 'skill)         ; List test files for skill
(ltr 'skill)        ; Run tests for skill
(lattice-tests-summary)  ; Test coverage overview

;; Validation
(lc 'skill)         ; Cycle check (validate deps)
(lattice-would-cycle? 'from 'to)  ; Proactive cycle detection
```

### Shell Subsystems

Shell is organized into functional subdirectories (with backwards-compatible stubs at root):

| Directory | Purpose | Key Modules |
|----|----|----|
| `repl/` | REPL & session management | repl-daemon.ss, session-manager.ss |
| `blocks/` | Block system tools | block-explorer.ss, block-navigator.ss |
| `debug/` | Developer inspection | debug-repl.ss (time-travel debugger) |
| `diagnostics/` | Profiling & analysis | fuel-viz.ss, profile-viewer.ss |
| `storage/` | Persistence & identity | store-manager.ss, cas-persist.ss |
| `io/` | Low-level IO utilities | fs.ss, json.ss, process.ss |
| `git/` | Git operations | git.ss, git-workflow.ss |
| `assistants/` | AI agents | duckie-*.ss |
| `media/` | Creative tools | music-gen.ss, create-art.ss |
| `ui/` | Graphics & display | graphics.ss, color.ss, layers.ss |
| `discord/` | Discord bot integration | bot.js, bridge.js |
| `mcp-server/` | MCP server integration | External tool access |
| `lens/` | Optics & lenses | capability-lens.ss |
| `introspect/` | System introspection | type-inspect.ss, xref.ss |
| `pipeline/` | Agent pipelines | workflow integration |
| `bbs/` | Issue tracker | bbs.ss, ops.ss, index.ss |
| `tools/` | Utility tools | template-session.ss, template-parser.ss |
| `lsp/` | Language server protocol | lsp-server.ss, protocol.ss |
| `web/` | Web servers | fold-explorer (Rust CAS visualizer) |
| `tests/` | Shell test suite | test-*.ss files |

Root-level files like `commands.ss` and `validate.ss` remain for shared infrastructure.

### Block Explorer Web UI

Visual block explorer for the content-addressed store:

```bash
# Build and run
cd shell/web/fold-explorer
cargo build --release
./target/release/fold-explorer [store-path] [static-dir] [port]

# Default: serves .store/ on http://localhost:8080
./target/release/fold-explorer
```

**Features:**
- Block browser with pagination, tag filtering, search
- Graph visualization (Canvas-based, D3.js-compatible JSON)
- Subgraph exploration with configurable depth
- Heads listing, orphan/popular block analysis

**API Endpoints:**
| Endpoint | Description |
|----------|-------------|
| `GET /api/blocks?limit=&offset=&tag=` | List blocks (paginated) |
| `GET /api/blocks/{hash}` | Block details + payload preview |
| `GET /api/blocks/{hash}/refs` | Block references |
| `GET /api/graph/stats` | Store statistics |
| `GET /api/graph/subgraph/{hash}?depth=` | D3.js-format subgraph |
| `GET /api/graph/orphans` | Unreferenced blocks |
| `GET /api/graph/popular` | Most-referenced blocks |
| `GET /api/heads` | Named references |
| `GET /api/search?q=` | Full-text search |

**Security:** Payloads served as `application/octet-stream` with `X-Content-Type-Options: nosniff` to prevent XSS.

### Template DSL (AI Code Generation)

Grammar-driven code construction for building S-expressions without tracking parentheses.

**Batch Mode (Recommended):** Chain template + fills with `---`:

```scheme
(load "shell/tools/template-parser.ss")

;; Build quicksort - template with holes, then fill them
(tp-batch "
  define (qs lst) $body
  --- $body := if $cond $then $else
  --- $cond := null? lst
  --- $then := '()
  --- $else := append (qs (filter $pred (cdr lst))) (cons (car lst) (qs (filter $pred2 (cdr lst))))
  --- $pred := lambda (x) (< x (car lst))
  --- $pred2 := lambda (x) (>= x (car lst))
")
;; → (define (qs lst)
;;     (if (null? lst)
;;         '()
;;         (append (qs (filter (lambda (x) (< x (car lst))) (cdr lst)))
;;                 (cons (car lst)
;;                       (qs (filter (lambda (x) (>= x (car lst))) (cdr lst)))))))
```

**Interactive Mode:** Build incrementally with hole propagation:

```scheme
(tp-parse "define $sig $body")           ; Start template
(tp-parse "$sig := factorial n")         ; Fill hole (implicit parens)
(tp-parse "$body := if $cond $then $else")
(tp-parse "$cond := = n 0")              ; Implicit parens: (= n 0)
(tp-parse "$then := 1")
(tp-parse "$else := * n (factorial (- n 1))")
(ts-compile)
;; → (define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))
```

**Key concepts:**
- Holes (`$name`) are non-terminals that get filled incrementally
- Multi-token statements get implicit parentheses (no outer `()` needed)
- Batch mode: `tp-batch` chains definitions/fills separated by `---`
- Filling a hole with a value containing holes propagates those holes
- Session manager provides undo support

**Files:** `lattice/dsl/template/template.ss` (core), `shell/tools/template-session.ss` (session), `shell/tools/template-parser.ss` (parser)

---

## Core Principles

### The Core Is Pure

- Core code in `core/` is functionally pure and type-checked
- Core code assumes perfect input — no defensive code
- Core functions are total (enforced via **fuel** parameter)
- Evaluation strategy is **call-by-value**

### The Shell (Thimble) Is Fallen

- `shell/` handles all IO
- Contains all defensive logic
- Validates before passing to Core
- Mints capabilities from Outside

### Everything Is S-expressions

Assets, logs, knowledge base — all valid S-expressions. The system can introspect everything. Use `README.sexp` for directory documentation.

### Normalization and Content Addressing

S-expressions are α-normalized (de Bruijn indices) before hashing:

```scheme
(lambda (x) (+ x 1))
(lambda (y) (+ y 1))
;; These produce the SAME hash
```

### No Third-Party Dependencies

Everything is built in-house. Exceptions require approval from Andy.

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

## Agent Operations

For agents operating within The Fold, see [`docs/agent-operating-manual.md`](docs/agent-operating-manual.md) — algorithmic procedures for:

- **Capability discovery** — Lattice search (`lf`, `li`, `le`) and shell capability scanning
- **Trust/verification** — Capability audits, static analysis, mint-only-in-shell rule
- **Fuel prediction** — Cost estimation, budgeting, parallel hazards
- **Shell boundary** — Request/response protocol, validation checklist
- **Provenance** — Normalization, hashing, storing with refs, pinning

---

## Issue Tracking with BBS

Use `/bbs` skill for full documentation. Quick reference:

```scheme
(bbs-list)              ; List open issues
(bbs-ready)             ; Show unblocked work
(bbs-show 'fold-001)    ; View issue details
(bbs-create "Title")    ; Create issue
(bbs-update 'id 'status 'in_progress)
(bbs-close 'id)         ; Close issue
```

Priority: 0-4 (0=critical, 4=backlog). Types: `task`, `bug`, `feature`, `epic`.

---

## File Locations

| Path | Purpose |
|----|----|
| `/home/oso/the-fold` | Project root |
| `.fold-repl/ready` | Daemon ready file |
| `.fold-repl/requests/<session>.ss` | Session requests |
| `.fold-repl/responses/<session>.txt` | Session responses |
| `.fold-repl/daemon.log` | Daemon log |
| `.fold-repl/discord-outbox/` | Discord message outbox |
| `.fold-sessions/` | Persistent session state |
| `.fold-users/` | User profile data |
| `.store/` | Content-addressed store |
| `.store/heads/bbs/` | BBS issue heads (current hash per issue) |
| `.bbs/` | BBS runtime data (counter, deps, index cache) |
| `archives/` | Historical exports (e.g., forum archive) |
| `TAXONOMY.sexp` | Machine-readable project taxonomy |

---

## Troubleshooting

**Daemon won't start:**
```bash
./daemon.sh stop     # Clear stale state
./daemon.sh cleanup  # Kill orphan workers
./daemon.sh start
```

**Session state corruption:**
```bash
rm -rf .fold-repl/   # Nuclear option
./daemon.sh start
```

**Tests hanging:** Check fuel consumption. Infinite loops exhaust fuel and return `out-of-fuel` error.

---

## Critical Reminders

1. **Always use the daemon** — State doesn't persist between Bash calls otherwise
2. **Load from project root** — All `(load ...)` paths are relative to `/home/oso/the-fold`
3. **Land the Plane** — A session is NOT complete until work is committed and pushed
4. **Maintain The Fold**

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
