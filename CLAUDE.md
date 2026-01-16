# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Identity

**The Fold** is a content-addressable homoiconic universe built on Chez Scheme. This server (`debian-8gb-ash-1`) is the first production deployment.

Repository: `git@github.com:osoleve/the-fold`

---

## Interacting with The Fold

### Using ./fold (Recommended)

```bash
./fold "+ 1 2"                     # Implicit parens: becomes (+ 1 2)
./fold "bye"                       # Single-token commands work: becomes (bye)
./fold -s dev "define x 10"        # Named session with -s flag
./fold -s dev "x"                  # Retrieve value from session
./fold --status                    # Check if daemon is running
```

**Key features:**
- **Auto-starts daemon** if not running (disable with `--no-auto-start`)
- Implicit outer parentheses: `"+ 1 2"` becomes `(+ 1 2)` automatically
- Single-token symbols auto-wrap: `"bye"` becomes `(bye)`
- Literals stay unwrapped: `"42"` stays `42`, `"'(a b)"` stays `'(a b)`
- Short session flag: `-s` instead of `--session`
- Colorized errors (disable with `NO_COLOR=1`)

Returns result on stdout, errors to stderr with exit codes: 0=success, 1=error, 2=timeout.

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

# Test subsets
scheme --script test-all.ss quick     # Skip slow tests
scheme --script test-all.ss core      # Core tests only
scheme --script test-all.ss shell     # Shell tests only

# Core tests
scheme --script core/run-tests.ss

# Individual lattice tests
scheme --script lattice/linalg/test-vec.ss
scheme --script lattice/info/test-entropy.ss
scheme --script lattice/physics/diff/test-rollout.ss
scheme --script lattice/meta/test-meta.ss
scheme --script lattice/fp/game/test-voting-games.ss
scheme --script lattice/fp/game/test-coop-games.ss
scheme --script lattice/fp/clp/test-clp.ss
scheme --script lattice/statistics/test-statistics.ss
scheme --script lattice/topology/homology-test.ss

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

**Note on Technical Report:** `docs/technical-report.md` is **generated** from chapter files in `docs/technical-report/`. Edit the chapter files (e.g., `00-abstract.md`, `06-the-module-system.md`), then run `scheme --script docs/technical-report/assemble.ss` to rebuild. Chapter order is defined in `docs/technical-report/manifest.sexp`.

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

| Class | Kind | Methods | Purpose |
|-------|------|---------|---------|
| `Eq` | `* → Constraint` | `==`, `/=` | Equality comparison |
| `Ord` | `* → Constraint` | `<`, `<=`, `>`, `>=`, `compare` | Ordering |
| `Show` | `* → Constraint` | `show : a → String` | String conversion |
| `Pretty` | `* → Constraint` | `pretty : a → Doc`, `pretty-prec : Int → a → Doc` | Width-aware pretty-printing |
| `Functor` | `(* → *) → Constraint` | `fmap` | Mappable containers |
| `Monad` | `(* → *) → Constraint` | `>>=`, `return` | Sequencing with context |

### Lattice (Skill DAG)

The lattice is a DAG of verified skills. "Stdlib" = tier 0 (foundational nodes).

**Tier 0 — Foundational (no lattice deps):**
| Directory | Purpose |
|----|----|
| `linalg/` | Vectors, matrices, decomposition, solvers |
| `data/` | Data structures, graphs, collections, community detection |
| `algebra/` | Groups, rings, polynomial algebra, Gröbner bases |
| `random/` | PRNG, distributions |

**Tier 1 — Intermediate:**
| Directory | Purpose |
|----|----|
| `numeric/` | Complex numbers, DFT, signal processing |
| `geometry/` | Shapes, transforms, raymarching, SDFs, mesh topology |
| `diffgeo/` | Charts, atlases, Lie groups, Riemannian curvature |
| `autodiff/` | Reverse-mode AD, computational graphs |
| `fp/` | Monads, parsers, streams, protocols, game theory, control systems |
| `fp/clp/` | Constraint logic programming (cKanren-style CLP(FD)) |
| `query/` | Query DSL, SQL parser, patterns |
| `dsl/` | Tagless final, chronicle, staging, template DSL |
| `info/` | Entropy, coding, information theory |
| `number-theory/` | Primes, modular arithmetic |
| `meta/` | Lattice navigation, search, introspection |
| `topology/` | Simplicial complexes, homology, Betti numbers |
| `crypto/` | SHA-512, BLAKE2b, HMAC |
| `optimization/` | LP, ILP, gradient descent, Newton, L-BFGS, interval global |
| `statistics/` | Regression, GLM, time series, hypothesis testing |
| `physics/lenses/` | Optics for physics state access |

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

**Key Lattice Subsystems:**

*FP Toolkit (`lattice/fp/`):* Monads, parsers, streams, zippers, game theory (cooperative games, matching theory, Nash equilibrium, voting theory with power indices, fair division), symbolic math, control systems (state-space, Kalman filters, PID, stability analysis), term rewriting. Use `(li 'fp)` and `(le 'fp)` for details.

*Statistics (`lattice/statistics/`):* Linear/GLM regression (IRLS), regularization (ridge, lasso, elastic net), time series (AR, MA, exponential smoothing), hypothesis testing (t-test, F-test, ANOVA, chi-squared). Use `(li 'statistics)` for details.

*CLP(FD) (`lattice/fp/clp/`):* cKanren-style constraint logic programming with finite domains, arithmetic constraints, global constraints (all-different), and intelligent search strategies. Classic problems: N-Queens, Sudoku, cryptarithmetic.

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

Shell is organized into functional subdirectories:

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
| `tools/` | Utility & refactoring tools | refactor-toolkit.ss, template-*.ss |
| `lsp/` | Language server protocol | lsp-server.ss, protocol.ss |
| `web/` | Web tools | fold-tui (Rust CAS terminal explorer) |
| `tests/` | Shell test suite | test-*.ss files |

### Open Protocol System

Extensible type dispatch for the Open/Closed Principle (`lattice/fp/protocol.ss`):

```scheme
(load "lattice/fp/protocol.ss")

;; Define a protocol (generic operation)
(define-protocol (draw obj ctx) "Draw object to context")

;; Register implementations per type tag
(implement-protocol! 'draw 'circle
  (lambda (c ctx) (draw-circle (circle-center c) ctx)))
(implement-protocol! 'draw 'rectangle
  (lambda (r ctx) (draw-rect (rect-pos r) ctx)))

;; Use (automatic dispatch via type tag)
(draw my-circle canvas)  ; calls circle implementation
```

Objects must be tagged lists: `(list 'type-tag ...)`. Dispatch is O(1) via hashtable.

### Refactoring Toolkit

Unified interface for codebase refactoring operations (`shell/tools/refactor-toolkit.ss`):

```scheme
(load "shell/tools/refactor-toolkit.ss")

;; Help and discovery
(refactor 'help)                           ; Show all operations

;; Rename symbols globally
(refactor 'rename 'old-name 'new-name)     ; Preview rename
(refactor 'apply)                          ; Apply staged changes

;; Move symbols between modules
(refactor 'move 'symbol "target-file.ss")  ; Preview move
(refactor-move-apply!)                     ; Apply staged move

;; Dead code analysis
(refactor 'dead-code)                      ; Scan entire codebase
(refactor 'dead-code "lattice/fp")         ; Scan specific path

;; Dependency analysis
(refactor 'deps 'symbol)                   ; Show callers/callees

;; Change management
(refactor 'status)                         ; Show pending changes
(refactor 'undo)                           ; Undo last operation
(refactor 'clear)                          ; Discard pending changes
```

**Quick aliases:** `rr` (rename), `rm` (move), `rd` (deps), `rdc` (dead-code)

### Block Explorer TUI

Terminal UI for exploring the content-addressed store:

```bash
# Build
cd shell/web/fold-explorer
cargo build --release

# Interactive TUI mode
./target/release/fold-tui [store-path]

# Non-interactive store verification
./target/release/fold-tui .store --check
```

**Key bindings:** `j/k` navigate, `Enter` view/follow, `b/Esc` back, `/` search, `t` tag filter, `o` orphans, `p` popular, `h` heads, `r` reset, `q` quit.

### Template DSL (AI Code Generation)

Grammar-driven code construction for building S-expressions without tracking parentheses.

**Batch Mode (Recommended):**

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
```

**Key concepts:**
- Holes (`$name`) are non-terminals that get filled incrementally
- Multi-token statements get implicit parentheses (no outer `()` needed)
- Batch mode: `tp-batch` chains definitions/fills separated by `---`

**Files:** `lattice/dsl/template/template.ss` (core), `shell/tools/template-session.ss` (session), `shell/tools/template-parser.ss` (parser)

---

## Core Principles

### The Core Is Pure

- Core code in `core/` is functionally pure and type-checked
- Core code assumes perfect input — no defensive code
- Core functions are total (enforced via **fuel** parameter)
- Evaluation strategy is **call-by-value**

### The Shell Is Fallen

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

## BBS: Issues and Posts

Use `/bbs` skill for full documentation. The BBS supports both issues (for tracking work) and posts (for changelogs, notes, announcements).

### Issues

```scheme
(bbs-list)              ; List open issues
(bbs-ready)             ; Show unblocked work
(bbs-show 'fold-001)    ; View issue details
(bbs-create "Title")    ; Create issue
(bbs-update 'id 'status 'in_progress)
(bbs-close 'id)         ; Close issue
```

Priority: 0-4 (0=critical, 4=backlog). Types: `task`, `bug`, `feature`, `epic`.

### Posts

```scheme
(post-create "Title" "Body..." 'changelog)  ; Create post
(post-show 'post-1)                         ; View post
(post-list)                                 ; List all posts
(post-list 'type 'changelog)                ; Filter by type
(post-update 'post-1 'body "New content")   ; Edit post
```

Post types: `changelog`, `note`, `announcement`, `session-summary`.

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
| `.store/heads/bbs/fold-*.head` | BBS issue heads (current hash per issue) |
| `.store/heads/bbs/post-*.head` | BBS post heads (current hash per post) |
| `.bbs/` | BBS runtime data (counters, deps, index cache) |
| `archives/` | Historical exports (e.g., forum archive) |
| `TAXONOMY.sexp` | Machine-readable project taxonomy |

---

## Troubleshooting

**Check daemon status:**
```bash
./fold --status      # Quick check
```

**Daemon won't start:**
```bash
./daemon.sh stop     # Clear stale state
./daemon.sh cleanup  # Kill orphan workers
./daemon.sh start    # Or just run ./fold - it auto-starts
```

**Session state corruption:**
```bash
rm -rf .fold-repl/   # Nuclear option
./fold "(help)"      # Auto-starts fresh daemon
```

**Tests hanging:** Check fuel consumption. Infinite loops exhaust fuel and return `out-of-fuel` error.

---

## Critical Reminders

1. **Use `./fold` for REPL interaction** — It auto-starts the daemon and handles sessions
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
