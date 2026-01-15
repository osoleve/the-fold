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
./fold -s dev "define x 10"        # Named session with -s flag
./fold -s dev "x"                  # Retrieve value from session
./fold script.ss                   # Run script file
```

**Key features:**
- Implicit outer parentheses: `"+ 1 2"` becomes `(+ 1 2)` automatically
- Short session flag: `-s` instead of `--session`
- Auto-sessions: Omit `-s` for ephemeral sessions

Returns JSON output with status, result, output, and any errors.

### Session Cleanup

```bash
./fold -s dev "(bye)"              # Logout and clean up session files
./fold -s dev "(who)"              # Show current session info
```

Note: Single-token commands need explicit parens since they're procedure calls.

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
| `autodiff/` | Reverse-mode AD, computational graphs |
| `fp/` | Monads, parsers, streams, rewriting |
| `query/` | Query DSL, SQL parser, patterns |
| `dsl/` | Tagless final, chronicle, staging, template DSL |
| `info/` | Entropy, coding, information theory |
| `number-theory/` | Primes, modular arithmetic |
| `meta/` | Lattice navigation, search, introspection |

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

**FP Toolkit (`lattice/fp/`):**
- `control/` — Monads, effects, continuations, free monads
- `numeric/` — Transcendental functions
- `parsing/` — Parser combinators with memoization
- `meta/` — DSL utilities, logic programming
- `data/` — Lazy streams, zippers (list/tree/generic), zipper-lens integration
- `game/` — Game theory, Nash equilibrium
- `symbolic/` — Symbolic expressions
- `measure/` — Units of measure
- `control-systems/` — Control theory, state space models
- `analysis/` — Numerical analysis
- `rewrite/` — Term rewriting systems
- `pretty-instances.ss` — Pretty instances for Vec2, Matrix, Complex, Polynomial, Expr

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

**Meta-Tooling (`lattice/meta/`):**

Agent-facing navigation and introspection for the skill lattice. Builds a CAS-backed knowledge graph from manifests with BM25 search ranking. Uses persistent caching for fast initialization.

```scheme
;; Lattice meta-tooling is auto-initialized at startup with persistent caching.
;; First run builds cache (~10s); subsequent runs load from cache (~1s).

;; Search — BM25 ranked results
(lf "matrix decomposition")        ; Full-text search
(lfe 'vec3)                        ; Exact lookup (falls back to substring)
(lfp 'matrix)                      ; Prefix search (matrix*, matrix-*)
(lfs 'c2d)                         ; Substring search (finds c2d-zoh, etc.)
(lattice-complete "mat")           ; Autocomplete suggestions

;; Type-Aware Search (Hoogle-style)
(load "lattice/meta/type-search.ss")
(lf-type "Monad")                  ; Find functions with Monad in type signature
(lf-input "Matrix")                ; Functions that take Matrix as input
(lf-output "Maybe")                ; Functions that return Maybe

;; Cross-Reference Queries (function-level call graph)
(load "lattice/meta/xref.ss")
(build-xref-cache!)                ; Build call graph (~25k edges)
(lxu 'matrix-rows)                 ; What functions call this?
(lxc 'floyd-warshall)              ; What does this function call?
(xref-callers-transitive 'fn)      ; All transitive callers
(xref-most-called 10)              ; Most-called functions

;; DAG Navigation
(ld 'physics/diff)                 ; What does this skill depend on?
(lu 'linalg)                       ; What skills use this?
(lattice-path 'physics/diff 'linalg) ; Find dependency path
(lattice-roots)                    ; Tier 0 skills (no deps)
(lattice-leaves)                   ; Skills with no dependents
(lattice-hubs)                     ; Most-depended-on skills

;; Inspection
(li 'linalg)                       ; Full skill description
(le 'linalg)                       ; List all exports
(lm 'linalg)                       ; List modules with descriptions
(lattice-summary)                  ; One-line summary of all skills
(lattice-info 'linalg)             ; Structured data for programmatic use

;; Analytics
(ls)                               ; Lattice statistics
(lh)                               ; Health check (missing deps, cycles)
(lattice-coverage-pretty)          ; Metadata coverage report
(lattice-graph)                    ; Print full DAG structure

;; Manifest Auditing
(load "lattice/meta/audit.ss")
(audit-skill-pretty 'fp)           ; Find missing exports, phantom exports
(suggest-missing 'fp)              ; List exports to add to manifest
```

**Search Best Practices:**

- **Start broad, then narrow**: Use `(lf "concept")` first, then `(lfe 'symbol)` for exact matches
- **Use substring for partial names**: If you know part of a name (like `c2d`), use `(lfs 'c2d)` to find all matches
- **Try multiple query variations**: Function might be named differently than expected (e.g., `c2d-zoh` vs `ss-c2d`)
- **Check skill exports**: Use `(le 'skill-name)` to see what a skill actually exports
- **Not all functions are exported**: Use `(audit-skill 'name)` to find functions defined in source but missing from manifests

| Module | Purpose |
|----|----|
| `kg.ss` | Knowledge graph builder from manifests |
| `bm25.ss` | BM25 search engine with TF-IDF ranking |
| `search.ss` | Unified search API, autocomplete, prefix/substring |
| `type-search.ss` | Type-aware search (Hoogle-style lf-type, lf-input, lf-output) |
| `xref.ss` | Cross-reference queries (lxu, lxc, call graph analysis) |
| `dag.ss` | DAG traversal, paths, tiers, hubs |
| `analytics.ss` | Stats, health, coverage, purity |
| `inspect.ss` | Skill descriptions, exports, sources |
| `persist.ss` | Cache KG to disk for fast init |
| `audit.ss` | Find gaps between source and manifests |
| `meta.ss` | Unified entry point + `lattice-help` |

### Shell Subsystems

Shell is organized into functional subdirectories (with backwards-compatible stubs at root):

| Directory | Purpose | Key Modules |
|----|----|----|
| `repl/` | REPL & session management | repl-daemon.ss, session-manager.ss |
| `blocks/` | Block system tools | block-explorer.ss, block-navigator.ss |
| `debug/` | Developer inspection | debug-repl.ss (time-travel debugger) |
| `diagnostics/` | Profiling & analysis | fuel-viz.ss, profile-viewer.ss |
| `storage/` | Persistence & identity | store-manager.ss, cas-persist.ss |
| `io/` | Low-level IO utilities | fs.ss, json.ss |
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
| `tests/` | Shell test suite | test-*.ss files |

Root-level files like `commands.ss` and `validate.ss` remain for shared infrastructure.

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

This project uses **BBS** (Bulletin Board System), a CAS-native issue tracker built on The Fold's block primitives.

### Initialization

BBS is auto-initialized when the REPL starts. No manual setup required.

### Finding Work

```scheme
(bbs-list)                        ; List open issues (default)
(bbs-list 'status 'closed)        ; List closed issues
(bbs-list 'status 'all)           ; List all issues
(bbs-ready)                       ; Show unblocked work
(bbs-blocked)                     ; Show blocked issues
(bbs-show 'fold-001)              ; View issue details
(bbs-find "query")                ; Search issue titles
```

### Creating & Updating

```scheme
(bbs-create "Issue title")                              ; Basic create
(bbs-create "Title" 'priority 1 'type 'bug)            ; With options
(bbs-create "Title" 'description "Details..." 'labels '(core urgent))

(bbs-update 'fold-001 'status 'in_progress)            ; Update status
(bbs-update 'fold-001 'priority 0)                     ; Change priority
(bbs-close 'fold-001)                                  ; Close issue
(bbs-close 'fold-001 'reason "Done!")                  ; Close with reason
```

Priority: 0-4 (0=critical, 2=medium, 4=backlog).
Types: `'task`, `'bug`, `'feature`, `'epic`.
Status: `'open`, `'in_progress`, `'closed`.

**Note:** Issue IDs can be symbols (`'fold-001`) or strings (`"fold-001"`).

### Dependencies

```scheme
(bbs-dep 'fold-001 'fold-002)     ; fold-001 blocks fold-002
(bbs-blockers 'fold-002)          ; What blocks this issue?
(bbs-blocking 'fold-001)          ; What does this issue block?
(bbs-ready)                       ; All unblocked open issues
```

### History & Stats

```scheme
(bbs-history 'fold-001)           ; Show version history
(bbs-stats)                       ; Database statistics
```

### Pipeline Integration

BBS effects are available in agent pipelines (`lattice/pipeline/effects.ss`):

```scheme
(bbs-create "title")              ; Create issue, return ID
(bbs-create-full title desc type priority)
(bbs-update id updates-alist)     ; Update issue fields
(bbs-close id)                    ; Close issue
(bbs-ready)                       ; Get unblocked issues
(bbs-show id)                     ; Get issue details
```

### Session End

**Work is NOT complete until `git push` succeeds:**

```bash
git status              # Check changes
git add <files>         # Stage changes
git commit -m "..."     # Commit code
git push                # Push to remote
```

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
| `.bbs/` | BBS runtime data (counter, deps) |
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

This is non-negotiable. Uncommitted work is lost work. Follow this checklist before ending any session:

### Pre-Flight Checklist

```bash
# 1. Verify all tests pass
scheme --script <relevant-test-file>.ss

# 2. Check what's changed
git status

# 3. Review the diff
git diff
```

### Commit Sequence

```bash
# 4. Stage changes (be specific, not `git add .`)
git add <specific-files>

# 5. Commit with descriptive message
git commit -m "feat(module): Brief description

Longer explanation if needed.

Co-Authored-By: Claude <noreply@anthropic.com>"

# 6. Push to remote
git push
```

### Verification

```bash
# 7. Confirm push succeeded
git status
# Should show: "Your branch is up to date with 'origin/main'"
```

### If Something Goes Wrong

- **Merge conflict?** Resolve locally, then push
- **Push rejected?** Pull first: `git pull --rebase && git push`
- **Tests failing?** Fix before committing — don't push broken code

### Why This Matters

1. **Durability**: Local state can be lost; remote is backed up
2. **Collaboration**: Others can't use uncommitted work
3. **Auditability**: Git history documents what was done and why
4. **Recovery**: Easy to rollback if something breaks

**Remember: The plane hasn't landed until `git push` succeeds.**
