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

### Using fold-agent.py (Recommended)

```bash
./fold-agent.py "(+ 1 2)"                              # Evaluate expression
./fold-agent.py --session my-session "(define x 10)"   # With specific session
./fold-agent.py script.ss                              # Run script file
echo '{"code": "(+ 1 2)", "session": "my-session"}' | ./fold-agent.py --json  # JSON input
```

Returns JSON output with status, result, output, and any errors.

### Essential Commands

```scheme
(help)                           ; Show all commands
(blocks)                         ; CAS statistics
(explore-block hash)             ; Explore a block
(search "query")                 ; Search blocks
(commit! "message")              ; Git commit
(push!)                          ; Git push
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
|-----------|---------|
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
|-----------|---------|-------------|
| `base/` | Foundation (no deps) | prelude.ss, sha256.ss, error.ss |
| `blocks/` | Block system & CAS | block.ss, cas.ss, normalize.ss |
| `types/` | Type system | types.ss, dep-types.ss, infer.ss, kinds.ss |
| `lang/` | Evaluation & compilation | eval.ss, compile.ss, module.ss, nbe.ss |
| `util/` | Core utilities | debug.ss, pretty.ss, cost-model.ss |
| `testing/` | Test infrastructure | test-framework.ss |
| `benchmarks/` | Performance benchmarks | bench-core.ss, bench-prim.ss |

### Lattice (Skill DAG)

The lattice is a DAG of verified skills. "Stdlib" = tier 0 (foundational nodes).

**Tier 0 — Foundational (no lattice deps):**
| Directory | Purpose |
|-----------|---------|
| `linalg/` | Vectors, matrices, decomposition, solvers |
| `data/` | Data structures, graphs, collections |
| `algebra/` | Groups, rings, polynomial algebra, Gröbner bases |
| `random/` | PRNG, distributions |

**Tier 1 — Intermediate:**
| Directory | Purpose |
|-----------|---------|
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
|-----------|---------|
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
- `data/` — Lazy streams, persistent structures
- `game/` — Game theory, Nash equilibrium
- `symbolic/` — Symbolic expressions
- `measure/` — Units of measure
- `control-systems/` — Control theory, state space models
- `analysis/` — Numerical analysis
- `rewrite/` — Term rewriting systems

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
;; Initialize (required once per session — uses cache if manifests unchanged)
(load "lattice/meta/meta.ss")
(lattice-init!)                    ; Build KG + search indices (~2000 exports)

;; Search — BM25 ranked results
(lf "matrix decomposition")        ; Full-text search
(lfe 'vec3)                        ; Exact lookup (falls back to substring)
(lfp 'matrix)                      ; Prefix search (matrix*, matrix-*)
(lfs 'c2d)                         ; Substring search (finds c2d-zoh, etc.)
(lattice-complete "mat")           ; Autocomplete suggestions

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
|--------|---------|
| `kg.ss` | Knowledge graph builder from manifests |
| `bm25.ss` | BM25 search engine with TF-IDF ranking |
| `search.ss` | Unified search API, autocomplete, prefix/substring |
| `dag.ss` | DAG traversal, paths, tiers, hubs |
| `analytics.ss` | Stats, health, coverage, purity |
| `inspect.ss` | Skill descriptions, exports, sources |
| `persist.ss` | Cache KG to disk for fast init |
| `audit.ss` | Find gaps between source and manifests |
| `meta.ss` | Unified entry point + `lattice-help` |

### Shell Subsystems

Shell is organized into functional subdirectories (with backwards-compatible stubs at root):

| Directory | Purpose | Key Modules |
|-----------|---------|-------------|
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
| `tools/` | Utility tools | template-session.ss, template-parser.ss |
| `lsp/` | Language server protocol | lsp-server.ss, protocol.ss |
| `tests/` | Shell test suite | test-*.ss files |

Root-level files like `commands.ss` and `validate.ss` remain for shared infrastructure.

### Template DSL (AI Code Generation)

Grammar-driven code construction for building S-expressions without tracking parentheses.

**Batch Mode (Recommended):** Chain multiple complete definitions with `---`:

```scheme
(load "shell/tools/template-parser.ss")

;; Build quicksort with helper - all in one command
(tp-batch "
  define qs $params $body
  --- $params := lst
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

## Issue Tracking with Beads

This project uses **bd** (beads) for dependency-aware issue tracking.

### Finding Work

```bash
bd ready --limit 0                # Show unblocked work (no blockers)
bd list --status=open             # All open issues
bd list --status=in_progress      # Your active work
bd show <id>                      # View issue details with dependencies
bd search "query" --status open   # Full-text search with filters
```

### Creating & Updating

```bash
bd create --title="..." --type=task --priority=2   # New issue
bd q "Quick task title"           # Quick capture (returns ID only, for scripting)
bd update <id> --status=in_progress  # Claim work
bd close <id> --reason="..."      # Complete work
bd close <id1> <id2> ...          # Close multiple at once
```

Priority: 0-4 (0=critical, 2=medium, 4=backlog). NOT "high"/"medium"/"low".

### Dependencies & Structure

```bash
bd dep add <issue> <depends-on>   # Add dependency
bd dep tree <id>                  # Text tree view
bd graph <id>                     # ASCII DAG visualization
bd blocked                        # Show all blocked issues
```

### Planning for Parallelism

**Prioritize swarm-compatible plans.** When breaking down work:

1. **Identify independent tracks** — Tasks that don't share dependencies can run in parallel
2. **Minimize dependency chains** — Prefer wide DAGs over deep chains
3. **Create clear interfaces** — Define boundaries so parallel work doesn't conflict
4. **Use epics as coordination points** — Group related parallel work under a parent issue

A swarm-compatible plan enables multiple agents (or sessions) to work simultaneously, dramatically improving throughput.

### Parallel Work (Swarms)

For epics with multiple parallel tracks:

```bash
bd swarm validate <epic-id>       # Check DAG structure, parallelism
bd swarm create <epic-id>         # Create coordination molecule
bd swarm status <epic-id>         # Show completed/active/ready/blocked
```

### Hygiene & Search

```bash
bd stale                          # Issues with no recent updates
bd orphans                        # Issues in commits but still open
bd count --by-status              # Aggregate statistics
bd comments add <id> "note"       # Add comment without state change
bd defer <id>                     # Put on ice (not blocked, just postponed)
```

### Sync & Session End

**Work is NOT complete until `git push` succeeds:**

```bash
git status              # Check changes
git add <files>         # Stage changes
bd sync                 # Commit beads
git commit -m "..."     # Commit code
bd sync                 # Commit new beads
git push                # Push to remote
```

---

## File Locations

| Path | Purpose |
|------|---------|
| `/home/oso/the-fold` | Project root |
| `.fold-repl/ready` | Daemon ready file |
| `.fold-repl/requests/<session>.ss` | Session requests |
| `.fold-repl/responses/<session>.txt` | Session responses |
| `.fold-repl/daemon.log` | Daemon log |
| `.fold-repl/discord-outbox/` | Discord message outbox |
| `.fold-sessions/` | Persistent session state |
| `.fold-users/` | User profile data |
| `.store/` | Content-addressed store |
| `.beads/` | Issue tracking database |
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
3. **Push before ending** — Work is not complete until `git push` succeeds
4. **Maintain The Fold**
