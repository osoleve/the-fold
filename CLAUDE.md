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
./daemon.sh start    # Start persistent REPL
./daemon.sh status   # Verify running
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

# Shell tests
scheme --script shell/tests/test-string-utils.ss
```

Test framework: `core/testing/test-framework.ss` provides unified API across all tests.
Tests are co-located with their modules (e.g., `test-vec.ss` next to `vec.ss`).

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
| `algebra/` | Groups, rings, fields |
| `random/` | PRNG, distributions |

**Tier 1 — Intermediate:**
| Directory | Purpose |
|-----------|---------|
| `numeric/` | Complex numbers, DFT, signal processing |
| `geometry/` | Shapes, transforms, raymarching, SDFs |
| `autodiff/` | Reverse-mode AD, computational graphs |
| `fp/` | Monads, parsers, streams, rewriting |
| `query/` | Query DSL, SQL parser, patterns |
| `dsl/` | Tagless final, chronicle, staging |
| `info/` | Entropy, coding, information theory |
| `number-theory/` | Primes, modular arithmetic |

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

Each lattice skill has a `manifest.sexp` declaring version, purity, fuel-bound, and dependencies.

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
| `tools/` | Utility tools | Various shell utilities |
| `lsp/` | Language server protocol | lsp-server.ss, protocol.ss |
| `tests/` | Shell test suite | test-*.ss files |

Root-level files like `commands.ss` and `validate.ss` remain for shared infrastructure.

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

---

## Issue Tracking with Beads

This project uses **bd** (beads) for dependency-aware issue tracking.

### Finding Work

```bash
bd ready                          # Show unblocked work (no blockers)
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

## Critical Reminders

1. **Always use the daemon** — State doesn't persist between Bash calls otherwise
2. **Load from project root** — All `(load ...)` paths are relative to `/home/oso/the-fold`
3. **Push before ending** — Work is not complete until `git push` succeeds
