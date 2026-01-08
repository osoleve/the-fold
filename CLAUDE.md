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

# Single test file (pattern: test-<module>.ss adjacent to module)
scheme --script core/test-block.ss
scheme --script core/info-theory/test-entropy.ss
scheme --script shell/tests/test-string-utils.ss
```

Test framework: `core/test-framework.ss` provides unified API across all tests.

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
| `core/` | Pure, typed, load-bearing code |
| `shell/` | IO layer, defensive code, impurity |
| `user/` | Build and play area |
| `agents/` | Multi-agent ecosystem |
| `ops/` | Operational deployment (systemd, scripts) |
| `docs/` | Documentation and policy |
| `archives/` | Historical exports |

### Core Subsystems

Core is organized into domain-driven subdirectories:

| Directory | Purpose | Key Modules |
|-----------|---------|-------------|
| `base/` | Foundation (no deps) | prelude.ss, sha256.ss, error.ss |
| `blocks/` | Block system & CAS | block.ss, cas.ss, normalize.ss |
| `types/` | Type system | types.ss, dep-types.ss, infer.ss, kinds.ss |
| `lang/` | Evaluation & compilation | eval.ss, compile.ss, module.ss, nbe.ss |
| `linalg/` | Linear algebra (331 tests) | vec.ss, matrix.ss, matrix-decomp.ss, matrix-solvers.ss |
| `numeric/` | Numerical computing | complex.ss (56 tests), dft.ss (46 tests) |
| `autodiff/` | Automatic differentiation | comp-graph.ss, reverse-diff.ss |
| `data/` | Data structures | data-structures.ss, graph-algorithms.ss |
| `query/` | Query DSL & patterns | query.ss, query-dsl.ss, aho-corasick.ss |
| `util/` | General utilities | debug.ss (time-travel debugger), pretty.ss, help.ss |
| `info-theory/` | Information theory (57 tests) | entropy.ss |
| `random/` | Probability | prng.ss, distributions.ss |
| `pipeline/` | Agent workflows | stage.ss, effects.ss, council.ss |

**FP Toolkit (`core/fp/`):**
- `control/` — Monads, effects, continuations, free monads
- `numeric/` — Transcendental functions (59 tests)
- `parsing/` — Parser combinators with memoization
- `meta/` — DSL utilities, logic programming
- `data/` — Lazy streams, persistent structures
- `game/` — Game theory, Nash equilibrium (26 tests)
- `symbolic/` — Symbolic expressions (55 tests)
- `measure/` — Units of measure (47 tests)
- `control-systems/` — Control theory, state space models (21 tests)

### Shell Subsystems

- `repl-daemon.ss` — Multi-session REPL daemon
- `commands.ss` — Extensible command system
- `debug-repl.ss` — Time-travel debugger REPL (step, undo, redo, watch, explain)
- `string-utils.ss` — String utilities (86 tests)
- `validate.ss` — Input validation
- `git.ss` — Git operations

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

### Using bv for Triage

**Use ONLY `--robot-*` flags — bare `bv` launches interactive TUI.**

```bash
bv --robot-triage        # Main entry point: recommendations, quick wins
bv --robot-next          # Single top pick
bv --robot-plan          # Parallel execution tracks
bv --robot-insights      # Full graph metrics
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
| `.store/` | Content-addressed store |
| `.beads/` | Issue tracking database |
| `archives/` | Historical exports (e.g., forum archive) |

---

## Critical Reminders

1. **Always use the daemon** — State doesn't persist between Bash calls otherwise
2. **Load from project root** — All `(load ...)` paths are relative to `/home/oso/the-fold`
3. **Push before ending** — Work is not complete until `git push` succeeds
