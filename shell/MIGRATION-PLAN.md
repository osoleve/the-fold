# Shell Directory Reorganization Migration Plan

**Epic:** fold-zbed
**Plan Reference:** `/home/oso/.claude/plans/silly-bubbling-starlight.md`
**Status:** Prepared, awaiting execution

## Overview

Reorganize 70 shell root files into domain-based subdirectories per TAXONOMY.sexp.

## Proposed Directory Structure

```
shell/
├── assistants/     # AI assistants (duckie-*.ss)
├── blocks/         # Block navigation/query
├── debug/          # Debug tools
├── diagnostics/    # Profiling and analysis
├── io/             # File system and data formats
├── media/          # Creative tools
├── repl/           # REPL infrastructure
├── storage/        # CAS and store APIs
│
├── benchmarks/     # (existing)
├── discord/        # (existing)
├── examples/       # (existing)
├── git/            # (existing)
├── introspect/     # (existing)
├── lens/           # (existing)
├── lsp/            # (existing)
├── mcp-server/     # (existing)
├── pipeline/       # (existing)
├── tests/          # (existing)
├── tools/          # (existing)
├── ui/             # (existing)
│
├── commands.ss     # (keep at root)
├── history.ss      # (keep at root)
├── tutorial.ss     # (keep at root)
├── toolkit.ss      # (keep at root)
└── ...             # Other root files TBD
```

## Dependency Analysis

### Tier 0: Most Depended (move first with stubs)

| File | Dependents | Move To |
|----|----|----|
| `fs.ss` | 15 files (core/, user/, shell/) | `io/` |
| `store-api.ss` | 8 files | `storage/` |
| `repl.ss` | 12 files | `repl/` |

### Tier 1: Internal Dependencies

| File | Depends On | Move To |
|----|----|----|
| `store-api.ss` | fs.ss | `storage/` |
| `identity.ss` | cas-persist.ss | `storage/` |
| `profiler-unified.ss` | alloc-tracker.ss, profile-call-graph.ss | `diagnostics/` |
| `profile-persist.ss` | profiler-unified.ss | `diagnostics/` |
| `profile-repl.ss` | profile-*.ss | `diagnostics/` |
| `duckie-loop.ss` | duckie-persist.ss, ui/* | `assistants/` |
| `error-improvements.ss` | error-fmt.ss | `debug/` |
| `debug-repl.ss` | session-debugger.ss | `debug/` |

### Tier 2: Standalone Files

These have no internal dependencies (move freely):
- `json.ss` → `io/`
- `alloc-tracker.ss` → `diagnostics/`
- `fuel-analysis.ss`, `fuel-profile.ss` → `diagnostics/`
- `block-*.ss` → `blocks/`
- `duckie-interact.ss`, `duckie-persist.ss` → `assistants/`
- `music-gen.ss`, `create-art.ss`, `wave-synth.ss` → `media/`
- `type-inspect.ss`, `xref.ss`, `error-fmt.ss` → `debug/`

## Migration Strategy: Forwarding Stubs

For highly-depended files, use forwarding stubs (proven safe in core/ migration):

```scheme
;;; shell/fs.ss — Forwarding stub
;;; The module has moved to shell/io/fs.ss
;;; This stub exists for backwards compatibility.
(load "shell/io/fs.ss")
```

This allows:
- Immediate move without breaking dependents
- Gradual update of dependents in future sessions
- Zero-risk migration

## Execution Order

### Phase A: Create New Directories

```bash
mkdir -p shell/{io,storage,blocks,diagnostics,debug,assistants,media,repl}
```

### Phase B: Move Tier 2 (Standalone) Files

1. **io/** - `json.ss`
2. **diagnostics/** - `alloc-tracker.ss`, `fuel-analysis.ss`, `fuel-profile.ss`
3. **blocks/** - `block-diff.ss`, `block-explorer.ss`, `block-index.ss`, `block-navigator.ss`, `block-query.ss`, `block-query-advanced.ss`
4. **debug/** - `type-inspect.ss`, `xref.ss`, `error-fmt.ss`
5. **assistants/** - `duckie-interact.ss`, `duckie-persist.ss`
6. **media/** - `music-gen.ss`, `create-art.ss`, `wave-synth.ss`

### Phase C: Move Tier 1 (Internal Deps) Files

After their dependencies from Phase B are moved:

1. **storage/** - `cas-persist.ss`, `store-analyze.ss`
2. **diagnostics/** - `profile-call-graph.ss`, `profiler-unified.ss`, `profile-analysis.ss`, `profile-persist.ss`, `profile-repl.ss`
3. **debug/** - `error-improvements.ss`, `exploration-error-handler.ss`, `debug-repl.ss`, `session-debugger.ss`
4. **assistants/** - `duckie-loop.ss`

### Phase D: Move Tier 0 (Highly Depended) Files + Stubs

1. **io/fs.ss** - Move file, create stub at `shell/fs.ss`
2. **storage/store-api.ss** - Move file, create stub at `shell/store-api.ss`
3. **storage/identity.ss** - Move file, create stub at `shell/identity.ss`

### Phase E: Move REPL Infrastructure

This is the most complex - many interdependencies:

1. **repl/** - `session-manager.ss`, `session-debug.ss`
2. **repl/** - `eval-repl.ss`, `patches.ss`
3. **repl/** - `repl-quiet.ss`, `repl-worker.ss`
4. **repl/** - `repl-daemon.ss`, `repl-daemon-mcp.ss`, `cleanup-workers.ss`
5. **repl/repl.ss** - Move file, create stub at `shell/repl.ss`

### Phase F: Add README.sexp to Each New Directory

Use template from TAXONOMY.sexp.

### Phase G: Update Load Paths (Future Sessions)

Gradually update files to use new paths, removing need for stubs.

## Files Remaining at Root (By Decision)

| File | Reason |
|----|----|
| `commands.ss` | Core command registry |
| `history.ss` | Command history |
| `tutorial.ss` | User-facing tutorial |
| `toolkit.ss` | Aggregation loader |
| `edit.ss` | General editing utilities |
| `meta.ss` | Metadata utilities |
| `patches.ss` | Runtime patches |

## Files Requiring Further Review

These don't fit cleanly into proposed categories:

| File | Current Location | Question |
|----|----|----|
| `watch*.ss` | root | Watch system - own directory? |
| `graph-export.ss` | root | `blocks/` or `io/`? |
| `capability-lens.ss` | root | `lens/` or `debug/`? |
| `task-tracker.ss` | root | `tools/`? |
| `project-status.ss` | root | `tools/`? |
| `concept-map.ss` | root | `tools/`? |
| `tdd.ss` | root | `tools/`? |
| `verify-commands.ss` | root | `tools/`? |
| `fold-client.ss` | root | `io/`? |
| `universe-*.ss` | root | `storage/`? |
| `perf-monitor.ss` | root | `diagnostics/`? |
| `paren-check.ss` | root | `tools/`? |
| `login-help.ss` | root | `tools/`? |
| `lzr.ss` | root | `tools/`? |
| `tier-info.ss` | root | `tools/`? |
| `interactive-tutorial.ss` | root | Keep with tutorial.ss? |
| `tutorial-*.ss` | root | Keep with tutorial.ss? |

## Test Verification

After each phase:
```bash
scheme --script shell/tests/run-tests.ss
```

After complete migration:
```bash
scheme --script test-all.ss
```

## Risk Mitigations

| Risk | Mitigation |
|----|----|
| Load path breakage | Forwarding stubs |
| Missing files | Git status before each commit |
| Circular deps | Analyze deps before move |
| Test failures | Run tests after each phase |

## Commit Strategy

One commit per phase:
1. "refactor(shell): Create new subdirectory structure"
2. "refactor(shell): Move standalone files to domain directories"
3. "refactor(shell): Move files with internal dependencies"
4. "refactor(shell): Move foundational files with forwarding stubs"
5. "refactor(shell): Move REPL infrastructure"
6. "docs(shell): Add README.sexp to new directories"
