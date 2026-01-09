# Taxonomy Organization Proposal

**Date:** 2026-01-08
**Author:** Claude (Shepherd session)
**Status:** Draft for Review

## Executive Summary

This proposal outlines opportunities to improve the taxonomy of The Fold's codebase through:
1. **Centralized taxonomy definition** - A single source of truth for module classification
2. **Hierarchical reorganization** - Grouping shell's 70 root files into logical subdirectories
3. **Standardized naming conventions** - Extending the FP toolkit's conventions system-wide
4. **Consistent test naming** - Resolving the `test-*.ss` vs `*-test.ss` inconsistency

---

## Current State Analysis

### Core Directory (Excellent Structure)

The `core/` directory demonstrates exceptional organization:

| Metric | Value |
|--------|-------|
| Top-level directories | 24 |
| Total .ss files | 375 |
| Test files | 186 (49.6% ratio) |
| README.sexp coverage | 100% (36/36 directories) |

**Organizational patterns already established:**
- Domain-driven subdirectories (`linalg/`, `types/`, `autodiff/`, etc.)
- Consistent test file naming (`test-*.ss`)
- Every directory has `README.sexp` documentation
- Formal naming conventions in `core/fp/NAMING-CONVENTIONS.sexp`

**Minor inconsistencies:**
- `pipeline/` has nested `tests/` subdirectory (unique exception)
- No centralized taxonomy definition across all of core

### Shell Directory (Opportunities for Improvement)

The `shell/` directory has structural debt:

| Issue | Impact |
|-------|--------|
| 70 root-level .ss files | Discoverability and cognitive load |
| Mixed test naming (`test-*.ss` vs `*-test.ss`) | Inconsistent patterns |
| No formalized naming conventions | Style drift over time |

**Current shell subdirectories (10):**
```
shell/
├── benchmarks/    (2 files)
├── discord/       (JS/node integration)
├── examples/      (9 files)
├── git/           (3 files)
├── introspect/    (4 files)
├── lens/          (6 files)
├── lsp/           (5 files)
├── mcp-server/    (TypeScript)
├── pipeline/      (6 files + effects/)
├── tests/         (96 files)
├── tools/         (25 files)
├── ui/            (21 files)
└── [70 root files]  <- Reorganization opportunity
```

---

## Proposal 1: Centralized Taxonomy Definition

Create `/home/oso/the-fold/TAXONOMY.sexp` as the authoritative source for module classification.

### Proposed Structure

```scheme
;;; TAXONOMY.sexp — The Fold Module Taxonomy
;;;
;;; Authoritative classification of all modules in the system.
;;; This file defines the hierarchical structure, naming conventions,
;;; and categorization rules for the entire codebase.

((version . "1.0")
 (date . "2026-01-08")

 (domains
  ;; Top-level conceptual domains
  ((domain . foundation)
   (description . "Core language primitives and block machine")
   (modules . (core/base core/blocks)))

  ((domain . types)
   (description . "Type system, inference, and dependent types")
   (modules . (core/types)))

  ((domain . language)
   (description . "Parsing, evaluation, compilation, modules")
   (modules . (core/lang core/dsl)))

  ((domain . mathematics)
   (description . "Pure mathematical computation")
   (subdomains . (
     ((name . algebra)
      (modules . (core/algebra core/number-theory)))
     ((name . linear-algebra)
      (modules . (core/linalg)))
     ((name . analysis)
      (modules . (core/numeric core/fp/numeric)))
     ((name . probability)
      (modules . (core/random core/info-theory))))))

  ((domain . computation)
   (description . "Computational abstractions and algorithms")
   (subdomains . (
     ((name . autodiff)
      (modules . (core/autodiff)))
     ((name . physics)
      (modules . (core/diff-physics core/diff-physics-3d core/dynamics)))
     ((name . geometry)
      (modules . (core/geometry core/sim)))
     ((name . data-structures)
      (modules . (core/data core/query))))))

  ((domain . functional-programming)
   (description . "Type classes, effects, and FP abstractions")
   (modules . (core/fp/*)))

  ((domain . shell)
   (description . "IO layer, validation, and user interface")
   (subdomains . (
     ((name . repl)
      (description . "Interactive environment"))
     ((name . storage)
      (description . "Persistence and CAS"))
     ((name . developer-tools)
      (description . "Profiling, debugging, refactoring"))
     ((name . visualization)
      (description . "Graphics, UI, rendering"))
     ((name . integration)
      (description . "External systems: git, lsp, discord, mcp"))))))

 (naming-conventions
  ;; System-wide conventions extending core/fp/NAMING-CONVENTIONS.sexp
  (file-naming
   ((pattern . "module-name.ss")
    (description . "Hyphenated lowercase for all module files"))
   ((pattern . "test-module-name.ss")
    (description . "Test files prefixed with test-"))
   ((pattern . "README.sexp")
    (description . "S-expression documentation in every directory")))

  (directory-naming
   ((pattern . "lowercase-hyphenated")
    (examples . (diff-physics info-theory number-theory))
    (avoid . (camelCase underscores abbreviations))))

  (function-naming
   ;; Imported from core/fp/NAMING-CONVENTIONS.sexp
   (reference . "lattice/fp/NAMING-CONVENTIONS.sexp")))

 (classification-rules
  ;; Rules for determining where new code belongs
  ((rule . "Pure computation without IO → core/")
   (rationale . "Maintains the Fabric/Thimble separation"))

  ((rule . "IO, validation, or effects → shell/")
   (rationale . "Defensive code lives in the shell"))

  ((rule . "Type class or FP abstraction → core/fp/")
   (rationale . "FP toolkit is the home for abstract patterns"))

  ((rule . "Mathematical computation → core/{domain}/")
   (rationale . "Use domain-specific directories"))

  ((rule . "Developer tooling → shell/tools/")
   (rationale . "Keep tools consolidated"))))
```

### Benefits
- Single source of truth for module organization
- Machine-readable for tooling (linters, navigation, documentation generators)
- Explicit classification rules prevent organizational drift
- Enables automated consistency checking

---

## Proposal 2: Shell Hierarchical Reorganization

Reorganize shell's 70 root-level files into functional subdirectories.

### Proposed New Structure

```
shell/
├── repl/                    # REPL and session management (NEW)
│   ├── repl.ss              # Main REPL
│   ├── repl-daemon.ss
│   ├── repl-daemon-mcp.ss
│   ├── repl-quiet.ss
│   ├── repl-worker.ss
│   ├── session-manager.ss
│   ├── session-state.ss
│   ├── cleanup-workers.ss
│   └── README.sexp
│
├── blocks/                  # Block operations (NEW)
│   ├── block-navigator.ss
│   ├── block-explorer.ss
│   ├── block-query.ss
│   ├── block-diff.ss
│   ├── block-index.ss
│   └── README.sexp
│
├── storage/                 # Persistence layer (NEW)
│   ├── cas-persist.ss
│   ├── store-api.ss
│   ├── store-analyze.ss
│   ├── duckie-*.ss
│   └── README.sexp
│
├── profiling/               # Performance analysis (NEW)
│   ├── fuel-profile.ss
│   ├── fuel-histogram.ss
│   ├── fuel-trace.ss
│   ├── fuel-viz.ss
│   ├── profile-analyzer.ss
│   ├── profiler-unified.ss
│   └── README.sexp
│
├── debug/                   # Debugging tools (NEW)
│   ├── debug-repl.ss
│   ├── type-inspect.ss
│   ├── xref.ss
│   ├── error-*.ss
│   └── README.sexp
│
├── io/                      # Core IO operations (NEW)
│   ├── fs.ss
│   ├── json.ss
│   ├── text.ss
│   ├── validate.ss
│   └── README.sexp
│
├── tools/                   # Developer tools (EXISTS)
├── ui/                      # Visualization (EXISTS)
├── lens/                    # Navigation (EXISTS)
├── git/                     # Version control (EXISTS)
├── pipeline/                # Effects (EXISTS)
├── lsp/                     # Language server (EXISTS)
├── tests/                   # Test suite (EXISTS)
├── examples/                # Examples (EXISTS)
├── benchmarks/              # Benchmarks (EXISTS)
├── introspect/              # Code analysis (EXISTS)
│
├── commands.ss              # Command registry (ROOT - shared)
├── history.ss               # Command history (ROOT - shared)
├── tutorial.ss              # User tutorial (ROOT - shared)
└── README.sexp
```

### Migration Plan

**Phase 1: Create directories and move files** (non-breaking)
- Create new subdirectories with README.sexp
- Move files maintaining all functionality
- Update `(load ...)` paths in affected files

**Phase 2: Update load paths**
- Update `shell/repl.ss` to use new paths
- Update any cross-references

**Phase 3: Deprecate old locations**
- Leave forwarding comments for any external references

### File Movement Summary

| New Directory | Files to Move | From Root |
|--------------|---------------|-----------|
| `repl/` | 9 files | repl*.ss, session*.ss, cleanup-workers.ss |
| `blocks/` | 5 files | block-*.ss |
| `storage/` | 6 files | cas-persist.ss, store-*.ss, duckie-*.ss |
| `profiling/` | 7 files | fuel-*.ss, profile*.ss, profiler*.ss |
| `debug/` | 5 files | debug-repl.ss, type-inspect.ss, xref.ss, error-*.ss |
| `io/` | 5 files | fs.ss, json.ss, text.ss, validate.ss |

**Remaining at root (shared infrastructure):** ~15 files including commands.ss, history.ss, tutorial.ss, and other cross-cutting utilities.

---

## Proposal 3: Naming Convention Standardization

Extend the FP toolkit's naming conventions to the entire codebase.

### Current Inconsistencies

| Location | Issue | Count |
|----------|-------|-------|
| `shell/tests/` | Files named `*-test.ss` instead of `test-*.ss` | 9 files |
| Documentation | Mix of `.md` and `.sexp` | Various |
| Root files | Some lack clear domain prefix | ~10 files |

### Standardization Rules

**1. Test File Naming**

```
STANDARD:    test-<module>.ss
DEPRECATED:  <module>-test.ss

Files to rename:
  shell/tests/block-query-test.ss    → shell/tests/test-block-query.ss
  shell/tests/easing-test.ss         → shell/tests/test-easing.ss
  shell/tests/layout-test.ss         → shell/tests/test-layout.ss
  shell/tests/layout-color-test.ss   → shell/tests/test-layout-color.ss
  shell/tests/layout-debug-test.ss   → shell/tests/test-layout-debug.ss
  shell/tests/particle-test.ss       → shell/tests/test-particle.ss
  shell/tests/svg-export-test.ss     → shell/tests/test-svg-export.ss
  shell/tests/turtle-graphics-test.ss → shell/tests/test-turtle-graphics.ss
  shell/tests/type-inspect-test.ss   → shell/tests/test-type-inspect.ss
```

**2. Documentation Format**

```
STANDARD:    README.sexp (for all module documentation)
ALLOWED:     *.md (for human-facing guides like CLAUDE.md, COMMANDS.md)

Rule: If it describes module structure → .sexp
      If it's a guide/tutorial → .md
```

**3. Module File Naming**

```
Pattern: domain-specific-name.ss

Good:
  block-navigator.ss     (domain: block, function: navigator)
  fuel-profile.ss        (domain: fuel, function: profile)
  type-inspect.ss        (domain: type, function: inspect)

Avoid:
  navigator.ss           (missing domain prefix)
  fuelprofile.ss         (missing hyphen)
  TypeInspect.ss         (camelCase)
```

---

## Proposal 4: Directory README.sexp Template

Standardize the README.sexp structure across all directories.

### Template

```scheme
;;; <path>/README.sexp — <One-line description>

((name . "<directory-name>")
 (purpose . "<Brief purpose statement>")
 (tier-access . player|builder|shepherd)
 (purity . pure|impure)

 (description . "<Multi-line detailed description>")

 (modules
  ;; Grouped by functional category
  ((category . "<category-name>")
   (files . (<file1.ss> <file2.ss>))
   (description . "<What this category contains>"))
  ...)

 (dependencies
  ;; What this directory depends on
  (internal . (<core/module> <shell/module>))
  (external . ()))

 (dependents
  ;; What depends on this directory
  (<other/module> ...))

 (key-concepts
  ;; Domain-specific terminology
  ((<term> . "<definition>")
   ...))

 (testing
  (test-file . "<test file or test directory>")
  (run-command . "<command to run tests>"))

 (for-builders . "<Guidance for builders>")
 (for-shepherds . "<Guidance for shepherds>")

 (see-also . (<related-path> ...)))
```

---

## Implementation Priority

| Priority | Proposal | Effort | Impact |
|----------|----------|--------|--------|
| **P1** | Centralized TAXONOMY.sexp | Medium | High - Single source of truth |
| **P2** | Test naming standardization | Low | Medium - Consistency |
| **P3** | Shell reorganization | High | High - Major discoverability improvement |
| **P4** | README.sexp template | Low | Medium - Documentation consistency |

### Recommended Approach

1. **Start with TAXONOMY.sexp** - Defines the target state before making changes
2. **Fix test naming** - Small, safe, immediate consistency win
3. **Plan shell reorganization** - Create detailed migration plan with dependency analysis
4. **Execute shell reorganization** - Phased approach with backwards compatibility
5. **Apply README template** - Update documentation as directories are touched

---

## Risk Analysis

| Risk | Mitigation |
|------|------------|
| Load path breakage | Thorough grep for all `(load ...)` references before moving |
| Test discovery | Update test runners to find tests in new locations |
| External tool integration | Announce changes, provide transition period |
| Documentation staleness | Update TAXONOMY.sexp as single source of truth |

---

## Success Metrics

1. **Discoverability**: Time to find a module decreases
2. **Consistency**: Zero naming convention violations (tooling-verifiable)
3. **Documentation**: 100% README.sexp coverage maintained
4. **Test organization**: All tests follow `test-*.ss` pattern

---

## Appendix A: Current Directory Census

### Core Directories (24)

```
core/
├── algebra/           # Abstract algebra
├── autodiff/          # Automatic differentiation
├── automata/          # State machines
├── base/              # Foundation (prelude, sha256, error)
├── blocks/            # Block system and CAS
├── data/              # Data structures
├── diff-physics/      # 2D differentiable physics
├── diff-physics-3d/   # 3D differentiable physics
├── dsl/               # DSL infrastructure
├── dynamics/          # ODE systems
├── fp/                # FP toolkit (11 subdirectories)
├── geometry/          # Computational geometry
├── info-theory/       # Information theory
├── lang/              # Language core
├── linalg/            # Linear algebra
├── lsp/               # Language server protocol
├── number-theory/     # Number theory
├── numeric/           # Numerical computing
├── pipeline/          # Agent workflows
├── query/             # Query DSL
├── random/            # Probability
├── sim/               # Simulation
├── types/             # Type system
└── util/              # Utilities
```

### FP Subdirectories (11)

```
core/fp/
├── analysis/          # Static analysis
├── control/           # Effects, continuations
├── control-systems/   # Control theory
├── data/              # Streams, lazy structures
├── game/              # Game theory
├── measure/           # Units of measure
├── meta/              # DSL utilities
├── numeric/           # Transcendental functions
├── parsing/           # Parser combinators
├── rewrite/           # Term rewriting
└── symbolic/          # Symbolic computation
```

### Shell Directories (10 + root)

```
shell/
├── benchmarks/        # Performance benchmarks
├── discord/           # Discord integration
├── examples/          # Example code
├── git/               # Git integration
├── introspect/        # Code analysis
├── lens/              # Navigation tools
├── lsp/               # LSP transport
├── mcp-server/        # MCP integration
├── pipeline/          # Effect interpretation
├── tests/             # Test suite
├── tools/             # Developer tools
├── ui/                # Visualization
└── [70 root files]    # To be reorganized
```

---

## Appendix B: Files Requiring Test Rename

```bash
# Commands to rename test files
cd /home/oso/the-fold/shell/tests

git mv block-query-test.ss test-block-query.ss
git mv easing-test.ss test-easing.ss
git mv layout-test.ss test-layout.ss
git mv layout-color-test.ss test-layout-color.ss
git mv layout-debug-test.ss test-layout-debug.ss
git mv particle-test.ss test-particle.ss
git mv svg-export-test.ss test-svg-export.ss
git mv turtle-graphics-test.ss test-turtle-graphics.ss
git mv type-inspect-test.ss test-type-inspect.ss
```

---

## Approval Requested

- [ ] TAXONOMY.sexp creation
- [ ] Test file renaming
- [ ] Shell directory reorganization (detailed plan to follow)
- [ ] README.sexp template adoption
