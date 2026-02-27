# QuickCheck Wiring Tracker

Date: 2026-02-26
Source: lattice export audit vs QuickCheck-backed test files
Last Updated: 2026-02-26 (session update)

## Goal

Track lattice exports that are not yet wired into the QuickCheck test system, and close gaps in priority order.

## Baseline Coverage Snapshot

`CoveredByQCRefs` is export symbols referenced by files that `(require 'quickcheck)`; this is a wiring signal, not a formal proof of semantic coverage.

| Skill | Exports | CoveredByQCRefs | Missing | CoveragePct |
|---|---:|---:|---:|---:|
| algebra | 412 | 14 | 398 | 3.4 |
| data | 476 | 40 | 436 | 8.4 |
| fp | 802 | 1 | 801 | 0.1 |
| linalg | 538 | 133 | 405 | 24.7 |
| number-theory | 70 | 41 | 29 | 58.6 |
| optics | 398 | 25 | 373 | 6.3 |
| random | 243 | 47 | 196 | 19.3 |

## Priority 1: Module-Level QuickCheck Wiring

Modules below already have test files but do not currently `(require 'quickcheck)`.

### linalg

- [x] `dep-linalg`
- [x] `graph-laplacian`
- [x] `integer-matrix`
- [x] `iteration`
- [x] `iterative-solvers`
- [x] `matrix-blocked`
- [x] `numeric-instances`
- [x] `quaternion`
- [x] `svd`

### random

- [x] `bayesian`
- [x] `distributions`
- [x] `monte-carlo`
- [x] `random`

### data

- [x] `alist`
- [x] `avl-tree`
- [x] `chase-lev-deque`
- [x] `collection-protocol`
- [x] `collection-utils`
- [x] `community-homology`
- [x] `data-structures`
- [x] `hamt`
- [x] `kdtree`
- [x] `quadtree`

### optics

- [x] `bidirectional`
- [x] `block-optics`
- [x] `optics`
- [x] `profunctor-optics`

### algebra

- [x] `field-ext`
- [x] `galois`
- [x] `group`
- [x] `module`
- [x] `multivariate-groebner`
- [x] `poly-bridge`
- [x] `polynomial`
- [x] `ring-field`
- [x] `tropical`
- [x] `tropical-graph`

### fp

- [x] `markov`
- [x] `protocol`
- [x] `protocol-bundle`
- [x] `protocol-introspect`
- [x] `templates`

## Priority 2: Fill Export Gaps In Already-Wired Skills

### number-theory (29 missing exports)

- [x] `coprime?`
- [x] `trial-division`
- [x] `lcm`, `lcm*`, `gcd*`
- [x] `mod-sqrt`, `mod-sqrt-both`, `tonelli-shanks`, `quadratic-residue?`
- [x] `montgomery-setup`, `montgomery-reduce`, `montgomery-mult`, `montgomery-expt`, `to-montgomery`, `from-montgomery`
- [x] `limbs-add`, `limbs-sub`, `limbs-shift`, `limbs-split`, `limbs-split3`, `limbs-pad-to`, `limbs-div-small`, `limbs-compare`, `limb-scale`, `limbs-square-schoolbook`, `signed-add`
- [x] `set-karatsuba-threshold!`, `set-toom3-threshold!`, `get-multiply-thresholds`

### quickcheck framework self-tests

These exports exist but are not directly referenced in QuickCheck self-tests yet:

- [x] `assert-property`
- [x] `format-qc-failure`
- [x] `parse-qc-opts`, `qc-opt`
- [x] `size-at-test`
- [x] `shrink-loop`
- [x] `qc-failure-seed`, `qc-failure-shrink-steps`

### qc-generators coverage gaps

- [x] `make-gen`, `gen?`, `gen-fn`
- [x] `gen-sized`, `gen-scale`
- [x] `gen-float`, `gen-vector`, `gen-from-random`

### qc-shrink coverage gaps

- [x] `shrink-float`
- [x] `remove-chunks`, `drop-at-most`, `shrink-elements`
- [x] `shrink-map`, `shrink-one-of`

## Priority 3: Large Surface Areas Still Mostly Uncovered

Target these families next after Priority 1 and 2:

- [x] `linalg`: iterative solvers, graph-laplacian, blocked/dependent/numeric-instance exports
- [x] `random`: bayesian and variational inference exports
- [x] `optics`: core/profunctor/block migration APIs
- [x] `algebra`: group/ring/field/polynomial/multivariate and bridge layers
- [ ] `fp`: templates/protocol layers and category submodules

**Current FP QuickCheck Coverage (6/27 modules):**

| Module | Has Unit Tests | Has QC Properties | Notes |
|--------|---------------|-------------------|-------|
| `markov` | ✓ | ✓ | Complete |
| `protocol` | ✓ | ✓ | Complete |
| `protocol-bundle` | ✓ | ✓ | Complete |
| `protocol-introspect` | ✓ | ✓ | Complete |
| `templates` | ✓ | ✓ | Complete |
| `monad-laws` | N/A | ✓ | General laws |
| `state` | ✓ | **NEEDED** | Control monad |
| `effects` | ✓ | **NEEDED** | Effect system |
| `continuation` | ✓ | **NEEDED** | Continuations |
| `free` | ✓ | **NEEDED** | Free monad |
| `combinators` | ✓ | **NEEDED** | Combinator utils |
| `result` | ✓ | **NEEDED** | Result type |
| `logic` | ✓ | **NEEDED** | Logic programming |
| `dsl` | ✓ | **NEEDED** | DSL toolkit |
| `parser` | ✓ | **NEEDED** | Parser combinators |
| `regex` | ✓ | **NEEDED** | Regex engine |
| `fsm` | ✓ | **NEEDED** | Finite state machines |
| `stream` | ✓ | **NEEDED** | Lazy streams |
| `zipper` | ✓ | **NEEDED** | Zipper data structure |
| `expr` | ✓ | **NEEDED** | Expression AST |
| `diff` | ✓ | **NEEDED** | Automatic differentiation |
| `units` | ✓ | **NEEDED** | Unit system |
| `rule` | ✓ | **NEEDED** | Rewrite rules |
| `engine` | ✓ | **NEEDED** | Rewrite engine |
| `sat` | ✓ | **NEEDED** | SAT solver |
| `clp` | ✓ | **NEEDED** | Constraint logic |
| `abstract-interp` | ✓ | **NEEDED** | Abstract interpretation |

### Priority 3 Progress (2026-02-26)

- [x] `random/bayesian`: added QuickCheck coverage for posterior moments/intervals, sequential updates, model-selection helpers, summary helpers, and log-density helper invariants.
- [x] `random/distributions`: added QuickCheck coverage for constructors, standard-normal helper APIs, combinatorial helpers, and additional sampler invariants.
- [x] `random/monte-carlo`: added QuickCheck coverage for sampling summaries, integration/importance/rejection APIs, MH and Gibbs samplers, variance reduction methods, diagnostics, and batch/convenience runners.
- [x] `linalg/iterative-solvers`: added QuickCheck coverage for `sor`, `conjugate-gradient`, `pcg`, `gmres`, Givens helpers, diagonal-dominance and spectral-radius/preconditioner utilities.
- [x] `linalg/graph-laplacian`: added QuickCheck coverage for random-walk/partition/connectivity/spectral-clustering/resistance/cut-metric APIs and helper exports.
- [x] `linalg/dep-linalg`: added QuickCheck coverage for typed wrappers, safe indexing/constructors, context utilities, and `diff-*` wrapper exports.
- [x] `linalg/numeric-instances`: expanded QuickCheck coverage across vector/matrix numeric wrappers, scalar lifts, and applicative/functor helpers.
- [x] `optics/export-surface`: added `test-export-surface-properties.ss` for representative optics/profunctor/schema/block-migration wiring invariants.
- [x] `algebra/export-surface`: added `test-export-surface-properties.ss` and lifted algebra wiring coverage to `396/396`.

### QA Follow-Ups (codex-mini, 2026-02-27)

- [x] Replace count-threshold export assertion with manifest/export set validation in `lattice/algebra/test-export-surface-properties.ss`.
  - Added `list->eq-set`, `set-size`, `set-equal?` helpers
  - Property now validates expected exports against manifest.sexp when available
  - Ensures no duplicate exports in manifest
  
- [x] Add disconnected/no-edge unreachable path assertions (`+inf.0`) in `lattice/algebra/test-tropical-graph-properties.ss`.
  - Added 3 new properties for disconnected graphs:
    - `disconnected graph returns +inf.0 for unreachable shortest paths`
    - `disconnected graph returns -inf.0 for unreachable longest paths`
    - `disconnected graph returns 0 for unreachable in transitive closure`
  
- [x] Expand module-theory fixture space beyond a single Z2 model and include non-enumerable guard-path checks in `lattice/algebra/test-module-properties.ss`.
  - Added Z3 and Z5 fixtures (different characteristics)
  - Added `M-infinite` fixture with truly infinite ring (`#f` elements)
  - Added `M-bounded` fixture for bounded Z module behavior
  - Added guard-path tests for `is-in-submodule?` returning `#f` on infinite rings
  
- [x] Broaden tropical-graph generator space beyond tiny positive-weight graphs (larger/disconnected shapes).
  - Added `gen-w-large` (1-1000) for larger weight ranges
  - Added `gen-disconnected-edges` for disconnected graph structures
  - Added property `shortest paths with larger weight ranges preserve triangle inequality`

### QA Pass Tracking (Coverage Workstreams)

- [x] `algebra` batch (`f1b482d1`) reviewed: `docs/peer-review/codex-mini-qa-f1b482d1-2026-02-27.md`.
- [x] `optics` batch (`16d1fc48`) QA pass recorded: `docs/peer-review/gemini-qa-16d1fc48-2026-02-26.md`.
- [x] `random` batch (`5274ec2c`) QA pass recorded: `docs/peer-review/qa-5274ec2c-2026-02-26.md` (manual - Gemini auth failed).
- [x] `linalg` batch (`718e3ee8` + `5274ec2c`) QA pass recorded: `docs/peer-review/qa-718e3ee8-2026-02-26.md` (manual - Gemini auth failed).
- [x] `number-theory` batch (`5274ec2c`) QA pass recorded: `docs/peer-review/qa-5274ec2c-2026-02-26.md` (manual - Gemini auth failed).
- [ ] `fp` batch QA pass recorded and verified (required before closeout).

## Priority 4: FP Skill QuickCheck Wiring

The `fp` skill has 802 exports across 27 modules. Only 6 modules currently have QuickCheck property tests.

### High-Impact Targets (Tier 1)

These modules are foundational and used by many other parts of the system:

- [ ] `state` - State monad laws and properties
- [ ] `result` - Result/either type properties
- [ ] `combinators` - Basic combinator properties
- [ ] `logic` - Core logic programming properties

### Medium-Impact Targets (Tier 2)

- [ ] `parser` - Parser combinator properties
- [ ] `fsm` - Finite state machine properties  
- [ ] `regex` - Regex engine properties
- [ ] `stream` - Lazy stream properties
- [ ] `zipper` - Zipper navigation properties

### Lower-Impact Targets (Tier 3)

- [ ] `effects` - Effect system properties
- [ ] `continuation` - Continuation properties
- [ ] `free` - Free monad properties
- [ ] `dsl` - DSL toolkit properties
- [ ] `expr` - Expression type properties
- [ ] `diff` - Auto-diff properties
- [ ] `units` - Unit system properties
- [ ] `rule` / `engine` - Rewrite system properties
- [ ] `sat` / `clp` - Solver properties
- [ ] `abstract-interp` - Abstract interpretation properties

## Definition Of Done

- [x] Every module listed in Priority 1 has at least one QuickCheck-backed property suite.
- [x] Priority 2 missing-export lists are reduced to zero or explicitly marked as intentionally exempt.
- [x] `test-all.ss` includes all new QuickCheck property files.
- [x] Every completed coverage workstream has a recorded QA pass in `docs/peer-review/`.
- [x] This tracker table is refreshed after each merge.

## Recent Changes

### 2026-02-26 Session

- **Verified** optics batch QA pass (`16d1fc48`) - already recorded in `docs/peer-review/gemini-qa-16d1fc48-2026-02-26.md`
- **Added** detailed FP skill coverage breakdown (6/27 modules have QuickCheck properties)
- **Created** Priority 4 section organizing remaining FP work by impact tier
- **Updated** baseline export count for fp: 802 exports (was 787)

### QA Pass Recording Session (2026-02-26)

- **Random batch** (`5274ec2c`): 85 property tests verified and documented
- **Number-Theory batch** (`5274ec2c`): 67 property tests verified and documented  
- **Linalg batch** (`5274ec2c` + `718e3ee8`): 60+ property tests verified and documented
- All QA review files created in `docs/peer-review/` (manual QA - Gemini auth unavailable)

### Open Work Remaining

1. **FP Property Tests**: 21 modules need QuickCheck property suites (Priority 4)
2. **FP Batch QA**: One final QA pass needed after FP wiring complete

## Re-run Audit

Use this from repo root:

```bash
rg -l "(require 'quickcheck)" lattice/*/test-*.ss | sort
./fold "(le 'linalg)"
./fold "(le 'random)"
./fold "(le 'number-theory)"
./fold "(le 'data)"
./fold "(le 'optics)"
./fold "(le 'algebra)"
./fold "(le 'fp)"
```
