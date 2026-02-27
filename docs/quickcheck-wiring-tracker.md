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
- [ ] `fp`, `rewrite`, `symbolic`, `category`: templates/protocol layers and submodules

**Note**: FP has been split into 4 skills:
- `fp`: 686 exports (core FP, control, parsing, CLP, SAT)
- `rewrite`: 30 exports (rule/engine)
- `symbolic`: 45 exports (expr, diff, simplify, integrate, solve)
- `category`: 102 exports (adjunctions, monads, comonads, abstract interpretation)

### Current QuickCheck Coverage by Skill

| Skill | Exports | QC Property Files | Coverage |
|-------|---------|-------------------|----------|
| `fp` | 686 | 5 files | ~5% |
| `rewrite` | 30 | 0 files | 0% |
| `symbolic` | 45 | 0 files | 0% |
| `category` | 102 | 0 files | 0% |

### FP Modules with QC Properties (5/37)

| Module | Has Unit Tests | Has QC Properties | Notes |
|--------|---------------|-------------------|-------|
| `markov` | ✓ | ✓ | Complete |
| `protocol` | ✓ | ✓ | Complete |
| `protocol-bundle` | ✓ | ✓ | Complete |
| `protocol-introspect` | ✓ | ✓ | Complete |
| `templates` | ✓ | ✓ | Complete |
| `monad-laws` | N/A | ✓ | General laws |

### FP Modules Needing QC Properties (32 modules)

**Tier 1 (High Impact - Control/Data)**
| Module | Has Unit Tests | Notes |
|--------|---------------|-------|
| `state` | ✓ | State monad laws |
| `result` | ✓ | Result/either type |
| `combinators` | ✓ | Basic combinators |
| `logic` | ✓ | Core logic programming |

**Tier 2 (Medium Impact - Parsing/Streams)**
| Module | Has Unit Tests | Notes |
|--------|---------------|-------|
| `parser` | ✓ | Parser combinators |
| `fsm` | ✓ | Finite state machines |
| `regex` | ✓ | Regex engine |
| `stream` | ✓ | Lazy streams |
| `zipper` | ✓ | Zipper navigation |

**Tier 3 (Lower Impact - Specialized)**
| Module | Has Unit Tests | Notes |
|--------|---------------|-------|
| `effects` | ✓ | Effect system |
| `continuation` | ✓ | Continuations |
| `free` | ✓ | Free monad |
| `dsl` | ✓ | DSL toolkit |
| `units` | ✓ | Unit system |

**Tier 4 (Solver/Constraint Modules)**
| Module | Has Unit Tests | Notes |
|--------|---------------|-------|
| `sat` | ✓ | SAT solver |
| `clp` | ✓ | Constraint logic programming |
| `maxsat` | ✓ | MaxSAT solver |

### Rewrite Skill (0% coverage)

| Module | Has Unit Tests | Has QC Properties | Notes |
|--------|---------------|-------------------|-------|
| `rule` | ✓ | **NEEDED** | Rewrite rules |
| `engine` | ✓ | **NEEDED** | Rewrite engine |

### Symbolic Skill (0% coverage)

| Module | Has Unit Tests | Has QC Properties | Notes |
|--------|---------------|-------------------|-------|
| `expr` | ✓ | **NEEDED** | Expression AST |
| `diff` | ✓ | **NEEDED** | Auto-diff |
| `simplify` | ✓ | **NEEDED** | Expression simplification |
| `integrate` | ✓ | **NEEDED** | Symbolic integration |
| `solve` | ✓ | **NEEDED** | Equation solving |
| `poly-canonical` | ✓ | **NEEDED** | Polynomial canonical forms |
| `egraph-simplify` | ✓ | **NEEDED** | E-graph simplification |

### Category Skill (0% coverage)

| Module | Has Unit Tests | Has QC Properties | Notes |
|--------|---------------|-------------------|-------|
| `adjunction` | ✓ | **NEEDED** | Adjunctions |
| `monad-derivation` | ✓ | **NEEDED** | Monad derivation |
| `comonad` | ✓ | **NEEDED** | Comonads |
| `kan-extension` | ✓ | **NEEDED** | Kan extensions |
| `natural-transform` | ✓ | **NEEDED** | Natural transformations |
| `abstract-interp` | ✓ | **NEEDED** | Abstract interpretation |
| `free-algebra` | ✓ | **NEEDED** | Free algebras |

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

## Priority 4: FP/Related Skills QuickCheck Wiring

### 4a: FP Core (686 exports, 5/37 modules covered)

#### Tier 1: Control & Data Flow (High Impact)
- [x] `state` - State monad laws (bind/return, get/put laws) - 10 properties
- [x] `result` - Result/either type (functor/applicative/monad laws) - 22 properties
- [x] `combinators` - Basic combinators (identity, composition laws) - 21 properties
- [x] `logic` - Core logic programming (unification properties) - 18 properties

#### Tier 2: Parsing & Streams (Medium Impact)
- [ ] `parser` - Parser combinators (monad laws, alternation properties)
- [ ] `fsm` - Finite state machines (acceptance equivalence, determinization)
- [ ] `regex` - Regex engine (matching equivalence, compilation roundtrip)
- [ ] `stream` - Lazy streams (lazy eval preserves semantics, memoization)
- [ ] `zipper` - Zipper navigation (left/right inverse, focus preservation)

#### Tier 3: Control Structures
- [ ] `effects` - Effect system (handler composition, row polymorphism)
- [ ] `continuation` - Continuations (shift/reset laws, abort properties)
- [ ] `free` - Free monad (fold-free roundtrip, interpreter composition)
- [ ] `dsl` - DSL toolkit (interpreter correctness, trace properties)
- [ ] `units` - Unit system (dimensional analysis, conversion consistency)

#### Tier 4: Solvers & Constraints
- [ ] `sat` - SAT solver (satisfiability preservation, model correctness)
- [ ] `clp` - Constraint logic (propagation correctness, solution validity)
- [ ] `maxsat` - MaxSAT (optimality bounds, soft constraint handling)

### 4b: Rewrite Skill (30 exports, 0% covered)

- [ ] `rule` - Rewrite rules (pattern matching, substitution correctness)
- [ ] `engine` - Rewrite engine (confluence, termination, strategy correctness)

### 4c: Symbolic Skill (45 exports, 0% covered)

- [ ] `expr` - Expression AST (construction/deconstruction roundtrip)
- [ ] `diff` - Auto-diff (derivative correctness, chain rule verification)
- [ ] `simplify` - Expression simplification (semantic preservation)
- [ ] `integrate` - Symbolic integration (derivative of integral = original)
- [ ] `solve` - Equation solving (solution verification, completeness)
- [ ] `poly-canonical` - Polynomial forms (canonicalization uniqueness)
- [ ] `egraph-simplify` - E-graph simplification (equivalence preservation)

### 4d: Category Skill (102 exports, 0% covered)

- [ ] `adjunction` - Adjunctions (triangle identities, unit/counit laws)
- [ ] `monad-derivation` - Monad derivation (adjunction→monad correctness)
- [ ] `comonad` - Comonads (comonad laws, extract/duplicate)
- [ ] `kan-extension` - Kan extensions (universal property, computation)
- [ ] `natural-transform` - Natural transformations (naturality squares)
- [ ] `abstract-interp` - Abstract interpretation (soundness, Galois connection)
- [ ] `free-algebra` - Free algebras (universal property, fold correctness)

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
