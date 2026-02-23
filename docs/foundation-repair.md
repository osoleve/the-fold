# Lattice Scheme Primitives Audit Report

**Scope:** 472 implementation `.ss` files in `lattice/`, excluding all `test-*.ss` files.

---

## Top-Line Numbers

| Metric | Pre-Migration | Current |
|---|---|---|
| Total mutation operations | ~4,700 | ~2,500 (hashtable ops eliminated, `vector-set!`/`set!` remain) |
| Chez hashtable creation sites | ~95 in ~69 files | 18 in 9 files (all documented exceptions) |
| Residual hashtable operations | ~537 | ~65 across 21 files (reads + exceptions) |
| `vector-set!` | 1,343 in 137 files | 1,412 in 152 files (unchanged, P3 target) |
| ~~Files falsely claiming `'total` purity~~ | ~~181 of 405~~ | **FIXED** (P1) |
| P0–P2 completion | — | **ALL DONE** |

---

## HIGH Severity (Breaks fuel/codegen/containment)

| Category | Count | Top Offenders |
|---|---|---|
| `vector-set!` | 1,412 in 152 files | `numeric/` (227), `statistics/` (211), `linalg/` (190), `diffgeo/` (115), `data/` (131) |
| ~~Chez hash tables~~ | ~~537 in 69 files~~ | **MIGRATED** (P1+P2) — 67 files migrated to HAMT. Residual: 18 creation sites in 9 files (see P2 exceptions below) |
| `set!` | ~795 in 222 files | `diffgeo/geodesics.ss` (26), `fp/meta/dsl.ss` (20), `data/graph/graph-community.ss` (16) |
| ~~`list-sort` (Chez built-in)~~ | ~~16 in 10 files~~ | **FIXED** (P0) — replaced with Fold-native `sort-by` |
| ~~`eval`~~ | ~~6 in 3 files~~ | **FIXED** (P0) — replaced with protocol dispatch |
| ~~`call/cc`~~ | ~~3 in 1 file~~ | **FIXED** (P0) — replaced with fold-based short-circuit |
| ~~`gensym`~~ | ~~1 in 1 file~~ | **FIXED** (P0) — deterministic `%__stage-fix-rec__` |
| `set-car!`/`set-cdr!` | 39 in 12 files | Original: `stream.ss`, `dag.ss`, `inspect.ss`. P2 mutable-box pattern added ~29 in `world.ss`, `constraint-graph.ss`, `world3d.ss`, `collision-detection3d.ss`, `tagless.ss` |

---

## MEDIUM Severity (Impure but contained)

| Category | Count | Notes |
|---|---|---|
| `format` | ~162 files | No Fold-native string formatter exists |
| `display`/`printf`/`write` | ~362 files | Beyond load banners & examples |
| `guard` (exceptions) | ~49 files | Non-local control flow |
| `box`/`unbox`/`set-box!` | 41 in ~10 files | autodiff tapes, egraph union-find |
| File I/O / `read` | 19 in 9 files | Mostly `meta/` and parser code |
| `make-parameter` | 2 in 1 file | `game-theory/mechanism.ss` |

---

## Recommended Remediation

### P0 — Point fixes (immediate): DONE

All P0 items completed in `8c2739e0`:

- ~~Replace 16 `list-sort` calls with Fold-native `sort-by`~~ (16/16)
- ~~Replace 3 `call/cc` in `collection-protocol.ss` with explicit short-circuit fold~~ (3/3, note: fold still traverses O(n) — true early termination requires `coll-fold-while` protocol, tracked as future work)
- ~~Replace 6 `eval` calls with protocol dispatch~~ (6/6)
- ~~Replace 1 `gensym` with deterministic name generation~~ (`staging.ss` — `%__stage-fix-rec__`)
- ~~Fix 1 `string-set!`~~ (1/1)

### P1 — Architecture (medium effort): DONE

All P1 items completed across `a80dd309`, `01fa299a`, `7c49a91f`, `9fe23fb2`:

- ~~Correct 181 false `'total` purity annotations~~ (181/181 → `'partial`)
- ~~Migrate tiles SDK from hash tables to `avl-tree/dict`~~ — migrated to HAMT instead (more appropriate for set membership and keyed lookup patterns):
  - `tiles/core.ss`, `tiles/units.ss`, `tiles/turns.ss`, `tiles/triangle.ss` — dict-based migration (`01fa299a`)
  - `tiles/render.ss` — overlay sets → HAMT (`9fe23fb2`)
  - `tiles/pathfinding.ss` — BFS/Dijkstra/A*/reachability → threaded HAMTs (`9fe23fb2`)
  - `tiles/visibility.ss` — FOV → three threaded HAMTs (`9fe23fb2`)
  - `tiles/topology-analysis.ss` — coord sets + edge dedup → HAMT, DFS vectors kept as-is (`9fe23fb2`)
- ~~Add a Fold-native persistent hash map (HAMT) for e-graph/autodiff~~ — implemented in `lattice/data/hamt.ss` (`01fa299a`), 67 tests including true hash-collision coverage

**Review fixes applied:** `hamt-merge-with` #f-value bug (`9fe23fb2`), staging.ss name capture hardening (`9fe23fb2`).

### P2 — Structural (high effort): HASHTABLE MIGRATION DONE

Hashtable migration completed across `e30379ea` (round 1: 52 files) and `7872c9ae` (round 2: 15 files):

- ~~Migrate remaining ~95 `make-hashtable` calls across ~40 non-tiles lattice files to HAMT~~ — 67 files migrated, ~200 hashtable operations replaced
  - Round 1 (`e30379ea`): fp/, autodiff/, egraph/ (eclass, union-find), data/graph/, topology/, meta/, query/, game-theory/, sim/, info/, dsl/, optics/, optimization/, linalg/, physics/classical/ (collision), geometry/voronoi
  - Round 2 (`7872c9ae`): egraph/ (cost, extract, dirty set), physics/classical/ (world, constraint-graph), physics/classical3d/ (world3d, collision-detection3d), geometry/ (mesh-gen, mesh-topology, mesh-sdf), numeric/fem
  - Added `hamt-first-key`/`hamt-first-entry` for O(log32 N) single-entry access
  - Fixed `egraph-pop-dirty!` O(N²) regression (flagged by Gemini QA)

**Residual hashtable inventory** (18 creation sites in 9 files, 65 total ops across 21 files):

| Category | Creation sites | Files | Rationale |
|---|---|---|---|
| eq-identity (`make-eq-hashtable`) | 6 | mesh-gen.ss (5), mesh-sdf.ss (1) | True pointer identity for triangle/vertex objects — structural equality doesn't apply |
| Core-boundary interop | 4 | sparse-autodiff.ss (3), higher-order-diff.ss (1) | `backward()` returns Chez hashtables; must use `hashtable-ref` to read core return values |
| Core-return readers (no creation) | — | profiling.ss, traced-optics.ss, sketch.ss, engine.ss, dsl.ss | Read-only `hashtable-ref` on core-returned gradient tables |
| Egraph hot-path | 2 | egraph.ss | Hashcons table (mutable by design) + rebuild visited set (ephemeral) |
| Parser cache | 2 | parser.ss | Packrat-style memo table, inherently mutable |
| Meta tooling | 1 | manifest-migration.ss | Schema migration registry, not lattice computation |
| Test mocks | 2 | test-module-manifest.ss (1), test-block-optics.ss (1) | Test infrastructure only |

**Trade-off note:** P2 mutable-box pattern (HAMT stored in `(cons hamt-empty '())`, mutated via `set-car!`) increased `set-car!` usage from 8→39. These are contained to module-internal state in imperative-API files (world.ss, constraint-graph.ss, world3d.ss, collision-detection3d.ss). The alternative — rewriting public APIs to pure functional threading — is a larger refactor tracked as future work.

Remaining P2 items:

- ~~Define Fold-native vector combinators with fuel instrumentation~~ **DONE** — `vec-tabulate` and `vec-scan` macros added to `lattice/linalg/iteration.ss`. Both expand to efficient `do`/named-`let` loops with encapsulated `vector-set!`. 25/25 iteration tests pass including new combinator coverage.
- ~~Move `display`/`printf` from library bodies to boundary wrappers~~ **ASSESSED** — Load banners removed from ~22 non-meta lattice files (~150 printf lines total). Meta-tools, examples, and benchmarks retained (their purpose IS output). Architecture already correct: rendering functions return strings, boundary provides display wrappers (see `animation-io.ss` pattern).

**Pilot migrations** validating the combinators:
- `lattice/numeric/finite-diff.ss` — 13 build patterns → `vec-tabulate` + `range-fold` (50→28 `vector-set!`, 44% reduction). 10/10 tests pass.
- `lattice/statistics/timeseries/exponential.ss` — SES/Holt/Holt-Winters scan patterns → `vec-scan` + `vec-tabulate` (33→10 `vector-set!`, 70% reduction). 202/202 statistics tests pass.

### P3 — Deep numerical rewrite (very high effort):

Combinators are now available for mass migration. Three patterns identified:

| Pattern | Combinator | Estimated files | Example |
|---|---|---|---|
| Build (index→value) | `vec-tabulate` | ~80 files | `finite-diff.ss`, `sparse.ss`, `voronoi.ss` |
| Scan (sequential dep) | `vec-scan` | ~20 files | `exponential.ss`, `markov.ss`, `digital-filters.ss` |
| In-place update | Keep imperative | ~50 files | Iterative solvers, convergence algorithms |

Remaining scope:
- Convert ~1,400 `vector-set!` across ~150 files using `vec-tabulate`/`vec-scan` where applicable
- Primary clusters: `numeric/` (227), `statistics/` (211), `linalg/` (190), `diffgeo/` (115), `data/` (131)
- Eliminate ~795 bare `(set! ...)` across 222 files — many are loop accumulators convertible to `fold-left`/named `let`/`range-fold`