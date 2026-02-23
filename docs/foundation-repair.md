# Lattice Scheme Primitives Audit Report

**Scope:** 472 implementation `.ss` files in `lattice/`, excluding all `test-*.ss` files.

---

## Top-Line Numbers

| Metric | Count |
|---|---|
| Total mutation operations | ~4,700 |
| Files with at least one mutation | 181+ |
| ~~Files falsely claiming `'total` purity~~ | ~~181 of 405~~ **FIXED** (P1) |
| Distinct violation categories | 14+ |

---

## HIGH Severity (Breaks fuel/codegen/containment)

| Category | Count | Top Offenders |
|---|---|---|
| `vector-set!` | 1,343 in 137 files | `numeric/` (227), `statistics/` (211), `linalg/` (190), `diffgeo/` (115), `data/` (131) |
| ~~Chez hash tables~~ | ~~537 in 69 files~~ | **MIGRATED** (P1+P2) — 67 files migrated to HAMT, 6 eq-identity exceptions retained, ~17 core-boundary interop exceptions |
| `set!` | 468 in 112 files | `diffgeo/geodesics.ss` (26), `fp/meta/dsl.ss` (20), `data/graph/graph-community.ss` (17) |
| ~~`list-sort` (Chez built-in)~~ | ~~16 in 10 files~~ | **FIXED** (P0) — replaced with Fold-native `sort-by` |
| ~~`eval`~~ | ~~6 in 3 files~~ | **FIXED** (P0) — replaced with protocol dispatch |
| ~~`call/cc`~~ | ~~3 in 1 file~~ | **FIXED** (P0) — replaced with fold-based short-circuit |
| ~~`gensym`~~ | ~~1 in 1 file~~ | **FIXED** (P0) — deterministic `%__stage-fix-rec__` |
| `set-car!`/`set-cdr!` | 8 in 6 files | `fp/data/stream.ss`, `numeric/fem.ss`, `meta/dag.ss` |

---

## MEDIUM Severity (Impure but contained)

| Category | Count | Notes |
|---|---|---|
| `format` | 345 in 93 files | No Fold-native string formatter exists |
| `display`/`printf`/`write` | ~745 in function bodies | Beyond load banners & examples |
| `guard` (exceptions) | 33 in 18 files | Non-local control flow |
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
  - Documented eq-identity exceptions: mesh-gen.ss (5), mesh-sdf.ss (1) — true pointer identity semantics
  - Core-boundary interop exceptions: sparse-autodiff (3), higher-order-diff (1), egraph hashcons (1), parser cache (2)

Remaining P2 items (not yet started):

- Define Fold-native vector combinators with fuel instrumentation
- Move `display`/`printf` from library bodies to boundary wrappers

### P3 — Deep numerical rewrite (very high effort):

- Convert 1,343 `vector-set!` + 1,269 `do` patterns to Fold-native vector ops — prerequisite for Rust codegen