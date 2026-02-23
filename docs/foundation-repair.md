# Lattice Scheme Primitives Audit Report

**Scope:** 472 implementation `.ss` files in `lattice/`, excluding all `test-*.ss` files.

---

## Top-Line Numbers

| Metric | Pre-Migration | Current |
|---|---|---|
| Total mutation operations | ~4,700 | ~2,500 (hashtable ops eliminated, `vector-set!`/`set!` remain) |
| Chez hashtable creation sites | ~95 in ~69 files | 18 in 9 files (all documented exceptions) |
| Residual hashtable operations | ~537 | ~65 across 21 files (reads + exceptions) |
| `vector-set!` | 1,343 in 137 files | ~893 in ~110 files (P3 in progress) |
| ~~Files falsely claiming `'total` purity~~ | ~~181 of 405~~ | **FIXED** (P1) |
| P0–P2 completion | — | **ALL DONE** |
| P3 `vector-set!` eliminated | — | ~316 across 47 files (pilots + batches 1-7) |

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

### P3 — Deep numerical rewrite (very high effort): IN PROGRESS

Combinators are now available for mass migration. Three patterns identified:

| Pattern | Combinator | Estimated files | Example |
|---|---|---|---|
| Build (index→value) | `vec-tabulate` | ~80 files | `finite-diff.ss`, `sparse.ss`, `voronoi.ss` |
| Scan (sequential dep) | `vec-scan` | ~20 files | `exponential.ss`, `markov.ss`, `digital-filters.ss` |
| In-place update | Keep imperative | ~50 files | Iterative solvers, convergence algorithms |

**Batch 1** (`80cf0497`): 5 files, 79 `vector-set!` eliminated:

| File | Before | After | Technique |
|---|---|---|---|
| `diffgeo/symbolic-metrics.ss` | 41 | 0 | Nested `vec-tabulate` + `range-fold` for Christoffel tensor assembly |
| `statistics/core/design-matrix.ss` | 19 | 4 | `vec-tabulate` with flat 2D indexing (`quotient`/`remainder`) |
| `statistics/timeseries/forecast.ss` | 12 | 3 | `vec-tabulate` for naive/drift/seasonal/mean forecasts |
| `statistics/timeseries/differencing.ss` | 11 | 2 | `vec-tabulate` + `vec-scan` for diffs/transforms/integrate |
| `linalg/vec.ss` | 8 | 3 | `vec-tabulate` for take/drop/slice/range/linspace |

Residual `vector-set!` in batch 1 (12 total): multi-pass accumulation (combine-forecasts), reverse scan (integrate-from-last), sparse conditional fills (dummy/one-hot encode), multi-region copy (vec-append), single-element set (vec-unit).

**Batch 2** (`17bd0ed5`): 5 files, 30 `vector-set!` eliminated + polynomial-features bug fix:

| File | Before | After | Technique |
|---|---|---|---|
| `statistics/timeseries/ar.ss` | 16 | 14 | `vec-tabulate` for center-series, ar-forecast-se |
| `statistics/timeseries/ma.ss` | 24 | 11 | `vec-scan` for exponential MA; `vec-tabulate` for gammas/forecasts |
| `diffgeo/tangent.ss` | 12 | 8 | `vec-tabulate` + `range-fold` for lie-bracket derivatives/result |
| `diffgeo/curvature.ss` | 16 | 10 | Nested `vec-tabulate` + `range-fold` for christoffel-symbols |
| `linalg/matrix.ss` | 17 | 12 | `vec-tabulate` for row/col/diagonal/map/map2 |

QA fix: `polynomial-features` generated powers up to x^(degree+1) instead of x^degree.

**Batch 3** (`c05e578d`): 6 files, 46 `vector-set!` eliminated:

| File | Before | After | Technique |
|---|---|---|---|
| `numeric/window-functions.ss` | 8 | 0 | All 8 window builders → `vec-tabulate` |
| `numeric/convolution.ss` | 14 | 2 | Direct/FFT convolution + correlate → `vec-tabulate` + `range-fold` |
| `numeric/spectral-analysis.ss` | 14 | 7 | apply-window, magnitude/power-spectrum → `vec-tabulate` |
| `data/graph/graph-matrix.ss` | 32 | 23 | degree-matrix, complete/cycle/path/star-graph → `vec-tabulate` |
| `data/graph/graph-layout.ss` | 6 | 0 | Force-directed layout → `vec-tabulate` + `range-fold` |
| `data/graph/pagerank.ss` | 4 | 0 | Transition/Google matrix → `vec-tabulate` |

**Batch 4** (`107f328f`): 7 files, 48 `vector-set!` eliminated:

| File | Before | After | Technique |
|---|---|---|---|
| `numeric/spectral-pde.ss` | 33 | 19 | Fourier wavenumbers/diff/heat/advection, Chebyshev nodes → `vec-tabulate` |
| `numeric/dft.ss` | 16 | 8 | bit-reverse-copy, real/complex conversions, spectra → `vec-tabulate` |
| `data/graph/random-graphs.ss` | 16 | 8 | Erdos-Renyi, Barabási-Albert clique, Watts-Strogatz ring → `vec-tabulate` |
| `numeric/polynomial.ss` | 9 | 3 | monomial, scale, derivative, strip-zeros, linspace → `vec-tabulate` |
| `numeric/pde-time.ss` | 9 | 4 | vec-add/sub/scale/madd, forward-euler-mass-step → `vec-tabulate` |
| `numeric/fft-convolve.ss` | 5 | 0 | pointwise-mul, conj-mul, vec-reverse, result extraction → `vec-tabulate` |
| `data/graph/graph-filtration.ss` | 2 | 0 | vertex-birth times → `vec-tabulate` |

**Cluster survey** (BUILD+SCAN convertibility):

| Cluster | Total `vector-set!` | Convertible | Imperative |
|---|---|---|---|
| statistics/ | ~148 | ~54 | ~94 |
| linalg/ | ~163 | ~32 | ~131 |
| diffgeo/ | ~115 | ~50 | ~65 |
| numeric/ | ~228 | ~70 | ~158 |
| data/ | ~132 | ~88 | ~44 |

**Batch 5** (`d8cf93d1`): 7 files, 28 `vector-set!` eliminated:

| File | Before | After | Technique |
|---|---|---|---|
| `numeric/digital-filters.ss` | 27 | 15 | All window/FIR builders, freqz, magnitude/phase response → `vec-tabulate` |
| `numeric/interpolate.ss` | 12 | 8 | Spline M-vector + segment coefficients → `vec-tabulate` |
| `numeric/wavelet.ss` | 7 | 1 | QMF, reverse-filter, convolve-downsample, IDWT, threshold → `vec-tabulate` |
| `data/graph/centrality.ss` | 13 | 10 | closeness, eigenvector-avg, betweenness normalization → `vec-tabulate` |
| `data/graph/shortest-path.ss` | 3 | 2 | Bellman-Ford distance init → `vec-tabulate` |
| `data/graph/spectral-community.ss` | 5 | 4 | Bipartition label init → `vec-tabulate` |

**Batch 6** (`99fcc8fa`): 7 files, 38 `vector-set!` eliminated:

| File | Before | After | Technique |
|---|---|---|---|
| `statistics/regression/glm.ss` | 15 | 3 | mu/eta/W/z/predict/classify/se/pvalues/residuals → `vec-tabulate` |
| `data/graph/graph-community.ss` | 31 | 27 | label/union-find/ILP init → `vec-tabulate` |
| `linalg/iterative-solvers.ss` | 21 | 14 | Jacobi x-new, GMRES g-vec, preconditioner → `vec-tabulate` + `range-fold` |
| `game-theory/coop-games.ss` | 19 | 14 | nucleolus/permutation/Shapley init → `vec-tabulate` |
| `numeric/fem.ss` | 19 | 14 | mesh trim, vec-add/sub/scale/div-pointwise → `vec-tabulate` |
| `linalg/sparse.ss` | 37 | 35 | identity/diagonal matrix, CSR matvec → `vec-tabulate` + `range-fold` |

**Batch 7** (`c67bf0d0`): 10 files, 47 `vector-set!` eliminated:

| File | Before | After | Technique |
|---|---|---|---|
| `sim/dynamics/bifurcation.ss` | 16 | 5 | bordered system/matrix construction, column/subvector extraction → `vec-tabulate` |
| `optimization/ilp.ss` | 18 | 4 | Gomory cuts, tableau init, vec arithmetic → `vec-tabulate` + `range-fold` |
| `statistics/core/diagnostics.ss` | 12 | 1 | hat-matrix diagonal, all residual types, Cook's, DFFITS, VIF → `vec-tabulate` + `range-fold` |
| `sim/dynamics/stability.ss` | 16 | 13 | augment-matrix, inverse extraction → `vec-tabulate` |
| `diffgeo/forms.ss` | 17 | 15 | k-form-basis, vec-copy → `vec-tabulate` |
| `diffgeo/geodesics.ss` | 23 | 21 | geodesic-acceleration, parallel-transport-derivative → `vec-tabulate` + `range-fold` |
| `linalg/matrix-solvers.ss` | 19 | 17 | unit vector construction in matrix-inverse → `vec-tabulate` |
| `statistics/hypothesis/chi-squared.ss` | 4 | 3 | expected frequencies → `vec-tabulate` |
| `statistics/hypothesis/t-test.ss` | 1 | 0 | paired differences → `vec-tabulate` |
| `statistics/hypothesis/f-test.ss` | 1 | 0 | Levene deviations → `vec-tabulate` |

**P3 running totals:** 316 `vector-set!` eliminated across 47 files (batches 1-7). Down from ~1,412 → ~893.

Remaining scope:
- ~893 `vector-set!` across ~110 files. The vast majority are legitimately imperative: iterative solvers (Gauss-Seidel, SOR, CG, GMRES), ODE integrators (RK4, Dormand-Prince), autodiff tapes, in-place algorithms (FFT butterflies, Fisher-Yates shuffle, Gaussian elimination), and scatter/accumulate patterns.
- Further conversion would require new combinators (e.g., `vec-scatter`, `vec-accumulate`) or API-level refactors to pure functional threading — tracked as future work.
- Eliminate ~795 bare `(set! ...)` across 222 files — many are loop accumulators convertible to `fold-left`/named `let`/`range-fold`