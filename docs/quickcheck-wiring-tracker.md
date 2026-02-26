# QuickCheck Wiring Tracker

Date: 2026-02-26
Source: lattice export audit vs QuickCheck-backed test files

## Goal

Track lattice exports that are not yet wired into the QuickCheck test system, and close gaps in priority order.

## Baseline Coverage Snapshot

`CoveredByQCRefs` is export symbols referenced by files that `(require 'quickcheck)`; this is a wiring signal, not a formal proof of semantic coverage.

| Skill | Exports | CoveredByQCRefs | Missing | CoveragePct |
|---|---:|---:|---:|---:|
| algebra | 412 | 14 | 398 | 3.4 |
| data | 476 | 40 | 436 | 8.4 |
| fp | 787 | 1 | 786 | 0.1 |
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
- [ ] `chase-lev-deque`
- [ ] `collection-protocol`
- [ ] `collection-utils`
- [ ] `community-homology`
- [ ] `data-structures`
- [x] `hamt`
- [ ] `kdtree`
- [ ] `quadtree`

### optics

- [x] `bidirectional`
- [x] `block-optics`
- [x] `optics`
- [x] `profunctor-optics`

### algebra

- [ ] `field-ext`
- [ ] `galois`
- [ ] `group`
- [ ] `module`
- [ ] `multivariate-groebner`
- [ ] `poly-bridge`
- [ ] `polynomial`
- [ ] `ring-field`
- [ ] `tropical`
- [ ] `tropical-graph`

### fp

- [x] `markov`
- [x] `protocol`
- [x] `protocol-bundle`
- [x] `protocol-introspect`
- [x] `templates`

## Priority 2: Fill Export Gaps In Already-Wired Skills

### number-theory (29 missing exports)

- [ ] `coprime?`
- [ ] `trial-division`
- [ ] `lcm`, `lcm*`, `gcd*`
- [ ] `mod-sqrt`, `mod-sqrt-both`, `tonelli-shanks`, `quadratic-residue?`
- [ ] `montgomery-setup`, `montgomery-reduce`, `montgomery-mult`, `montgomery-expt`, `to-montgomery`, `from-montgomery`
- [ ] `limbs-add`, `limbs-sub`, `limbs-shift`, `limbs-split`, `limbs-split3`, `limbs-pad-to`, `limbs-div-small`, `limbs-compare`, `limb-scale`, `limbs-square-schoolbook`, `signed-add`
- [ ] `set-karatsuba-threshold!`, `set-toom3-threshold!`, `get-multiply-thresholds`

### quickcheck framework self-tests

These exports exist but are not directly referenced in QuickCheck self-tests yet:

- [ ] `assert-property`
- [ ] `format-qc-failure`
- [ ] `parse-qc-opts`, `qc-opt`
- [ ] `size-at-test`
- [ ] `shrink-loop`
- [ ] `qc-failure-seed`, `qc-failure-shrink-steps`

### qc-generators coverage gaps

- [ ] `make-gen`, `gen?`, `gen-fn`
- [ ] `gen-sized`, `gen-scale`
- [ ] `gen-float`, `gen-vector`, `gen-from-random`

### qc-shrink coverage gaps

- [ ] `shrink-float`
- [ ] `remove-chunks`, `drop-at-most`, `shrink-elements`
- [ ] `shrink-map`, `shrink-one-of`

## Priority 3: Large Surface Areas Still Mostly Uncovered

Target these families next after Priority 1 and 2:

- [ ] `linalg`: iterative solvers, graph-laplacian, blocked/dependent/numeric-instance exports
- [ ] `random`: bayesian and variational inference exports
- [ ] `optics`: core/profunctor/block migration APIs
- [ ] `algebra`: group/ring/field/polynomial/multivariate and bridge layers
- [ ] `fp`: templates/protocol layers and category submodules

## Definition Of Done

- [ ] Every module listed in Priority 1 has at least one QuickCheck-backed property suite.
- [ ] Priority 2 missing-export lists are reduced to zero or explicitly marked as intentionally exempt.
- [ ] `test-all.ss` includes all new QuickCheck property files.
- [ ] This tracker table is refreshed after each merge.

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
