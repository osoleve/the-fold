# QA Review: b2a16ace — Tropical Semiring Module

**Date**: 2026-02-16
**Commit**: `b2a16ace` — feat(algebra): add tropical semiring module
**Reviewer**: Claude Opus 4.6

## Files Reviewed

- `lattice/algebra/tropical.ss` (467 lines)
- `lattice/algebra/test-tropical.ss` (382 lines)
- `lattice/algebra/ring.ss` (convention reference)
- `lattice/linalg/matrix.ss` (convention reference)

---

## P0: Must Fix

### P0-1: `tropical-eigenvalue` is hardcoded for min-plus semantics

The function accepts a generic `Semiring` parameter but the Karp's formula extraction phase uses `min`, `max`, and ordinary subtraction/division on the accumulated values. This **only works correctly for the min-plus semiring**, where `mul = +` and the cycle-mean formula involves conventional subtraction and division.

For `max-plus-semiring`, the formula should find the **maximum** cycle mean, which means the `min`/`max` in the Karp extraction should be **swapped**. Passing `max-plus-semiring` silently produces wrong answers.

Either restrict the type signature to only accept min-plus, or parameterize the comparison operators.

### P0-2: `tropical-critical-edges` assumes `mul = +`

Uses `+` directly instead of `(semiring-mul sr)`, and conventional `/` division. The semiring parameter is cosmetic.

### P0-3: `tropical-critical-edges` fails when `eigenval = 0`

The guard `(not (zero? eigenval))` means that if the minimum cycle mean is 0 (a valid and common case), **all critical edges are silently dropped**. Should handle `eigenval = 0` explicitly: an edge is critical iff `cycle-weight = 0`.

### P0-4: `doc 'purity 'total` is questionable

`tropical-eigenvalue` and `tropical-critical-edges` use `set!` on let-bound variables (not just `vector-set!` on fresh allocations). Whether this violates `'total` depends on how strictly The Fold interprets purity, but the fuel model and potential Rust codegen need to see through mutation. Could be rewritten to use `let loop` accumulation.

---

## P1: Should Fix

### P1-1: No dimension validation in `tropical-matrix-mul`

If `A` is 3×2 and `B` is 5×3, the function reads out-of-bounds indices. `matrix-ref` returns error values that get fed into arithmetic, producing garbage.

### P1-2: No dimension validation in `tropical-matrix-add`

Same — mismatched dimensions produce silent garbage.

### P1-3: `tropical-matrix-power` doesn't verify the matrix is square

### P1-4: `tropical-matrix-closure` doesn't verify the matrix is square

### P1-5: `tropical-eigenvalue` doesn't verify the matrix is square

### P1-6: `tropical-poly-eval` is O(d²) instead of O(d)

`tropical-power sr x i` recomputes `x^i` from scratch per term. A Horner-like running accumulator would be O(d).

### P1-7: `trim-hull` is O(n) per call due to `(length hull)`

`(length hull)` traverses the entire list each time. Turns the convex hull from amortized O(n) to O(n²). Use `(null? (cdr hull))` instead of `(< (length hull) 2)`.

### P1-8: `adjacency->tropical` doc comment is min-plus-specific

The function correctly uses `(semiring-zero sr)` internally, but the doc says "+inf.0" which is only correct for min-plus.

### P1-9: `newton-polygon` and `tropical-poly-roots` assume min-plus

No semiring parameter. Lower convex hull gives correct roots for min-plus only; max-plus needs upper hull.

---

## P2: Nice to Have

### P2-1: Misleading comments in `trim-hull`

"Clockwise/counterclockwise" language is confusing. Clearer as "b is above/below the line a→p".

### P2-2: No `tropical-matrix?` predicate

Both tropical and regular matrices are `(matrix rows cols data)`. Fine for now, but noted.

### P2-3: `tropical-critical-edges` uses `set!` + `cons` + `reverse`

Could be a `let loop` with accumulator. Minor style point.

### P2-4: Module header is missing the bootstrap guard

`(unless (top-level-bound? 'require) (load "core/lang/module.ss"))` — but sibling modules also lack it, so consistent with neighborhood.

---

## Test Coverage Gaps

| Gap | Description |
|-----|-------------|
| 1 | No test for `tropical-eigenvalue` with max-plus semiring (would expose P0-1) |
| 2 | No test for `tropical-eigenvalue` on 1×1 matrix |
| 3 | No test for all-infinity matrix |
| 4 | **Zero test coverage on `tropical-critical-edges`** |
| 5 | No test for `newton-polygon` with empty list |
| 6 | No test for `newton-polygon` with single coefficient |
| 7 | No test for `tropical-poly-eval` with empty coefficients |
| 8 | No negative tests for dimension mismatch |
| 9 | No test for `tropical-matrix-power` with k=0 |
| 10 | No test for NaN propagation |
| 11 | No test for negative edge weights in eigenvalue |

---

## Summary

| Priority | Count | Key Themes |
|----------|-------|------------|
| P0 | 4 | Generic semiring API with min-plus-only internals; eigenval=0 bug; purity claim |
| P1 | 9 | Missing dimension guards; asymptotic waste in poly eval and hull; max-plus gaps |
| P2 | 4 | Comments, style, missing predicates |
| Test gaps | 11 | Zero coverage on critical-edges; no max-plus eigenvalue test; no edge cases |

The dominant issue is the **leaky abstraction**: functions accept a `Semiring` parameter but internally assume min-plus semantics (conventional `+`, `-`, `/`, `min`, `max`). The cleanest fix is probably to either (a) restrict eigenvalue/critical-edge functions to `min-plus-semiring` explicitly, or (b) add a `semiring-compare` field that parameterizes the optimization direction.

---
*Reviewed by Claude Opus 4.6 — post-commit QA*
