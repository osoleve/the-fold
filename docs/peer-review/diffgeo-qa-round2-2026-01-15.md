# Differential Geometry QA - Round 2

**Date:** 2026-01-15
**Reviewer:** Gemini 3 Pro
**Files Reviewed:** `lattice/diffgeo/charts.ss`, `lattice/linalg/matrix-decomp.ss`

## Summary

Second QA pass following implementation of fold-zxlo fixes (domain verification, LU-based determinant).

## Findings

### 1. `transition-apply` Source Domain Verification

**Location:** `lattice/diffgeo/charts.ss:117-129`
**Severity:** Medium
**Status:** Documented (by design)

**Issue:** The function verifies the computed point lies in the target chart's domain but does not verify it lies in the source chart's domain. If input coords are invalid, `chart-inverse-map` might return garbage.

**Resolution:** This is by design - `transition-apply` assumes valid input coordinates. The function validates *reachability* of the target chart from a point, not validity of the input. Added documentation note to `diffgeo-guide.md`:

> `transition-apply` verifies that the intermediate point lies in the target chart's domain

### 2. `jacobian-determinant` Error Propagation

**Location:** `lattice/diffgeo/charts.ss:305-328`
**Severity:** High
**Status:** FIXED

**Issue:** The fallback to `matrix-det` for n > 3 could return error objects. Consumer `transition-smooth?` calls `(< (abs det) 1e-10)` which crashes if `det` is an error list.

**Fix Applied:** Modified `transition-smooth?` to handle error returns:

```scheme
(cond
 [(and (pair? det) (eq? (car det) 'error)) #f]  ; Error → not smooth
 [(< (abs det) 1e-10) #f]  ; Singular Jacobian → not smooth
 [else (loop (cdr coords))])
```

### 3. `permutation-sign` Efficiency

**Location:** `lattice/linalg/matrix-decomp.ss:284-300`
**Severity:** Low
**Status:** Deferred

**Issue:** Implementation counts inversions in O(n²). Could be O(1) if `matrix-lu` tracked swap count.

**Resolution:** Acceptable for current use cases. LU decomposition dominates at O(n³), so inversion counting is not the bottleneck. Created note for future optimization if large matrices become common.

### 4. `matrix-det` Error Format Coupling

**Location:** `lattice/linalg/matrix-decomp.ss:302-326`
**Severity:** Low
**Status:** Acknowledged

**Issue:** Singularity check relies on `(eq? (cadr result) 'singular-matrix)` - tightly coupled to `matrix-lu` error format.

**Resolution:** This coupling is acceptable since both functions are in the same module and co-evolve. A structured result type could be added later if the module grows.

## Test Coverage

- **Branch cut tests:** 5 tests covering polar, spherical, cylindrical exclusions
- **Domain verification:** Test verifying origin → polar returns error
- **4x4 determinant:** Test using diagonal matrix (det = 120)
- **Transition smoothness:** 2 tests for polar ↔ cartesian transitions

All 37 tests pass.

## Recommendations for Future Work

1. Consider structured result types for matrix operations if the linalg module expands
2. Add optional source domain verification flag to `transition-apply` for debug builds
3. Track permutation sign in `matrix-lu` for O(1) sign computation if needed
