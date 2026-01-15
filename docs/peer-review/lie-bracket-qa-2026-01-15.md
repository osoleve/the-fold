# Lie Bracket QA Review

**Date:** 2026-01-15
**Reviewer:** Gemini 3 Pro Preview
**File:** `lattice/diffgeo/tangent.ss` (lines 607-673)

## Summary

Gemini reviewed the Lie bracket implementation and found it **mathematically correct** but identified a **critical performance issue** with O(N²) vs O(N) field evaluations.

## Findings

### 1. Critical Performance Issue (Fixed)

**Original Problem:** The nested loops (k then i) caused redundant field evaluations:
- For each component k and direction i, the code called `(X p-plus)`, `(X p-minus)`, `(Y p-plus)`, `(Y p-minus)`
- Since `p-plus` depends only on `i`, this re-evaluated entire vector fields for every component
- For 3D: 3×3×4 = 36 field evaluations instead of 3×4 = 12

**Fix Applied:** Two-phase approach:
1. **Phase 1:** Compute all partial derivatives first (O(N) field evaluations)
2. **Phase 2:** Assemble bracket components algebraically (O(N²) arithmetic, but no field calls)

### 2. Chart Compatibility Edge Case (Fixed)

**Original Problem:** Code assumed `(X point)` returns a vector in the same chart passed to `lie-bracket`. If a vector field returns vectors in a different chart, raw component access would be incorrect.

**Fix Applied:** Added explicit chart compatibility check:
```scheme
[X-comps (tangent-vector-components
          (if (eq? (chart-name (tangent-vector-chart X-at-p)) (chart-name chart))
              X-at-p
              (tangent-change-chart X-at-p chart)))]
```

### 3. Memory Optimization (Partial)

**Suggestion:** Reuse scratch buffers instead of allocating new vectors in loops.

**Applied:** `coords-plus` and `coords-minus` are now allocated once and reused.

### 4. Missing Functionality (Noted)

**Suggestion:** Extract `vector-field-jacobian` as a helper function for computing dX/dx matrices.

**Status:** Deferred to future work — would be useful for connection coefficients.

## Test Results

All 40 tests pass after optimization.

## Performance Improvement

| Dimension | Old (field evals) | New (field evals) | Improvement |
|-----------|-------------------|-------------------|-------------|
| 2D | 2×2×4 = 16 | 2×4 = 8 | 2× |
| 3D | 3×3×4 = 36 | 3×4 = 12 | 3× |
| nD | n²×4 | n×4 | n× |
