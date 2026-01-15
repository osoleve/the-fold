# Tangent Spaces QA Review

**Date:** 2026-01-15
**Reviewer:** Gemini 3 Pro Preview
**File:** `lattice/diffgeo/tangent.ss`

## Summary

Gemini conducted a QA review of the newly implemented tangent and cotangent spaces module. The implementation is mathematically correct but has some code quality issues.

## Findings

### 1. Bugs and Edge Cases

#### Missing Input Validation
**Lines 132, 146:** Functions like `tangent-add` do not validate that inputs are valid `tangent-vector?` structures. If an error object `(error ...)` is passed from a failed operation, accessor functions will return garbage.

**Recommendation:** Consider adding validation or relying on the type system.

#### Error Propagation in Pushforward
**Line 300:** If `f` maps a point to coordinates not covered by `target-chart`, the error may not propagate cleanly.

### 2. Mathematical Correctness

**Verified Correct:**
- Tangent vector covariant transformation (v' = Jv)
- Cotangent vector contravariant transformation (ω' = J⁻ᵀω)
- Numerical differential using central differences

### 3. Redundant Code (Fixed)

**Lines 549-573:** `matrix-vec-mul` and `matrix-transpose` were defined locally but already exist in `lattice/linalg/matrix.ss`. **Removed.**

### 4. Dead Code (Fixed)

**Lines 333-345:** The `pullback` function was a stub returning `(error pullback-requires-source-point)`. The correct implementation is `pullback-at`. **Removed.**

### 5. Missing Functionality (Future Work)

These are advanced features for future tickets:
- **Lie bracket** [X, Y] for vector fields
- **Exterior derivative** dω for k-forms
- **Tensor products** for metric tensors

## Actions Taken

1. Removed redundant `matrix-vec-mul` and `matrix-transpose` definitions
2. Removed stub `pullback` function
3. Added documentation in `docs/diffgeo-guide.md`

## Test Results

All 35 tests continue to pass after changes.
