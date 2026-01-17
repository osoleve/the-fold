# Affine Arithmetic QA Review

**Date:** 2026-01-17
**Reviewer:** Gemini 2.5 Pro
**Module:** `lattice/numeric/affine.ss`
**Status:** Critical bug fixed, issues addressed

## Summary

Gemini 2.5 Pro reviewed the affine arithmetic implementation for mathematical correctness, edge cases, bugs, code quality, and test coverage. One critical bug was identified and fixed.

## Findings and Resolutions

### 1. Bugs & Soundness Violations

#### 1.1 `affine-recip` Alpha Calculation (CRITICAL - FIXED)

**Issue:** The Chebyshev approximation formula for alpha mathematically simplified to zero.

**Details:**
- Original code: `alpha = (/ (+ (/ 1 a) (/ 1 b) (* beta (+ a b))) 2)`
- With `beta = -1/(ab)`, the term `(* beta (+ a b))` equals `-(1/a + 1/b)`
- Therefore: `alpha = ((1/a + 1/b) - (1/a + 1/b)) / 2 = 0`
- Impact: Reciprocal of positive numbers returned negative values (e.g., 1/2 → -0.5)

**Resolution:** Rewrote `affine-recip` using the same conservative linearization approach as `affine-exp` and `affine-log`. Now uses linearization at center with proper error bounds.

**Status:** Fixed, tests added

#### 1.2 `affine-sqrt` Suboptimal Bounds (Acknowledged)

**Issue:** The sqrt implementation uses a conservative bound that is approximately 2x wider than the optimal Chebyshev approximation.

**Analysis:** The implementation uses tangent line bounds plus full gap as error, rather than shifting halfway between tangent and secant.

**Resolution:** Acceptable for correctness. Could be optimized in future for tighter bounds.

**Status:** Acknowledged, future optimization

### 2. Missing Edge Cases

#### 2.1 Infinity and NaN Handling (Acknowledged)

**Issue:** No explicit checks for `+inf.0` or `+nan.0`. Behavior depends on Scheme's underlying numeric types.

**Status:** Acknowledged, documented limitation

#### 2.2 Floating Point Underflow in log/sqrt (Acknowledged)

**Issue:** Clamping to `1e-300` can produce massive slopes (~10^300) potentially causing overflow.

**Status:** Acknowledged, edge case documented

### 3. Code Quality

**Positive:**
- Excellent documentation explaining the Dependency Problem theory
- Idiomatic Scheme style
- Consistent variable naming (`x0`, `terms`, `iv`)
- Clear function signatures

**Addressed:**
- Misleading comments in `affine-recip` updated to match new implementation

### 4. Test Coverage Gaps (Addressed)

**Added tests:**
- `direct reciprocal of constant` - explicit test for `affine-recip`
- `reciprocal contains true reciprocal` - range test for reciprocal

**Remaining gaps (future work):**
- Tightness tests (verify bounds are within optimal factor)
- Tests for `affine-min`, `affine-max`, `affine-abs`
- Tests for `affine-sum`, `affine-product`, `affine-linear-combination`
- Corner case intervals near zero and very large numbers

## Test Results

All 26 tests pass after fixes:
- 24 original tests
- 2 new tests for reciprocal (Gemini QA recommendation)

## Commits

- Initial implementation: `568a1fb`
- QA fixes: (this commit)
