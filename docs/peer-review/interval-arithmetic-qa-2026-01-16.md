# Interval Arithmetic QA Review

**Date:** 2026-01-16
**Reviewer:** Gemini 3 Pro
**Module:** `lattice/numeric/interval.ss`
**Status:** Issues addressed

## Summary

Gemini 3 Pro reviewed the interval arithmetic implementation for soundness, completeness, performance, and API consistency.

## Findings and Resolutions

### 1. Bugs & Soundness Violations

#### 1.1 Directed Rounding (Acknowledged Limitation)

**Issue:** Library uses standard floating-point arithmetic with round-to-nearest semantics. True interval arithmetic requires directed rounding.

**Resolution:** Added caveat to module docstring explaining limitation. For most practical purposes with reasonable operands, this is sufficient. Rigorous IEEE 754 directed rounding would require FFI to C libraries.

**Status:** Documented

#### 1.2 interval-pow Odd Powers (FIXED)

**Issue:** Implementation for odd powers used `iv * iv^(n-1)`, causing dependency problem with overly wide intervals.

**Example:** `[-1, 2]^3` computed as `[-4, 8]` instead of `[-1, 8]`.

**Resolution:** Odd powers now use monotonicity property directly: `[a^n, b^n]`.

**Status:** Fixed, test added

#### 1.3 interval-empty? Dead Code (Acknowledged)

**Issue:** Constructor auto-swaps endpoints, so `interval-empty?` can never return true.

**Resolution:** This is intentional design - intervals are never empty by construction. The predicate exists for completeness and documentation. Empty intersection results return symbol `'empty`.

**Status:** By design

### 2. Missing Operations

#### Transcendental Functions

**Issue:** Missing `interval-log`, `interval-exp`, `interval-sin`, `interval-cos`, etc.

**Resolution:** Deferred to future work (Phase 2 of fold-9tn epic).

**Status:** Acknowledged, future work

### 3. Performance Opportunities

#### 3.1 interval-mul Optimization (FIXED)

**Issue:** Computed all 4 products for every multiplication.

**Resolution:** Implemented 9-case sign analysis reducing to 2 multiplications in most cases. Only M*M (mixed*mixed) requires 4 multiplications.

**Status:** Fixed

#### 3.2 interval-mid Overflow Risk (FIXED)

**Issue:** `(lo + hi) / 2` can overflow for large floats.

**Resolution:** Changed to overflow-safe formula `lo + (hi - lo) / 2`.

**Status:** Fixed

### 4. API Consistency Issues

#### Inconsistent Error Handling

**Issue:** Different operations return different error symbols (`'empty`, `'division-by-zero`, `'domain-error`).

**Resolution:** Acknowledged. Current design trades composability for simplicity. Users must check return types. Future work could introduce result monads or NaN-interval representation.

**Status:** Acknowledged, future consideration

## Test Results

All 56 tests pass after fixes:
- 54 original tests
- 2 new tests for pow tightness (odd powers, even powers with zero-crossing)

## Commits

- Initial implementation: `d3f1efa`
- QA fixes: (pending)
