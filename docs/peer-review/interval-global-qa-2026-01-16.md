# Interval Global Optimization QA Review

**Date:** 2026-01-16
**Reviewer:** Gemini 3 Pro
**Module:** `lattice/optimization/interval-global.ss`

## Summary

Gemini 3 Pro reviewed the new interval branch-and-bound global optimization module for correctness, edge cases, performance opportunities, and API design.

## Findings Addressed

### 1. API Simplification (Major - Addressed)

**Issue:** Required two functions (`f-interval` and `f-scalar`) which is error-prone and burdensome.

**Resolution:** Made `f-scalar` optional. If not provided, the optimizer evaluates `f-interval` on point intervals (singletons) to compute scalar values. This uses the inclusion property: `f([x,x]) = [f(x), f(x)]`.

```scheme
;; New API: f-scalar optional
(interval-minimize f-interval box criteria)           ; Auto-derives scalar
(interval-minimize f-interval box criteria f-scalar)  ; Explicit scalar (faster)
```

### 2. Enclosure Property Tests (Minor - Addressed)

**Issue:** Tests verified minimum values but not that the true global minimum point was contained in candidate boxes.

**Resolution:** Added "Enclosure Property" test group with 4 new tests verifying:
- Origin in candidates for x²
- Origin in candidates for 2D Sphere
- Point (1,1) in candidates for Rosenbrock
- `ior-solution-box` contains true minimum

### 3. Result Hull Helper (Minor - Addressed)

**Issue:** No convenient way to get a single "conservative answer" box.

**Resolution:** Added `ior-solution-box` which returns the interval hull (union) of all candidate boxes.

```scheme
(define sol-box (ior-solution-box result))  ; Single box enclosing all candidates
```

## Findings Not Addressed (Future Work)

### 4. Monotonicity/Newton Steps (Major - Deferred)

**Recommendation:** Add gradient-based pruning or interval Newton steps for quadratic convergence.

**Rationale:** This is an advanced optimization that would significantly increase complexity. The current implementation provides correct results; this would improve performance for smooth functions. Tracked as future work.

### 5. Vector vs List for Boxes (Minor - Deferred)

**Recommendation:** Use vectors instead of lists for O(1) access.

**Rationale:** For typical dimensions (D ≤ 3), list overhead is negligible. Worth revisiting if high-dimensional optimization becomes a use case.

### 6. Unbounded Interval Handling (Minor - Deferred)

**Recommendation:** Guard against infinite intervals.

**Rationale:** Users are expected to provide bounded search domains. Could add explicit check if issues arise.

## Test Results

After changes: **22/22 tests pass**

New tests added:
- `x² with explicit f-scalar (backward compat)` - verifies old API still works
- `ior-solution-box returns hull of candidates` - tests new helper
- 4 enclosure property tests - verifies fundamental guarantees

## API Changes Summary

| Function | Change |
|----------|--------|
| `interval-minimize` | `f-scalar` now optional (4th arg) |
| `interval-maximize` | `f-scalar` now optional (4th arg) |
| `interval-find-minimum` | Removed `f-scalar` arg |
| `ior-solution-box` | **New** - returns hull of candidates |
| `box-hull` | **New** - internal helper |
| `point->box` | **New** - convert point to singleton box |
| `eval-at-point` | **New** - evaluate interval function at point |
