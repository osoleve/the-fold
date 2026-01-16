# Interval Constraint Contractors QA Review

**Date:** 2026-01-16
**Reviewer:** Gemini 3 Pro
**Modules:** `lattice/optimization/interval-contract.ss`, `lattice/autodiff/interval-autodiff.ss`

## Summary

Gemini 3 Pro reviewed the new interval constraint contractors and interval autodiff modules for correctness, edge cases, performance, and API design.

## Findings

### 1. Directed Rounding (Major - Documented)

**Issue:** Contractors use standard Scheme arithmetic (`/`, `sqrt`) instead of directed rounding. This means bounds calculations like `(/ (- rhs other-lo) coef)` use default floating-point rounding rather than outward rounding.

**Analysis:** For rigorous proofs of inclusion, operations should use directed rounding (div-up, div-down). However, for optimization (finding approximate minima rather than proving inclusion), standard rounding is acceptable.

**Resolution:** Documented as limitation. The module provides "engineering accuracy" rather than "proof-level rigor". Users requiring formal verification should use a directed rounding library.

### 2. Linear Contractor Complexity (Minor - Deferred)

**Issue:** `linear-le-contract` recomputes `sum-interval-bounds` inside the loop for each variable, making single contraction pass O(N²) in constraint variables.

**Optimization:** Compute total sum once, then subtract each variable's contribution.

**Rationale:** For typical dimensions (N ≤ 3), overhead is negligible. Worth revisiting if high-dimensional constrained optimization becomes a use case.

### 3. Autodiff Hashtable vs Vector (Minor - Deferred)

**Issue:** `interval-autodiff.ss` uses hashtable for gradient accumulation in backward pass. Since IDs are dense integers starting from 0, a vector would be faster.

**Rationale:** Current implementation prioritizes clarity over micro-optimization. The hashtable approach is correct and readable.

### 4. Generic Nonlinear Contractor (Suggestion - Future Work)

**Recommendation:** Implement Newton-style contractor using interval gradients:
```
x ∈ N(x) = c - f(c)/∇f(x)
```

**Rationale:** This would leverage the new interval-autodiff module to handle arbitrary nonlinear constraints, not just linear and sphere.

### 5. Thread Safety (Suggestion - Documented)

**Issue:** `interval-autodiff` uses global mutable state (`*interval-traced-id*`) for ID generation, which is not thread-safe.

**Resolution:** Documented. The Fold's evaluation model is single-threaded with fuel-bounded execution, so this is acceptable for current use cases.

## Test Coverage Assessment

**Status:** Good (23/23 tests pass)

**Covered:**
- Basic bound contractors (narrowing, infeasibility)
- Equality contractors (singleton creation)
- Linear inequality contractors (le, ge, eq)
- Sphere contractors (feasible region, shrinking, offset center)
- Contractor combinators (contract-all, fixpoint)
- Constrained optimization integration
- Zero coefficient handling (added post-review)

**Gaps Addressed:**
- Added test for zero coefficient variables in linear constraints

**Remaining Gaps:**
- High-dimensional stress tests (deferred - typical use is D ≤ 3)

## Correctness Verification

Gemini confirmed:
- HC4-style box consistency algorithm is sound
- Linear variable isolation math is correct
- Positive/negative coefficient handling is correct
- Sphere contractor minimum-sum-of-squares logic is correct
- Autodiff reverse-mode chain rule propagation is correct

## API Summary

| Module | Key Functions |
|--------|---------------|
| interval-contract | `make-bound-contractor`, `make-equality-contractor`, `make-linear-le-contractor`, `make-linear-ge-contractor`, `make-linear-eq-contractor`, `make-sphere-contractor`, `interval-minimize-constrained` |
| interval-autodiff | `interval-gradient`, `monotonicity-info`, `reduce-box-by-monotonicity`, `interval-minimize-with-gradient` |

## Conclusion

The modules are algorithmically sound and well-structured. Primary concern (directed rounding) is acceptable for engineering optimization tasks. The integration of constraint contractors with interval branch-and-bound provides significant pruning capability for constrained optimization problems.
