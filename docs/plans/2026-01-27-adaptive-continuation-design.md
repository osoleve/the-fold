# Adaptive Step-Size Control for Bifurcation Continuation

**Issue:** fold-zxrv
**Date:** 2026-01-27
**Status:** Design complete, ready for implementation

## Problem

The bifurcation continuation methods (`continue-fixed-point`, `continue-fixed-point-arclength`) use fixed step sizes. This causes:
- Missed narrow bifurcation regions where rapid changes occur
- Wasted computation in stable regions where larger steps would suffice
- Potential failures near fold points where the curve turns sharply

## Solution

Implement adaptive step-size control based on three indicators:

### Indicators

| Indicator | Detects | Formula |
|-----------|---------|---------|
| **Eigenvalue proximity** | Approaching bifurcations | `min(\|Re(λ)\|)` for all eigenvalues |
| **Branch curvature** | Tight turns near folds | `acos(dot(t_old, t_new))` |
| **Newton iterations** | Numerical difficulty | Count from corrector |

### Step-Size Formula

```
factor = min(eigenvalue_factor, curvature_factor, residual_factor)
new_step = clamp(current_step * factor, min_step, max_step)
```

Where:
- `eigenvalue_factor = 0.5` if `min(|Re(λ)|) < 0.1`, else `1.0`
- `curvature_factor = 0.5` if `angle > 15°`, else `1.0`
- `residual_factor = 0.5` if `iters > 4`, `0.25` if `iters > 8`, else `1.0`

### Expansion Logic

If all factors = 1.0 for 3 consecutive successful steps:
- `new_step = min(current_step * 1.5, base_step * 2.0)`

### Failure Recovery

If Newton correction fails:
1. Halve the step size
2. Retry from the previous point
3. Only abort if `step < min_step`

## Thresholds

| Parameter | Value | Rationale |
|-----------|-------|-----------|
| `*adaptive-eigenvalue-threshold*` | 0.1 | Generous margin before critical values |
| `*adaptive-curvature-threshold*` | 0.26 rad (15°) | Standard for predictor accuracy |
| `*adaptive-residual-threshold*` | 4 | Half of `*arclength-max-correct*` (10) |
| `*adaptive-residual-critical*` | 8 | Near-failure threshold |
| `*adaptive-min-step*` | 1e-5 | Prevents stalling at bifurcations |
| `*adaptive-max-step-multiplier*` | 2.0 | Cap on expansion |
| `*adaptive-expansion-count*` | 3 | Consecutive good steps before expanding |

## Method-Specific Strategies

### Arc-Length Continuation (`continue-fixed-point-arclength-adaptive`)
- Uses all three indicators
- Curvature is meaningful (full tangent in phase space)
- Can handle fold points

### Fixed-Parameter Continuation (`continue-fixed-point-adaptive`)
- Uses eigenvalue + residual indicators only
- Curvature disabled (dx/dp blows up at folds)
- Cannot traverse folds regardless of step size

## Implementation Changes

### 1. Modify `arclength-correct` to return iteration count

```scheme
;; Current: returns (fp . param) or #f
;; New: returns (fp param iterations) or #f
```

### 2. Add adaptive continuation functions

```scheme
(define (continue-fixed-point-arclength-adaptive psys param0 fp0 num-steps base-step)
  ...)

(define (continue-fixed-point-adaptive psys param0 fp0 param-end base-step)
  ...)
```

### 3. Add step-size computation helpers

```scheme
(define (compute-eigenvalue-factor eigenvalues) ...)
(define (compute-curvature-factor old-tangent new-tangent) ...)
(define (compute-residual-factor iterations) ...)
(define (compute-adaptive-step current factors base min max) ...)
```

## Testing

1. **Pitchfork normal form** - Verify detection at r=0 with adaptive steps
2. **Hopf normal form** - Verify detection with complex eigenvalue crossing
3. **Saddle-node normal form** - Verify step shrinkage near fold
4. **Lorenz system** - Stress test on real chaotic system
5. **Comparison** - Same accuracy with fewer total steps than fixed

## References

- AUTO-07p documentation (Doedel et al.)
- MATCONT continuation package (Dhooge et al.)
- Kuznetsov, "Elements of Applied Bifurcation Theory"
