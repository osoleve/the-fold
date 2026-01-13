# V2 Normalization QA Review

**Date**: 2026-01-13
**Reviewer**: Gemini 3 Pro Preview
**Component**: core/blocks/ (v2 normalization pipeline)

---

## Summary

QA review of the v2 normalization implementation for content addressing. The implementation is **correct** with some medium-priority concerns around edge cases and memory management.

---

## Findings

### Critical

None.

### High

| File | Finding | Recommendation | Status |
|------|---------|----------------|--------|
| `core/blocks/hash-cons.ss` | Unbounded memory growth in `*cons-table*`. Strong references retain all canonicalized expressions indefinitely. | Ensure `hash-cons-reset!` is called in the daemon loop or use weak references if supported. | **Documented** - Design choice with `hash-cons-reset!` for explicit control |

### Medium

| File | Finding | Recommendation | Status |
|------|---------|----------------|--------|
| `core/blocks/normalize.ss` | Recursion allows stack overflow on deeply nested inputs. | Add depth limit check similar to `poly-canon`. | **Acknowledged** - poly-canon has limits; general normalize follows Scheme's stack |
| `core/blocks/hash-cons.ss` | Cyclic structures would cause infinite loop. | Document assumption of tree/DAG structures. | **By Design** - CAS operates on DAG structures only |

### Low

| File | Finding | Recommendation | Status |
|------|---------|----------------|--------|
| `core/blocks/cas.ss` | Comment describes operation order differently than code. | Update comments to match actual pipeline. | **FIXED** - Updated normalize.ss:571-575 |
| `core/blocks/poly-canon.ss` | `term->sexpr` expands powers verbosely (x³ → `(* x x x)`). | Consider `expt` for powers > 1 if canonical format allows. | **By Design** - Maintains S-expression purity |
| `core/blocks/test-normalize.ss` | Missing tests for limit enforcement. | Add tests triggering `*poly-canon-max-terms*`. | **FIXED** - Added Test 28-29 |

---

## Correctness Assessment

### Hash-Consing ✓
Correctly uses global hashtable with `equal?` keys. Returns canonical representatives (pointer equality).

### Polynomial Canonicalization ✓
Correctly implements polynomial arithmetic (add/multiply/simplify) with proper term sorting.

### η-Reduction ✓
Correctly identifies reducible forms and preserves irreducible ones.

### Identity/Absorbing Elimination ✓
Correctly eliminates identity elements and short-circuits on absorbing elements.

### Version Separation ✓
Version `0x02` properly distinguishes from `0x00` and `0x01`.

### Pipeline Order ✓
Identity elimination happens **last**, which is better than the documented order (exposes more simplification opportunities after algebraic flattening).

---

## Edge Cases Handled

- **Float handling**: `poly-canon.ss` correctly bails out on floating-point numbers via `exact-number?`
- **Term explosion**: `*poly-canon-max-terms*` (100) prevents exponential blowup
- **Depth limits**: `*poly-canon-max-depth*` (10) prevents deep recursion in poly-canon

---

## Security Assessment

- **Cryptographic strength**: SHA-256 is secure for content addressing
- **Collision handling**: Version bytes prevent ambiguity between normalization modes
- **No injection vectors**: Pure functional transformations with no external input interpretation

---

## Test Coverage

### Existing (Good)
- Basic functionality for all v2 features
- Commutativity, associativity, η-reduction, identity elimination
- Hash version byte verification
- Round-trip normalization

### Missing (Recommended)
1. Tests verifying `poly-canon` aborts when limits exceeded
2. Tests for complex identity chains after flattening
3. Tests confirming non-arithmetic expressions are untouched by poly-canon

---

## Recommendations

1. **High Priority**: Document that `hash-cons-reset!` should be called periodically in long-running processes
2. **Medium Priority**: Add limit-enforcement tests to test suite
3. **Low Priority**: Update cas.ss comments to match implementation order
