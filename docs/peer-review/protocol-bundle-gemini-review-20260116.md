# Protocol Bundle Review - Gemini 3 Pro

**Date**: 2026-01-16
**Reviewer**: Gemini 3 Pro
**Subject**: Protocol bundle system for reducing implement-protocol! boilerplate

---

## Part 1: Design Review (Pre-Implementation)

### Summary

The proposed API (`define-protocol-bundle`, `derive-bundle!`, `implement-bundle!`) is clean, intent-revealing, and drastically reduces boilerplate for the common case.

However, **the implementation should use `syntax-case` macros instead of `eval`**.

### Key Findings

#### 1. eval vs Macros

`eval` is not the right tool here because:
- **Runtime dependency**: Forces symbol resolution at runtime, postponing errors that could be caught at compile-time
- **Environment scope**: In Chez Scheme, `eval` requires an environment argument and relies on global environment
- **Performance**: Slower and prevents optimizations

**Recommendation**: Use `syntax-case` with `datum->syntax` to construct identifiers at expansion time.

#### 2. Getter/Setter Pairing

The assumption that every slot is a getter/setter pair is slightly rigid:
- Read-only protocols (like particle mass) force a no-op setter
- Action protocols (like `apply-impulse`) don't fit the model

**Recommendation**: Consider allowing more flexible slot definitions in future iterations.

#### 3. Edge Cases

- **Hygiene**: If implementation functions aren't exported or in scope, `eval` fails. Macros share lexical scope.
- **Partial Implementation**: If a type only implements some protocols, current design tries to register everything.

### Verdict

The design is solid. The **implementation should switch to `syntax-case` macros** to avoid `eval`. This fits better with the high-quality engineering standards seen elsewhere in the codebase.

### API Assessment

| Question | Answer |
|----------|--------|
| Is API minimal enough? | Yes |
| Is eval acceptable? | No - use macros |
| Edge cases missed? | Partial implementations, hygiene |
| Flexibility concerns? | Getter/setter pairing is rigid but acceptable for v1 |

---

## Part 2: Implementation QA Review (Post-Implementation)

### 1. Bugs & Edge Cases

**Critical: Runtime `eval` in Setter Loop (protocol-bundle.ss:149)**

In `derive-bundle-runtime!`, the setter function was wrapped in a lambda that called `eval` *every time the setter is invoked*:
```scheme
[setter-fn (lambda (obj val) ((eval setter-name) obj val))])
```
This means calling `(vehicle-set-speed car 50)` performs a dynamic environment lookup on every single call.

**Status**: Fixed - now evaluates setter name once at binding time.

**Major: Macro Expansion Limit (protocol-bundle.ss:120-134)**

The `derive-bundle!` macro is hardcoded to accept exactly 0, 1, 2, or 3 overrides. If a user needs 4+ overrides, the macro fails to match.

**Status**: Accepted limitation for v1. Recursive expansion would add complexity. Current physics use cases need at most 1 override per derive call.

**Edge Case: `eval` Scope**

The use of `eval` evaluates symbols in the interaction environment. If `derive-bundle!` is used inside a `let` where target functions are local, `eval` will fail.

**Status**: Known limitation. Workaround: use `implement-bundle!` for local functions.

### 2. API Improvements Identified

**Move Resolution to Compile-Time**

A future version could rewrite `derive-bundle!` as a pure hygienic macro using `datum->syntax` to synthesize identifiers at expansion time. Benefits:
- Compiler catches typos (undefined function errors at compile-time)
- Direct function calls without `eval` overhead
- Works correctly with module-scoped functions

**Status**: Deferred to v2. Current implementation is functional and tested.

### 3. Performance Issues

**Repeated `eval` (Line 149)** - Fixed (see above)

**String Operations in Loops**

`implement-bundle-runtime!` and `derive-bundle-runtime!` iterate through slots with linear lookups using `string=?`. For bundles with many slots, this is O(N×M).

**Status**: Acceptable for v1. Bundle definitions are static and small (3 slots in body-ops). Registration happens once at load time.

### 4. Missing Tests Identified

| Gap | Priority | Notes |
|-----|----------|-------|
| 4+ overrides (macro limit) | Low | Would fail - document limit instead |
| Non-existent function error | Medium | Currently throws eval error |
| Local scope integration | Low | Known limitation |
| Performance benchmark | Low | Fixed the critical eval bug |

### 5. Quantified Impact

| Metric | Before | After |
|--------|--------|-------|
| Lines in lenses.ss (impl section) | 41 | ~12 |
| Lines in traced-body-protocols.ss | 29 | ~10 |
| Per-type registration calls | 6 | 1 |
| Boilerplate reduction | - | ~65% |

---

## Recommendations Summary

1. **Fixed**: Removed eval from setter lambda body (v1.0.1)
2. **Accepted**: Override limit of 3 (document in API)
3. **Deferred**: Full hygienic macro rewrite (v2)
4. **Deferred**: Recursive override pattern matching (v2)
