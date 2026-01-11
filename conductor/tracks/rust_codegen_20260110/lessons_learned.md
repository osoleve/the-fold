# Lessons Learned: Rust Codegen Track

## Summary
This track implemented Rust code generation for Layer 1 primitives. Initial implementation (Phases 1-3) had gaps that were identified during review and fixed in Phase 4.

## Issues Found in Review

### 1. Missing Helper Functions
**Problem:** `rust-mapping.ss` called three undefined functions:
- `function-param-types`
- `function-return-type`
- `join-strings`

**Root Cause:** Functions were referenced but never implemented. The code path wasn't exercised by tests until function type mapping was attempted.

**Lesson:** When referencing helper functions, implement them immediately or mark them as TODO with compile-time errors. Every function call should have a corresponding definition.

### 2. Incomplete Operator Coverage
**Problem:** Only `+`, `-`, `*`, `/` were mapped to Rust. Missing 13+ operators.

**Root Cause:** The task "Implement Layer 1 Arithmetic and Logic Codegen" was marked complete after implementing only arithmetic. Logic and comparison operators were overlooked.

**Lesson:** Before marking a task complete, verify against the full specification. In this case, `prim.ss` defines ~70 primitives - the implementation should have been checked against this reference.

### 3. No Scheme→IR Translator
**Problem:** Tests manually constructed Rust IR. No function existed to translate actual Scheme expressions.

**Root Cause:** The pipeline was incomplete. The architecture document shows `infer → rust-codegen` but the translator was never built.

**Lesson:** A complete pipeline requires all stages. Testing with manually-constructed IR proves the serializer works but doesn't prove the system can actually compile Scheme code. Integration tests should use the full pipeline.

### 4. Excessive Whitespace in Generated Code
**Problem:** `rust-codegen.ss` contained triple blank lines throughout, making the code hard to read.

**Root Cause:** Model output formatting issue during initial implementation.

**Lesson:** Code review should catch formatting issues. Pre-commit hooks that enforce style can prevent this.

### 5. TestResult Struct Duplication
**Problem:** Each generated function included its own TestResult struct definition.

**Root Cause:** The struct was embedded inline for "simplicity" but this breaks when generating multiple functions.

**Lesson:** Shared types should be defined once and imported. For standalone compilation, generate a header file or use conditional compilation.

### 6. Type Information Discarded
**Problem:** `ret-type` was extracted from IR but never used - result always cast to f64.

**Root Cause:** Incomplete implementation. The type was captured but the type-aware conversion logic wasn't written.

**Lesson:** If you extract data, use it. Dead code (extracting then ignoring) suggests incomplete work. Consider: why extract `ret-type` if we don't use it?

## What Worked Well

### 1. Clear Architecture Document
The `codegen_architecture.md` provided a solid foundation. The IR node types (R-Fn, R-Let, R-If, R-Call, R-Literal) were well-designed and didn't need changes.

### 2. Existing Rust Crate Infrastructure
`shell/ffi/rust-accel/` already had:
- TestResult struct with correct layout
- Fuel tracking infrastructure
- Working FFI examples

This provided a reference implementation to match.

### 3. Test-Driven Development (Partial)
The existing tests caught issues quickly. Adding 40 new tests in Phase 4 ensures future changes won't regress.

### 4. Prelude Utilities
`core/base/prelude.ss` already had `init`, `last`, and `string-join` - we just needed to use them.

## Recommendations for Future Tracks

1. **Verify against specification:** Cross-check implementation against the full spec (e.g., prim.ss for primitives).

2. **Test the full pipeline:** Don't just test components - test end-to-end with real inputs.

3. **Use existing infrastructure:** Check what already exists before implementing. The helpers we needed were already in prelude.

4. **Review before marking complete:** A second set of eyes (human or AI) catches gaps the implementer missed.

5. **Track remaining work:** Create issues for known gaps so they don't get lost.

## Files Modified

| File | Lines Changed | Purpose |
|------|---------------|---------|
| `core/lang/rust-mapping.ss` | +31 | Added missing helpers |
| `core/lang/rust-codegen.ss` | +153, -147 | Operators, translator, cleanup |
| `core/lang/test-rust-codegen.ss` | +85 | 40 new tests |

## Timeline
- Phase 1-3: ~2 hours (initial implementation)
- Phase 4: ~30 minutes (review and fixes)
- Documentation: ~15 minutes

The fix phase was fast because the architecture was sound - only implementation gaps needed filling.
