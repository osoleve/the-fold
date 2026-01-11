# Layer 2 FFI QA Review

**Date:** 2026-01-11
**Reviewer:** Gemini 3 Pro
**Files Reviewed:**
- `shell/ffi/rust-accel/src/mat4.rs`
- `shell/ffi/bytevector-ffi.ss`
- `user/bench-mat4-bytevector.ss`

## Summary

The review identified 3 issues, all of which have been fixed.

## Findings

### 1. `shell/ffi/rust-accel/src/mat4.rs` (Rust Implementation)

**Issues Found:**

| Issue | Severity | Status |
|-------|----------|--------|
| Silent truncation on 32-bit (n as usize) | Medium | **FIXED** |
| Arithmetic overflow in fuel calc (MAT4_VEC_MUL * n) | Medium | **FIXED** |

**Fixes Applied:**
- Added `usize::try_from(n)` with error handling
- Added `checked_mul()` for fuel calculation
- Both return status=3 (runtime error) on overflow

**Matrix Math:** Correct (row-major order).

### 2. `user/bench-mat4-bytevector.ss` (Benchmark)

**Issues Found:**

| Issue | Severity | Status |
|-------|----------|--------|
| Unfair comparison: Scheme didn't write results | High | **FIXED** |
| Missing bytevector locking | Low | **FIXED** |

**Fixes Applied:**
- `scheme-batch-transform` now writes all 4 output components per point
- Added `with-locked-bytevectors` around Rust FFI calls

**Updated Results (Fair Comparison):**
- Rust: 3.8ns/point
- Scheme: 237ns/point
- Speedup: **63x** (previously claimed ~12x was understated due to unfair comparison)

### 3. `shell/ffi/bytevector-ffi.ss` (FFI Helpers)

**Quality:** High
**Issues Found:** None

- Correct size calculations (n * 8 for f64/i64)
- Proper `dynamic-wind` for lock/unlock pairing
- Good documentation

## Conclusion

All identified issues have been addressed. The Layer 2 FFI implementation is now:
- Memory-safe (overflow checks added)
- Fairly benchmarked (Scheme writes results)
- GC-safe (bytevector locking)

The **63x speedup** for batch transforms is legitimate and reproducible.
