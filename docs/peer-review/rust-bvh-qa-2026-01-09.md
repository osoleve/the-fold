# Rust-Accelerated BVH Implementation - QA Review

**Date:** 2026-01-09
**Reviewer:** Gemini 3 Pro via Claude Code

## Summary

The implementation is generally well-structured with good FFI patterns, but several issues were identified that need attention.

## Critical Issues

### 1. Memory Leak in Guardian System (bvh-cache.ss:44-50)

**Issue:** The Guardian pattern is flawed. Guardians track objects for cleanup when they become unreachable, but `cached` is stored in `*rust-bvh-cache*` which keeps a strong reference. The guardian will **never** trigger cleanup.

**Fix Required:** Either:
- Use weak references in the hashtable, OR
- Don't use Guardians - rely on explicit cleanup via `clear-cache!`

### 2. Double-Free Risk (bvh-cache.ss)

If `cleanup-stale-handles!` is called followed by `clear-cache!`, the same handle could be freed twice.

**Fix Required:** Add a freed flag to cached-bvh records or use a set to track freed handles.

### 3. Silent Null Pointer Handling (bvh.rs:401-402)

When null pointers are passed to FFI functions, they silently return without writing to the output struct. Scheme code will read uninitialized memory.

**Fix Required:** Initialize output struct to error status before returning.

## High Priority Issues

### 4. Struct Alignment Mismatch Risk (ffi-core.ss:83-87)

The Scheme struct definition doesn't explicitly account for C struct padding between `status` (u8) and `value` (f64).

**Mitigation:** Chez Scheme's define-ftype does handle C struct alignment correctly by default, but this should be verified with explicit padding fields.

### 5. Scheme Fallback Doesn't Track Actual Fuel (bvh-accel.ss:93-99)

The pure Scheme fallback uses estimated fuel rather than tracking actual operations.

**Impact:** Inconsistent fuel reporting between Rust and Scheme paths.

## Medium Priority Issues

### 6. Double Serialization on Cache Miss (bvh-cache.ss)

BVH is serialized twice on cache miss - once for hashing, once for building.

**Fix:** Cache the serialized bytevector or pass it through.

### 7. Missing Bounds Checks in Rust Parsing (bvh.rs:156-163)

If `first_tri + tri_count > triangles.len()`, this silently returns fewer triangles than expected.

### 8. Stack Overflow Risk in parse_node (bvh.rs:138-184)

Recursive parsing without depth limit could overflow stack on malicious input.

## Low Priority / Future Improvements

- Sub-optimal ray traversal order (always left-then-right)
- Triangle data copied unnecessarily during BVH build
- SHA-256 on full BVH for cache key is expensive

## Recommendations

1. Fix memory safety issues (#1, #2, #3) before production use
2. Add bounds checks and depth limits in Rust parser
3. Consider removing Guardian system and using explicit lifecycle management
4. Profile to determine if serialization caching is worth the complexity

## Test Coverage

Existing tests cover:
- Basic FFI roundtrip
- BVH serialization/deserialization
- Closest point queries
- Ray intersection queries
- Cache hit/miss
- Transparent fallback

Missing tests:
- Null pointer handling
- Empty BVH handling
- Large mesh stress tests
- Concurrent access
- Memory leak detection
