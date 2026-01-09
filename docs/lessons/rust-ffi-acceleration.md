# Lessons Learned: Rust FFI Acceleration in Chez Scheme

**Date:** 2026-01-09
**Project:** BVH acceleration supernodes

## Executive Summary

Successfully implemented Rust-accelerated BVH operations achieving 32-328x speedup over pure Scheme. Key insight: the FFI boundary design matters more than the algorithm optimization.

---

## FFI Design Patterns

### DO: Use Out-Pointers with #[repr(C)] Structs

```rust
#[repr(C)]
pub struct ClosestPointResult {
    pub status: u8,      // 0=miss, 1=hit, 2=out-of-fuel
    pub px: f64,
    pub py: f64,
    pub pz: f64,
    pub distance: f64,
    pub fuel_out: u64,
}

#[no_mangle]
pub extern "C" fn fold_bvh_closest_point(
    handle: *mut BVHHandle,
    px: f64, py: f64, pz: f64,
    fuel_in: u64,
    out: *mut ClosestPointResult,  // Write result here
)
```

```scheme
(define-ftype closest-point-result-t
  (struct
   [status unsigned-8]
   [px double] [py double] [pz double]
   [distance double]
   [fuel unsigned-64]))
```

### DON'T: Return scheme-object from Rust

Returning `scheme-object` requires linking against Chez internals (`Scons`, `Sflonum`), risks GC corruption, and is fragile across Chez versions.

### DO: Use Explicit Status Codes

| Status | Meaning | Scheme Result |
|--------|---------|---------------|
| 0 | Miss | `(miss fuel)` |
| 1 | Hit | `(ok result fuel)` |
| 2 | Out of fuel | `(out-of-fuel)` |

This makes the protocol explicit and debuggable.

### DO: Initialize Output on Null Pointer

```rust
if handle.is_null() {
    unsafe {
        (*out).status = 0; // miss
        (*out).px = 0.0;
        // ... initialize all fields
    }
    return;
}
```

Without this, Scheme reads uninitialized memory on error paths.

---

## Serialization Lessons

### Alignment Matters

Bytevector serialization must respect alignment:
- u8 fields need no alignment
- f64 fields need 8-byte alignment
- Pad structs to power-of-2 sizes (we used 64 bytes per BVH node)

```scheme
;;; Node format (64 bytes, 8-byte aligned):
;;;   [0]     u8:  node type
;;;   [1-7]   padding (7 bytes)
;;;   [8-55]  AABB (48 bytes)
;;;   [56-63] indices (8 bytes)
```

### Serialize Once, Use Twice

Bad (serializes twice on cache miss):
```scheme
(let* ([hash (sha256 (bvh->bytes bvh))]     ; serialize #1
       [handle (scheme-bvh->rust-handle bvh)]) ; serialize #2
  ...)
```

Good:
```scheme
(let* ([bv (bvh->bytes bvh)]           ; serialize once
       [hash (sha256 bv)]
       [handle (build-rust-bvh bv)])   ; reuse bytevector
  ...)
```

---

## Caching Strategy

### Content-Hash Caching Works Well

Using SHA-256 of serialized BVH as cache key:
- Automatic deduplication of identical structures
- No need to track object identity
- Works across sessions if persisted

### Guardian-Based Cleanup Has Gotchas

Guardians track objects for cleanup when unreachable, but:
- Objects stored in a hashtable are always reachable
- Guardian cleanup never triggers while cache holds references
- Solution: Use explicit `clear-cache!` or weak references

### Cache Hit Rate Matters More Than Algorithm Speed

For repeated queries on the same BVH, cache hit eliminates serialization entirely. First query pays serialization cost, subsequent queries are pure Rust speed.

---

## Algorithm Optimizations

### Ordered Traversal for Early Termination

For ray intersection, visit the closer child first:

```rust
let left_t = left.bbox().intersect_ray(origin, dir).map(|(t, _)| t);
let right_t = right.bbox().intersect_ray(origin, dir).map(|(t, _)| t);

match (left_t, right_t) {
    (Some(lt), Some(rt)) if rt < lt => {
        traverse(right, ...); traverse(left, ...)  // closer first
    }
    _ => {
        traverse(left, ...); traverse(right, ...)
    }
}
```

This improved ray-intersect speedup from 19-26x to 32-37x.

### Bounds Checking Prevents Crashes

Always validate indices from untrusted data:
```rust
if first_tri > triangles.len() || first_tri + tri_count > triangles.len() {
    return None;
}
```

---

## Fuel Tracking

### Fuel Must Be Tracked Per-Operation in Rust

```rust
const COST_NODE_VISIT: u64 = 2;
const COST_AABB_TEST: u64 = 3;
const COST_TRIANGLE_TEST: u64 = 10;

fn deduct(fuel: &mut u64, cost: u64) -> bool {
    if *fuel >= cost { *fuel -= cost; true } else { false }
}
```

### Scheme Fallback Should Match Rust Fuel Semantics

The pure Scheme fallback used estimated fuel instead of per-operation tracking. This creates inconsistency. Future work: make Scheme BVH operations also track per-operation fuel.

---

## Benchmarking Lessons

### Benchmark Both Paths

Always compare:
1. Pure Scheme baseline
2. Rust-accelerated path
3. With and without cache hits

### Speedup Varies by Workload

| Operation | Small Mesh | Large Mesh |
|-----------|------------|------------|
| closest-point | 112x | 328x |
| ray-intersect | 37x | 32x |

Closest-point benefits more from Rust because it's more compute-intensive per query.

### Include Serialization in First-Query Benchmarks

First query includes serialization cost. Subsequent queries (cache hits) are much faster. Report both.

---

## QA Process

### Gemini Review Caught Real Issues

Having Gemini 3 Pro review the implementation found:
- Memory safety issues (Guardian semantics)
- Silent failure modes (null pointer handling)
- Performance pitfalls (double serialization)
- Missing bounds checks

### Fix Critical Issues First

Priority order:
1. Memory safety (crashes, corruption)
2. Correctness (wrong results)
3. Performance (slow but correct)

---

## File Organization

```
shell/ffi/
├── ffi-core.ss          # Library loading, basic FFI
├── serialize.ss         # Scheme → bytevector
├── bvh-ffi.ss          # BVH-specific FFI bindings
├── bvh-cache.ss        # Content-hash cache + Guardians
├── bench-bvh.ss        # Baseline benchmark
├── bench-bvh-accel.ss  # Comparison benchmark
├── README.sexp         # Documentation
└── rust-accel/
    ├── Cargo.toml
    └── src/
        ├── lib.rs      # FFI exports
        ├── vec3.rs     # #[repr(C)] Vec3
        ├── triangle.rs # #[repr(C)] Triangle
        ├── aabb.rs     # #[repr(C)] AABB
        ├── fuel.rs     # Fuel tracking
        └── bvh.rs      # BVH algorithms
```

---

## Future Work Tracked

1. **SDF Transpiler**: For user-defined SDFs that are Scheme closures, build a compiler: Scheme SDF → Rust code → compile and load dynamically.

2. **Raymarcher Acceleration**: The raymarcher itself could be accelerated, not just BVH queries.

3. **Mesh SDF Acceleration**: `mesh-sdf` function calls BVH internally - could be a single Rust call.

4. **Parallel Queries**: Rust side could parallelize multiple queries using rayon.

---

## Key Takeaways

1. **FFI boundary design is critical** - out-pointers + status codes + explicit alignment
2. **Serialize once** - cache the bytevector, not just the handle
3. **QA review catches real bugs** - external review is worth the time
4. **Benchmark before and after** - optimization without measurement is guessing
5. **Ordered traversal matters** - simple algorithm changes can double performance
