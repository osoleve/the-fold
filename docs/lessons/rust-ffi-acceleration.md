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

---

# Phase 2: Raymarcher Supernode (2026-01-10)

## Executive Summary

Extended the BVH acceleration to include complete mesh raymarching. Moved the entire sphere tracing loop into Rust as a single FFI call, achieving **162-257x speedup** over pure Scheme.

---

## Critical Bug: Dynamic Traversal Indexing

### The Problem

We needed to return the triangle index (for texturing/materials) when a ray hits the mesh. Initial approach used a counter during BVH traversal:

```rust
// BROKEN: Counter-based indexing
fn traverse(..., tri_counter: &mut u32) {
    match node {
        BVHNode::Internal { left, right, .. } => {
            if left_dist <= right_dist {
                traverse(left, ..., tri_counter);
                traverse(right, ..., tri_counter);
            } else {
                // RIGHT visited first - counter increments in wrong order!
                traverse(right, ..., tri_counter);
                traverse(left, ..., tri_counter);
            }
        }
        BVHNode::Leaf { triangles, .. } => {
            for (i, tri) in triangles.iter().enumerate() {
                if hit { return base_index + i; }  // Index depends on visit order
            }
            *tri_counter += triangles.len();
        }
    }
}
```

The `get_triangle_by_index` function assumed static left-then-right order, but traversal is dynamic (visits closer child first). Same triangle gets different indices depending on query point location!

### The Solution

**Embed the triangle's original index in the Triangle struct itself:**

```rust
pub struct Triangle {
    pub p1: Vec3,
    pub p2: Vec3,
    pub p3: Vec3,
    pub id: u32,  // Stable index assigned during BVH construction
}

// In bvh_from_bytes:
for i in 0..num_tris {
    triangles.push(parse_triangle(&data[offset..], i as u32));
}
```

Now `hit.triangle.id` is stable regardless of traversal order.

### Lesson

**Don't track state that depends on traversal order.** If you need to identify which element was found, embed the identity in the element itself.

---

## Memory Safety: dynamic-wind for FFI Cleanup

### The Problem

FFI calls allocate memory that must be freed. If an exception occurs between allocation and `foreign-free`, memory leaks:

```scheme
;; LEAK RISK: exception before foreign-free
(let* ([result-ptr (make-ftype-pointer ... (foreign-alloc ...))])
  (rust-call ... result-ptr)
  (let ([data (ftype-ref ... result-ptr)])  ; Exception here?
    (foreign-free (ftype-pointer-address result-ptr))
    data))
```

### The Solution

Use `dynamic-wind` to guarantee cleanup:

```scheme
(let* ([result-ptr (make-ftype-pointer ... (foreign-alloc ...))])
  (dynamic-wind
   (lambda () #f)  ; before
   (lambda ()      ; body
     (rust-call ... result-ptr)
     (extract-result result-ptr))
   (lambda ()      ; after - ALWAYS runs
     (foreign-free (ftype-pointer-address result-ptr)))))
```

### Lesson

**Always use `dynamic-wind` for FFI memory management.** It's the Scheme equivalent of RAII or try-finally.

---

## Performance: Explicit Stack vs Recursion

### The Problem

Recursive BVH traversal uses the call stack, which can overflow on very deep/unbalanced trees (common in "folded" geometry).

### The Solution

Use an explicit stack:

```rust
// BEFORE: Recursive (stack overflow risk)
fn traverse(node: &BVHNode, ...) -> bool {
    match node {
        BVHNode::Internal { left, right, .. } => {
            traverse(left, ...) && traverse(right, ...)
        }
        ...
    }
}

// AFTER: Explicit stack (no overflow, faster!)
let mut stack: Vec<&BVHNode> = Vec::with_capacity(64);
stack.push(&handle.root);

while let Some(node) = stack.pop() {
    match node {
        BVHNode::Internal { left, right, .. } => {
            // Push in reverse order for correct traversal
            if left_dist <= right_dist {
                stack.push(right);
                stack.push(left);
            } else {
                stack.push(left);
                stack.push(right);
            }
        }
        ...
    }
}
```

### Performance Impact

| Mesh | Recursive | Explicit Stack |
|------|-----------|----------------|
| Small (12 tris) | 105x | 162x |
| Medium (80 tris) | 213x | 257x |
| Large (320 tris) | 163x | 188x |

**50-60% faster** due to reduced function call overhead.

### Lesson

**Prefer explicit stacks over recursion for tree traversal in hot paths.** It's safer AND faster.

---

## Sign Convention Matters

### The Issue

Scheme's `mesh-sdf.ss` used inverted sign convention:
- `dot >= 0` (outside) → **negative** distance
- `dot < 0` (inside) → **positive** distance

Standard SDF convention is opposite:
- Outside → **positive**
- Inside → **negative**

### Resolution

Documented in code. Rust uses standard convention. The raymarcher uses `abs(dist)` anyway so the sign only affects rendering normals.

### Lesson

**Document sign conventions explicitly.** When two systems interact, mismatched conventions cause subtle bugs.

---

## Final Performance Summary

| Mesh Size | Triangles | Scheme (ms) | Rust (ms) | Speedup |
|-----------|-----------|-------------|-----------|---------|
| Small | 12 | 190 | 1.2 | **162x** |
| Medium | 80 | 499 | 1.9 | **257x** |
| Large | 320 | 470 | 2.5 | **188x** |

*64 rays per frame, 3-5 iterations each*

---

## Updated File Organization

```
shell/ffi/
├── ffi-core.ss          # Library loading, basic FFI
├── serialize.ss         # Scheme → bytevector
├── bvh-ffi.ss          # BVH-specific FFI bindings
├── bvh-cache.ss        # Content-hash cache + Guardians
├── raymarch-ffi.ss     # Raymarcher FFI bindings (NEW)
├── bench-bvh.ss        # BVH baseline benchmark
├── bench-bvh-accel.ss  # BVH comparison benchmark
├── bench-raymarch.ss   # Raymarcher benchmark (NEW)
└── rust-accel/
    └── src/
        ├── lib.rs
        ├── bvh.rs
        ├── raymarch.rs  # Raymarcher acceleration (NEW)
        ├── triangle.rs  # Now includes id field
        └── ...

lattice/geometry/
├── raymarch.ss         # Pure Scheme raymarcher
├── raymarch-accel.ss   # Transparent wrapper (NEW)
└── ...
```

---

## Phase 2 Key Takeaways

1. **Embed identity in data** - Don't rely on traversal order for indexing
2. **dynamic-wind for FFI** - Guarantee cleanup even on exceptions
3. **Explicit stacks are faster** - AND safer for deep trees
4. **Document conventions** - Sign conventions, coordinate systems, etc.
5. **QA review twice** - Gemini caught the indexing bug before deployment
