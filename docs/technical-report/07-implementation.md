## 7. Implementation


### 7.1 Technology Stack

**Runtime**: Chez Scheme (R6RS-compatible)
- High-performance native code compilation
- Efficient continuation support
- Rich numeric tower

**Dependencies**: None external
- SHA-256: Self-contained FIPS 180-4 implementation
- UTF-8: Built-in Scheme support
- Data structures: All implemented in-house

**Rationale**: Third-party dependencies introduce supply chain risk and verification burden. By implementing everything in-house, The Fold is fully auditable and self-contained.

### 7.2 Key Design Decisions

**Why Blocks?**

Blocks provide *uniform representation*. Code, data, types, modules—all are blocks. This enables:
- Universal content addressing
- Introspection and reflection
- Serialization of anything
- Merkle DAG structure

**Why Content Addressing?**

Content addressing provides *semantic identity*:
- Same content → same identity (automatic)
- Immutable by construction
- Deduplication for free
- Tamper-evident (hash verification)

**Why De Bruijn Indices?**

De Bruijn indices eliminate naming from identity:
- α-equivalent terms hash identically
- No variable naming conventions needed
- Canonical representation enables structural comparison
- Proven technique from proof assistants

**Why Algebraic Canonicalization?**

De Bruijn alone misses semantic equivalences:
- `(+ a b)` and `(+ b a)` are mathematically equal but hash differently
- Independent bindings in different orders are semantically equivalent
- Associativity allows multiple valid parenthesizations

Algebraic canonicalization extends semantic identity:
- Commutative operations sorted: same hash regardless of argument order
- Associative operations flattened: same hash regardless of nesting
- Independent bindings sorted: same hash regardless of declaration order
- Conservative purity analysis prevents unsafe reordering

The version byte (0x01) distinguishes algebraically-normalized hashes from α-only hashes (0x00), ensuring backwards compatibility.

**Why Pure Core + Impure Shell?**

Separation enables verification:
- Core: small, pure, formally verifiable
- Shell: practical, handles messy reality
- Clear boundary for trust decisions
- Neither compromises the other

### 7.3 Performance Considerations

**Space Complexity**:

| Structure | Space |
|----|----|
| Block | O(tag + payload + refs) |
| Address | 33 bytes (fixed) |
| CAS lookup | O(1) average |

**Time Complexity**:

| Operation | Time |
|----|----|
| `hash-block` | O(payload size) |
| `store!` / `fetch` | O(1) average |
| `normalize` (α-only) | O(expression size) |
| `normalize-algebraic` | O(n log n) for sorting |
| `normalize-full` | O(n log n) |
| `gc!` | O(stored blocks) |
| BM25 search | O(n log n) |

**Cryptographic Properties**:
- SHA-256: 256-bit collision resistance
- Avalanche: 1-bit input change → ~50% output change
- Preimage resistance: Cannot reverse hash

### 7.4 Rust Acceleration Layer

Performance-critical paths have optional Rust acceleration via FFI, located in `shell/ffi/rust-accel/`. This layer is designed for operations where computation significantly exceeds FFI overhead.

#### 7.4.1 Architecture

The Rust layer follows strict design principles to maintain The Fold's guarantees:

**FFI Safety**:
- All exposed types use `#[repr(C)]` for stable ABI
- Out-pointers pattern: Scheme allocates, Rust writes results
- No panics—all errors return status codes
- Null pointer checks on all inputs

**Fuel Preservation**:
- Each operation declares fuel costs matching Scheme's fuel model
- Fuel is checked before expensive operations
- Status code 2 indicates fuel exhaustion
- Remaining fuel is always returned to caller

**Result Struct Pattern**:
```rust
#[repr(C)]
pub struct F64Result {
    pub status: u8,      // 1=success, 2=out-of-fuel, 3=runtime-error
    pub value: f64,
    pub fuel_out: u64,
}
```

#### 7.4.2 Spatial Acceleration (BVH)

The BVH module provides fuel-tracked Bounding Volume Hierarchy operations:

**BVH Construction** (`fold_bvh_build`):
- Parses serialized BVH from bytevector
- Format: header (16 bytes) + nodes (64 bytes each) + triangles (72 bytes each)
- Returns opaque handle for subsequent queries

**Closest Point Query** (`fold_bvh_closest_point`):
- Finds closest point on mesh surface to query point
- Traverses closer children first for better pruning
- Fuel costs: base query (5) + per node (2) + AABB test (3) + triangle test (10)

**Ray Intersection** (`fold_bvh_intersect_ray`):
- Finds first ray-mesh intersection
- Returns distance and surface normal
- Fuel costs: base query (5) + per node (2) + AABB test (3) + triangle ray test (8)

#### 7.4.3 Raymarching

The raymarching module moves entire sphere-tracing loops to Rust:

**Mesh Raymarching** (`fold_raymarch_mesh`):
- Complete sphere tracing in single FFI call
- Computes signed distance via BVH queries
- Returns hit point, normal, distance, step count, and triangle index
- Gradient-based normal computation (6 SDF queries)

This eliminates per-step FFI overhead—critical for raymarching which may require hundreds of steps.

#### 7.4.4 Matrix Operations

4x4 matrix operations where computation exceeds FFI overhead (~112 ops for matrix multiply):

| Operation | Fuel Cost | Description |
|----|----|----|
| `fold_mat4_mul` | 112 | Matrix multiplication (fully unrolled) |
| `fold_mat4_vec_mul` | 28 | Matrix-vector multiplication |
| `fold_mat4_transform_points` | 28×N | Batch transform N points |
| `fold_mat4_transpose` | 16 | Matrix transpose |
| `fold_mat4_determinant` | 100 | 4x4 determinant via cofactors |

**Batch Operations**: `fold_mat4_transform_points` demonstrates Layer 2 FFI design—amortizing overhead across N points makes FFI cost negligible.

#### 7.4.5 Core Types

The Rust layer defines FFI-safe equivalents of Scheme types:

```rust
#[repr(C)]
pub struct Vec3 { pub x: f64, pub y: f64, pub z: f64 }

#[repr(C)]
pub struct AABB { pub min: Vec3, pub max: Vec3 }

#[repr(C)]
pub struct Triangle { pub p1: Vec3, pub p2: Vec3, pub p3: Vec3, pub id: u32 }
```

All operations are pure and inlined for performance.

### 7.5 Developer Experience

This section addresses practical concerns for developers using The Fold.

#### 7.5.1 Error Messages

Type errors include source locations and contextual information:

```
Type error at vec.ss:45:12

  (vec+ v1 v2)
        ^^
  Expected: (Vec n Num)
  Got:      (List Num)

  In the expression:
    (define (combine v1 v2)
      (vec+ v1 v2))

  Hint: vec+ requires vectors, not lists.
        Use (list->vec v1) to convert.
```

**Error message principles**:
1. **Location**: File, line, column, with source excerpt
2. **Expected vs. actual**: Clear type comparison
3. **Context**: Enclosing expression for clarity
4. **Hints**: Actionable suggestions where possible

#### 7.5.2 Incremental Development with Holes

Holes enable incremental typing without sacrificing safety:

**Workflow**:

1. **Start untyped**: Use `?` everywhere
   ```scheme
   (define (process x) : ?
     (complex-operation x))
   ```

2. **Add types incrementally**: Specify what you know
   ```scheme
   (define (process [x : InputData]) : ?
     (complex-operation x))
   ```

3. **Let inference propagate**: Type checker fills in constraints
   ```scheme
   ;; Checker reports: return type is (Result OutputData Error)
   ```

4. **Finalize**: Replace holes with concrete types
   ```scheme
   (define (process [x : InputData]) : (Result OutputData Error)
     (complex-operation x))
   ```

**Named holes for documentation**:
```scheme
(define (transform [x : (? input-format)]) : (? output-format)
  ...)
;; IDE shows: input-format = JSON, output-format = XML
```

**Hole reports**: Query what the checker inferred for each hole:
```scheme
(hole-report 'my-function)
; → ((? input-format) . JSON)
;   ((? output-format) . XML)
```

#### 7.5.3 REPL-Driven Development

The persistent REPL daemon supports interactive development:

```scheme
;; Load module under development
> (load "my-module.ss")

;; Test incrementally
> (my-function test-input)
#(result ...)

;; Check types interactively
> (type-of 'my-function)
(→ InputType OutputType)

;; Explore inferred types
> (infer '(lambda (x) (+ x 1)))
(→ Num Num)

;; Reload after edits
> (reload "my-module.ss")
```

**Session persistence**: State survives across invocations. Define a function, close the terminal, return later—it's still there.

#### 7.5.4 Tooling Integration

**Lattice search**: Find relevant functions without memorizing names:
```scheme
> (lf "matrix inverse")
((matrix-inverse 0.92 export (linalg matrix))
 (solve-linear 0.78 export (linalg solvers))
 ...)
```

**Dependency exploration**:
```scheme
> (ld 'physics/diff)        ; What does this need?
(autodiff linalg data)

> (lu 'linalg)              ; What uses this?
(autodiff geometry physics/diff physics/diff3d ...)
```

**Type inspection**:
```scheme
> (describe 'matrix-mul)
matrix-mul : (∀ (m n p) (→ (Matrix m n) (→ (Matrix n p) (Matrix m p))))

Multiplies two matrices. Requires inner dimensions to match.
Complexity: O(m·n·p)
Module: linalg/matrix
```

---
