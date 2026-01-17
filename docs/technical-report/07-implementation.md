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

**Rationale**: Third-party dependencies introduce supply chain risk and verification burden. But more fundamentally, external code is a black box—you can't measure its fuel consumption, can't introspect its behavior, can't extend it without forking, can't trace exactly what happens when it runs. By implementing everything in-house, The Fold is fully *introspectable* (you can follow any execution path), *measurable* (fuel tracking works everywhere), and *hackable* (no behavior is opaque or off-limits). No surprises, no black boxes.

Note: The Rust acceleration layer (§7.4) is an exception that proves the rule—it's in-house code that provides the same guarantees (fuel tracking, no hidden state, no opaque behavior), just implemented in a faster language for performance-critical paths.

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

**Block explorer TUI** (`shell/web/fold-explorer/`):
A Rust-based terminal UI for visualizing the content-addressed store. Navigate blocks by tag, search content, follow references to traverse the Merkle DAG, and analyze orphan or highly-referenced blocks. All untrusted content is sanitized before display to prevent terminal escape sequence injection.

#### 7.5.5 LSP Integration

The Language Server Protocol implementation (`shell/lsp/`) provides IDE features with real type inference integration.

**Hover Type Inference**:

Rather than relying solely on pre-indexed type signatures, the LSP hover handler performs real type inference using the document's content:

```
Document → parse-definitions → [(name, expr)] → build-tenv-from-defs → TEnv → try-infer-type → "Type"
```

**Process**:
1. `parse-definitions` extracts top-level `define` forms from the document text
2. `build-tenv-from-defs` infers types for each definition, building a type environment incrementally
3. `try-infer-type` looks up the hovered symbol in this environment
4. Falls back to `get-type-string` (primitive/indexed types) if inference fails

**Definition extraction** handles both forms:
```scheme
(define x 42)              ; → (x . 42)
(define (f a b) body)      ; → (f . (fn (a b) body))
```

**Integration with bidirectional inference**:

The implementation uses the core type inference engine (`core/types/infer.ss`), including hole constraint tracking (§5.9.1). Each definition is inferred independently with fresh type variables, then generalized before being added to the environment:

```scheme
(reset-fresh!)
(let* ([result (infer init env)]
       [type (apply-subst subst (cadr result))]
       [gen-type (generalize env type)])
  (tenv-extend env name gen-type))
```

**Fallback strategy**:

The layered fallback ensures useful hover information is always available:

1. **Real inference**: Best for user-defined symbols in the current file
2. **Primitive table**: Built-in operations like `+`, `map`, `cons`
3. **Symbol index**: Pre-indexed module exports

**Known limitations**:

| Limitation | Impact | Status |
|----|----|----|
| Forward references | Mutually recursive functions may show `Any` | Planned: multi-pass inference |
| Local bindings | `let`-bound variables not typed | Planned: body traversal |
| Re-parsing overhead | O(N) per hover request | Planned: tenv caching |

These limitations are acceptable for initial deployment—the fallback ensures primitive operations always display types, and real inference succeeds for the common case of sequential top-level definitions.

### 7.6 Shell IO Infrastructure

The Shell layer provides IO primitives that maintain consistency guarantees despite operating in an impure environment.

#### 7.6.1 Atomic File Writes

The `shell/io/atomic.ss` module implements atomic file writes using the *write-then-rename* pattern:

```
1. Write content to unique temporary file (path.pid.nanoseconds.counter.tmp)
2. Flush buffers to OS
3. Rename temporary file to target path (atomic on POSIX)
```

**Guarantees**:
- Readers never see partial writes—files are either complete-old or complete-new
- Crash during write leaves target unchanged (temp file may be orphaned)
- Error during write triggers cleanup of temporary file
- Unique temp file names prevent collision when multiple processes write concurrently

**Limitations**:
- `flush-output-port` flushes to OS buffers, not to disk; true durability requires `fsync()` which is not yet implemented
- On power failure after rename but before disk sync, data may be lost

#### 7.6.2 File Locking

The `shell/io/file-lock.ss` module provides file locking for multi-step atomic operations:

```scheme
(with-file-lock path
  (lambda ()
    ;; read-modify-write safely here
    ))
```

**Three-layer protection** (belt-and-suspenders approach):
1. **Process-internal mutex**: Prevents thread races within a single Scheme process
2. **POSIX flock()**: OS-managed advisory locks via Rust FFI with automatic cleanup on process death
3. **Cross-process lockfile**: Uses identity tokens for verification and handles cases where `flock()` is unavailable

**POSIX FFI layer** (`shell/ffi/posix-ffi.ss`, `shell/ffi/rust-accel/src/posix.rs`):
- Provides `flock()` with `LOCK_NB` (non-blocking) to avoid hanging Chez Scheme's cooperative runtime
- Uses `O_CLOEXEC` to prevent file descriptor inheritance to child processes
- Returns real OS PID via `getpid()` for unique identity token generation

**Identity tokens**: Each process generates a unique token combining:
- Real OS PID (via FFI) or memory-address fallback
- High-resolution timestamp (nanoseconds)
- Process-local counter

**Stale lock recovery**: Locks older than 60 seconds are considered stale. Breaking uses atomic rename with identity token verification:
1. Write new lock to unique temp file
2. Atomically rename over stale lock
3. Verify our token is in the final file
4. If verification fails, another process won—retry

This eliminates the race condition where two processes both detect and break a stale lock simultaneously.

#### 7.6.3 BBS: Case Study

The Bulletin Board System (BBS) demonstrates these primitives in practice:

**Counter generation** (`bbs-next-id!`):
- Requires atomic read-increment-write
- Protected by `with-file-lock` on counter file
- Concurrent stress test: 10 parallel processes correctly generate 10 unique sequential IDs

**Lock-aware function design**:
- Public functions (e.g., `bbs-write-head!`) acquire their own locks
- Internal functions (e.g., `%bbs-write-head!`) assume caller holds lock
- This prevents deadlock when composing operations while allowing efficient nested calls

**Compare-and-swap** (`bbs-cas-head!`):
- Implements optimistic concurrency control for issue updates
- Protected by `with-file-lock` on individual head files
- Uses internal `%bbs-write-head!` since it already holds the lock
- Returns `#f` on conflict, allowing retry

**In-memory indices with cache persistence**:

Both issues and posts use in-memory hashtable indices for O(1) lookups, with disk-based cache persistence:

| Index | Purpose | Key → Value |
|----|----|---|
| `*bbs-issues*` | Issue lookup by ID | id-string → hash-bytevector |
| `*bbs-by-status*` | Filter by status | status-symbol → (id ...) |
| `*bbs-by-priority*` | Filter by priority | priority-int → (id ...) |
| `*bbs-posts*` | Post lookup by ID | id-string → hash-bytevector |
| `*bbs-posts-by-type*` | Filter by type | type-symbol → (id ...) |

**Cache invalidation strategy**:
```
1. On save: Store head-file count as version marker
2. On load: Compare cached count vs actual disk head count
3. If mismatch: Full rebuild from disk (conservative but correct)
4. On cache hit: Individual items auto-refresh on hash lookup miss
```

This approach trades off stale cache detection granularity for simplicity—no complex change tracking is needed, and the count check is O(1).

**Design achieved**: The current implementation provides production-ready concurrency for single-server deployments. The hybrid `flock()` + lockfile approach handles both normal operation (via fast OS-level locks) and edge cases (via identity-verified lockfiles).

#### 7.6.4 REPL History: Case Study

The REPL History module (`shell/history/`) demonstrates command replay as an alternative to state serialization—a key insight for systems with opaque runtime objects.

**The Problem**: Time-travel debugging and undo/redo typically require environment snapshots. But Scheme environments contain *closures* (captured lexical scopes), *continuations* (call stack snapshots), and *ports* (OS file handles)—none of which can be serialized portably.

**The Solution**: Command replay. Instead of snapshotting state, record the commands that produced it:

```
Execute command → Create history entry block → Link to previous → Update head
                                                      ↓
Undo → Walk back prev chain → Reset environment → Replay to target position
```

**Command Classification**:

| Type | Examples | Replay Behavior |
|----|----|----|
| `definition` | `define`, `define-syntax` | Always replay (modifies environment) |
| `effect` | `load`, `display`, `write-file` | Skip in safe mode (side effects) |
| `expression` | `(+ 1 2)`, `(map f xs)` | Replay if needed for result |

Classification is determined by inspecting the head form of each parsed expression.

**Block Schema** (`history/entry`):

```scheme
;; Payload
((session-id . "cli-123")
 (index . 42)
 (command . "(define x 10)")
 (cmd-type . definition)
 (result-type . success)
 (result-hash . "a4f5...")
 (defined-name . x)
 (timestamp . "2026-01-17T...")
 (version . 1))
;; Refs: [prev-entry-hash]
```

Each entry links to its predecessor, forming a chain like git commits.

**Branching via Content Addressing**:

Creating a branch is O(1)—no data copying required:

```
Before:  main.head → entry-5 → entry-4 → entry-3 → ...

(branch 'experiment)

After:   main.head → entry-5 → entry-4 → entry-3 → ...
                              ↑
         experiment.head ─────┘
```

Both branches share the same underlying blocks. Divergence only occurs when new commands are added to different branches.

**Environment Reset Challenge**:

Chez Scheme provides no `unbind!` primitive. The workaround:

1. Track all symbols defined via history
2. On reset, overwrite each with a tombstone value
3. Replay definitions to rebuild correct bindings

This isn't true unbinding—`(top-level-bound? x)` still returns `#t`—but it's sufficient for replay semantics.

**Divergence Detection**:

When replaying, commands that succeeded originally might fail due to:
- Changed external state (files, network)
- Stale dependencies
- Non-deterministic behavior

On divergence (replay error where original succeeded), the system pauses and reports the conflict rather than silently corrupting state.

**Head Files** (per session):

```
.store/heads/history/<session-id>/
  main.head           # Branch tip hash
  experiment.head     # Other branches
  __current__.head    # Active branch name
  __redo__.sexp       # Redo stack (list of entry hashes)
```

The redo stack enables `(redo)` after `(undo)`—popping from the stack re-executes the undone command.

**Integration**: The REPL worker (`shell/repl/repl-worker.ss`) hooks command recording into evaluation:

```scheme
;; After successful evaluation:
(history-record-success! session-id cmd-str result defined-name)

;; After error:
(history-record-error! session-id cmd-str error-value)
```

Recording is guarded to prevent history failures from breaking the REPL itself.

**Design Insight**: Command replay is the right abstraction for systems with opaque runtime state. It's portable (commands are strings), auditable (history is inspectable), and debuggable (replay can be traced). The tradeoff is replay cost—O(n) for n commands—but practical REPL sessions rarely exceed hundreds of commands, making this acceptable.

### 7.7 Probabilistic Programming and Automatic Differentiation

The Fold integrates automatic differentiation with probabilistic programming, enabling gradient-based inference for scalable Bayesian computation.

#### 7.7.1 Automatic Differentiation

The autodiff module (`core/autodiff/`) provides multiple differentiation modes:

| Mode | Type | Best For |
|----|----|----|
| Forward (Dual numbers) | `Dual` | Few inputs, many outputs |
| Reverse (Traced values) | `Traced` | Many inputs, few outputs (e.g., loss functions) |
| Hyperdual | `Hyperdual` | Exact second derivatives (Hessians) |

**Traced values** record a computation graph during the forward pass, then backpropagate gradients:

```scheme
(gradient
  (lambda (x y)
    (traced-mul x (traced-add x y)))  ; f(x,y) = x(x+y)
  '(3.0 2.0))
; → (8.0 3.0)  ; ∂f/∂x = 2x+y = 8, ∂f/∂y = x = 3
```

The `Differentiable` type class (`core/autodiff/differentiable.ss`) provides a uniform interface:

```scheme
(class Differentiable d where
  lift    : Real → d           ; Lift constant
  primal  : d → Real           ; Extract value
  d+, d*, d-, d/ : d → d → d   ; Arithmetic
  d-exp, d-log, d-sin, d-cos   ; Transcendentals
  ...)
```

This enables generic differentiable programming—write once, differentiate with any AD mode.

#### 7.7.2 Variational Inference

Variational inference (`lattice/random/variational-inference.ss`) transforms Bayesian integration into optimization:

**Key insight**: Instead of computing the intractable posterior p(z|x), find the closest approximation from a tractable family:

```
q*(z) = argmin_q KL(q(z) || p(z|x)) = argmax_q ELBO(q)
```

where ELBO (Evidence Lower Bound) is:

```
L(φ) = E_q[log p(x,z)] - E_q[log q(z;φ)]
     = E_q[log p(x,z)] + H[q]
```

**The Reparameterization Trick**: Naive sampling z ~ q(z|φ) blocks gradient flow. The solution: sample noise ε ~ N(0,1) and compute z = g(ε, φ) deterministically:

```
For Gaussian q(z|μ,σ): z = μ + σ * ε
```

Now z is a deterministic function of φ, enabling ∇_φ E_q[f(z)] via backpropagation.

**Variational Families**:

| Family | Parameters | Expressiveness |
|----|----|----|
| Mean-field Gaussian | μ, diag(σ) | Fast, independent marginals |
| Full-covariance Gaussian | μ, LL^T (Cholesky) | Captures correlations |

**Optimization**: Adam optimizer with momentum and adaptive learning rates:

```scheme
(vi-fit-normal-mean observations variance num-iters learning-rate)
; Infers posterior over mean given observations with known variance
```

**Convergence Diagnostics**:
- ELBO history tracking
- `vi-summary` for posterior statistics
- `vi-check-convergence` for monitoring

**Example: Bayesian Linear Regression**

```scheme
;; Model: β ~ N(0, 10*I), y_i ~ N(X_i · β, σ²)
(let ([result (vi-fit-linear-regression X y 1.0 2000 0.01)])
  (vi-summary result))
; → Posterior mean and uncertainty over regression coefficients
```

This approach scales to large datasets where MCMC would be prohibitively slow.

---
