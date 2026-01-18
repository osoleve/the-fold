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

### 7.8 Optics Tower

The Fold includes a complete optics implementation (`lattice/fp/optics/`) providing composable data accessors for principled navigation and transformation of nested structures.

#### 7.8.1 The Optics Hierarchy

Optics form a hierarchy based on their capabilities:

```
                  Fold
                 /    \
            Getter    Traversal
                 \    /    \
                  Affine   Setter
                 /    \     |
              Prism   Lens  |
                 \    /    /
                   Iso ---- Grate
```

Grate is the categorical dual of Lens. While a Lens focuses on extracting and replacing a single value within a structure, a Grate enables zipping multiple copies of a structure together.

| Optic | Targets | Read | Write | Laws |
|-------|---------|------|-------|------|
| Iso | exactly 1 | ✓ | ✓ | forward∘backward = id, backward∘forward = id |
| Lens | exactly 1 | ✓ | ✓ | get-put, put-get, put-put |
| Prism | 0 or 1 | ✓ | ✓ | preview-review, review-preview |
| Affine | 0 or 1 | ✓ | ✓ | get-set, set-get |
| Grate | exactly 1 | ✗ | ✓ | review-over, zipWith-identity |
| Traversal | 0+ | ✓ | ✓ | identity, composition |
| Fold | 0+ | ✓ | ✗ | — |
| Getter | exactly 1 | ✓ | ✗ | — |
| Setter | 0+ | ✗ | ✓ | identity, composition |

**Composition rules**: When optics compose, the result type is the "least upper bound" in the hierarchy. Lens + Prism = Affine (can read/write, but target may not exist).

#### 7.8.2 Operator Syntax

Ergonomic operators mirror Haskell's lens library:

| Operator | Name | Type | Usage |
|----------|------|------|-------|
| `^.` | view | `s × Lens → a` | `(^. pair lens-fst)` |
| `^?` | preview | `s × Affine → Maybe a` | `(^? maybe prism-just)` |
| `^..` | to-list | `s × Traversal → [a]` | `(^.. list traversal-each)` |
| `.~` | set | `Optic × b → s → t` | `((.~ lens-fst 99) pair)` |
| `%~` | over | `Optic × (a→b) → s → t` | `((%~ lens-fst add1) pair)` |
| `&` | pipe | `s × (s→t) → t` | `(& pair (.~ lens-fst 99))` |
| `>>>` | compose | `Optic × Optic → Optic` | `(>>> outer inner)` |

**Example**:
```scheme
(& body (%~ (>>> body-pos-lens vec2-x-lens) add1))  ; Increment x coordinate
```

#### 7.8.3 Block Optics

The CAS-aware optics (`block-optics.ss`) provide principled access to content-addressed blocks:

**Basic lenses**:
- `block-tag-lens` — Focus on block tag (Symbol)
- `block-payload-lens` — Focus on payload (Bytevector)
- `block-refs-lens` — Focus on refs vector

**Reference optics**:
- `block-ref-at n` — Affine for ref at index n (returns nothing if out of bounds)
- `block-refs-each` — Traversal over all refs
- `follow-ref fetch` — Affine that dereferences through CAS

**Type prisms**:
- `block-type-prism tag` — Match blocks by tag
- `block-lambda-prism`, `block-app-prism`, etc. — Common type matchers

**Tree traversal**:
```scheme
(collect-block-tree fetch root)  ; DFS all reachable blocks (O(N))
```

#### 7.8.4 Profunctor Encoding

The profunctor optics module (`profunctor-optics.ss`) provides an alternative representation where optics are polymorphic functions `p a b → p s t` constrained by profunctor type classes.

**Type classes**:
- `Profunctor` — `dimap : (a'→a) → (b→b') → p a b → p a' b'`
- `Strong` — Adds `pfirst : p a b → p (a,c) (b,c)` (enables lenses)
- `Choice` — Adds `pleft : p a b → p (Either a c) (Either b c)` (enables prisms)
- `Closed` — Adds `pclosed : p a b → p (x→a) (x→b)` (enables grates)
- `Wander` — Combines Strong + Choice with `wander : ((a → F b) → s → F t) → p a b → p s t` (enables traversals)

**Advantages**:
1. Composition is function composition (automatic type inference)
2. Type class hierarchy mirrors optic hierarchy
3. Separation of concerns: `Forget` for reading, `Tagged` for writing

**Profunctor optic types**:
- `p-iso`, `p-lens`, `p-prism`, `p-affine`, `p-grate` — Core optics
- `p-traversal` — Multi-target optic using Wander class
- `p-fold` — Read-only multi-target (composition: any optic + fold = fold)

**Conversions**:
```scheme
(lens->p-lens concrete-lens)    ; Concrete → Profunctor
(p-lens->lens profunctor-lens)  ; Profunctor → Concrete
(traversal->p-traversal trav)   ; Concrete traversal → Profunctor
(p-traversal->traversal ptrav)  ; Profunctor → Concrete
```

#### 7.8.5 Grate: The Dual of Lens

Grate is the categorical dual of Lens. The key insight is in their representations:

| Optic | Representation | Profunctor Class |
|-------|----------------|------------------|
| Lens | `(s → a, s → b → t)` — get/set | Strong (`pfirst`) |
| Grate | `((s → a) → b) → t` — cotraverse | Closed (`pclosed`) |

Where a Lens says "I can extract a focus and replace it," a Grate says "given any way to extract a focus, I can produce a result."

**Primary operation — zipWith**:
```scheme
;; Zip two pairs element-wise with a combining function
(grate-zipWith grate-pair-same + '(1 . 2) '(3 . 4))  ; → (4 . 6)

;; Apply the same argument to two functions and combine results
(let ([f (lambda (x) (+ x 1))]
      [g (lambda (x) (* x 2))])
  ((grate-zipWith grate-fn + f g) 5))  ; → 16  ((5+1) + (5*2))
```

**Common grates**:
- `grate-id` — Identity grate
- `grate-fn` — Functions as a grate (apply same input, combine outputs)
- `grate-pair-same` — Homogeneous pairs `(a . a)`
- `grate-list-rep n` — Fixed-length lists of n elements

**Laws**:
1. `(grate-over g id s) = s` — Identity
2. `(grate-zipWith g (λ (a b) a) s1 s2) = s1` — Left projection
3. `(grate-review g (grate-over g f (grate-review g a))) = (grate-review g (f a))` — Review-over coherence

**Use cases**:
- Parallel structure processing (zip vectors, matrices)
- Distributing functions over containers
- Gradient computation (apply same perturbation, collect partial derivatives)

#### 7.8.6 Physics Integration

The physics lens integration (`lattice/physics/lenses/optics-integration.ss`) demonstrates domain-specific optics:

**Traversals**:
- `bodies-each` — All bodies in a list
- `particles-alive` — Only alive particles
- `rigid-bodies-only` — Type-filtered traversal

**World optics**:
```scheme
(^? world (world-body 'player))                    ; Maybe get player
(traversal-over world-all-bodies (step-body dt) w) ; Simulate all bodies
```

**Physics operations as optic transformations**:
```scheme
(define (apply-gravity g)
  (lambda (body)
    (& body (%~ (>>> body-vel-lens vec2-y-lens) (lambda (vy) (+ vy g))))))
```

#### 7.8.7 Design Rationale

**Why optics?**

The Fold's content-addressed architecture benefits from optics in several ways:

1. **Immutable updates**: Blocks are immutable; optics provide functional update syntax that creates new blocks with modified content.

2. **Composable paths**: Complex CAS traversals (follow ref → match type → extract field) compose cleanly.

3. **Type-safe partiality**: Affines and prisms encode "might not exist" in their types, preventing runtime errors.

4. **Domain modeling**: Physics simulations, game state, and agent pipelines all benefit from declarative data access.

**Future directions**: The optics foundation enables query languages (optics + pattern matching) and reactive derivations (optic-based dependency tracking).

#### 7.8.8 Differentiable Data Access

The traced optics module (`lattice/autodiff/traced-optics.ss`) bridges optics with automatic differentiation, enabling gradient computation through optic-focused paths:

```scheme
;; Gradient of loss w.r.t. optic focus
(optic-gradient
  (lambda (p) (traced-sq (car p)))  ; Loss = x²
  lens-fst
  '(3.0 . 4.0))
; → 6.0  ; ∂(x²)/∂x = 2x = 6 at x=3

;; Gradient through composed lenses
(optic-gradient
  (lambda (s) (traced-sq (view (>>> lens-fst lens-snd) s)))
  (>>> lens-fst lens-snd)
  '((1.0 . 2.0) . (3.0 . 4.0)))
; → 4.0  ; Gradient at nested position

;; Gradients for traversal targets (one per focus)
(optic-gradient-list
  (lambda (xs) (traced-sum (map traced-sq xs)))
  traversal-each
  '(1.0 2.0 3.0))
; → (2.0 4.0 6.0)  ; Gradient for each element
```

**Key API**:
| Function | Purpose |
|----|-----|
| `optic-gradient` | Gradient of loss w.r.t. lens/affine/iso focus |
| `optic-gradient-list` | List of gradients for traversal targets |
| `optic-gradient-maybe` | Maybe gradient for prism/affine (nothing if no match) |
| `optimize-at` | Single gradient descent step at optic focus |
| `optimize-steps` | Multiple gradient descent steps |

**Architecture insight**: The `lift-at-optic` function traces only the optic focus, not the entire structure. This "pair of traced" pattern (vs "traced pair") enables efficient gradient computation through deeply nested structures while maintaining the compositional nature of optics.

### 7.9 Bidirectional Transformations

The bidirectional transformations system (`lattice/fp/optics/bidirectional.ss`) extends the optics tower with reversible migrations, enabling schema evolution, format conversion, and CAS block migrations with automatic rollback.

#### 7.9.1 Migration Type

A migration is a named, versioned isomorphism:

```scheme
(define user-v1->v2
  (make-migration 'user-v1->v2 'v1 'v2
    (make-p-iso
      (lambda (u) (cons '(created-at . 0) u))    ; forward
      (lambda (u) (assq-remove u 'created-at))))) ; backward

;; Apply forward (migrate) or backward (rollback)
(migrate user-v1->v2 old-user)    ; v1 → v2
(rollback user-v1->v2 new-user)   ; v2 → v1
```

**Key insight**: Profunctor optics already encode bidirectionality via `p-iso-forward` and `p-iso-backward`. Migrations leverage this to provide automatic rollback without writing reverse transformations manually.

**Composition**: Migrations compose like optics—`v1→v2` + `v2→v3` = `v1→v3`:

```scheme
(define v1->v3 (migration-compose v1->v2 v2->v3))
```

**Flip**: Reverse any migration:
```scheme
(define v2->v1 (migration-flip v1->v2))
```

#### 7.9.2 Schema DSL

The schema module (`schema.ss`) provides field-level operations for alist-based schemas:

| Operation | Forward | Backward |
|-----------|---------|----------|
| `field-rename-iso` | Rename field | Rename back |
| `field-add-iso` | Add with default | Remove field |
| `field-remove-iso` | Remove field | Add with preserved value |
| `field-transform-iso` | Transform value | Reverse transform |
| `field-split-iso` | Split field into two | Merge fields into one |
| `field-merge-iso` | Merge fields | Split field |

**Example: Schema evolution**
```scheme
(define v2-schema
  (schema-compose
    (field-rename-iso 'desc 'description)
    (field-add-iso 'created-at 0)
    (field-transform-iso 'priority iso-number-string)))
```

These compose via `schema-compose`, maintaining bidirectionality throughout.

#### 7.9.3 Block Migrations

Block migrations (`block-migration.ss`) specialize the migration infrastructure for The Fold's content-addressed blocks:

```scheme
(define issue-v1->v2
  (make-block-migration 'bbs-issue-v1 'bbs-issue-v2
    (field-add-iso 'created-at 0)))

;; Apply to block (pure transformation)
(block-migrate-payload issue-v1->v2 old-block)
```

**Tag-based versioning**: Version is encoded in the block tag (e.g., `'bbs-issue-v2`). Migrations check tag match before applying.

**Version detection**:
```scheme
(parse-versioned-tag 'bbs-issue-v2)  ; → (bbs-issue . 2)
(make-versioned-tag 'bbs-issue 3)    ; → 'bbs-issue-v3
```

#### 7.9.4 Merkle DAG Correctness

The shell runner (`shell/migrations/runner.ss`) executes migrations against the CAS with Merkle DAG correctness.

**The problem**: In a content-addressed system, changing a block changes its hash. If block A references block B, and B is migrated to B', then A must also be updated to reference B' instead of B. This cascades up to the root.

**Solution: Bottom-up (post-order) traversal**

```
migrate(root) → migrate(child₁), migrate(child₂), ... → then migrate(root with new refs)
```

1. Recursively migrate all children first
2. Collect their new hashes
3. Migrate parent payload with updated refs
4. Store parent, producing new hash
5. Memoize to handle shared subtrees (DAGs, not just trees)

```scheme
(define (migrate-tree-impl! bm hash visited)
  (let ([cached (hashtable-ref visited hash #f)])
    (if cached cached  ; DAG memoization
        (let* ([blk (fetch hash)]
               [new-refs (map-refs (lambda (h)
                           (migrate-tree-impl! bm h visited))
                         (block-refs blk))]
               [migrated (block-migrate-with-refs bm blk new-refs)]
               [new-hash (store! migrated)])
          (hashtable-set! visited hash new-hash)
          new-hash))))
```

**Memoization is critical**: Without it, shared subtrees would be migrated multiple times, causing exponential blowup in DAGs.

#### 7.9.5 Migration Registry

The registry (`shell/migrations/registry.ss`) manages migrations and computes paths:

```scheme
;; Register migrations
(register-migration! user-v1->v2)
(register-migration! user-v2->v3)

;; Find path between versions (BFS)
(find-migration-path 'v1 'v3)  ; → (user-v1->v2 user-v2->v3)

;; Get composed migration
(get-migration-chain 'v1 'v3)  ; → single migration v1→v3
```

**Version graph**: The registry builds a directed graph where nodes are versions and edges are migrations. Path finding uses BFS to find the shortest migration chain.

**Automatic rollback**: Flipped migrations are registered alongside forward migrations, enabling rollback path discovery.

#### 7.9.6 Format Isomorphisms

Standard format conversions (`format-iso.ss`) for common transformations:

| Iso | Forward | Backward |
|-----|---------|----------|
| `iso-utf8` | bytevector → string | string → bytevector |
| `iso-sexpr-string` | sexpr → string | string → sexpr |
| `iso-sexpr-bytevector` | sexpr → bytevector | bytevector → sexpr |
| `iso-number-string` | number → string | string → number |
| `iso-bool-int` | boolean → integer | integer → boolean |
| `iso-time-unix` | time-utc → integer | integer → time-utc |

**Convenience wrappers**:
```scheme
(sexpr->bytevector '(a b c))   ; For block payloads
(bytevector->sexpr payload)    ; For payload parsing
```

#### 7.9.7 Law Verification

Migrations satisfy isomorphism laws by construction:

```scheme
(verify-migration-laws user-v1->v2 test-user)
; Checks:
; 1. (rollback m (migrate m x)) = x  (roundtrip)
; 2. (migrate m (rollback m y)) = y  (reverse roundtrip)
```

**Design rationale**: By building migrations from profunctor isos, law compliance is compositional—if components satisfy laws, compositions do too.

#### 7.9.8 Use Cases

**CAS schema evolution**:
```scheme
;; Upgrade all issues in store
(migrate-tree! issue-v1->v2 (read-head 'issue-42))
```

**Format conversion**:
```scheme
(define json->sexpr (make-migration 'json->sexpr 'json 'sexpr
                      iso-json-sexpr))
```

**Batch migration with statistics**:
```scheme
(migration-stats issue-v1->v2 root-hash)
; → ((total . 150) (matching . 42) (non-matching . 108))

(migrate-dry-run issue-v1->v2 root-hash)  ; Preview without storing
```

### 7.10 Provenance Tracking

The provenance module (`shell/provenance/`) instruments optic operations to create a complete audit trail in the CAS. Every traced operation produces a provenance record capturing what happened, enabling "explain this value" queries and regulatory compliance.

#### 7.10.1 Architecture

**Provenance record schema** (tag: `provenance/record`):

```scheme
((operation . <symbol>)        ; view, set, over, preview, etc.
 (optic-name . <symbol|#f>)    ; Registered name if available
 (optic-type . <symbol>)       ; lens, prism, traversal, etc.
 (source-hash . <hex-string>)  ; Input structure
 (result-hash . <hex-string>)  ; Output structure
 (value-hash . <hex-string|#f>); Set/over value
 (timestamp . <iso8601>)
 (agent-id . <symbol|#f>)
 (session-id . <string|#f>)
 (version . 1))
```

The refs vector links to actual blocks: `[source-block, result-block, value-block?]`.

**Head pointer indices** enable O(1) lookup:
- `.store/heads/provenance/by-result/<hash>.head` — Find what created a result
- `.store/heads/provenance/by-source/<hash>.head` — Find what used a source
- `.store/heads/provenance/log.head` — Most recent operation

#### 7.10.2 Traced Operations

The `traced-optics.ss` module wraps standard optic operations with provenance recording:

| Traced Function | Base Operation | Records |
|-----------------|----------------|---------|
| `traced-view` | `^.` | source, result |
| `traced-set` | `.~` | source, result, value |
| `traced-over` | `%~` | source, result, function name |
| `traced-preview` | `^?` | source, Maybe result |
| `traced-to-list` | `^..` | source, list result |

**Usage**:
```scheme
(load "shell/provenance/traced-optics.ss")
(load "shell/provenance/query.ss")

;; Chain transformations with full tracking
(define v1 '(1 . 2))
(define v2 (traced-set lens-fst 10 v1))     ; (10 . 2)
(define v3 (traced-set lens-snd 20 v2))     ; (10 . 20)

;; Explain how v3 was created
(explain v3)
; → Lineage for 00abc123...
;   Step 1: set via lens-fst (lens)
;     Source: 00def456...
;     Result: 00789abc...
;   Step 2: set via lens-snd (lens)
;     ...
```

**Optic registry**: Since optics are functions, they can't be serialized. The registry maps optic values to symbolic names for traceable provenance:

```scheme
(register-optic! 'my-optic (>>> lens-fst lens-snd))
(lookup-optic-name my-optic)  ; → 'my-optic
```

Standard optics (`lens-fst`, `prism-just`, `traversal-each`, etc.) are pre-registered.

#### 7.10.3 Query API

**Direct queries**:
```scheme
(provenance-for-result result-hex)  ; → Block | #f
(provenance-for-source source-hex)  ; → Block | #f
(latest-provenance)                 ; → Block | #f
```

**Lineage traversal**:
```scheme
(trace-lineage result-hex)     ; → (List Block) oldest first
(trace-descendants source-hex) ; → (List Block) forward lineage
```

**Value retrieval** (from provenance records):
```scheme
(provenance-get-source record)  ; → original value
(provenance-get-result record)  ; → result value
(provenance-get-value record)   ; → set/over argument
```

**Search and statistics**:
```scheme
(find-provenance-by-optic 'lens-fst)       ; All uses of this optic
(find-provenance-by-operation 'set)        ; All set operations
(find-provenance-by-agent 'my-agent)       ; All by this agent
(provenance-stats)                         ; Counts by operation/optic/agent
```

#### 7.10.4 Context and Scoping

**Agent identity** for audit trails:
```scheme
(with-agent-id 'migration-tool
  (lambda ()
    (traced-set lens-fst new-value data)))
; Provenance record shows agent-id: migration-tool
```

**Selective tracing**:
```scheme
(without-tracing
  (lambda ()
    ;; Performance-critical section
    (traced-view data lens-fst)))  ; No provenance recorded

(with-tracing
  (lambda ()
    ;; Ensure tracing even if globally disabled
    ...))
```

#### 7.10.5 Security Considerations

**Path validation**: Hex strings used in head file paths are validated to contain only `[0-9a-fA-F]`, preventing path traversal attacks:

```scheme
(valid-hex-string? "../etc/passwd")  ; → #f (rejected)
(valid-hex-string? "00abcdef1234")   ; → #t
```

**Data sensitivity**: Values are stored as plaintext S-expressions. Sensitive data (keys, PII) will appear in provenance blocks. Consider:
- Using `without-tracing` for sensitive operations
- Restricting CAS directory permissions
- Future: encrypted payload option

#### 7.10.6 Performance Considerations

**Per-operation overhead**: Each traced operation creates:
- 3-4 block stores (source, result, value, record)
- 3 head file writes (log, by-result, by-source)

For performance-critical code, use `without-tracing` or raw optics.

**Query scalability**: `find-provenance-by-*` functions scan the entire store—O(N). For production use with large stores, consider external indices (BBS pattern).

**Storage growth**: Provenance creates ~4 blocks per operation. For high-volume systems, implement retention policies or archived storage tiers.

---

### 7.11 Reactive Derivations

The reactive module (`shell/reactive/`) builds on provenance tracking to provide automatic reactivity via optic-based dependency graphs. When a derivation is computed, we track which optics were accessed. When those optics are written to, the derivation invalidates and recomputes on next access.

This is the pattern behind lens-based state management systems (MobX, Recoil, Jotai), adapted to The Fold's optic foundation.

#### 7.11.1 Core Concepts

**Derivations** are cached computed values that track their optic dependencies:

```scheme
(define-reactive 'player-health
  world-state
  (lambda (world)
    (reactive-view world (>>> (body-lens 'player) health-lens))))

(reactive-value 'player-health)  ; => 100 (computed and cached)
(reactive-value 'player-health)  ; => 100 (from cache)
```

**Access tracking** discovers dependencies automatically during computation:

```scheme
(with-access-tracking
 (lambda ()
   (reactive-view data lens-fst)
   (reactive-view data lens-snd)))
; => (values result '(lens-fst lens-snd))
```

**Invalidation** marks derivations stale when their dependencies change:

```scheme
(reactive-set! lens-fst 999 data)  ; Invalidates all derivations using lens-fst
(reactive-value 'player-health)    ; Recomputes with fresh value
```

#### 7.11.2 Implementation Architecture

**Data structures**:

```
*derivations*           : Hashtable (Symbol → Derivation-Record)
*optic-to-derivations*  : Hashtable (Symbol → (List Symbol))  ; Reverse index
```

The derivation record (stored as a mutable vector) tracks:
- `source`: The root data structure being observed
- `dependencies`: List of optic names accessed during computation
- `compute-fn`: The function that produces the value
- `cached-value`: Last computed result
- `stale?`: Boolean flag for lazy recomputation

**Dependency discovery**: During `define-reactive` or `reactive-recompute!`, computation runs inside `with-access-tracking`. Each `reactive-view` call logs the optic name (via the provenance optic registry) to `*access-log*`. After computation, these become the derivation's dependencies.

**Reverse index maintenance**: When dependencies change, we update `*optic-to-derivations*` to enable O(1) invalidation lookup. Old dependencies are removed, new ones are added.

**Lazy recomputation**: `reactive-value` checks `stale?`. If true, it recomputes (updating dependencies in the process). If false, it returns the cached value.

#### 7.11.3 API Reference

**Derivation management**:
```scheme
(define-reactive name source compute-fn)  ; Create derivation
(reactive-value name)                     ; Get value (recompute if stale)
(reactive-refresh! name)                  ; Force recomputation
(reactive-stale? name)                    ; Check if needs recomputation
(reactive-dependencies name)              ; Get optic dependencies
(undefine-reactive name)                  ; Remove derivation
```

**Reactive optic operations**:
```scheme
(reactive-view s optic)      ; View with access tracking
(reactive-preview s optic)   ; Preview with access tracking
(reactive-to-list s optic)   ; To-list with access tracking
(reactive-set! optic val s)  ; Set with invalidation
(reactive-over! optic f s)   ; Modify with invalidation
```

**Batch operations**:
```scheme
(with-batch
 (lambda ()
   ;; Multiple changes, single invalidation pass
   (reactive-set! lens-fst 10 data)
   (reactive-set! lens-snd 20 data)))
```

**Introspection**:
```scheme
(list-derivations)        ; => (deriv-a deriv-b ...)
(derivation-info name)    ; => ((name . foo) (dependencies . (...)) ...)
(dependency-graph)        ; => ((lens-fst . (deriv-a deriv-b)) ...)
```

#### 7.11.4 Relationship to Provenance

Reactive derivations and provenance tracking share infrastructure:

| Aspect | Provenance | Reactive |
|--------|------------|----------|
| **Purpose** | Audit trail | Automatic updates |
| **Tracking** | All operations | Access paths only |
| **Storage** | CAS blocks | In-memory hashtables |
| **Optic registry** | Names in records | Dependency keys |
| **Persistence** | Survives restart | Session-scoped |

Both use the optic registry from `traced-optics.ss` to map optic values to symbolic names.

#### 7.11.5 Design Decisions

**Why not automatic invalidation on all traced-set?**

The reactive operations (`reactive-set!`, etc.) are separate from traced operations (`traced-set`, etc.) to avoid unwanted coupling. Code using provenance tracking shouldn't automatically trigger reactive invalidation.

**Why session-scoped, not persistent?**

Reactive derivations are designed for interactive state management (UI, simulation dashboards). Persistence would require serializing compute functions, which conflicts with The Fold's pure/impure separation.

**Why lazy recomputation?**

Immediate recomputation on invalidation would cascade through the dependency graph, potentially wasting work if the value is never accessed. Lazy evaluation ensures we only compute what's needed.

#### 7.11.6 Limitations

**Unregistered optics**: If an optic isn't registered with `register-optic!`, its accesses won't be tracked. Custom optics should be registered before use in reactive derivations.

**No circular dependency detection**: A derivation that reads and writes through the same optic could create infinite loops. The current implementation doesn't detect this.

**Memory management**: Derivations persist until explicitly removed with `undefine-reactive`. Long-running sessions should clean up unused derivations.

**Single-threaded**: The global mutable state (`*derivations*`, etc.) isn't thread-safe. Concurrent access requires external synchronization.

---

### 7.12 Optic Query Language

The optic query module (`lattice/query/optic-query.ss`) builds on the optics foundation to provide a declarative query language. The key insight is that optics are already a *typed path language*—they describe how to navigate through data structures. By combining optic paths with predicate filtering, projection, and aggregation, we get a composable query system.

#### 7.12.1 Design Philosophy

Traditional query languages separate "navigation" from "filtering":

```sql
SELECT pos FROM bodies WHERE vel.y > 0
```

The optic query language unifies these through composition:

```scheme
(oquery-pipe world
  (optic-having world-each-body body-vel-y (lambda (vy) (> vy 0)))
  (lambda (b) #t)
  (lambda (b) (^. b body-pos)))
```

Here, `optic-having` creates a *filtered traversal* that only yields bodies whose y-velocity satisfies the predicate. This traversal composes with other optics via `>>>`, enabling reusable query fragments.

#### 7.12.2 Core API

**Query functions** operate on a structure, an optic path, and optional predicate/projector:

| Function | Signature | Purpose |
|----------|-----------|---------|
| `oquery` | `s × Optic → [a]` | Get all targets |
| `oquery-where` | `s × Optic × (a→Bool) → [a]` | Filter by predicate |
| `oquery-select` | `s × Optic × (a→b) → [b]` | Project through function |
| `oquery-pipe` | `s × Optic × (a→Bool) × (a→b) → [b]` | Filter then project |
| `oquery-first` | `s × Optic → Maybe a` | First target |
| `oquery-first-where` | `s × Optic × (a→Bool) → Maybe a` | First matching target |

**Example**: Find all bodies with positive y-velocity, return their names:

```scheme
(oquery-pipe world world-each-body
  (lambda (b) (> (^. b body-vel-y) 0))
  (lambda (b) (^. b body-name)))
; => ("alpha" "delta")
```

#### 7.12.3 Optic Combinators

Combinators create new optics that can be composed with `>>>`:

| Combinator | Type | Purpose |
|------------|------|---------|
| `optic-where` | `Optic × (a→Bool) → Traversal` | Filtered traversal |
| `optic-having` | `Optic × Optic × (b→Bool) → Traversal` | Filter by nested value |
| `optic-select` | `Optic × (a→b) → Fold` | Projected fold |
| `optic-limit` | `Optic × Nat → Fold` | Take first n |
| `optic-skip` | `Optic × Nat → Fold` | Drop first n |

**Example**: Reusable query components:

```scheme
;; Define once
(define fast-bodies
  (optic-where world-each-body
    (lambda (b) (> (body-speed b) 100))))

;; Use anywhere via composition
(^.. game-state (>>> world-lens fast-bodies))
(oquery-count game-state (>>> world-lens fast-bodies))
```

The `optic-having` combinator is particularly powerful—it filters based on a nested value:

```scheme
;; Bodies whose velocity y-component is positive
(optic-having world-each-body body-vel-y
  (lambda (vy) (> vy 0)))
```

This composes the outer traversal (`world-each-body`) with an inner optic (`body-vel-y`) and filters based on the inner value.

#### 7.12.4 Aggregations

Standard aggregation operations:

| Function | Purpose |
|----------|---------|
| `oquery-count` | Count targets |
| `oquery-count-where` | Count matching targets |
| `oquery-sum` | Sum numeric targets |
| `oquery-sum-by` | Sum extracted values |
| `oquery-any` | Any target matches? |
| `oquery-all` | All targets match? |
| `oquery-min/max` | Min/max target value |
| `oquery-min-by/max-by` | Target with min/max value |

**Example**: Total mass of all bodies:

```scheme
(oquery-sum-by world world-each-body
  (lambda (b) (^. b body-mass)))
```

#### 7.12.5 Grouping and Joins

**Grouping** partitions targets by a key function:

```scheme
(oquery-group-by world world-each-body
  (lambda (b) (if (> (^. b body-mass) 10) 'heavy 'light)))
; => ((heavy . [bodies...]) (light . [bodies...]))
```

**Joins** combine results from multiple optic paths:

| Function | Purpose |
|----------|---------|
| `oquery-join` | Cross-product with predicate filter |
| `oquery-zip` | Pairwise combination (shortest length) |
| `oquery-union` | Concatenate results |
| `oquery-intersect` | Keep only shared targets |

**Example**: Find bodies near each other:

```scheme
(oquery-join world world-each-body world-each-body
  (lambda (a b)
    (and (not (eq? a b))
         (< (distance (^. a body-pos) (^. b body-pos)) 10))))
```

#### 7.12.6 Query Builder DSL

For complex queries, the builder pattern provides a chainable interface:

```scheme
(define my-query
  (-> (make-query world-each-body)
      (q-where (lambda (b) (> (^. b body-pos-y) 0)))
      (q-where (lambda (b) (> (^. b body-mass) 5)))
      (q-map (lambda (b) (^. b body-name)))))

(q-run world my-query)    ; => ("alpha" "delta")
(q-count world my-query)  ; => 2
(q-first world my-query)  ; => (just "alpha")
```

Filters and transforms accumulate; `q-run` executes them in order.

#### 7.12.7 Predicate Helpers

Convenience functions for building predicates:

| Function | Creates predicate for... |
|----------|--------------------------|
| `optic-eq?` | Target equals value |
| `optic-matches?` | Target satisfies condition |
| `optic-exists?` | Target exists (non-nothing) |
| `optic-gt?/lt?/gte?/lte?` | Numeric comparisons |
| `optic-between?` | Range check (inclusive) |

**Example**:

```scheme
(filter (optic-between? body-mass 5 20) bodies)
```

#### 7.12.8 Integration with Block Optics

The query language works with CAS block optics for querying the content-addressed store:

```scheme
;; Find all lambda blocks with arity > 2
(oquery-where store
  (>>> all-blocks-trav (block-type-prism 'lambda))
  (lambda (blk)
    (> (^. blk block-arity-lens) 2)))
```

This integrates with the existing `lattice/query/query-dsl.ss` block query infrastructure while providing the composability benefits of optics.

#### 7.12.9 Performance Characteristics

| Operation | Complexity |
|-----------|------------|
| `oquery` | O(targets) |
| `oquery-where` | O(targets) |
| `oquery-group-by` | O(targets × groups) |
| `oquery-join` | O(n × m) |
| `oquery-sort-by` | O(n log n) |

Queries are eager—all targets are collected before filtering. For large datasets, consider:
- Using `optic-limit` to cap results
- Composing filters before traversals to reduce intermediate results
- Caching frequently-used filtered traversals

#### 7.12.10 Design Rationale

**Why optics as the path language?**

1. **Type-safe composition**: `>>>` ensures paths compose correctly
2. **Unified read/write**: Same optic for queries and updates
3. **Reusability**: Define query fragments once, compose everywhere
4. **Hierarchy awareness**: Lens+Prism=Affine; composition preserves semantics

**Relationship to existing query-dsl**:

The traditional query-dsl (`query-dsl.ss`) uses pattern matching on block fields:

```scheme
(query fs '(and (tag . entity) (payload-contains . "Turing")))
```

The optic query language is more general—it works on any data structure navigable by optics, not just CAS blocks. The two approaches complement each other: use `query-dsl` for declarative block searches, use `optic-query` for structured data navigation.

---
