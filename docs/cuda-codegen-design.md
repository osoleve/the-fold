# CUDA Codegen: The Fold Renders to GPU

## Vision

The Fold is a homoiconic system where code is data. Computation graphs are S-expressions. S-expressions normalize and hash. This means:

**A traced computation IS its hash. Compile once, cache forever.**

We're not building "Scheme with GPU bindings." We're building a system where The Fold *renders* to CUDA the same way it could render to any target. The computation is the invariant; the execution substrate is a choice.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         User Code                               │
│   (gradient-of (lambda (W) (matrix-multiply W X)) weights)      │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Trace / Graph Build                        │
│   Autodiff builds computation graph as S-expression             │
│   Optics compile to access patterns                             │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                       Normalize & Hash                          │
│   α-normalize (de Bruijn), compute content hash                 │
│   This hash IS the kernel identity                              │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                        Cache Lookup                             │
│   Check CAS for compiled kernel at this hash                    │
│   HIT → load .cubin, skip codegen                               │
│   MISS → continue to codegen                                    │
└─────────────────────────────┬───────────────────────────────────┘
                              │ (cache miss)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                         Code Emitter                            │
│   Walk graph, emit CUDA C (or PTX directly)                     │
│   Fuse operations where possible                                │
│   Generate memory access patterns from optics                   │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                        NVCC / NVRTC                             │
│   Compile to .cubin                                             │
│   Store in CAS keyed by computation hash                        │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                         FFI Bridge                              │
│   Load .cubin, manage device memory, launch kernels             │
│   Return results to Scheme                                      │
└─────────────────────────────────────────────────────────────────┘
```

## Primitives

### Compute Primitives

These are the atoms that combine into computation graphs:

| Category | Primitives | CUDA Mapping |
|----------|-----------|--------------|
| **Elementwise Unary** | `neg`, `exp`, `log`, `sqrt`, `sin`, `cos`, `tanh`, `sigmoid` | One thread per element |
| **Elementwise Binary** | `add`, `sub`, `mul`, `div`, `pow`, `max`, `min` | One thread per element |
| **Reductions** | `sum`, `product`, `max-reduce`, `min-reduce`, `mean` | Parallel reduction pattern |
| **Matrix** | `matmul`, `transpose`, `outer-product` | cuBLAS or custom tiled |
| **Scatter/Gather** | `index`, `scatter`, `gather` | Coalesced memory access |
| **Scan** | `prefix-sum`, `prefix-product` | Work-efficient parallel scan |
| **Comparison** | `eq`, `lt`, `gt`, `le`, `ge` | Elementwise, returns mask |
| **Conditional** | `where` (mask select) | Elementwise ternary |

### Memory Primitives

| Primitive | Purpose |
|-----------|---------|
| `gpu-alloc` | Allocate device memory, return handle |
| `gpu-free` | Release device memory |
| `gpu-copy-to` | Host → Device transfer |
| `gpu-copy-from` | Device → Host transfer |
| `gpu-pool` | Memory pool for allocation reuse |

### Optic → Access Pattern Mapping

This is where it gets interesting. Optics describe paths through data; these become memory access patterns:

| Optic | Access Pattern | CUDA Mapping |
|-------|---------------|--------------|
| **Lens** | Single element | Direct index: `data[i]` |
| **Prism** | Conditional access | Masked operation: `if (tag == X) ...` |
| **Traversal** | Multiple elements | Parallel map: one thread per target |
| **Fold** (optic) | Read multiple, combine | Parallel reduction |
| **Setter** | Write path | Scatter operation |
| **Iso** | Bidirectional transform | Fused transform kernel |

Example - optic composition to CUDA:

```scheme
;; Fold expression
(over (>>> world-bodies each-traversal body-vel)
      (lambda (v) (vec2-scale v 0.99)))

;; Compiles to something like:
__global__ void apply_drag(World* w, float factor) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < w->n_bodies) {
        w->bodies[i].vel.x *= factor;
        w->bodies[i].vel.y *= factor;
    }
}
```

## Computation Graph IR

The traced computation needs a well-defined IR before codegen:

```scheme
;; Example IR node types
(ir-const value dtype)           ; Constant tensor
(ir-input name shape dtype)      ; Input parameter
(ir-unary op arg)                ; Unary operation
(ir-binary op lhs rhs)           ; Binary operation
(ir-reduce op axis arg)          ; Reduction along axis
(ir-matmul lhs rhs)              ; Matrix multiply
(ir-transpose arg perm)          ; Transpose with permutation
(ir-index arg indices)           ; Gather operation
(ir-fused ops args)              ; Fused operation sequence
```

Each IR node carries:
- Shape information (for bounds checking, launch config)
- Dtype (f32, f64, i32, etc.)
- Lineage (for gradient computation)

## Autodiff Integration

The existing autodiff system builds computation graphs. We extend it:

1. **Trace Phase**: Build IR graph instead of/alongside Scheme closures
2. **Gradient Phase**: Apply reverse-mode AD rules on IR
3. **Codegen Phase**: Emit forward and backward kernels
4. **Cache Phase**: Store compiled kernels by hash

For a typical gradient computation:
```scheme
(define-gpu-differentiable (loss W X Y)
  (mean (square (sub (matmul W X) Y))))

;; Calling (gradient-of loss W) at specific shapes:
;; 1. Traces the computation → IR graph
;; 2. Normalizes → hash
;; 3. Cache hit? → use cached kernel
;; 4. Cache miss? → emit CUDA, compile, cache
;; 5. Execute kernel, return gradient
```

## Kernel Fusion

Adjacent operations should fuse into single kernels to minimize memory bandwidth:

```scheme
;; These three ops:
(exp (add (mul x 2.0) 1.0))

;; Should become ONE kernel:
__global__ void fused_exp_add_mul(float* x, float* out, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) out[i] = expf(x[i] * 2.0f + 1.0f);
}

;; NOT three separate kernels with intermediate buffers
```

Fusion rules:
- Elementwise chains always fuse
- Reduction can absorb preceding elementwise
- Matmul boundaries generally don't fuse (use cuBLAS)

## DSL Sketch

User-facing API should feel native to The Fold:

```scheme
;; Declare GPU-accelerated computation
(define-gpu matmul-grad
  (lambda (W X)
    (let ((Y (matmul W X)))
      (sum (square Y)))))

;; Use it - compilation happens transparently
(define grad-W (gradient matmul-grad 'W weights inputs))

;; Optics work too
(define-gpu update-velocities
  (lambda (world dt)
    (over (>>> world-bodies each body-vel)
          (lambda (v) (vec2-add v (vec2-scale gravity dt))))))

;; Explicit GPU tensor operations
(gpu-let ((a (gpu-tensor [[1 2] [3 4]]))
          (b (gpu-tensor [[5 6] [7 8]])))
  (gpu->host (matmul a b)))
```

## Phased Implementation

### Phase 0: Foundation (Week 1)
- [ ] FFI bridge to CUDA runtime (`cuInit`, `cuCtxCreate`, etc.)
- [ ] Basic memory management (`gpu-alloc`, `gpu-free`, `gpu-copy-*`)
- [ ] Hello world: call a hardcoded kernel from Scheme
- [ ] Verify round-trip: host → device → compute → host

### Phase 1: Primitives (Week 2)
- [ ] Implement compute primitives as individual kernels
- [ ] cuBLAS integration for matrix ops
- [ ] Memory pool for allocation reuse
- [ ] Benchmark against CPU baseline

### Phase 2: IR & Codegen (Weeks 3-4)
- [ ] Define IR node types
- [ ] IR builder from traced computations
- [ ] CUDA C emitter (walk IR, emit code)
- [ ] NVRTC integration (runtime compilation)
- [ ] CAS integration (cache compiled kernels by hash)

### Phase 3: Autodiff Integration (Weeks 5-6)
- [ ] Extend tracer to build IR
- [ ] Implement backward rules for IR nodes
- [ ] Generate gradient kernels
- [ ] End-to-end: `(gradient-of f x)` compiles and runs on GPU

### Phase 4: Optics Integration (Weeks 7-8)
- [ ] Optic → access pattern compiler
- [ ] Traversal → parallel map
- [ ] Fold → parallel reduction
- [ ] Lens composition → nested access

### Phase 5: Optimization (Ongoing)
- [ ] Kernel fusion pass
- [ ] Memory layout optimization
- [ ] Async execution / streams
- [ ] Multi-GPU support

## File Structure

```
lattice/gpu/
├── manifest.sexp           # Skill metadata
├── primitives.ss           # GPU primitive operations
├── memory.ss               # Device memory management
├── ir.ss                   # Computation graph IR
├── trace.ss                # Trace computations to IR
├── emit-cuda.ss            # IR → CUDA C emitter
├── compile.ss              # NVRTC compilation + caching
├── autodiff-gpu.ss         # GPU-accelerated gradients
├── optic-access.ss         # Optic → access pattern compiler
└── test-gpu.ss             # Tests

boundary/gpu/
├── ffi.ss                  # Raw CUDA FFI bindings
├── runtime.ss              # Context, device management
├── cubin-cache.ss          # CAS integration for compiled kernels
└── bridge.c                # C glue code for FFI
```

## Open Questions

1. **PTX vs CUDA C**: Emit PTX directly for more control, or CUDA C for readability/debuggability? Start with CUDA C, optimize to PTX later?

2. **Shape Inference**: How much shape information do we track? Full symbolic shapes or just concrete at compile time?

3. **Dtype Handling**: Start f32-only for simplicity? Or generic from the start?

4. **Error Handling**: GPU errors are notoriously hard to debug. How do we surface useful information back to Scheme?

5. **Memory Pressure**: When device memory is exhausted, spill to host? Fail fast? Recompute?

## Success Criteria

Phase 1 success: Matrix multiply benchmark shows >100x speedup over CPU for large matrices (4096x4096).

Phase 3 success: `(gradient-of (lambda (W) (sum (square (matmul W X)))) weights)` compiles to GPU and runs correctly.

Phase 4 success: Physics simulation with 10k bodies runs at 60fps using optic-derived kernels.

---

*The Fold renders to CUDA. The computation is the invariant. The substrate is a choice.*
