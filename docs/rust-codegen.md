# Rust Codegen v1.0

Generate high-performance Rust code from Scheme IR with fuel-tracked FFI integration.

## Overview

The Rust codegen system translates Scheme expressions into Rust functions that:
- Execute with deterministic fuel consumption
- Return typed results via out-pointers
- Integrate with the `rust-accel` crate for production builds

## Quick Start

```scheme
(load "core/lang/rust-codegen.ss")
(load "core/lang/rust-compile.ss")
(load "shell/ffi/rust-loader.ss")

;; 1. Create IR for a simple add function
(define add-ir
  '(R-Fn add_nums ((a i64) (b i64)) i64
         (R-Call add (R-Var a) (R-Var b))))

;; 2. Generate Rust code and add to crate
(compile-to-crate "add_nums" add-ir)
;; → (ok "shell/ffi/rust-accel/src/generated/add_nums.rs")

;; 3. Build the crate
(rust-build!)
;; → (ok "shell/ffi/rust-accel/target/release/libfold_accel.so")

;; 4. Load and call
(rust-load-fn! "fold_add_nums" '(i64 i64) 'i64)
(rust-call "fold_add_nums" 10 20 1000)
;; → (ok 30 999)
```

## IR Node Reference

### R-Literal
```scheme
(R-Literal value)
```
Constant value. Type inferred from context or value.

Examples:
- `(R-Literal 42)` → `42i64` or `42.0` depending on context
- `(R-Literal 3.14)` → `3.14`
- `(R-Literal #t)` → `true`

### R-Var
```scheme
(R-Var name)
```
Variable reference.

Example: `(R-Var x)` → `x`

### R-Call
```scheme
(R-Call op arg1 arg2 ...)
```
Operator/function call. Supports infix, method, and prefix operators.

Infix operators: `add`, `sub`, `mul`, `div`, `mod`, `lt`, `le`, `gt`, `ge`, `eq`, `ne`, `and`, `or`, `bitand`, `bitor`, `bitxor`, `shl`, `shr`

Method operators: `min`, `max`, `abs`, `floor`, `ceil`, `round`, `sqrt`, `sin`, `cos`

Prefix operators: `not`, `neg`, `bitnot`

Variadic support: `add`, `mul`, `and`, `or`, `bitand`, `bitor`, `bitxor` accept 0+ args:
- `(R-Call add)` → `0`
- `(R-Call mul (R-Var x))` → `x`
- `(R-Call add (R-Var a) (R-Var b) (R-Var c))` → `((a + b) + c)`

### R-Let
```scheme
(R-Let name value body)
```
Local binding.

Example:
```scheme
(R-Let sum (R-Call add (R-Var a) (R-Var b))
       (R-Call mul (R-Var sum) (R-Literal 2)))
```
→
```rust
let sum = (a + b);
(sum * 2)
```

### R-If
```scheme
(R-If condition then-expr else-expr)
```
Conditional expression.

Example:
```scheme
(R-If (R-Call lt (R-Var x) (R-Literal 0))
      (R-Call neg (R-Var x))
      (R-Var x))
```
→
```rust
if (x < 0) { (-(x)) } else { x }
```

### R-Block
```scheme
(R-Block expr1 expr2 ... exprN)
```
Sequence of expressions. Last expression is the result.

### R-Fn
```scheme
(R-Fn name ((param type) ...) ret-type body)
```
Function definition. This is the top-level IR node for codegen.

Types: `i64`, `f64`, `bool`, `u64`

Example:
```scheme
(R-Fn clamp ((x f64) (lo f64) (hi f64)) f64
      (R-Call max (R-Var lo)
              (R-Call min (R-Var hi) (R-Var x))))
```

## Codegen API

### rust-emit
```scheme
(rust-emit ir [cost]) → String
```
Generate standalone Rust code with inline result struct. For testing only.

### rust-emit-module
```scheme
(rust-emit-module ir [cost]) → String
```
Generate Rust module code with `use crate::{...}` imports. For crate integration.

### scheme->rust-ir
```scheme
(scheme->rust-ir name params ret-type body) → IR
```
Translate Scheme expression to IR.

```scheme
(scheme->rust-ir 'distance '((x f64) (y f64)) 'f64
                 '(sqrt (+ (* x x) (* y y))))
;; → (R-Fn distance ((x f64) (y f64)) f64 ...)
```

### ir-fuel-cost
```scheme
(ir-fuel-cost ir) → Nat
```
Compute fuel cost of an IR expression.

### op-fuel-cost
```scheme
(op-fuel-cost op) → Nat
```
Get fuel cost for a primitive operation.

### op-local-gradient
```scheme
(op-local-gradient op) → (List Expr)
```
Get gradient formulas for autodiff compatibility.

## Compilation API (rust-compile.ss)

### compile-to-crate
```scheme
(compile-to-crate name ir [cost]) → (Result String Error)
```
Generate Rust code and add to the `rust-accel` crate.

### rust-build!
```scheme
(rust-build!) → (Result String Error)
```
Build the crate with `cargo build --release`.

### rust-build-debug!
```scheme
(rust-build-debug!) → (Result String Error)
```
Build in debug mode.

### rust-test!
```scheme
(rust-test!) → (Result String Error)
```
Run crate tests with `cargo test`.

### compile-and-build!
```scheme
(compile-and-build! name ir [cost]) → (Result String Error)
```
Generate code and rebuild in one step.

### remove-from-crate
```scheme
(remove-from-crate name) → (Result #t Error)
```
Remove a generated module from the crate.

### list-generated-modules
```scheme
(list-generated-modules) → (List String)
```
List all generated module names.

## FFI Loader API (rust-loader.ss)

### rust-load-fn!
```scheme
(rust-load-fn! name param-types ret-type) → (Result #t Error)
```
Register and bind a Rust function.

```scheme
(rust-load-fn! "fold_add_nums" '(i64 i64) 'i64)
```

### rust-call
```scheme
(rust-call name arg1 ... argN fuel) → Result
```
Call a registered function with fuel tracking.

Returns:
- `(ok value fuel-remaining)` — Success
- `(out-of-fuel)` — Insufficient fuel
- `(error runtime-error)` — Runtime error (e.g., div-by-zero)

### rust-load-and-call
```scheme
(rust-load-and-call name param-types ret-type arg1 ... argN fuel) → Result
```
Register, bind, and call in one step.

### rust-reload!
```scheme
(rust-reload!) → Void
```
Force rebinding of all functions. Use after rebuilding the library.

### rust-fn-info
```scheme
(rust-fn-info name) → Alist | #f
```
Get info about a registered function.

### rust-loader-status
```scheme
(rust-loader-status) → Alist
```
Get loader status summary.

## Type System

### Supported Types

| Type | Scheme | Rust | Result Struct |
|----|----|----|----|
| Integer | `i64`, `int` | `i64` | `I64Result` |
| Float | `f64`, `float`, `real` | `f64` | `F64Result` |
| Boolean | `bool`, `boolean` | `bool` | `BoolResult` |
| Unsigned | `u64`, `unsigned` | `u64` | `U64Result` |
| Buffer | `buffer` | writes to `*mut u8` | `BufferResult` |

### Result Structs

All functions write results to caller-provided out-pointers:

```rust
#[repr(C)]
pub struct I64Result {
    pub status: u8,      // 1=success, 2=out-of-fuel, 3=runtime-error
    pub value: i64,
    pub fuel_out: u64,
}
```

### Identifier Sanitization

Function names are sanitized for Rust compatibility:
- Hyphens → underscores: `add-nums` → `add_nums`
- Keywords prefixed: `fn` → `m_fn`
- Leading numbers prefixed: `3d-point` → `m_3d_point`
- Special chars → underscores

## Fuel Accounting

Fuel costs are computed automatically from IR:

```scheme
(ir-fuel-cost '(R-Call add (R-Var a) (R-Var b)))  ; → 1
(ir-fuel-cost '(R-Call sqrt (R-Var x)))           ; → 10
(ir-fuel-cost '(R-If cond then else))             ; → 1 + max(then, else)
```

Operation costs match `prim-fuel-cost` in `prim.ss`.

## Autodiff Compatibility

Gradient formulas are documented for each operation:

```scheme
(op-local-gradient 'add)  ; → (1 1)        ; d(a+b)/da=1, d(a+b)/db=1
(op-local-gradient 'mul)  ; → (b a)        ; d(a*b)/da=b, d(a*b)/db=a
(op-local-gradient 'sqrt) ; → ((/ 1 (* 2 (sqrt a))))
```

This ensures 1:1 compatibility with `reverse-diff.ss`.

## Directory Structure

```
core/lang/
├── rust-codegen.ss      # IR nodes, serialization, fuel costs
├── rust-compile.ss      # Crate integration, cargo commands
└── test-rust-codegen.ss # 235 unit tests

shell/ffi/
├── ffi-core.ss          # Base FFI infrastructure
├── rust-loader.ss       # Dynamic function loading
├── bytevector-ffi.ss    # Zero-copy bytevector FFI + Layer 2 wrappers
├── test-rust-loader.ss  # 19 loader tests
└── rust-accel/
    ├── src/
    │   ├── lib.rs       # Result structs, version
    │   ├── bytes.rs     # Layer 2: bytevector operations
    │   ├── string.rs    # Layer 2: string operations
    │   └── generated/   # Auto-generated modules
    │       ├── mod.rs
    │       └── *.rs
    └── Cargo.toml
```

## Error Handling

### Compile-Time Errors
- Unknown operator → `error: Unknown op`
- Invalid IR structure → `error: Invalid IR`

### Runtime Errors
- Division by zero → `(error runtime-error)`
- Out of fuel → `(out-of-fuel)`

Division guards are automatically generated:
```rust
if divisor == 0 {
    result.status = 3;  // RUNTIME_ERROR
    return;
}
```

## Limitations

### v1.0 Scope
- Layer 1 primitives only (numeric operations)
- No closures or recursion in generated code
- Manual compilation workflow (no JIT)

### Future Work (v2.0+)
- Layer 2 codegen extensions: Slice type expansion in IR (`ByteSlice`, `StrSlice`)
- Layer 3: Crypto primitives (SHA-256, HMAC via ring crate)
- Automatic hot-path compilation
- Rust-native autodiff
- SIMD string search (memmem, Boyer-Moore)

## Performance Layers

Understanding when Rust FFI wins vs native Scheme is critical for deciding what to accelerate.

### Layer 1: Primitives (FFI Loses)

Single arithmetic operations like `(+ a b)`, `(* x y)`, `(sqrt z)`:

| Implementation | Time/call | Why |
|----|----|----|
| Chez Scheme native | ~2-5ns | Direct CPU operation |
| Rust FFI | ~200-500ns | Marshal, call, unmarshal |
| **Ratio** | **100-250x slower** | FFI overhead dominates |

Layer 1 codegen is useful for:
- Testing the codegen pipeline
- Foundation for Layer 2
- NOT for performance

### Layer 2: Bytevector & String Operations

The `bytevector-ffi.ss` module now includes Rust-accelerated bytevector and string operations with fuel tracking:

**Bytevector Operations** (zero-copy FFI):
```scheme
(rust-bv-hash bv fuel)           ; → (ok hash fuel-remaining)
(rust-bv-compare bv1 bv2 fuel)   ; → (ok -1|0|1 fuel-remaining)
(rust-bv-equal? bv1 bv2 fuel)    ; → (ok #t|#f fuel-remaining)
(rust-bv-copy! src dst n fuel)   ; → (ok bytes-written fuel-remaining)
(rust-bv-fill! bv val fuel)      ; → (ok bytes-written fuel-remaining)
```

**String Operations** (involves UTF-8 conversion):
```scheme
(rust-levenshtein s1 s2 fuel)       ; → (ok distance fuel-remaining)
(rust-str-contains? haystack needle fuel)  ; → (ok #t|#f fuel-remaining)
(rust-str-index-of haystack needle fuel)   ; → (ok index|-1 fuel-remaining)
(rust-str-starts-with? s prefix fuel)      ; → (ok #t|#f fuel-remaining)
(rust-str-ends-with? s suffix fuel)        ; → (ok #t|#f fuel-remaining)
(rust-str-upcase s fuel)                   ; → (ok "RESULT" fuel-remaining)
(rust-str-downcase s fuel)                 ; → (ok "result" fuel-remaining)
```

**Note**: String operations use `string->utf8` internally, which allocates a bytevector. Bytevector operations are true zero-copy. Case conversion is ASCII-only (non-ASCII characters preserved unchanged).

**Result Types**:
- Scalar results use existing `U64Result`, `I64Result`, `BoolResult`
- Operations writing to buffers use new `BufferResult`:
  ```rust
  #[repr(C)]
  pub struct BufferResult {
      pub status: u8,           // 1=success, 2=out-of-fuel, 3=error, 4=buffer-overflow
      pub bytes_written: usize,
      pub fuel_out: u64,
  }
  ```

### Layer 2: Single Complex Ops (Depends on Approach)

Operations like mat4×4 multiply (112 ops):

**With element-by-element copying (old approach):**

| Implementation | Time/call | Why |
|----|----|----|
| Scheme unrolled | ~800-900ns | 112 native ops |
| Rust FFI (copy) | ~2000ns | Marshal 32 doubles, call, unmarshal 16 |
| **Ratio** | **2-3x slower** | Marshaling overhead > computation |

**With bytevector pass-through (new approach):**

| Implementation | Time/call | Why |
|----|----|----|
| Scheme unrolled | ~800-900ns | 112 native ops |
| Rust FFI (bytevector) | **~30ns** | Zero-copy pass, pure compute |
| **Ratio** | **26x faster!** | No marshaling overhead |

The difference: bytevectors passed as `u8*` pointers avoid copying entirely.

### Layer 2: Batched with Fresh Data (Break-Even)

1000-point batch transform with per-call allocation:

| Implementation | Time/point | Why |
|----|----|----|
| Scheme batch | ~165-200ns | Iterate list, 16 MADs |
| Rust batch | ~210-220ns | Marshal 4000 doubles per call |
| **Ratio** | **~1:1** | Marshaling ≈ computation |

### Layer 2: Batched with Cached Buffers (FFI Wins!)

1000-point batch with pre-allocated Rust memory:

| Implementation | Time/point | Why |
|----|----|----|
| Scheme batch | ~165-200ns | Same as above |
| Rust (cached) | **~3ns** | Pure SIMD computation |
| **Ratio** | **50-60x faster** | Computation >> overhead |

### When to Use Rust FFI

The pattern for Rust FFI wins:

1. **Allocate once, reuse many times** (like BVH handles)
2. **Batch operations** (100+ items per call)
3. **Keep data in Rust memory** (avoid Scheme↔Rust copying)
4. **Algorithms with internal iteration** (raymarching, BVH traversal)

Examples that work well:
- BVH queries: **112-328x speedup** (single alloc, many queries)
- Raymarching: **162-257x speedup** (64+ rays per call)
- Batch transforms: **50x speedup** (with cached buffers)

Examples that don't work:
- Single arithmetic ops: 100x slower
- Single mat4 multiply: 2x slower
- Per-call allocated batches: break-even

### The Bytevector Solution (Zero-Copy FFI)

The key breakthrough: **Chez Scheme bytevectors can be passed directly as `u8*` pointers**.

```scheme
;; Load bytevector FFI helpers
(load "shell/ffi/bytevector-ffi.ss")

;; Create bytevectors for matrices
(define mat-a (make-mat4-bytevector))
(define mat-b (make-mat4-bytevector))

;; Fill with data using indexed accessors
(f64-bv-set! mat-a 0 1.0)  ; mat[0][0] = 1.0
(f64-bv-set! mat-a 5 1.0)  ; mat[1][1] = 1.0
;; ...

;; Pass directly to Rust - NO COPYING!
(define rust-mat4-mul
  (foreign-procedure "fold_mat4_mul" (u8* u8* unsigned-64 void*) void))

(rust-mat4-mul mat-a mat-b fuel result-ptr)
;; Total time: ~30ns (vs ~2000ns with element copying)
```

**Benchmark Results (fair comparison, both write results):**

| Approach | Time | Notes |
|----|----|----|
| Element copy (foreign-set! loop) | 1790ns | 64 copies per matrix |
| Bytevector direct (u8*) | **39ns** | Zero copy |
| **Improvement** | **46x** | |

| Batch 1000 points | Time | Per-point |
|----|----|----|
| Rust (bytevector) | 3.8μs | 3.8ns |
| Scheme (bytevector) | 237μs | 237ns |
| **Speedup** | | **63x** |

### Bytevector FFI API

The `bytevector-ffi.ss` module provides:

```scheme
;; Typed bytevector constructors
(make-f64-bytevector 16)    ; → 128-byte bytevector for 16 doubles
(make-i64-bytevector 100)   ; → 800-byte bytevector for 100 i64s
(make-mat4-bytevector)      ; → 4x4 matrix (16 f64s)
(make-points-bytevector n)  ; → n points (4 f64s each)

;; Indexed accessors (element index, not byte offset)
(f64-bv-ref bv 5)           ; → 6th double
(f64-bv-set! bv 5 3.14)     ; Set 6th double

;; Conversions
(vector->f64-bytevector vec)      ; → bytevector
(f64-bytevector->vector bv)       ; → vector
(list->f64-bytevector lst)        ; → bytevector

;; Safety for long-running ops
(with-locked-bytevector bv thunk)      ; Lock during GC-unsafe ops
(with-locked-bytevectors bvs thunk)    ; Lock multiple
```

### Type Mapping for Bytevectors

The `rust-loader.ss` now supports bytevector types:

| Scheme Type | Chez FFI Type | Use Case |
|----|----|----|
| `bv`, `bytevector`, `u8*` | `u8*` | Generic bytevector |
| `f64*`, `f64-bv` | `u8*` | f64 array bytevector |
| `i64*`, `i64-bv` | `u8*` | i64 array bytevector |
| `ptr`, `void*` | `void*` | Foreign-allocated memory |

### Recommended Architecture

For Layer 2+ acceleration with bytevectors:

```scheme
(load "shell/ffi/bytevector-ffi.ss")

;; Allocate bytevectors ONCE (reuse across many calls)
(define mat (make-mat4-bytevector))
(define points-in (make-points-bytevector 1000))
(define points-out (make-points-bytevector 1000))

;; Fill matrix
(mat4-identity! mat)  ; or load from data

;; Fill points
(do ([i 0 (+ i 1)]) ((= i 1000))
  (points-bv-set! points-in i x y z 1.0))

;; Transform - ZERO COPY, reuse buffers
(rust-mat4-transform-bv mat points-in 1000 points-out fuel ...)
;; Time: ~3ns per point (vs ~200ns with fresh allocation)

;; Read results from points-out bytevector
(f64-bv-ref points-out 0)  ; → transformed x₀
```

## Examples

### Euclidean Distance
```scheme
(define dist-ir
  (scheme->rust-ir 'distance '((x1 f64) (y1 f64) (x2 f64) (y2 f64)) 'f64
                   '(sqrt (+ (* (- x2 x1) (- x2 x1))
                             (* (- y2 y1) (- y2 y1))))))

(compile-to-crate "distance" dist-ir)
(rust-build!)
(rust-load-fn! "fold_distance" '(f64 f64 f64 f64) 'f64)
(rust-call "fold_distance" 0.0 0.0 3.0 4.0 1000)
;; → (ok 5.0 976)
```

### Comparison with Clamping
```scheme
(define clamp-ir
  '(R-Fn clamp ((x f64) (lo f64) (hi f64)) f64
         (R-If (R-Call lt (R-Var x) (R-Var lo))
               (R-Var lo)
               (R-If (R-Call gt (R-Var x) (R-Var hi))
                     (R-Var hi)
                     (R-Var x)))))

(compile-to-crate "clamp" clamp-ir)
```

### Safe Division
```scheme
(define safe-div-ir
  '(R-Fn safe_div ((a i64) (b i64)) i64
         (R-If (R-Call eq (R-Var b) (R-Literal 0))
               (R-Literal 0)
               (R-Call div (R-Var a) (R-Var b)))))

(compile-to-crate "safe_div" safe-div-ir)
```
