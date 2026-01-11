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
|------|--------|------|---------------|
| Integer | `i64`, `int` | `i64` | `I64Result` |
| Float | `f64`, `float`, `real` | `f64` | `F64Result` |
| Boolean | `bool`, `boolean` | `bool` | `BoolResult` |
| Unsigned | `u64`, `unsigned` | `u64` | `U64Result` |

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
├── test-rust-loader.ss  # 19 loader tests
└── rust-accel/
    ├── src/
    │   ├── lib.rs       # Result structs, version
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
- Layer 2: Bytevectors, strings
- Layer 3: Crypto primitives
- Automatic hot-path compilation
- Rust-native autodiff

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
