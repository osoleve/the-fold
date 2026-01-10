# Codegen Architecture: Scheme to Rust

## Overview
The Rust codegen pipeline transforms The Fold's alpha-normalized and type-checked Blocks into optimized, FFI-safe Rust source code. This enables "strategic acceleration" where performance-critical computations are offloaded to Rust while maintaining the safety and homoiconicity of the Scheme-based core.

## Pipeline Integration
The codegen is implemented as a new phase in the compilation pipeline:
`read` → `parse` → `infer` → **`rust-codegen`**

## Architectural Components

### 1. AST Transformation (Scheme)
- **Input:** Type-annotated S-expressions (after `infer` phase).
- **Transformation:** Mapping Scheme constructs to a simplified Rust AST representation.
- **Handling Effects:** Ensuring fuel consumption is explicitly tracked in the generated AST.

### 2. Rust AST (Intermediate Representation)
A simplified representation of Rust constructs:
- `R-Fn`: Functions with `extern "C"` and fuel tracking.
- `R-Let`: Variable bindings.
- `R-If`: Conditional expressions.
- `R-Call`: Function calls (including primitive operations).
- `R-Literal`: Base types (integers, booleans, etc.).

### 3. Code Emission (Scheme)
- **Input:** Rust AST.
- **Output:** Valid Rust source code string.
- **Formatting:** Basic indentation and syntax compliance (braces, semicolons).

### 4. FFI Wrapper Generation
- Automatically generate the `#[no_mangle] pub extern "C"` boilerplate.
- Map Scheme result types to C-compatible `Result` structs.
- Handle the fuel parameter (`fuel_in` / `fuel_out`).

## Mapping Strategy

| Scheme Construct | Rust Equivalent | Notes |
|------------------|-----------------|-------|
| `(fn (x) body)` | `move |x| { body }` | Closures may require lifting to top-level functions for FFI. |
| `(if c t e)` | `if c { t } else { e }` | Rust `if` is an expression. |
| `(let ((x v)) b)`| `let x = v; b` | |
| `(prim 'add a b)`| `a + b` | |
| `Nat` | `u64` | |
| `Int` | `i64` | |

## Performance Considerations
- **Zero-cost abstractions:** Use Rust's ownership and typing to avoid overhead.
- **Inlining:** Leverage `#[inline(always)]` for small primitives.
- **Static Linking:** Generated code is compiled into a dynamic library (`.so` / `.dll`) and loaded via Scheme's FFI.

## Totality and Safety
- All generated Rust code must include fuel checks.
- Recursive calls must decrement fuel and check for exhaustion.
- The codegen only supports a subset of the language known to be total.
