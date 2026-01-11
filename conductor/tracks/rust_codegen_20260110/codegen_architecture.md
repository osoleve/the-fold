# Codegen Architecture: Scheme to Rust

## Overview
The Rust codegen pipeline transforms The Fold's alpha-normalized and type-checked Blocks into optimized, FFI-safe Rust source code. This enables "strategic acceleration" where performance-critical computations are offloaded to Rust while maintaining the safety and homoiconicity of the Scheme-based core.

## Pipeline Integration
The codegen is implemented as a new phase in the compilation pipeline:
`read` → `parse` → `infer` → **`rust-codegen`**

## Architectural Components

### 1. AST Transformation (Scheme)
- **Input:** Type-annotated S-expressions (after `infer` phase).
- **Transformation:** `scheme->rust-ir` translates Scheme to Rust IR.
- **Handling Effects:** Fuel consumption explicitly tracked in generated code.

Implemented in `core/lang/rust-codegen.ss`:
```scheme
(scheme->rust-ir '(prim 'lt? (abs x) (abs y)))
;; => (R-Call lt? (R-Call abs (R-Var x)) (R-Call abs (R-Var y)))
```

### 2. Rust AST (Intermediate Representation)
A simplified representation of Rust constructs:
- `R-Fn`: Functions with `extern "C"` and fuel tracking.
- `R-Let`: Variable bindings.
- `R-If`: Conditional expressions.
- `R-Call`: Function calls (including primitive operations).
- `R-Literal`: Base types (integers, booleans, etc.).
- `R-Var`: Variable references.
- `R-Block`: Statement sequences with final expression.

### 3. Code Emission (Scheme)
- **Input:** Rust AST.
- **Output:** Valid Rust source code string.
- **Formatting:** Basic indentation and syntax compliance (braces, semicolons).

### 4. FFI Wrapper Generation
- Automatically generate the `#[no_mangle] pub extern "C"` boilerplate.
- Map Scheme result types to C-compatible `Result` structs.
- Handle the fuel parameter (`fuel_in` / `fuel_out`).

## Mapping Strategy

### Constructs

| Scheme Construct | Rust Equivalent | Notes |
|------------------|-----------------|-------|
| `(fn (x) body)` | `extern "C" fn(x, fuel) -> Result` | Top-level FFI functions. |
| `(if c t e)` | `if c { t } else { e }` | Rust `if` is an expression. |
| `(let ((x v)) b)`| `{ let x = v; b }` | Block with binding. |
| `(prim 'add a b)`| `(a + b)` | Binary infix. |

### Types

| Fold Type | Rust Type | Notes |
|-----------|-----------|-------|
| `Nat` | `u64` | |
| `Int` | `i64` | |
| `Bool` | `bool` | Requires conversion for f64 result |
| `Char` | `char` | |
| `String` | `String` | FFI uses `*const c_char` |
| `Bytes` | `Vec<u8>` | |
| `Hash` | `[u8; 33]` | Versioned block address |

### Layer 1 Operators (Implemented)

| Category | Scheme | Rust | Style |
|----------|--------|------|-------|
| Arithmetic | `add`, `sub`, `mul`, `div`, `mod` | `+`, `-`, `*`, `/`, `%` | Binary infix |
| Comparison | `lt?`, `le?`, `gt?`, `ge?`, `eq?` | `<`, `<=`, `>`, `>=`, `==` | Binary infix |
| Logical | `and`, `or` | `&&`, `\|\|` | Binary infix |
| Logical | `not` | `!` | Unary prefix |
| Bitwise | `bitand`, `bitor`, `bitxor` | `&`, `\|`, `^` | Binary infix |
| Bitwise | `shl`, `shr` | `<<`, `>>` | Binary infix |
| Math | `abs`, `sqrt`, `sin`, `cos`, `tan` | `.abs()`, `.sqrt()`, etc. | Method call |
| Math | `log`, `floor`, `ceiling`, `round` | `.ln()`, `.floor()`, `.ceil()`, `.round()` | Method call |
| Unary | `neg` | `-x` | Unary prefix |
| Binary | `expt` | `.powf(y)` | Method call |

## Performance Considerations
- **Zero-cost abstractions:** Use Rust's ownership and typing to avoid overhead.
- **Inlining:** Leverage `#[inline(always)]` for small primitives.
- **Static Linking:** Generated code is compiled into a dynamic library (`.so` / `.dll`) and loaded via Scheme's FFI.

## Totality and Safety
- All generated Rust code must include fuel checks.
- Recursive calls must decrement fuel and check for exhaustion.
- The codegen only supports a subset of the language known to be total.
