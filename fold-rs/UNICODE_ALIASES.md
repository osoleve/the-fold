# Unicode Symbol Aliases

## Overview

The fold-rs parser now supports Unicode mathematical symbols as aliases for ASCII equivalents, improving .ss file compatibility and enabling more mathematical notation in Scheme code.

## Supported Aliases

| Unicode | ASCII | Name/Purpose |
|---------|-------|--------------|
| λ (U+03BB) | `lambda` | Lambda function |
| ∧ (U+2227) | `and` | Logical AND |
| ∨ (U+2228) | `or` | Logical OR |
| π (U+03C0) | `pi` | Pi constant |
| ¬ (U+00AC) | `not` | Logical NOT |
| → (U+2192) | `->` | Right arrow |
| ← (U+2190) | `<-` | Left arrow |
| ≤ (U+2264) | `<=` | Less than or equal |
| ≥ (U+2265) | `>=` | Greater than or equal |
| ≠ (U+2260) | `<>` | Not equal |
| × (U+00D7) | `*` | Multiplication |
| ÷ (U+00F7) | `/` | Division |

## Implementation

The aliasing is implemented in `/home/user/the-fold/fold-rs/src/tools/fold_parse.rs`:

1. **`normalize_unicode_symbol()`** function maps Unicode symbols to ASCII equivalents
2. Called during symbol parsing in `parse_expr()`
3. Transparent to the rest of the system - symbols are stored as ASCII

## Usage Examples

```scheme
;; Lambda functions with λ
(λ (x) (* x 2))

;; Logical operators
(∧ #t #f)
(∨ (¬ x) y)

;; Comparisons
(≤ x 10)
(≥ y 5)
(≠ a b)

;; Arithmetic
(× 3 4)
(÷ 10 2)

;; Complex expressions
(λ (x y)
  (∧ (≤ x y)
     (≠ x 0)))
```

## Testing

### Unit Tests
Location: `/home/user/the-fold/fold-rs/tests/unicode_aliases.rs`

Run with: `cargo test --test unicode_aliases`

All 10 tests pass:
- Individual alias tests for each symbol
- Complex nested expressions
- Non-aliased Unicode symbols remain unchanged

### Demo Program
Location: `/home/user/the-fold/fold-rs/examples/unicode_parse.rs`

Run with: `cargo run --example unicode_parse`

Demonstrates all aliases with visual verification.

### Demo File
Location: `/home/user/the-fold/fold-rs/tests/unicode_demo.ss`

Sample Scheme code using Unicode symbols.

## Benefits

1. **Better .ss compatibility** - Matches common Scheme Unicode conventions
2. **Mathematical notation** - Code reads more like mathematical formulas
3. **No runtime overhead** - Aliasing happens during parsing
4. **Backward compatible** - ASCII versions still work
5. **Extensible** - Easy to add more aliases if needed

## Files Modified

- `/home/user/the-fold/fold-rs/src/tools/fold_parse.rs` - Core implementation
- `/home/user/the-fold/fold-rs/src/thimble/prim.rs` - Added missing match arms
- `/home/user/the-fold/fold-rs/src/fabric/block.rs` - Added Hash derive

## Files Created

- `/home/user/the-fold/fold-rs/tests/unicode_aliases.rs` - Comprehensive tests
- `/home/user/the-fold/fold-rs/examples/unicode_parse.rs` - Demo program
- `/home/user/the-fold/fold-rs/tests/unicode_demo.ss` - Example Scheme file
- `/home/user/the-fold/fold-rs/UNICODE_ALIASES.md` - This documentation

## Future Enhancements

Consider adding:
- More Greek letters (α, β, γ, δ, ε, etc.)
- Set theory symbols (∈, ∉, ⊂, ⊃, ∪, ∩, ∅)
- Mathematical operators (√, ∑, ∏, ∫, ∂)
- Arrows (⇒, ⇐, ↔, ⇔)

These symbols are already recognized by the parser but don't have ASCII aliases yet.
