# Rust Core Feature Parity Roadmap

**Goal:** Enable fold-rs to run .ss files that don't use Chez-specific features.

## Current Status Summary

| Feature | Status | Notes |
|---------|--------|-------|
| **Milestone 1: Core Language** | **Mostly Done** | Named let, set!, letrec done; values/let-values pending |
| **Milestone 2: Exceptions** | **Done** | guard, conditions, raise implemented (13 tests) |
| **Milestone 3: Macros** | Not Started | define-syntax explicitly skipped |
| **Milestone 4: Records** | **Done** | define-record-type implemented (14 tests) |
| **Milestone 5: Mutable Data** | **Done** | Hashtables implemented (13 tests), bytevector mutation pending |
| **Milestone 6: Ports** | **Done** | String ports + read primitive (17 tests), file ports pending |
| **Milestone 7: Test Framework** | **Unblocked** | Exceptions done; set! available |
| **Milestone 8: Prelude Compat** | **Partial** | Unicode aliases done (10 tests), format strings pending |

---

## Milestone 1: Core Language Completeness

**Goal:** Run simple .ss files without Chez-specific features

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| Named let loops | **Done** | `fold_lower.rs` | Lowers to fix |
| `set!` | **Done** | expr.rs, eval.rs | 15 tests passing |
| `set-car!` | **NOT IMPL** | prim.rs | Listed but returns UnknownPrimitive; pairs are immutable |
| `set-cdr!` | **NOT IMPL** | prim.rs | Listed but returns UnknownPrimitive; pairs are immutable |
| `values` | **Missing** | expr.rs, eval.rs | Multiple return values |
| `call-with-values` | **Missing** | eval.rs | Consumer for multiple values |
| `let-values` | **Missing** | fold_lower.rs | Destructuring multiple values |
| `letrec` | **Done** | fold_lower.rs | 13 tests passing; supports mutual recursion |
| Internal defines | **Partial** | fold_lower.rs | Works at top-level via `lower_program` |

### Implementation Plan

1. **set!** (Variable mutation)
   - Add `Expr::Set { name: Symbol, value: Box<SpannedExpr> }` to expr.rs
   - Add `Frame::Set` to eval.rs
   - Environment needs mutable cells (RefCell or similar)
   - Lowerer recognizes `(set! var val)` form

2. **values/call-with-values**
   - Add `Value::Values(Vec<Value>)` variant
   - `values` primitive creates Values
   - `call-with-values` receives Values and applies consumer
   - All single-value returns implicitly become Values of length 1

3. **letrec**
   - Extend `lower_let` to handle `letrec` keyword
   - Create placeholder bindings, then fill in values
   - Handle mutual recursion (multiple bindings)

---

## Milestone 2: Exception Handling

**Goal:** Support guard and condition system for error recovery
**Status:** ✅ Complete (13 tests passing)

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| Condition type | **Done** | value.rs | `Value::Condition { kind, message, irritants }` |
| `guard` form | **Done** | fold_lower.rs, eval.rs | Pattern-based exception catching |
| `raise`/`error` | **Done** | prim.rs | Full condition support |
| `condition?` | **Done** | prim.rs | Predicate for conditions |
| `condition-message` | **Done** | prim.rs | Extract message from condition |
| `with-exception-handler` | **Done** | eval.rs | Low-level handler registration |

### Implementation Details

The exception system uses a handler stack in the evaluator:
- `Frame::ExceptionHandler` tracks active handlers
- `raise` unwinds the stack looking for handlers
- `guard` expands to `with-exception-handler` + condition matching
- Conditions are structured values with kind, message, and irritants

---

## Milestone 3: Macro System

**Goal:** Support define-syntax and syntax-rules

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| `syntax-rules` patterns | **Missing** | New module | _, ..., literals |
| Pattern matching | **Missing** | New module | Destructure input forms |
| Template expansion | **Missing** | New module | Build output from patterns |
| Hygiene | **Missing** | New module | Basic variable capture avoidance |
| Nested macros | **Missing** | New module | Macros that expand to macros |
| `define-syntax` | **Skipped** | fold_lower.rs:296-300 | Currently returns nil |

### Implementation Plan

This is the hardest milestone. Consider:

1. **Phase 1: Simple syntax-rules**
   - Pattern matching without ellipsis
   - Basic template substitution
   - No hygiene initially

2. **Phase 2: Ellipsis patterns**
   - Match zero-or-more elements
   - Expand in templates

3. **Phase 3: Hygiene**
   - Mark introduced identifiers
   - Prevent accidental capture

Architecture options:
- **Eager expansion**: Expand macros during lowering (simpler)
- **Lazy expansion**: Expand during evaluation (more flexible)

---

## Milestone 4: Record Types

**Goal:** Support define-record-type for structured data

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| Constructor generation | **Done** | fold_lower.rs | `(make-foo field1 field2)` |
| Predicate generation | **Done** | fold_lower.rs | `(foo? x)` |
| Accessor generation | **Done** | fold_lower.rs | `(foo-field1 x)` |
| Mutator generation | **Missing** | fold_lower.rs | `(foo-field1-set! x val)` - not needed for basic support |
| Inheritance | **Missing** | fold_lower.rs | Parent record types - advanced feature |

### Implementation (Completed)

Records use tagged vectors: `#(record-type-tag field1 field2 ...)`

The `define-record-type` special form in `fold_lower.rs`:
- Generates constructor (`make-<type>`)
- Generates predicate (`<type>?`)
- Generates accessors (`<type>-<field>`)
- Works at expression level and via `lower_program` for top-level definitions
- 14 tests passing in `tests/records.rs`

---

## Milestone 5: Mutable Data Structures

**Goal:** Support hashtables and mutable bytevectors
**Status:** ✅ Hashtables Complete (13 tests passing)

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| `make-hashtable` | **Done** | prim.rs, value.rs | `Value::Hashtable(Rc<RefCell<HashMap>>)` |
| `hashtable-set!` | **Done** | prim.rs | Mutate hashtable |
| `hashtable-ref` | **Done** | prim.rs | Query hashtable |
| `hashtable-keys` | **Done** | prim.rs | List all keys |
| `hashtable-values` | **Done** | prim.rs | List all values |
| `hashtable-contains?` | **Done** | prim.rs | Key existence check |
| `hashtable-delete!` | **Done** | prim.rs | Remove key |
| `hashtable-size` | **Done** | prim.rs | Entry count |
| `hashtable-copy` | **Done** | prim.rs | Shallow copy |
| `hashtable-clear!` | **Done** | prim.rs | Remove all entries |
| `hashtable?` | **Done** | prim.rs | Type predicate |
| `make-bytevector` | **Exists** | prim.rs | `bv-make` |
| `bytevector-u8-set!` | **Missing** | prim.rs | Mutate bytevector |
| `bytevector-copy!` | **Missing** | prim.rs | Copy with mutation |
| Little-endian ops | **Missing** | prim.rs | `bytevector-u32-ref`, etc. |

### Implementation Notes

Hashtables use `Value::Hashtable(Rc<RefCell<HashMap<Value, Value>>>)` with interior mutability. Hash and Eq are implemented for Value to enable this.

---

## Milestone 6: Port System

**Goal:** Support string ports and file I/O with Scheme semantics
**Status:** ✅ String Ports Complete (17 tests passing)

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| `open-input-string` | **Done** | prim.rs, value.rs | String input port |
| `open-output-string` | **Done** | prim.rs, value.rs | String output port |
| `get-output-string` | **Done** | prim.rs | Extract from output port |
| `read` | **Done** | prim.rs | Parse S-expr from port (8 tests) |
| `read-char` | **Done** | prim.rs | Single char input |
| `peek-char` | **Done** | prim.rs | Lookahead |
| `write-char` | **Done** | prim.rs | Single char output |
| `port-eof?` | **Done** | prim.rs | EOF detection |
| `input-port?` | **Done** | prim.rs | Type predicate |
| `output-port?` | **Done** | prim.rs | Type predicate |
| `close-port` | **Done** | prim.rs | Close port |
| File ports | **Missing** | prim.rs | `open-input-file`, etc. |
| Current ports | **Missing** | Global state | `current-input-port`, etc. |

### Implementation Notes

Ports use `Value::Port(Rc<RefCell<Port>>)` where Port is an enum:
- `StringInput { content: String, position: usize }`
- `StringOutput { buffer: String }`

The `read` primitive integrates with the parser to read S-expressions from ports.

---

## Milestone 7: Test Framework Support

**Goal:** Run fabric/stitches/test-*.ss files natively

### Features

| Feature | Status | Blocked By |
|---------|--------|------------|
| Macro-based `define-test` | Partial | fold_lower.rs has special handling |
| `guard`-based assertions | **Blocked** | Milestone 2 |
| Statistics tracking | **Blocked** | Milestone 1 (set!) / Milestone 5 |
| `exit` with status | **Missing** | Need primitive |

### Current State

The lowerer has special handling for `define-test` and `test-group` that bypasses the need for macros. However, full compatibility requires exception handling.

---

## Milestone 8: Full Prelude Compatibility

**Goal:** All prelude.ss functions work
**Status:** Partial (Unicode aliases done)

### Features

| Feature | Status | Notes |
|---------|--------|-------|
| Unicode aliases | **Done** | Parser: `λ`, `∧`, `∨`, `¬`, `π`, `τ`, `φ`, `∞`, `≤`, `≥`, `≠`, `→` (10 tests) |
| Result type | **Partial** | `ok?`, `error?`, `unwrap-ok` in prelude |
| Monads | **Partial** | Maybe, Either work (pure Scheme) |
| Format strings | **Partial** | Basic `~a`, `~s` work |

---

## Recommended Implementation Order

Based on dependencies and unblocking value:

```
1. Milestone 2 (Exceptions)     → Unblocks test framework error handling
2. Milestone 5 (Mutable Data)   → Unblocks bytevector ops and hashtables
3. Milestone 6 (Ports)          → Unblocks sexpr->string pattern
4. Milestone 1 (Core Language)  → set! needed for test statistics
5. Milestone 4 (Records)        → Unblocks block.ss and core modules
6. Milestone 3 (Macros)         → Hardest, but enables full compatibility
7. Milestone 7 & 8              → Final integration
```

### Rationale

1. **Exceptions first**: Many test files use `guard` for error handling. Without this, tests can't report failures properly.

2. **Mutable data second**: Hashtables are used extensively. Bytevector mutations needed for block serialization.

3. **Ports third**: `sexpr->string` and `read` patterns are common. String ports enable in-memory parsing.

4. **Core language fourth**: `set!` is used sparingly but critical for stateful tests. `letrec` enables mutual recursion.

5. **Records fifth**: Many modules define record types. Can be approximated with tagged vectors initially.

6. **Macros last**: Most complex. The test framework already has lowerer support. Many macro patterns can be pre-expanded or implemented as special forms.

---

## Quick Wins

Status of quick wins:

1. ~~**Verify set-car!/set-cdr!**~~ - ❌ NOT implemented (pairs are immutable `Box<Value>`)
2. **Add `exit` primitive** - Still missing
3. ~~**Unicode symbol aliases in parser**~~ - ✅ Done (12 aliases, 10 tests)
4. ~~**Basic hashtable**~~ - ✅ Done (11 primitives, 13 tests)
5. ~~**letrec desugaring**~~ - ✅ Done (13 tests)

### Remaining Quick Wins

1. **Add `exit` primitive** - Simple process exit with status code
2. **Bytevector mutation** - Change from `Vec<u8>` to `Rc<RefCell<Vec<u8>>>`
3. **File ports** - Extend port system with file I/O

---

## Files to Modify

| File | Changes Needed |
|------|----------------|
| `src/fabric/value.rs` | Add Condition, Port, Hashtable, maybe Values |
| `src/fabric/expr.rs` | Add Set, maybe Guard |
| `src/fabric/eval.rs` | Exception handling, Set evaluation |
| `src/tools/fold_lower.rs` | New special forms: letrec, guard, set! |
| `src/tools/fold_parse.rs` | Unicode aliases |
| `src/thimble/prim.rs` | New primitives for ports, hashtables, etc. |

---

## Testing Strategy

1. Create `tests/milestone_*.rs` for each milestone
2. Port key .ss test files as integration tests
3. Use `ss_compatibility.rs` pattern for parity testing
4. Add specific unit tests for edge cases

---

## References

- Chez Scheme documentation: https://cisco.github.io/ChezScheme/csug9.5/
- R6RS standard: http://www.r6rs.org/
- SRFI documents for specific features

---

## Implementation Patterns & Lessons Learned

### Architecture: lower_expr vs lower_program

The lowering system has two entry points with different semantics:

- **`lower_expr()`**: Lowers a single S-expression to SpannedExpr. Returns one expression.
- **`lower_program()`**: Lowers multiple top-level forms, handling `define` and `define-record-type` specially by collecting bindings and wrapping the body in nested `let`s.

**Key insight**: When a special form like `define-record-type` generates multiple bindings, it needs integration with `lower_program` to work at top-level. The pattern is:

1. Create an `extract_*_bindings()` helper that returns `Vec<(Symbol, SpannedExpr)>`
2. Modify `lower_program` to recognize the form and call the helper
3. Keep `lower_*()` for expression-context usage (wraps bindings in nested lets)

### Testing Multi-Expression Programs

For tests with multiple top-level forms (like define followed by usage):

```rust
use fold_rs::tools::{format_value, lower_program, parse_fold_program};

fn eval(input: &str) -> String {
    let exprs = parse_fold_program(input, None).expect("parse failed");
    let lowered = lower_program(&exprs).expect("lower failed");
    let env = Env::new();

    let mut last_result = String::from("()");
    for expr in lowered {
        match eval_spanned(expr, env.clone(), 100000) {
            Ok(EvalOutcome::Done(value)) => last_result = format_value(&value),
            // ... error handling
        }
    }
    last_result
}
```

**Common mistake**: Using `parse_fold_expr` which only parses the first expression, silently ignoring the rest.

### Output Formatting

- **`format_value()`**: Pretty prints values in Scheme syntax (`#t`, `#f`, `(1 2 3)`)
- **`Debug` format**: Rust-style output (`Bool(true)`, `Pair(Number(1), ...)`)

Use `format_value` for test assertions that compare against expected Scheme output.

### Known Limitations

| Feature | Issue | Workaround |
|---------|-------|------------|
| `set-car!`/`set-cdr!` | Listed in builtins but returns UnknownPrimitive | Pairs use `Box<Value>` (immutable). Would need `Rc<RefCell<>>` |
| Pair mutation | Pairs are structurally immutable | Use vectors or hashtables for mutable collections |
| Bytevector mutation | `bv-make` creates immutable bytevector | Would need `Rc<RefCell<Vec<u8>>>` |

### Test Count Summary

| Test File | Count | Feature |
|-----------|-------|---------|
| `tests/records.rs` | 14 | define-record-type |
| `tests/hashtable.rs` | 13 | Hashtable operations |
| `tests/string_ports.rs` | 9 | String port I/O |
| `tests/read.rs` | 8 | read primitive |
| `tests/exception.rs` | 13 | guard/conditions |
| `tests/set_mutation.rs` | 9 | set! variable mutation |
| `tests/letrec.rs` | 13 | letrec mutual recursion |
| `tests/unicode_aliases.rs` | 10 | Unicode symbol aliases |
| **Total new tests** | **89+** | |
