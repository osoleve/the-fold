# Rust Core Feature Parity Roadmap

**Goal:** Enable fold-rs to run .ss files that don't use Chez-specific features.

## Current Status Summary

| Feature | Status | Notes |
|---------|--------|-------|
| **Milestone 1: Core Language** | Partial | Named let done, missing set!, letrec, internal defines |
| **Milestone 2: Exceptions** | Not Started | No guard, conditions, or exception handlers |
| **Milestone 3: Macros** | Not Started | define-syntax explicitly skipped |
| **Milestone 4: Records** | Not Started | No define-record-type |
| **Milestone 5: Mutable Data** | Partial | Have bytevectors, missing hashtables and mutation ops |
| **Milestone 6: Ports** | Not Started | Uses primitives, no port abstraction |
| **Milestone 7: Test Framework** | Blocked | Needs macros and exceptions |
| **Milestone 8: Prelude Compat** | Partial | Many functions work, Unicode aliases missing |

---

## Milestone 1: Core Language Completeness

**Goal:** Run simple .ss files without Chez-specific features

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| Named let loops | **Done** | `fold_lower.rs` | Lowers to fix |
| `set!` | **Missing** | expr.rs, eval.rs | Need new Expr::Set variant |
| `set-car!` | **Listed** | prim.rs:202 | Listed in builtins, verify impl |
| `set-cdr!` | **Listed** | prim.rs:202 | Listed in builtins, verify impl |
| `values` | **Missing** | expr.rs, eval.rs | Multiple return values |
| `call-with-values` | **Missing** | eval.rs | Consumer for multiple values |
| `let-values` | **Missing** | fold_lower.rs | Destructuring multiple values |
| `letrec` | **Missing** | fold_lower.rs | Currently only `fix` for single recursive binding |
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

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| Condition type | **Missing** | value.rs | Structured exception values |
| `guard` form | **Missing** | fold_lower.rs, eval.rs | Pattern-based exception catching |
| `raise`/`error` | **Partial** | prim.rs | `error` exists but needs condition support |
| `condition?` | **Missing** | prim.rs | Predicate for conditions |
| `condition-message` | **Missing** | prim.rs | Extract message from condition |
| `with-exception-handler` | **Missing** | eval.rs | Low-level handler registration |

### Implementation Plan

1. **Condition values**
   - Add `Value::Condition { kind: Symbol, message: String, irritants: Vec<Value> }`
   - Or use tagged blocks: `(condition kind message irritants)`

2. **Exception mechanism**
   - Evaluator maintains exception handler stack
   - `raise` unwinds to nearest handler
   - `guard` installs handler and catches matching conditions

3. **guard form**
   ```scheme
   (guard (exn
           ((file-error? exn) "file not found")
           ((io-error? exn) "io error"))
     body...)
   ```

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
| Constructor generation | **Missing** | New module | `(make-foo field1 field2)` |
| Predicate generation | **Missing** | New module | `(foo? x)` |
| Accessor generation | **Missing** | New module | `(foo-field1 x)` |
| Mutator generation | **Missing** | New module | `(foo-field1-set! x val)` |
| Inheritance | **Missing** | New module | Parent record types |

### Implementation Plan

1. **Record representation**
   - Use tagged vectors: `#(record-type-tag field1 field2 ...)`
   - Or dedicated `Value::Record { type_id, fields: Vec<Value> }`

2. **define-record-type macro/special form**
   - Parse the definition
   - Generate constructor, predicate, accessors
   - Register type in environment

---

## Milestone 5: Mutable Data Structures

**Goal:** Support hashtables and mutable bytevectors

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| `make-hashtable` | **Missing** | prim.rs, value.rs | Need Value::Hashtable |
| `hashtable-set!` | **Missing** | prim.rs | Mutate hashtable |
| `hashtable-ref` | **Missing** | prim.rs | Query hashtable |
| `hashtable-keys` | **Missing** | prim.rs | List all keys |
| `make-bytevector` | **Exists** | prim.rs | `bv-make` |
| `bytevector-u8-set!` | **Missing** | prim.rs | Mutate bytevector |
| `bytevector-copy!` | **Missing** | prim.rs | Copy with mutation |
| Little-endian ops | **Missing** | prim.rs | `bytevector-u32-ref`, etc. |

### Implementation Plan

1. **Hashtable**
   - Add `Value::Hashtable(Rc<RefCell<HashMap<Value, Value>>>)`
   - Implement hash/eq for Value (already have `hash-value`)
   - Add primitives: make, set!, ref, delete!, keys, values, contains?

2. **Mutable bytevectors**
   - Current bytevectors may be immutable
   - Need `Rc<RefCell<Vec<u8>>>` for mutation
   - Add u8/u16/u32/u64 accessors with endianness

---

## Milestone 6: Port System

**Goal:** Support string ports and file I/O with Scheme semantics

### Features

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| `open-input-string` | **Missing** | prim.rs, value.rs | String input port |
| `open-output-string` | **Missing** | prim.rs, value.rs | String output port |
| `get-output-string` | **Missing** | prim.rs | Extract from output port |
| `read` | **Missing** | prim.rs | Parse S-expr from port |
| `read-char` | **Missing** | prim.rs | Single char input |
| `peek-char` | **Missing** | prim.rs | Lookahead |
| File ports | **Missing** | prim.rs | `open-input-file`, etc. |
| Current ports | **Missing** | Global state | `current-input-port`, etc. |

### Implementation Plan

1. **Port type**
   - Add `Value::Port(Rc<RefCell<Port>>)`
   - Port enum: StringInput, StringOutput, FileInput, FileOutput
   - Track position, buffer, etc.

2. **Reading**
   - Integrate parser with port system
   - `read` calls parser on port contents

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

### Features

| Feature | Status | Notes |
|---------|--------|-------|
| Unicode aliases | **Missing** | Parser: `λ`, `∧`, `∨`, `π` |
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

These can be implemented quickly and provide immediate value:

1. **Verify set-car!/set-cdr!** - Listed in builtins, may already work
2. **Add `exit` primitive** - Simple process exit
3. **Unicode symbol aliases in parser** - λ = lambda, etc.
4. **Basic hashtable** - Enables many patterns
5. **letrec desugaring** - Transform to fix + mutation

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
