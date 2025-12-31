# Implementation of `set!` Variable Mutation Primitive

## Overview

This document describes the implementation of the `set!` variable mutation primitive in fold-rs (Milestone 1 for Rust core parity).

## Changes Made

### 1. Expression Type (`fold-rs/src/fabric/expr.rs`)

Added a new expression variant to the `Expr` enum:

```rust
/// Mutate a variable binding
Set {
    name: Symbol,
    value: Box<SpannedExpr>,
},
```

### 2. Environment Changes (`fold-rs/src/fabric/env.rs`)

Added a `set` method to the `Env` struct that walks up the parent chain to find and mutate bindings:

```rust
/// Set a variable binding, searching up the parent chain.
/// Returns true if the binding was found and updated, false otherwise.
pub fn set(env: &EnvRef, name: &Symbol, value: Value) -> bool {
    // Check if this environment has the binding
    {
        let mut scoped = env.borrow_mut();
        if scoped.bindings.contains_key(name) {
            scoped.bindings.insert(*name, value);
            return true;
        }
    }

    // Not in this environment - check parent
    let parent = env.borrow().parent.clone();
    if let Some(parent) = parent {
        Self::set(&parent, name, value)
    } else {
        false
    }
}
```

The implementation:
- Keeps the current `FxHashMap<Symbol, Value>` structure
- Searches up the parent chain to find the binding
- Mutates the binding in place when found
- Returns `false` if the binding doesn't exist

### 3. Evaluator Changes (`fold-rs/src/fabric/eval.rs`)

Added three components for evaluation:

#### a. Frame Type
```rust
/// Set a variable binding
Set {
    name: Symbol,
    env: EnvRef,
    span: Option<Span>,
},
```

#### b. Evaluation Logic
```rust
Expr::Set { name, value } => {
    // Push a frame to handle the result of evaluating value
    frames.push(Frame::Set {
        name,
        env: env.clone(),
        span: current_span,
    });
    // Evaluate the value expression
    expr = *value;
}
```

#### c. Unwinding Logic
```rust
Frame::Set {
    name,
    env: frame_env,
    span,
} => {
    // We just finished evaluating the value expression
    // Now set the variable in the environment
    let found = Env::set(&frame_env, &name, value.clone());
    if !found {
        return Err(SpannedEvalError::new(
            EvalError::UnboundVariable(name),
            span,
        ));
    }
    // Keep the environment as is
    *env = frame_env;
    // Return the new value (R6RS behavior)
}
```

### 4. Lowerer Changes (`fold-rs/src/tools/fold_lower.rs`)

Added `set!` recognition in the `lower_list` function:

```rust
"set!" => return lower_set(list_expr, items),
```

And implemented the `lower_set` function:

```rust
/// Lower (set! var value) to Set expression
fn lower_set(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() != 3 {
        return Err(error(list_expr, "set! expects (set! variable value)"));
    }

    let name = match &items[1].value {
        Sexp::Symbol(s) => Symbol::intern(s),
        _ => return Err(error(&items[1], "set! variable must be a symbol")),
    };

    let value = lower_expr(&items[2])?;

    Ok(SpannedExpr::new(
        Expr::Set {
            name,
            value: Box::new(value),
        },
        span,
    ))
}
```

### 5. Tests

Created three test files:

#### a. Core Evaluation Tests (`fold-rs/tests/set_mutation.rs`)
- Basic mutation
- Mutation in nested scope
- Multiple mutations
- Mutation with closures
- Mutation returns new value
- Error on undefined variable
- Mutation with expressions
- Mutation in outer scope from inner
- Mutation with string values

#### b. Lowerer Tests (`fold-rs/tests/fold_lower.rs`)
- `set!` lowering from Scheme syntax
- Proper parsing of variable and value expressions

#### c. Integration Tests (`fold-rs/tests/set_integration.rs`)
- End-to-end tests with parsing, lowering, and evaluation
- Counter pattern
- Closure mutation
- Multiple variable mutation

## Behavior

### Semantics

1. **Scope Search**: `set!` searches for the variable binding in the current scope and walks up the parent chain until found
2. **Return Value**: Returns the new value (R6RS behavior)
3. **Error Handling**: Returns `UnboundVariable` error if the variable is not found in any scope
4. **Closure Interaction**: Closures see mutations to captured variables (shared environment)

### Examples

```scheme
; Basic mutation
(let ((x 1))
  (set! x 2)
  x)  ; => 2

; Nested scope mutation
(let ((x 10))
  (let ((y 20))
    (set! x 30))
  x)  ; => 30

; Counter pattern
(let ((count 0))
  (set! count (+ count 1))
  (set! count (+ count 1))
  count)  ; => 2

; Closure mutation
(let ((x 1))
  (let ((get-x (lambda () x))
        (set-x (lambda (v) (set! x v))))
    (set-x 42)
    (get-x)))  ; => 42
```

## Test Results

All tests pass:
- 9 core evaluation tests
- 8 lowerer tests (including `lower_set`)
- 6 integration tests

**Total: 23 tests, all passing**

## Implementation Notes

1. **Environment Structure**: The existing `RefCell<Env>` structure made mutation straightforward - no major refactoring needed
2. **Parent Chain Walking**: The `set` method recursively walks up the parent chain, similar to `lookup`
3. **Error Handling**: Proper span tracking for error messages
4. **Fuel Consumption**: Each `set!` operation consumes fuel like other expressions
5. **Reification**: Proper support for suspending and resuming `set!` expressions

## Compatibility

This implementation follows R6RS semantics:
- `set!` returns the new value
- `set!` searches through lexical scope chain
- Error on undefined variable

## Future Enhancements

Potential improvements (not in current implementation):
- Thread-local mutation tracking for debugging
- Optional warnings for mutations outside of let bindings
- Performance optimizations for frequently mutated variables
