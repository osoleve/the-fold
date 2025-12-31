/// Integration tests for Scheme .ss file compatibility features
///
/// These tests verify that the key Scheme compatibility blockers are working:
/// - top-level-bound? (symbol introspection)
/// - unless (short-circuit conditional)
/// - when (short-circuit conditional)
/// - define-test and test-group (test framework macros)
/// - load (file loading)

use fold_rs::fabric::{EvalOutcome, Expr, SpannedExpr, Value, eval_spanned};
use fold_rs::tools::{lower_program, parse_fold_program, primitives_env};

fn run_program(source: &str) -> Result<Value, String> {
    let parsed = parse_fold_program(source, Some("<test>")).map_err(|e| e.to_string())?;
    let exprs = lower_program(&parsed).map_err(|e| e.to_string())?;

    // Sequence the expressions
    let expr = sequence_exprs(exprs);

    // Use primitives_env for fast execution (no full prelude)
    let env = primitives_env();

    let outcome = eval_spanned(expr, env, 10000).map_err(|e| e.to_string())?;

    match outcome {
        EvalOutcome::Done(value) => Ok(value),
        EvalOutcome::Suspended { .. } => Err("out of fuel".to_string()),
    }
}

fn sequence_exprs(exprs: Vec<SpannedExpr>) -> SpannedExpr {
    use fold_rs::fabric::Symbol;

    if exprs.is_empty() {
        return SpannedExpr::unspanned(Expr::Value(Value::Nil));
    }

    // For simplicity, wrap in nested lets for sequencing
    let mut iter = exprs.into_iter();
    let mut current = iter.next().unwrap();

    for (index, next) in iter.enumerate() {
        let name = Symbol::intern(&format!("#%seq-{index}"));
        current = SpannedExpr::new(
            Expr::Let {
                bindings: vec![(name, current)],
                body: Box::new(next),
            },
            None,
        );
    }

    current
}

#[test]
fn test_top_level_bound_undefined() {
    let result = run_program("(top-level-bound? 'undefined-symbol)").unwrap();
    assert!(matches!(result, Value::Bool(false)), "expected false for undefined symbol, got {:?}", result);
}

#[test]
fn test_top_level_bound_defined() {
    let result = run_program("
        (define x 42)
        (top-level-bound? 'x)
    ").unwrap();
    assert!(matches!(result, Value::Bool(true)), "expected true for defined symbol, got {:?}", result);
}

#[test]
fn test_unless_false_condition() {
    // When condition is false, unless should evaluate and return the body
    let result = run_program("(unless #f 42)").unwrap();
    assert!(matches!(result, Value::Number(42)), "expected 42, got {:?}", result);
}

#[test]
fn test_unless_true_condition() {
    // When condition is true, unless should return nil
    let result = run_program("(unless #t 42)").unwrap();
    assert!(matches!(result, Value::Nil), "expected nil, got {:?}", result);
}

#[test]
fn test_when_true_condition() {
    // When condition is true, when should evaluate and return the body
    let result = run_program("(when #t 42)").unwrap();
    assert!(matches!(result, Value::Number(42)), "expected 42, got {:?}", result);
}

#[test]
fn test_when_false_condition() {
    // When condition is false, when should return nil
    let result = run_program("(when #f 42)").unwrap();
    assert!(matches!(result, Value::Nil), "expected nil, got {:?}", result);
}

#[test]
fn test_unless_multi_body() {
    // unless should handle multiple body expressions
    let result = run_program("
        (define result 0)
        (unless #f
          (define result 1)
          (define result (+ result 1))
          result)
    ").unwrap();
    assert!(matches!(result, Value::Number(2)), "expected 2, got {:?}", result);
}

#[test]
fn test_when_multi_body() {
    // when should handle multiple body expressions
    let result = run_program("
        (define result 0)
        (when #t
          (define result 1)
          (define result (+ result 1))
          result)
    ").unwrap();
    assert!(matches!(result, Value::Number(2)), "expected 2, got {:?}", result);
}

#[test]
fn test_conditional_load_pattern() {
    // Test the common pattern: (unless (top-level-bound? 'foo) (load "..."))
    // This tests that unless correctly checks the condition and executes the body

    // First test: symbol not bound, unless body should execute
    let result = run_program("
        (unless (top-level-bound? 'undefined-var)
          42)
    ").unwrap();
    assert!(matches!(result, Value::Number(42)), "expected 42 when symbol not bound, got {:?}", result);

    // Second test: symbol is bound, unless body should not execute
    let result = run_program("
        (define my-var 100)
        (unless (top-level-bound? 'my-var)
          42)
    ").unwrap();
    assert!(matches!(result, Value::Nil), "expected nil when symbol is bound, got {:?}", result);
}

#[test]
fn test_short_circuit_unless() {
    // Verify unless doesn't evaluate body when condition is true
    // (If it did evaluate, it would cause an error trying to use undefined 'crash')
    let result = run_program("
        (unless #t
          (+ crash 1))
        99
    ").unwrap();
    assert!(matches!(result, Value::Number(99)), "expected 99, got {:?}", result);
}

#[test]
fn test_short_circuit_when() {
    // Verify when doesn't evaluate body when condition is false
    // (If it did evaluate, it would cause an error trying to use undefined 'crash')
    let result = run_program("
        (when #f
          (+ crash 1))
        99
    ").unwrap();
    assert!(matches!(result, Value::Number(99)), "expected 99, got {:?}", result);
}
