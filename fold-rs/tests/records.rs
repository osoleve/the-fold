//! Tests for define-record-type implementation
//!
//! Records are represented as tagged vectors: #(type-name field1 field2 ...)

use fold_rs::fabric::{env::Env, eval::{EvalOutcome, eval_spanned}};
use fold_rs::tools::{format_value, lower_program, parse_fold_program};

fn eval(input: &str) -> String {
    let exprs = parse_fold_program(input, None).expect("parse failed");
    let lowered = lower_program(&exprs).expect("lower failed");
    let env = Env::new();

    // Evaluate all expressions, return the last result
    let mut last_result = String::from("()");
    for expr in lowered {
        match eval_spanned(expr, env.clone(), 100000) {
            Ok(EvalOutcome::Done(value)) => last_result = format_value(&value),
            Ok(EvalOutcome::Suspended { .. }) => return "Error: Out of fuel".to_string(),
            Err(e) => return format!("Error: {}", e),
        }
    }
    last_result
}

/// Test basic record creation
#[test]
fn test_basic_record_creation() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (let ((p (make-point 10 20)))
          (list (point-x p) (point-y p)))
    "#);
    assert_eq!(result, "(10 20)");
}

/// Test record predicate returns true for correct type
#[test]
fn test_record_predicate_true() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (let ((p (make-point 10 20)))
          (point? p))
    "#);
    assert_eq!(result, "#t");
}

/// Test record predicate returns false for wrong type
#[test]
fn test_record_predicate_false_wrong_type() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (define-record-type rect (fields width height))
        (let ((r (make-rect 100 50)))
          (point? r))
    "#);
    assert_eq!(result, "#f");
}

/// Test record predicate returns false for non-vector
#[test]
fn test_record_predicate_false_not_vector() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (point? 42)
    "#);
    assert_eq!(result, "#f");
}

/// Test multiple record types don't interfere
#[test]
fn test_multiple_record_types() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (define-record-type person (fields name age))
        (let ((p (make-point 1 2))
              (per (make-person "Alice" 30)))
          (list (point-x p) (person-name per) (person-age per)))
    "#);
    assert_eq!(result, "(1 \"Alice\" 30)");
}

/// Test record with no fields
#[test]
fn test_record_with_no_fields() {
    let result = eval(r#"
        (define-record-type unit (fields))
        (let ((u (make-unit)))
          (unit? u))
    "#);
    assert_eq!(result, "#t");
}

/// Test record with many fields
#[test]
fn test_record_with_many_fields() {
    let result = eval(r#"
        (define-record-type big (fields a b c d e))
        (let ((b (make-big 1 2 3 4 5)))
          (list (big-a b) (big-b b) (big-c b) (big-d b) (big-e b)))
    "#);
    assert_eq!(result, "(1 2 3 4 5)");
}

/// Test accessor with correct index
#[test]
fn test_record_accessors() {
    let result = eval(r#"
        (define-record-type triple (fields first second third))
        (let ((t (make-triple 'a 'b 'c)))
          (list (triple-first t) (triple-second t) (triple-third t)))
    "#);
    assert_eq!(result, "(a b c)");
}

/// Test nested records
#[test]
fn test_nested_records() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (define-record-type line (fields start end))
        (let ((l (make-line (make-point 0 0) (make-point 10 10))))
          (list (point-x (line-start l)) (point-y (line-end l))))
    "#);
    assert_eq!(result, "(0 10)");
}

/// Test record as function return value
#[test]
fn test_record_as_function_return() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (let ((make-origin (lambda () (make-point 0 0))))
          (let ((p (make-origin)))
            (list (point-x p) (point-y p))))
    "#);
    assert_eq!(result, "(0 0)");
}

/// Test record in recursive function
#[test]
fn test_record_in_recursive_function() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (letrec ((sum-points
                   (lambda (ps acc)
                     (if (null? ps)
                         acc
                         (sum-points (cdr ps) (+ acc (point-x (car ps))))))))
          (sum-points (list (make-point 1 0) (make-point 2 0) (make-point 3 0)) 0))
    "#);
    assert_eq!(result, "6");
}

/// Test predicate with empty vector (edge case)
#[test]
fn test_record_predicate_with_empty_vector() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (point? (vec-make))
    "#);
    assert_eq!(result, "#f");
}

/// Test different record types have unique predicates
#[test]
fn test_different_record_types_dont_match() {
    let result = eval(r#"
        (define-record-type point (fields x y))
        (define-record-type size (fields width height))
        (let ((p (make-point 10 20))
              (s (make-size 10 20)))
          (list (point? p) (point? s) (size? p) (size? s)))
    "#);
    assert_eq!(result, "(#t #f #f #t)");
}

/// Test accessor indices are correct
#[test]
fn test_record_accessors_correct_indices() {
    let result = eval(r#"
        (define-record-type quad (fields a b c d))
        (let ((q (make-quad 1 2 3 4)))
          (list (quad-a q) (quad-d q)))
    "#);
    assert_eq!(result, "(1 4)");
}
