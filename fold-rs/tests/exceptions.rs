use fold_rs::fabric::{env::Env, eval::eval_spanned};

fn eval_code(code: &str) -> Result<String, String> {
    // Parse the code
    use fold_rs::tools::fold_parse::parse_fold_program;
    use fold_rs::tools::fold_lower::lower_program;

    let parsed = parse_fold_program(code, Some("<test>"))
        .map_err(|e| format!("Parse error: {}", e))?;

    if parsed.is_empty() {
        return Err("No expressions to evaluate".to_string());
    }

    // Lower to expressions
    let exprs = lower_program(&parsed)
        .map_err(|e| format!("Lower error: {}", e))?;

    // Evaluate each expression
    let env = Env::new();
    let mut result = String::new();
    for expr in exprs {
        match eval_spanned(expr, env.clone(), 100000) {
            Ok(outcome) => {
                use fold_rs::fabric::eval::EvalOutcome;
                match outcome {
                    EvalOutcome::Done(value) => {
                        result = format!("{:?}", value);
                    }
                    EvalOutcome::Suspended { .. } => {
                        return Err("Evaluation suspended (out of fuel)".to_string())
                    }
                }
            }
            Err(e) => return Err(format!("{}", e)),
        }
    }
    Ok(result)
}

#[test]
fn test_make_condition() {
    let result = eval_code(r#"(make-condition 'error "test error" 1 2 3)"#);
    assert!(result.is_ok(), "Failed: {:?}", result);
    let value = result.unwrap();
    assert!(value.contains("Condition"), "Expected Condition, got: {}", value);
    assert!(value.contains("error"), "Expected error kind, got: {}", value);
    assert!(value.contains("test error"), "Expected message, got: {}", value);
}

#[test]
fn test_condition_predicates() {
    let result = eval_code(r#"(condition? (make-condition 'error "test"))"#);
    assert_eq!(result, Ok("Bool(true)".to_string()));

    let result = eval_code(r#"(condition? 42)"#);
    assert_eq!(result, Ok("Bool(false)".to_string()));
}

#[test]
fn test_condition_accessors() {
    // Test condition-kind
    let result = eval_code(r#"(condition-kind (make-condition 'file-error "msg"))"#);
    assert!(result.is_ok(), "Failed: {:?}", result);
    let value = result.unwrap();
    assert!(value.contains("file-error"), "Expected file-error, got: {}", value);

    // Test condition-message
    let result = eval_code(r#"(condition-message (make-condition 'error "test message"))"#);
    assert_eq!(result, Ok("String(\"test message\")".to_string()));

    // Test condition-irritants
    let result = eval_code(r#"(condition-irritants (make-condition 'error "msg" 1 2 3))"#);
    assert!(result.is_ok(), "Failed: {:?}", result);
    let value = result.unwrap();
    // Should be a list of (1 2 3)
    assert!(value.contains("Pair"), "Expected list, got: {}", value);
}

#[test]
fn test_raise_uncaught() {
    let result = eval_code(r#"(raise (make-condition 'error "uncaught"))"#);
    assert!(result.is_err(), "Expected error, got: {:?}", result);
    let err = result.unwrap_err();
    assert!(err.contains("uncaught exception"), "Expected uncaught exception, got: {}", err);
}

#[test]
fn test_with_exception_handler_catches() {
    let code = r#"
    (with-exception-handler
      (lambda (exn) "caught!")
      (lambda () (raise (make-condition 'error "test"))))
    "#;
    let result = eval_code(code);
    assert_eq!(result, Ok("String(\"caught!\")".to_string()));
}

#[test]
fn test_with_exception_handler_normal_return() {
    let code = r#"
    (with-exception-handler
      (lambda (exn) "handler")
      (lambda () "normal"))
    "#;
    let result = eval_code(code);
    assert_eq!(result, Ok("String(\"normal\")".to_string()));
}

#[test]
fn test_guard_catch_specific() {
    let code = r#"
    (guard (exn
            ((eq? (condition-kind exn) 'file-error) "file error!")
            ((eq? (condition-kind exn) 'type-error) "type error!")
            (else "other error"))
      (raise (make-condition 'file-error "cannot open file")))
    "#;
    let result = eval_code(code);
    assert_eq!(result, Ok("String(\"file error!\")".to_string()));
}

#[test]
fn test_guard_else_clause() {
    let code = r#"
    (guard (exn
            ((eq? (condition-kind exn) 'file-error) "file error!")
            (else "other error"))
      (raise (make-condition 'network-error "timeout")))
    "#;
    let result = eval_code(code);
    assert_eq!(result, Ok("String(\"other error\")".to_string()));
}

#[test]
fn test_guard_re_raise() {
    let code = r#"
    (guard (exn
            ((eq? (condition-kind exn) 'file-error) "file error!")
            (else (raise exn)))
      (raise (make-condition 'other-error "unexpected")))
    "#;
    let result = eval_code(code);
    // Should re-raise and become uncaught
    assert!(result.is_err(), "Expected error, got: {:?}", result);
}

#[test]
fn test_guard_normal_return() {
    let code = r#"
    (guard (exn
            ((eq? (condition-kind exn) 'error) "error!")
            (else "other"))
      42)
    "#;
    let result = eval_code(code);
    assert_eq!(result, Ok("Number(42)".to_string()));
}

#[test]
fn test_nested_guards() {
    let code = r#"
    (guard (outer
            ((eq? (condition-kind outer) 'outer-error) "outer!")
            (else "outer-else"))
      (guard (inner
              ((eq? (condition-kind inner) 'inner-error) "inner!")
              (else (raise (make-condition 'outer-error "from inner"))))
        (raise (make-condition 'unknown-error "test"))))
    "#;
    let result = eval_code(code);
    // Inner guard catches unknown-error with else, re-raises as outer-error
    // Outer guard catches outer-error
    assert_eq!(result, Ok("String(\"outer!\")".to_string()));
}

#[test]
fn test_exception_in_primitive() {
    let code = r#"
    (guard (exn
            (else "caught"))
      (raise (make-condition 'test "from primitive")))
    "#;
    let result = eval_code(code);
    assert_eq!(result, Ok("String(\"caught\")".to_string()));
}

#[test]
fn test_condition_with_irritants() {
    let code = r#"
    (guard (exn
            ((eq? (condition-kind exn) 'error)
             (condition-irritants exn))
            (else "no match"))
      (raise (make-condition 'error "msg" 10 20 30)))
    "#;
    let result = eval_code(code);
    assert!(result.is_ok(), "Failed: {:?}", result);
    let value = result.unwrap();
    // Should return a list of irritants
    assert!(value.contains("Pair"), "Expected list, got: {}", value);
}
