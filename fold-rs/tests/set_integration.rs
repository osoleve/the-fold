use fold_rs::fabric::{env::Env, eval::eval_spanned};
use fold_rs::tools::{lower_expr, parse_fold_expr};

fn eval_str(input: &str) -> String {
    let env = Env::new();
    let parsed = parse_fold_expr(input, None).expect("parse failed");
    let lowered = lower_expr(&parsed).expect("lower failed");
    let result = eval_spanned(lowered, env, 100000).expect("eval failed");
    format!("{:?}", result)
}

#[test]
fn test_set_basic() {
    let result = eval_str("(let ((x 1)) (set! x 2) x)");
    assert!(result.contains("Done(Number(2))"));
}

#[test]
fn test_set_nested() {
    let result = eval_str("(let ((x 10)) (let ((y 20)) (set! x 30)) x)");
    assert!(result.contains("Done(Number(30))"));
}

#[test]
fn test_set_with_expr() {
    let result = eval_str("(let ((x 5)) (set! x (+ x 3)) x)");
    assert!(result.contains("Done(Number(8))"));
}

#[test]
fn test_set_counter() {
    // Test a simple counter pattern
    let code = r#"
        (let ((counter 0))
          (set! counter (+ counter 1))
          (set! counter (+ counter 1))
          (set! counter (+ counter 1))
          counter)
    "#;
    let result = eval_str(code);
    assert!(result.contains("Done(Number(3))"));
}

#[test]
fn test_set_with_closure() {
    // Test that closures see mutations
    let code = r#"
        (let ((x 1))
          (let ((get-x (lambda () x))
                (set-x (lambda (v) (set! x v))))
            (set-x 42)
            (get-x)))
    "#;
    let result = eval_str(code);
    assert!(result.contains("Done(Number(42))"));
}

#[test]
fn test_set_multiple_vars() {
    let code = r#"
        (let ((a 1) (b 2) (c 3))
          (set! a 10)
          (set! b 20)
          (set! c 30)
          (+ a (+ b c)))
    "#;
    let result = eval_str(code);
    assert!(result.contains("Done(Number(60))"));
}
