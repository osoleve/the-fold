use fold_rs::fabric::{env::Env, eval::{EvalOutcome, eval_spanned}};
use fold_rs::tools::{lower_expr, parse_fold_expr};

fn eval(input: &str) -> String {
    let expr = parse_fold_expr(input, None).expect("parse failed");
    let lowered = lower_expr(&expr).expect("lower failed");
    let env = Env::new();
    match eval_spanned(lowered, env, 100000) {
        Ok(EvalOutcome::Done(value)) => format!("{:?}", value),
        Ok(EvalOutcome::Suspended { .. }) => "Error: Out of fuel".to_string(),
        Err(e) => format!("Error: {}", e),
    }
}

#[test]
fn letrec_single_recursive_binding() {
    // Single recursive function - factorial
    let result = eval(
        r#"
        (letrec ((fact (lambda (n)
                        (if (= n 0)
                            1
                            (* n (fact (- n 1)))))))
          (fact 5))
        "#,
    );
    assert_eq!(result, "Number(120)");
}

#[test]
fn letrec_mutual_recursion_even_odd() {
    // Classic mutual recursion example
    let result = eval(
        r#"
        (letrec ((even? (lambda (n)
                         (if (= n 0)
                             #t
                             (odd? (- n 1)))))
                 (odd? (lambda (n)
                        (if (= n 0)
                            #f
                            (even? (- n 1))))))
          (even? 10))
        "#,
    );
    assert_eq!(result, "Bool(true)");

    let result = eval(
        r#"
        (letrec ((even? (lambda (n)
                         (if (= n 0)
                             #t
                             (odd? (- n 1)))))
                 (odd? (lambda (n)
                        (if (= n 0)
                            #f
                            (even? (- n 1))))))
          (odd? 10))
        "#,
    );
    assert_eq!(result, "Bool(false)");

    let result = eval(
        r#"
        (letrec ((even? (lambda (n)
                         (if (= n 0)
                             #t
                             (odd? (- n 1)))))
                 (odd? (lambda (n)
                        (if (= n 0)
                            #f
                            (even? (- n 1))))))
          (odd? 7))
        "#,
    );
    assert_eq!(result, "Bool(true)");
}

#[test]
fn letrec_empty_bindings() {
    // Empty bindings should just evaluate body
    let result = eval("(letrec () 42)");
    assert_eq!(result, "Number(42)");
}

#[test]
fn letrec_non_lambda_values() {
    // letrec should also work with non-lambda values
    // though they can't reference each other recursively
    let result = eval(
        r#"
        (letrec ((x 10)
                 (y 20))
          (+ x y))
        "#,
    );
    assert_eq!(result, "Number(30)");
}

#[test]
fn letrec_multiple_mutual_groups() {
    // Test with multiple mutually recursive functions
    let result = eval(
        r#"
        (letrec ((f (lambda (n) (if (= n 0) 0 (+ 1 (g (- n 1))))))
                 (g (lambda (n) (if (= n 0) 0 (+ 2 (h (- n 1))))))
                 (h (lambda (n) (if (= n 0) 0 (+ 3 (f (- n 1)))))))
          (f 3))
        "#,
    );
    // f(3) = 1 + g(2)
    // g(2) = 2 + h(1)
    // h(1) = 3 + f(0)
    // f(0) = 0
    // Working backwards: f(0)=0, h(1)=3+0=3, g(2)=2+3=5, f(3)=1+5=6
    assert_eq!(result, "Number(6)");
}

#[test]
fn letrec_fibonacci() {
    // Fibonacci using letrec
    let result = eval(
        r#"
        (letrec ((fib (lambda (n)
                       (if (<= n 1)
                           n
                           (+ (fib (- n 1)) (fib (- n 2)))))))
          (fib 10))
        "#,
    );
    assert_eq!(result, "Number(55)");
}

#[test]
fn letrec_nested() {
    // Nested letrec
    let result = eval(
        r#"
        (letrec ((outer (lambda (n)
                         (letrec ((inner (lambda (m)
                                          (if (= m 0)
                                              n
                                              (inner (- m 1))))))
                           (inner 5)))))
          (outer 42))
        "#,
    );
    assert_eq!(result, "Number(42)");
}

#[test]
fn letrec_with_multiple_body_expressions() {
    // Test implicit begin in body
    let result = eval(
        r#"
        (letrec ((x 10))
          (+ x 1)
          (+ x 2)
          (+ x 3))
        "#,
    );
    assert_eq!(result, "Number(13)");
}

#[test]
fn letrec_sum_list() {
    // Recursive sum-list function
    let result = eval(
        r#"
        (letrec ((sum-list (lambda (lst)
                            (if (null? lst)
                                0
                                (+ (car lst) (sum-list (cdr lst)))))))
          (sum-list '(1 2 3 4 5)))
        "#,
    );
    assert_eq!(result, "Number(15)");
}

#[test]
fn letrec_countdown() {
    // Test countdown function that builds a list
    let result = eval(
        r#"
        (letrec ((countdown (lambda (n)
                             (if (= n 0)
                                 '()
                                 (cons n (countdown (- n 1)))))))
          (countdown 5))
        "#,
    );
    assert_eq!(result, "Pair(Number(5), Pair(Number(4), Pair(Number(3), Pair(Number(2), Pair(Number(1), Nil)))))");
}

#[test]
fn letrec_ackermann() {
    // Ackermann function - a classic recursion test
    let result = eval(
        r#"
        (letrec ((ack (lambda (m n)
                       (if (= m 0)
                           (+ n 1)
                           (if (= n 0)
                               (ack (- m 1) 1)
                               (ack (- m 1) (ack m (- n 1))))))))
          (ack 3 2))
        "#,
    );
    assert_eq!(result, "Number(29)");
}

#[test]
fn letrec_mutual_recursion_three_functions() {
    // Three mutually recursive functions
    let result = eval(
        r#"
        (letrec ((a (lambda (n) (if (= n 0) 1 (b (- n 1)))))
                 (b (lambda (n) (if (= n 0) 2 (c (- n 1)))))
                 (c (lambda (n) (if (= n 0) 3 (a (- n 1))))))
          (list (a 0) (a 1) (a 2) (a 3)))
        "#,
    );
    assert_eq!(result, "Pair(Number(1), Pair(Number(2), Pair(Number(3), Pair(Number(1), Nil))))");
}

#[test]
fn letrec_vs_let_semantics() {
    // This should work with letrec but would fail with let
    // because f needs to reference itself
    let result = eval(
        r#"
        (letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))
          (f 4))
        "#,
    );
    assert_eq!(result, "Number(24)");
}
