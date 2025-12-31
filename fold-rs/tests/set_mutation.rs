use fold_rs::fabric::{
    env::Env,
    eval::{eval_loop, EvalOutcome},
    expr::{Expr, SpannedExpr},
    symbol::Symbol,
    value::Value,
};

fn sym(name: &str) -> Symbol {
    Symbol::intern(name)
}

fn spanned(expr: Expr) -> SpannedExpr {
    SpannedExpr::unspanned(expr)
}

#[test]
fn basic_mutation() {
    // (let ((x 1)) (set! x 2) x) => 2
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![(sym("x"), spanned(Expr::Value(Value::Number(1))))],
        body: Box::new(spanned(Expr::Let {
            bindings: vec![(
                sym("_set"),
                spanned(Expr::Set {
                    name: sym("x"),
                    value: Box::new(spanned(Expr::Value(Value::Number(2)))),
                }),
            )],
            body: Box::new(spanned(Expr::Var(sym("x")))),
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(2))));
}

#[test]
fn mutation_in_nested_scope() {
    // (let ((x 1)) (let ((y 2)) (set! x 3)) x) => 3
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![(sym("x"), spanned(Expr::Value(Value::Number(1))))],
        body: Box::new(spanned(Expr::Let {
            bindings: vec![
                (sym("y"), spanned(Expr::Value(Value::Number(2)))),
                (
                    sym("_set"),
                    spanned(Expr::Set {
                        name: sym("x"),
                        value: Box::new(spanned(Expr::Value(Value::Number(3)))),
                    }),
                ),
            ],
            body: Box::new(spanned(Expr::Var(sym("x")))),
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(3))));
}

#[test]
fn multiple_mutations() {
    // (let ((x 0)) (set! x 1) (set! x 2) x) => 2
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![(sym("x"), spanned(Expr::Value(Value::Number(0))))],
        body: Box::new(spanned(Expr::Let {
            bindings: vec![
                (
                    sym("_set1"),
                    spanned(Expr::Set {
                        name: sym("x"),
                        value: Box::new(spanned(Expr::Value(Value::Number(1)))),
                    }),
                ),
                (
                    sym("_set2"),
                    spanned(Expr::Set {
                        name: sym("x"),
                        value: Box::new(spanned(Expr::Value(Value::Number(2)))),
                    }),
                ),
            ],
            body: Box::new(spanned(Expr::Var(sym("x")))),
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(2))));
}

#[test]
fn mutation_with_closure() {
    // (let ((x 1))
    //   (let ((f (lambda () x)))
    //     (set! x 2)
    //     (f))) => 2
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![(sym("x"), spanned(Expr::Value(Value::Number(1))))],
        body: Box::new(spanned(Expr::Let {
            bindings: vec![(
                sym("f"),
                spanned(Expr::Fn {
                    params: vec![],
                    body: Box::new(spanned(Expr::Var(sym("x")))),
                }),
            )],
            body: Box::new(spanned(Expr::Let {
                bindings: vec![(
                    sym("_set"),
                    spanned(Expr::Set {
                        name: sym("x"),
                        value: Box::new(spanned(Expr::Value(Value::Number(2)))),
                    }),
                )],
                body: Box::new(spanned(Expr::Call {
                    func: Box::new(spanned(Expr::Var(sym("f")))),
                    args: vec![],
                })),
            })),
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(2))));
}

#[test]
fn mutation_returns_new_value() {
    // (let ((x 1)) (set! x 42)) => 42
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![(sym("x"), spanned(Expr::Value(Value::Number(1))))],
        body: Box::new(spanned(Expr::Set {
            name: sym("x"),
            value: Box::new(spanned(Expr::Value(Value::Number(42)))),
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(42))));
}

#[test]
fn error_on_undefined_variable() {
    // (set! undefined 1) => error
    let env = Env::new();
    let expr = Expr::Set {
        name: sym("undefined"),
        value: Box::new(spanned(Expr::Value(Value::Number(1)))),
    };

    let result = eval_loop(expr, env, 100);
    assert!(result.is_err());
    // Check that it's an UnboundVariable error
    match result {
        Err(e) => {
            assert!(
                format!("{:?}", e).contains("UnboundVariable"),
                "Expected UnboundVariable error, got: {:?}",
                e
            );
        }
        Ok(_) => panic!("Expected error, got Ok"),
    }
}

#[test]
fn mutation_with_expression() {
    // (let ((x 5)) (set! x (+ x 1)) x) => 6
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![(sym("x"), spanned(Expr::Value(Value::Number(5))))],
        body: Box::new(spanned(Expr::Let {
            bindings: vec![(
                sym("_set"),
                spanned(Expr::Set {
                    name: sym("x"),
                    value: Box::new(spanned(Expr::Prim {
                        op: sym("add"),
                        args: vec![
                            spanned(Expr::Var(sym("x"))),
                            spanned(Expr::Value(Value::Number(1))),
                        ],
                    })),
                }),
            )],
            body: Box::new(spanned(Expr::Var(sym("x")))),
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(6))));
}

#[test]
fn mutation_in_outer_scope_from_inner() {
    // (let ((x 10))
    //   (let ((y 20))
    //     (let ((z 30))
    //       (set! x 99)
    //       (set! y 88)))
    //   x) => 99
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![(sym("x"), spanned(Expr::Value(Value::Number(10))))],
        body: Box::new(spanned(Expr::Let {
            bindings: vec![(sym("y"), spanned(Expr::Value(Value::Number(20))))],
            body: Box::new(spanned(Expr::Let {
                bindings: vec![
                    (sym("z"), spanned(Expr::Value(Value::Number(30)))),
                    (
                        sym("_set1"),
                        spanned(Expr::Set {
                            name: sym("x"),
                            value: Box::new(spanned(Expr::Value(Value::Number(99)))),
                        }),
                    ),
                    (
                        sym("_set2"),
                        spanned(Expr::Set {
                            name: sym("y"),
                            value: Box::new(spanned(Expr::Value(Value::Number(88)))),
                        }),
                    ),
                ],
                body: Box::new(spanned(Expr::Var(sym("x")))),
            })),
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(99))));
}

#[test]
fn mutation_with_string_value() {
    // (let ((s "hello")) (set! s "world") s) => "world"
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![(
            sym("s"),
            spanned(Expr::Value(Value::String("hello".to_string()))),
        )],
        body: Box::new(spanned(Expr::Let {
            bindings: vec![(
                sym("_set"),
                spanned(Expr::Set {
                    name: sym("s"),
                    value: Box::new(spanned(Expr::Value(Value::String("world".to_string())))),
                }),
            )],
            body: Box::new(spanned(Expr::Var(sym("s")))),
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    match out {
        EvalOutcome::Done(Value::String(s)) => assert_eq!(s, "world"),
        _ => panic!("Expected string 'world', got: {:?}", out),
    }
}
