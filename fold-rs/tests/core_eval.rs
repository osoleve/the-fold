use fold_rs::fabric::{
    Address,
    block::Block,
    env::Env,
    eval::{EvalOutcome, eval_loop},
    expr::{CaseArm, Expr, SpannedExpr},
    symbol::Symbol,
    value::Value,
};

fn sym(name: &str) -> Symbol {
    Symbol::intern(name)
}

fn addr(byte: u8) -> Address {
    let mut bytes = [0u8; 33];
    bytes[0] = 0;
    bytes[1] = byte;
    Address::from(bytes)
}

fn spanned(expr: Expr) -> SpannedExpr {
    SpannedExpr::unspanned(expr)
}

#[test]
fn literal_consumes_fuel() {
    let env = Env::new();
    let expr = Expr::Value(Value::Number(42));
    let out = eval_loop(expr, env, 1).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(42))));

    let env = Env::new();
    let expr = Expr::Value(Value::Number(42));
    let out = eval_loop(expr, env, 0).unwrap();
    assert!(matches!(out, EvalOutcome::Suspended { .. }));
}

#[test]
fn if_selects_branch() {
    let env = Env::new();
    let expr = Expr::If {
        test: Box::new(spanned(Expr::Value(Value::Bool(false)))),
        then_branch: Box::new(spanned(Expr::Value(Value::Number(1)))),
        else_branch: Box::new(spanned(Expr::Value(Value::Number(2)))),
    };

    let out = eval_loop(expr, env, 10).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(2))));
}

#[test]
fn let_binds_values() {
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![
            (sym("x"), spanned(Expr::Value(Value::Number(3)))),
            (sym("y"), spanned(Expr::Value(Value::Number(4)))),
        ],
        body: Box::new(spanned(Expr::Prim {
            op: sym("add"),
            args: vec![spanned(Expr::Var(sym("x"))), spanned(Expr::Var(sym("y")))],
        })),
    };

    let out = eval_loop(expr, env, 10).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(7))));
}

#[test]
fn prim_with_no_args() {
    let env = Env::new();
    let expr = Expr::Prim {
        op: sym("vec-empty"),
        args: Vec::new(),
    };

    let out = eval_loop(expr, env, 1).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Vector(values)) if values.is_empty()));
}

#[test]
fn case_matches_block() {
    let env = Env::new();
    let block = Block::new(sym("Just"), Vec::new(), vec![addr(0xAA)]);
    let expr = Expr::Case {
        expr: Box::new(spanned(Expr::Value(Value::Block(block)))),
        arms: vec![
            CaseArm::new(
                sym("Nothing"),
                Vec::new(),
                spanned(Expr::Value(Value::Number(0))),
            ),
            CaseArm::new(
                sym("Just"),
                vec![sym("ref")],
                spanned(Expr::Var(sym("ref"))),
            ),
        ],
        else_body: None,
    };

    let out = eval_loop(expr, env, 10).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Address(_))));
}

/// Test that define inside a let binding propagates to subsequent bindings and body.
/// This is essential for (load ...) to work correctly.
#[test]
fn define_inside_let_propagates() {
    let env = Env::new();
    // (let ([_dummy (define y 42)])
    //   y)
    // Should evaluate to 42
    let expr = Expr::Let {
        bindings: vec![(
            sym("_dummy"),
            spanned(Expr::Define {
                name: sym("y"),
                value: Box::new(spanned(Expr::Value(Value::Number(42)))),
            }),
        )],
        body: Box::new(spanned(Expr::Var(sym("y")))),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(
        matches!(out, EvalOutcome::Done(Value::Number(42))),
        "Expected 42, got {:?}",
        out
    );
}

/// Test that multiple defines inside let bindings accumulate.
#[test]
fn multiple_defines_in_let_accumulate() {
    let env = Env::new();
    // (let ([_d1 (define x 10)]
    //       [_d2 (define y 20)])
    //   (+ x y))
    let expr = Expr::Let {
        bindings: vec![
            (
                sym("_d1"),
                spanned(Expr::Define {
                    name: sym("x"),
                    value: Box::new(spanned(Expr::Value(Value::Number(10)))),
                }),
            ),
            (
                sym("_d2"),
                spanned(Expr::Define {
                    name: sym("y"),
                    value: Box::new(spanned(Expr::Value(Value::Number(20)))),
                }),
            ),
        ],
        body: Box::new(spanned(Expr::Prim {
            op: sym("add"),
            args: vec![spanned(Expr::Var(sym("x"))), spanned(Expr::Var(sym("y")))],
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(
        matches!(out, EvalOutcome::Done(Value::Number(30))),
        "Expected 30, got {:?}",
        out
    );
}

/// Test that Bound expression checks if a symbol is bound
#[test]
fn bound_returns_true_for_defined_symbol() {
    let env = Env::new();
    // (let ((x 42)) (top-level-bound? 'x))
    let expr = Expr::Let {
        bindings: vec![(sym("x"), spanned(Expr::Value(Value::Number(42))))],
        body: Box::new(spanned(Expr::Bound { name: sym("x") })),
    };

    let out = eval_loop(expr, env, 10).unwrap();
    assert!(
        matches!(out, EvalOutcome::Done(Value::Bool(true))),
        "Expected true, got {:?}",
        out
    );
}

/// Test that Bound expression returns false for undefined symbol
#[test]
fn bound_returns_false_for_undefined_symbol() {
    let env = Env::new();
    let expr = Expr::Bound {
        name: sym("undefined-symbol"),
    };

    let out = eval_loop(expr, env, 10).unwrap();
    assert!(
        matches!(out, EvalOutcome::Done(Value::Bool(false))),
        "Expected false, got {:?}",
        out
    );
}

/// Test let* style nested lets where later bindings use earlier ones.
#[test]
fn nested_let_uses_earlier_bindings() {
    let env = Env::new();
    // (let ((a 10))
    //   (let ((b (+ a 5)))
    //     (+ a b)))
    // Simulates let* lowering
    let expr = Expr::Let {
        bindings: vec![(sym("a"), spanned(Expr::Value(Value::Number(10))))],
        body: Box::new(spanned(Expr::Let {
            bindings: vec![(
                sym("b"),
                spanned(Expr::Prim {
                    op: sym("add"),
                    args: vec![
                        spanned(Expr::Var(sym("a"))),
                        spanned(Expr::Value(Value::Number(5))),
                    ],
                }),
            )],
            body: Box::new(spanned(Expr::Prim {
                op: sym("add"),
                args: vec![spanned(Expr::Var(sym("a"))), spanned(Expr::Var(sym("b")))],
            })),
        })),
    };

    let out = eval_loop(expr, env, 100).unwrap();
    assert!(
        matches!(out, EvalOutcome::Done(Value::Number(25))),
        "Expected 25, got {:?}",
        out
    );
}
