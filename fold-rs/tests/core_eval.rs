use fold_rs::fabric::{
    Address,
    block::Block,
    env::Env,
    eval::{EvalOutcome, eval_loop},
    expr::{CaseArm, Expr},
    symbol::Symbol,
    value::Value,
};

fn sym(name: &str) -> Symbol {
    name.to_string()
}

fn addr(byte: u8) -> Address {
    let mut bytes = [0u8; 33];
    bytes[0] = 0;
    bytes[1] = byte;
    Address::from(bytes)
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
        test: Box::new(Expr::Value(Value::Bool(false))),
        then_branch: Box::new(Expr::Value(Value::Number(1))),
        else_branch: Box::new(Expr::Value(Value::Number(2))),
    };

    let out = eval_loop(expr, env, 10).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Number(2))));
}

#[test]
fn let_binds_values() {
    let env = Env::new();
    let expr = Expr::Let {
        bindings: vec![
            (sym("x"), Expr::Value(Value::Number(3))),
            (sym("y"), Expr::Value(Value::Number(4))),
        ],
        body: Box::new(Expr::Prim {
            op: sym("add"),
            args: vec![Expr::Var(sym("x")), Expr::Var(sym("y"))],
        }),
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
        expr: Box::new(Expr::Value(Value::Block(block))),
        arms: vec![
            CaseArm::new(sym("Nothing"), Vec::new(), Expr::Value(Value::Number(0))),
            CaseArm::new(sym("Just"), vec![sym("ref")], Expr::Var(sym("ref"))),
        ],
        else_body: None,
    };

    let out = eval_loop(expr, env, 10).unwrap();
    assert!(matches!(out, EvalOutcome::Done(Value::Address(_))));
}
