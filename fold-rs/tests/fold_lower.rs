use fold_rs::fabric::{Expr, Value};
use fold_rs::tools::{lower_expr, parse_fold_expr};

fn parse_and_lower(input: &str) -> Expr {
    let expr = parse_fold_expr(input, None).expect("parse failed");
    lower_expr(&expr).expect("lower failed")
}

fn list_to_vec(list: &Value) -> Vec<Value> {
    let mut items = Vec::new();
    let mut current = list;
    loop {
        match current {
            Value::Nil => return items,
            Value::Pair(head, tail) => {
                items.push((**head).clone());
                current = tail;
            }
            _ => panic!("expected proper list"),
        }
    }
}

#[test]
fn lower_literals_and_vars() {
    match parse_and_lower("42") {
        Expr::Value(Value::Number(n)) => assert_eq!(n, 42),
        other => panic!("expected number value, got {:?}", other),
    }

    match parse_and_lower("\"hi\"") {
        Expr::Value(Value::String(s)) => assert_eq!(s, "hi"),
        other => panic!("expected string value, got {:?}", other),
    }

    match parse_and_lower("#t") {
        Expr::Value(Value::Bool(true)) => {}
        other => panic!("expected bool value, got {:?}", other),
    }

    match parse_and_lower("x") {
        Expr::Var(name) => assert_eq!(name, "x"),
        other => panic!("expected var, got {:?}", other),
    }

    match parse_and_lower("()") {
        Expr::Value(Value::Nil) => {}
        other => panic!("expected nil, got {:?}", other),
    }
}

#[test]
fn lower_quote() {
    match parse_and_lower("'x") {
        Expr::Quote(Value::Symbol(sym)) => assert_eq!(sym, "x"),
        other => panic!("expected quoted symbol, got {:?}", other),
    }

    match parse_and_lower("'(1 2)") {
        Expr::Quote(Value::Pair(_, _)) => {}
        other => panic!("expected quoted list, got {:?}", other),
    }

    let expr = parse_and_lower("'(1 2 3)");
    match expr {
        Expr::Quote(value) => {
            let items = list_to_vec(&value);
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0], Value::Number(1)));
            assert!(matches!(items[1], Value::Number(2)));
            assert!(matches!(items[2], Value::Number(3)));
        }
        other => panic!("expected quoted list, got {:?}", other),
    }
}

#[test]
fn lower_core_forms() {
    match parse_and_lower("(fn (x y) x)") {
        Expr::Fn { params, body } => {
            assert_eq!(params, vec!["x".to_string(), "y".to_string()]);
            assert!(matches!(*body, Expr::Var(_)));
        }
        other => panic!("expected fn, got {:?}", other),
    }

    match parse_and_lower("(let ((x 1) (y 2)) x)") {
        Expr::Let { bindings, .. } => {
            assert_eq!(bindings.len(), 2);
            assert_eq!(bindings[0].0, "x");
            assert_eq!(bindings[1].0, "y");
        }
        other => panic!("expected let, got {:?}", other),
    }

    match parse_and_lower("(fix fact (fn (n) n))") {
        Expr::Fix { name, .. } => assert_eq!(name, "fact"),
        other => panic!("expected fix, got {:?}", other),
    }

    match parse_and_lower("(if #t 1 2)") {
        Expr::If { .. } => {}
        other => panic!("expected if, got {:?}", other),
    }

    match parse_and_lower("(case x ((True y) y) ((False) 0))") {
        Expr::Case { arms, .. } => {
            assert_eq!(arms.len(), 2);
            assert_eq!(arms[0].tag, "True");
            assert_eq!(arms[0].vars, vec!["y".to_string()]);
            assert_eq!(arms[1].tag, "False");
            assert!(arms[1].vars.is_empty());
        }
        other => panic!("expected case, got {:?}", other),
    }

    match parse_and_lower("(prim 'add 1 2)") {
        Expr::Prim { op, args } => {
            assert_eq!(op, "add");
            assert_eq!(args.len(), 2);
        }
        other => panic!("expected prim, got {:?}", other),
    }

    match parse_and_lower("(call f 1 2)") {
        Expr::Call { args, .. } => assert_eq!(args.len(), 2),
        other => panic!("expected call, got {:?}", other),
    }

    match parse_and_lower("(f 1 2)") {
        Expr::Call { args, .. } => assert_eq!(args.len(), 2),
        other => panic!("expected implicit call, got {:?}", other),
    }
}

#[test]
fn lower_accepts_float_literals() {
    let parsed = parse_fold_expr("2.5", None).expect("parse failed");
    let expr = lower_expr(&parsed).expect("lower failed");
    match expr {
        Expr::Value(Value::Float(n)) => {
            assert!((n - 2.5).abs() < 1e-9);
        }
        other => panic!("expected float literal, got {:?}", other),
    }
}
