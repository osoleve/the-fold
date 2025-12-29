use fold_rs::tools::{NumberLit, PlainSexp, parse_fold_expr, parse_fold_program, strip_spans};

fn parse_plain(input: &str) -> PlainSexp {
    let expr = parse_fold_expr(input, None).expect("parse failed");
    strip_spans(&expr)
}

fn expect_integer(expr: PlainSexp, expected: i64) {
    match expr {
        PlainSexp::Number(NumberLit::Integer(n)) => assert_eq!(n, expected),
        other => panic!("expected integer, got {:?}", other),
    }
}

fn expect_float(expr: PlainSexp, expected: f64) {
    match expr {
        PlainSexp::Number(NumberLit::Float(n)) => {
            assert!((n - expected).abs() < 1e-9, "expected {expected}, got {n}");
        }
        other => panic!("expected float, got {:?}", other),
    }
}

fn expect_symbol(expr: PlainSexp, expected: &str) {
    match expr {
        PlainSexp::Symbol(name) => assert_eq!(name, expected),
        other => panic!("expected symbol, got {:?}", other),
    }
}

#[test]
fn parse_numbers() {
    expect_integer(parse_plain("42"), 42);
    expect_integer(parse_plain("-17"), -17);
    expect_float(parse_plain("2.5"), 2.5);
    expect_float(parse_plain("-2.5"), -2.5);
}

#[test]
fn parse_strings_and_booleans() {
    match parse_plain("\"a\\nb\"") {
        PlainSexp::String(s) => assert_eq!(s, "a\nb"),
        other => panic!("expected string, got {:?}", other),
    }

    match parse_plain("#t") {
        PlainSexp::Bool(true) => {}
        other => panic!("expected true, got {:?}", other),
    }

    match parse_plain("#f") {
        PlainSexp::Bool(false) => {}
        other => panic!("expected false, got {:?}", other),
    }
}

#[test]
fn parse_symbols_and_lists() {
    expect_symbol(parse_plain("foo-bar"), "foo-bar");
    expect_symbol(parse_plain("list?"), "list?");
    expect_symbol(parse_plain("+"), "+");

    let list = parse_plain("(a b c)");
    match list {
        PlainSexp::List(items) => {
            assert_eq!(items.len(), 3);
            expect_symbol(items[0].clone(), "a");
            expect_symbol(items[1].clone(), "b");
            expect_symbol(items[2].clone(), "c");
        }
        other => panic!("expected list, got {:?}", other),
    }
}

#[test]
fn parse_quote_and_comments() {
    let quoted = parse_plain("'x");
    match quoted {
        PlainSexp::List(items) => {
            assert_eq!(items.len(), 2);
            expect_symbol(items[0].clone(), "quote");
            expect_symbol(items[1].clone(), "x");
        }
        other => panic!("expected list, got {:?}", other),
    }

    expect_integer(parse_plain("; comment\n42"), 42);
}

#[test]
fn parse_program_and_spans() {
    let exprs = parse_fold_program("a b", None).expect("program parse failed");
    assert_eq!(exprs.len(), 2);

    let expr = parse_fold_expr("42", Some("test.ss")).expect("parse failed");
    assert_eq!(expr.span.file, "test.ss");
    assert_eq!(expr.span.line, 1);
    assert_eq!(expr.span.column, 1);

    let expr = parse_fold_expr("  42", Some("test.ss")).expect("parse failed");
    assert_eq!(expr.span.column, 3);
}

#[test]
fn parse_errors() {
    assert!(parse_fold_expr("\"hello", None).is_err());
    assert!(parse_fold_expr("(a b", None).is_err());
    assert!(parse_fold_expr("", None).is_err());
}
