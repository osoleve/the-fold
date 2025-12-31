use fold_rs::tools::fold_parse::{parse_fold_expr, PlainSexp, strip_spans};

#[test]
fn test_lambda_alias() {
    let result = parse_fold_expr("(λ (x) x)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            assert_eq!(items.len(), 3);
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "lambda"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_and_alias() {
    let result = parse_fold_expr("(∧ #t #f)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "and"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_or_alias() {
    let result = parse_fold_expr("(∨ #t #f)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "or"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_pi_alias() {
    let result = parse_fold_expr("(* 2 π)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[2] {
                PlainSexp::Symbol(s) => assert_eq!(s, "pi"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_not_alias() {
    let result = parse_fold_expr("(¬ x)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "not"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_arrow_aliases() {
    // Right arrow →
    let result = parse_fold_expr("(→ a b)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "->"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }

    // Left arrow ←
    let result = parse_fold_expr("(← a b)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "<-"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_comparison_aliases() {
    // ≤
    let result = parse_fold_expr("(≤ x y)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "<="),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }

    // ≥
    let result = parse_fold_expr("(≥ x y)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, ">="),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }

    // ≠
    let result = parse_fold_expr("(≠ x y)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "<>"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_arithmetic_aliases() {
    // ×
    let result = parse_fold_expr("(× 3 4)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "*"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }

    // ÷
    let result = parse_fold_expr("(÷ 10 2)", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "/"),
                _ => panic!("Expected symbol"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_complex_expression_with_aliases() {
    // Test a more complex expression mixing Unicode and ASCII
    let result = parse_fold_expr("(λ (x y) (∧ (≤ x y) (≠ x 0)))", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::List(items) => {
            // Check lambda
            match &items[0] {
                PlainSexp::Symbol(s) => assert_eq!(s, "lambda"),
                _ => panic!("Expected lambda"),
            }
            // Check body (third element)
            match &items[2] {
                PlainSexp::List(body_items) => {
                    // Check and
                    match &body_items[0] {
                        PlainSexp::Symbol(s) => assert_eq!(s, "and"),
                        _ => panic!("Expected and"),
                    }
                    // Check first condition (≤ x y)
                    match &body_items[1] {
                        PlainSexp::List(cond1) => {
                            match &cond1[0] {
                                PlainSexp::Symbol(s) => assert_eq!(s, "<="),
                                _ => panic!("Expected <="),
                            }
                        }
                        _ => panic!("Expected list"),
                    }
                    // Check second condition (≠ x 0)
                    match &body_items[2] {
                        PlainSexp::List(cond2) => {
                            match &cond2[0] {
                                PlainSexp::Symbol(s) => assert_eq!(s, "<>"),
                                _ => panic!("Expected <>"),
                            }
                        }
                        _ => panic!("Expected list"),
                    }
                }
                _ => panic!("Expected list"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_non_aliased_unicode_symbols_unchanged() {
    // Unicode symbols not in the alias list should remain unchanged
    let result = parse_fold_expr("α", None).unwrap();
    let plain = strip_spans(&result);
    match plain {
        PlainSexp::Symbol(s) => assert_eq!(s, "α"),
        _ => panic!("Expected symbol"),
    }
}
