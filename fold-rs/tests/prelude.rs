use fold_rs::fabric::{EvalOutcome, eval_spanned};
use fold_rs::tools::{lower_expr, parse_fold_expr, prelude_env};

fn eval_with_prelude(source: &str) -> String {
    let env = prelude_env(100_000).expect("prelude failed to load");
    let parsed = parse_fold_expr(source, Some("<test>")).expect("parse failed");
    let expr = lower_expr(&parsed).expect("lower failed");
    match eval_spanned(expr, env, 100_000) {
        Ok(EvalOutcome::Done(value)) => fold_rs::tools::format_value(&value),
        Ok(EvalOutcome::Suspended { .. }) => panic!("suspended"),
        Err(e) => panic!("eval error: {}", e),
    }
}

#[test]
fn test_prelude_loads() {
    // Just verify prelude loads without error
    let _env = prelude_env(100_000).expect("prelude should load");
}

#[test]
fn test_map() {
    // (map (fn (x) (+ x 1)) '(1 2 3)) => (2 3 4)
    let result = eval_with_prelude("(map (fn (x) (+ x 1)) '(1 2 3))");
    assert_eq!(result, "(2 3 4)");
}

#[test]
fn test_filter() {
    // (filter (fn (x) (> x 2)) '(1 2 3 4 5)) => (3 4 5)
    let result = eval_with_prelude("(filter (fn (x) (> x 2)) '(1 2 3 4 5))");
    assert_eq!(result, "(3 4 5)");
}

#[test]
fn test_foldl() {
    // (foldl (fn (a b) (+ a b)) 0 '(1 2 3 4)) => 10
    // Note: primitives like + are not first-class, must wrap in lambda
    let result = eval_with_prelude("(foldl (fn (a b) (+ a b)) 0 '(1 2 3 4))");
    assert_eq!(result, "10");
}

#[test]
fn test_foldr() {
    // (foldr (fn (a b) (cons a b)) '() '(1 2 3)) => (1 2 3)
    // Note: primitives like cons are not first-class, must wrap in lambda
    let result = eval_with_prelude("(foldr (fn (a b) (cons a b)) '() '(1 2 3))");
    assert_eq!(result, "(1 2 3)");
}

#[test]
fn test_any() {
    // (any (fn (x) (> x 3)) '(1 2 3 4 5)) => #t
    let result = eval_with_prelude("(any (fn (x) (> x 3)) '(1 2 3 4 5))");
    assert_eq!(result, "#t");

    // (any (fn (x) (> x 10)) '(1 2 3)) => #f
    let result = eval_with_prelude("(any (fn (x) (> x 10)) '(1 2 3))");
    assert_eq!(result, "#f");
}

#[test]
fn test_all() {
    // (all (fn (x) (> x 0)) '(1 2 3)) => #t
    let result = eval_with_prelude("(all (fn (x) (> x 0)) '(1 2 3))");
    assert_eq!(result, "#t");

    // (all (fn (x) (> x 2)) '(1 2 3)) => #f
    let result = eval_with_prelude("(all (fn (x) (> x 2)) '(1 2 3))");
    assert_eq!(result, "#f");
}

#[test]
fn test_take_while() {
    // (take-while (fn (x) (< x 3)) '(1 2 3 4 5)) => (1 2)
    let result = eval_with_prelude("(take-while (fn (x) (< x 3)) '(1 2 3 4 5))");
    assert_eq!(result, "(1 2)");
}

#[test]
fn test_drop_while() {
    // (drop-while (fn (x) (< x 3)) '(1 2 3 4 5)) => (3 4 5)
    let result = eval_with_prelude("(drop-while (fn (x) (< x 3)) '(1 2 3 4 5))");
    assert_eq!(result, "(3 4 5)");
}

#[test]
fn test_zip_with() {
    // (zip-with (fn (a b) (+ a b)) '(1 2 3) '(10 20 30)) => (11 22 33)
    // Note: primitives like + are not first-class, must wrap in lambda
    let result = eval_with_prelude("(zip-with (fn (a b) (+ a b)) '(1 2 3) '(10 20 30))");
    assert_eq!(result, "(11 22 33)");
}

#[test]
fn test_sum_list() {
    // (sum-list '(1 2 3 4 5)) => 15
    let result = eval_with_prelude("(sum-list '(1 2 3 4 5))");
    assert_eq!(result, "15");
}

#[test]
fn test_product_list() {
    // (product-list '(1 2 3 4)) => 24
    let result = eval_with_prelude("(product-list '(1 2 3 4))");
    assert_eq!(result, "24");
}

#[test]
fn test_nested_map() {
    // Nested higher-order function usage
    // (map (fn (x) (* x x)) (filter (fn (x) (> x 2)) '(1 2 3 4 5)))
    // => (9 16 25)
    let result = eval_with_prelude("(map (fn (x) (* x x)) (filter (fn (x) (> x 2)) '(1 2 3 4 5)))");
    assert_eq!(result, "(9 16 25)");
}
