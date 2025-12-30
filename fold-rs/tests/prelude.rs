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
    // (foldl + 0 '(1 2 3 4)) => 10
    // Primitives are first-class and can be passed directly!
    let result = eval_with_prelude("(foldl + 0 '(1 2 3 4))");
    assert_eq!(result, "10");
}

#[test]
fn test_foldr() {
    // (foldr cons '() '(1 2 3)) => (1 2 3)
    // Primitives are first-class and can be passed directly!
    let result = eval_with_prelude("(foldr cons '() '(1 2 3))");
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
    // (zip-with + '(1 2 3) '(10 20 30)) => (11 22 33)
    // Primitives are first-class and can be passed directly!
    let result = eval_with_prelude("(zip-with + '(1 2 3) '(10 20 30))");
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

#[test]
fn test_first_class_primitives() {
    // Test that primitives are first-class values
    // They can be stored in variables and passed around
    let result = eval_with_prelude("(let ((f +)) (f 2 3))");
    assert_eq!(result, "5");

    // map with primitive
    let result = eval_with_prelude("(map inc '(1 2 3))");
    assert_eq!(result, "(2 3 4)");

    // Compose primitives with user functions
    let result = eval_with_prelude("(map (compose inc double) '(1 2 3))");
    assert_eq!(result, "(3 5 7)");
}

#[test]
fn test_apply() {
    // Basic apply with primitive
    let result = eval_with_prelude("(apply + '(1 2 3))");
    assert_eq!(result, "6");

    // Apply with extra arguments before list
    let result = eval_with_prelude("(apply + 1 2 '(3 4))");
    assert_eq!(result, "10");

    // Apply with list operations
    let result = eval_with_prelude("(apply list '(a b c))");
    assert_eq!(result, "(a b c)");

    // Apply with string-append
    let result = eval_with_prelude("(apply string-append '(\"hello\" \" \" \"world\"))");
    assert_eq!(result, "\"hello world\"");
}

#[test]
fn test_partition() {
    // Partition splits list into (matches, non-matches)
    let result = eval_with_prelude("(partition (fn (x) (> x 2)) '(1 2 3 4 5))");
    assert_eq!(result, "((3 4 5) 1 2)");
}

#[test]
fn test_find_if() {
    // Find first matching element
    let result = eval_with_prelude("(find-if (fn (x) (> x 3)) '(1 2 3 4 5))");
    assert_eq!(result, "4");

    // No match returns #f
    let result = eval_with_prelude("(find-if (fn (x) (> x 10)) '(1 2 3))");
    assert_eq!(result, "#f");
}

#[test]
fn test_remove_if() {
    // Remove elements matching predicate
    let result = eval_with_prelude("(remove-if (fn (x) (> x 2)) '(1 2 3 4 5))");
    assert_eq!(result, "(1 2)");
}

#[test]
fn test_count_if() {
    // Count matching elements
    let result = eval_with_prelude("(count-if (fn (x) (> x 2)) '(1 2 3 4 5))");
    assert_eq!(result, "3");
}

#[test]
fn test_concat() {
    // Concatenate list of lists
    let result = eval_with_prelude("(concat '((1 2) (3 4) (5)))");
    assert_eq!(result, "(1 2 3 4 5)");
}

#[test]
fn test_replicate() {
    // Create n copies
    let result = eval_with_prelude("(replicate 4 'x)");
    assert_eq!(result, "(x x x x)");
}

#[test]
fn test_iterate() {
    // Generate sequence by repeated application
    let result = eval_with_prelude("(iterate double 5 1)");
    assert_eq!(result, "(1 2 4 8 16)");
}

#[test]
fn test_scanl() {
    // Running fold with intermediate results
    let result = eval_with_prelude("(scanl + 0 '(1 2 3 4))");
    assert_eq!(result, "(0 1 3 6 10)");
}

#[test]
fn test_curry2() {
    // Curry a 2-argument function
    let result = eval_with_prelude("(((curry2 +) 3) 4)");
    assert_eq!(result, "7");
}
