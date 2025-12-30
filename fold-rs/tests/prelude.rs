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

#[test]
fn test_zip() {
    // Pair up two lists (pairs printed as 2-element lists)
    let result = eval_with_prelude("(zip '(1 2 3) '(a b c))");
    assert_eq!(result, "((1 a) (2 b) (3 c))");

    // Stops at shorter list
    let result = eval_with_prelude("(zip '(1 2) '(a b c d))");
    assert_eq!(result, "((1 a) (2 b))");
}

#[test]
fn test_unzip() {
    // Split list of pairs using zip output
    // unzip returns a 2-element list: (cars cdrs)
    let result = eval_with_prelude("(unzip (zip '(1 2 3) '(a b c)))");
    assert_eq!(result, "((1 2 3) (a b c))");
}

#[test]
fn test_intersperse() {
    // Insert separator between elements
    let result = eval_with_prelude("(intersperse 0 '(1 2 3))");
    assert_eq!(result, "(1 0 2 0 3)");

    // Empty list
    let result = eval_with_prelude("(intersperse 0 '())");
    assert_eq!(result, "()");

    // Single element
    let result = eval_with_prelude("(intersperse 0 '(1))");
    assert_eq!(result, "(1)");
}

#[test]
fn test_span() {
    // Split at first non-matching element
    let result = eval_with_prelude("(span (fn (x) (< x 3)) '(1 2 3 4 5))");
    assert_eq!(result, "((1 2) 3 4 5)");

    // All match
    let result = eval_with_prelude("(span (fn (x) (< x 10)) '(1 2 3))");
    assert_eq!(result, "((1 2 3))");
}

#[test]
fn test_flat_map() {
    // Map and flatten
    let result = eval_with_prelude("(flat-map (fn (x) (list x (* x 10))) '(1 2 3))");
    assert_eq!(result, "(1 10 2 20 3 30)");
}

#[test]
fn test_map_indexed() {
    // Map with index
    let result = eval_with_prelude("(map-indexed (fn (i x) (+ i x)) '(10 20 30))");
    assert_eq!(result, "(10 21 32)");
}

#[test]
fn test_filter_indexed() {
    // Filter keeping even indices
    let result = eval_with_prelude("(filter-indexed (fn (i x) (even? i)) '(a b c d e))");
    assert_eq!(result, "(a c e)");
}

#[test]
fn test_nth_safe() {
    // Valid index
    let result = eval_with_prelude("(nth-safe 2 '(a b c d))");
    assert_eq!(result, "c");

    // Out of bounds returns #f
    let result = eval_with_prelude("(nth-safe 10 '(a b c))");
    assert_eq!(result, "#f");
}

#[test]
fn test_max_by() {
    // Find max by absolute value
    let result = eval_with_prelude("(max-by abs '(-5 3 -2 4))");
    assert_eq!(result, "-5");

    // Empty list returns #f
    let result = eval_with_prelude("(max-by abs '())");
    assert_eq!(result, "#f");
}

#[test]
fn test_min_by() {
    // Find min by absolute value
    let result = eval_with_prelude("(min-by abs '(-5 3 -2 4))");
    assert_eq!(result, "-2");
}

#[test]
fn test_on() {
    // Compare by length
    let result = eval_with_prelude("((on < length) '(1 2) '(1 2 3))");
    assert_eq!(result, "#t");

    let result = eval_with_prelude("((on < length) '(1 2 3) '(1 2))");
    assert_eq!(result, "#f");
}

#[test]
fn test_pipe() {
    // Pipe: left-to-right composition
    let result = eval_with_prelude("((pipe inc double) 5)");
    assert_eq!(result, "12");

    // Compose would give: (double (inc 5)) = 12
    // Pipe gives: (inc 5) = 6, then (double 6) = 12
    // Actually both give 12 for this case, let me use a different example
    let result = eval_with_prelude("((pipe double inc) 5)");
    assert_eq!(result, "11"); // (double 5) = 10, (inc 10) = 11
}

#[test]
fn test_juxt() {
    // Apply multiple functions to same argument
    let result = eval_with_prelude("((juxt (list inc dec)) 5)");
    assert_eq!(result, "(6 4)");

    // With more functions
    let result = eval_with_prelude("((juxt (list inc dec double)) 10)");
    assert_eq!(result, "(11 9 20)");
}

#[test]
fn test_unfold() {
    // Generate list 1..5
    let result = eval_with_prelude("(unfold (fn (x) (> x 5)) id inc 1)");
    assert_eq!(result, "(1 2 3 4 5)");

    // Empty result
    let result = eval_with_prelude("(unfold (fn (x) #t) id inc 1)");
    assert_eq!(result, "()");
}

#[test]
fn test_tails() {
    // All suffixes
    let result = eval_with_prelude("(tails '(1 2 3))");
    assert_eq!(result, "((1 2 3) (2 3) (3) ())");
}

#[test]
fn test_inits() {
    // All prefixes
    let result = eval_with_prelude("(inits '(1 2 3))");
    assert_eq!(result, "(() (1) (1 2) (1 2 3))");
}

#[test]
fn test_group_consecutive() {
    // Group runs of equal elements
    let result = eval_with_prelude("(group-consecutive '(1 1 2 2 2 3))");
    assert_eq!(result, "((1 1) (2 2 2) (3))");

    // No duplicates
    let result = eval_with_prelude("(group-consecutive '(1 2 3))");
    assert_eq!(result, "((1) (2) (3))");
}

#[test]
fn test_range_list() {
    // Generate range
    let result = eval_with_prelude("(range-list 1 5)");
    assert_eq!(result, "(1 2 3 4)");
}

#[test]
fn test_repeat_fn() {
    // Apply function n times
    let result = eval_with_prelude("(repeat-fn inc 5 0)");
    assert_eq!(result, "5");

    let result = eval_with_prelude("(repeat-fn double 4 1)");
    assert_eq!(result, "16");
}

#[test]
fn test_maybe() {
    // Apply function if not #f
    let result = eval_with_prelude("(maybe 0 inc 5)");
    assert_eq!(result, "6");

    // Return default if #f
    let result = eval_with_prelude("(maybe 0 inc #f)");
    assert_eq!(result, "0");
}

#[test]
fn test_from_maybe() {
    let result = eval_with_prelude("(from-maybe 0 5)");
    assert_eq!(result, "5");

    let result = eval_with_prelude("(from-maybe 0 #f)");
    assert_eq!(result, "0");
}

#[test]
fn test_cat_maybes() {
    let result = eval_with_prelude("(cat-maybes '(1 #f 2 #f 3))");
    assert_eq!(result, "(1 2 3)");
}

#[test]
fn test_chunks() {
    let result = eval_with_prelude("(chunks 2 '(1 2 3 4 5))");
    assert_eq!(result, "((1 2) (3 4) (5))");

    let result = eval_with_prelude("(chunks 3 '(1 2 3 4 5 6))");
    assert_eq!(result, "((1 2 3) (4 5 6))");
}

#[test]
fn test_sliding() {
    let result = eval_with_prelude("(sliding 2 '(1 2 3 4))");
    assert_eq!(result, "((1 2) (2 3) (3 4))");

    let result = eval_with_prelude("(sliding 3 '(1 2 3 4))");
    assert_eq!(result, "((1 2 3) (2 3 4))");
}

#[test]
fn test_pairs() {
    let result = eval_with_prelude("(pairs '(1 2 3 4))");
    assert_eq!(result, "((1 2) (2 3) (3 4))");
}

#[test]
fn test_split_at() {
    let result = eval_with_prelude("(split-at 2 '(a b c d e))");
    assert_eq!(result, "((a b) c d e)");
}

#[test]
fn test_elem() {
    let result = eval_with_prelude("(elem? 3 '(1 2 3 4))");
    assert_eq!(result, "#t");

    let result = eval_with_prelude("(elem? 5 '(1 2 3 4))");
    assert_eq!(result, "#f");
}

#[test]
fn test_nub() {
    let result = eval_with_prelude("(nub '(1 2 1 3 2 4))");
    assert_eq!(result, "(1 2 3 4)");
}

#[test]
fn test_intercalate() {
    let result = eval_with_prelude("(intercalate '(0) '((1 2) (3 4) (5)))");
    assert_eq!(result, "(1 2 0 3 4 0 5)");
}

#[test]
fn test_transpose() {
    let result = eval_with_prelude("(transpose '((1 2 3) (4 5 6)))");
    assert_eq!(result, "((1 4) (2 5) (3 6))");
}

// ========== New Numeric and Comparison Tests ==========

#[test]
fn test_sum_by() {
    // Sum lengths of strings
    let result = eval_with_prelude("(sum-by length '((1) (1 2) (1 2 3)))");
    assert_eq!(result, "6");
}

#[test]
fn test_product_by() {
    let result = eval_with_prelude("(product-by length '((1 2) (1 2 3)))");
    assert_eq!(result, "6");
}

#[test]
fn test_average() {
    let result = eval_with_prelude("(average '(1 2 3 4 5))");
    assert_eq!(result, "3");

    // Empty list
    let result = eval_with_prelude("(average '())");
    assert_eq!(result, "0");
}

#[test]
fn test_even_indices() {
    let result = eval_with_prelude("(even-indices '(a b c d e))");
    assert_eq!(result, "(a c e)");
}

#[test]
fn test_odd_indices() {
    let result = eval_with_prelude("(odd-indices '(a b c d e))");
    assert_eq!(result, "(b d)");
}

#[test]
fn test_sort_by() {
    // Sort by length
    let result = eval_with_prelude("(sort-by length '((1 2 3) (1) (1 2)))");
    assert_eq!(result, "((1) (1 2) (1 2 3))");
}

#[test]
fn test_compare_by() {
    // Less than
    let result = eval_with_prelude("(compare-by length '(1) '(1 2))");
    assert_eq!(result, "-1");

    // Equal
    let result = eval_with_prelude("(compare-by length '(1 2) '(a b))");
    assert_eq!(result, "0");

    // Greater than
    let result = eval_with_prelude("(compare-by length '(1 2 3) '(a))");
    assert_eq!(result, "1");
}

#[test]
fn test_equal_by() {
    let result = eval_with_prelude("(equal-by length '(a b c) '(1 2 3))");
    assert_eq!(result, "#t");

    let result = eval_with_prelude("(equal-by length '(a b) '(1 2 3))");
    assert_eq!(result, "#f");
}

#[test]
fn test_group_by() {
    // Group by even/odd - odds first since 1 is encountered first (even? 1 = #f)
    let result = eval_with_prelude("(group-by even? '(1 2 3 4 5 6))");
    assert_eq!(result, "((1 3 5) (2 4 6))");
}

#[test]
fn test_apply_n() {
    // Apply inc 3 times, returning all intermediates
    let result = eval_with_prelude("(apply-n inc 3 0)");
    assert_eq!(result, "(0 1 2 3)");
}

#[test]
fn test_until() {
    // Double until > 10
    let result = eval_with_prelude("(until (fn (x) (> x 10)) double 1)");
    assert_eq!(result, "16");
}

#[test]
fn test_converge() {
    // Converge to fixed point (simple case)
    let result = eval_with_prelude("(converge (fn (x) (if (> x 5) x (+ x 1))) 0)");
    assert_eq!(result, "6");
}

#[test]
fn test_both() {
    // Both positive? and even?
    let result = eval_with_prelude("((both positive? even?) 4)");
    assert_eq!(result, "#t");

    let result = eval_with_prelude("((both positive? even?) 3)");
    assert_eq!(result, "#f");

    let result = eval_with_prelude("((both positive? even?) -2)");
    assert_eq!(result, "#f");
}

#[test]
fn test_either() {
    let result = eval_with_prelude("((either positive? even?) -2)");
    assert_eq!(result, "#t");

    let result = eval_with_prelude("((either positive? even?) 3)");
    assert_eq!(result, "#t");

    let result = eval_with_prelude("((either positive? even?) -3)");
    assert_eq!(result, "#f");
}

#[test]
fn test_neither() {
    let result = eval_with_prelude("((neither positive? even?) -3)");
    assert_eq!(result, "#t");

    let result = eval_with_prelude("((neither positive? even?) 2)");
    assert_eq!(result, "#f");
}

#[test]
fn test_frequencies() {
    let result = eval_with_prelude("(frequencies '(a b a c b a))");
    assert_eq!(result, "((a 3) (b 2) (c 1))");
}

#[test]
fn test_index_where() {
    let result = eval_with_prelude("(index-where even? '(1 3 4 5))");
    assert_eq!(result, "2");

    // No match
    let result = eval_with_prelude("(index-where even? '(1 3 5))");
    assert_eq!(result, "#f");
}

#[test]
fn test_indices_where() {
    let result = eval_with_prelude("(indices-where even? '(1 2 3 4 5 6))");
    assert_eq!(result, "(1 3 5)");
}

#[test]
fn test_last_where() {
    let result = eval_with_prelude("(last-where even? '(1 2 3 4 5))");
    assert_eq!(result, "4");

    // No match
    let result = eval_with_prelude("(last-where even? '(1 3 5))");
    assert_eq!(result, "#f");
}

#[test]
fn test_update_at() {
    let result = eval_with_prelude("(update-at 1 inc '(1 2 3))");
    assert_eq!(result, "(1 3 3)");
}

#[test]
fn test_insert_at() {
    let result = eval_with_prelude("(insert-at 1 'x '(a b c))");
    assert_eq!(result, "(a x b c)");

    // Insert at beginning
    let result = eval_with_prelude("(insert-at 0 'x '(a b c))");
    assert_eq!(result, "(x a b c)");

    // Insert at end
    let result = eval_with_prelude("(insert-at 3 'x '(a b c))");
    assert_eq!(result, "(a b c x)");
}

#[test]
fn test_remove_at() {
    let result = eval_with_prelude("(remove-at 1 '(a b c))");
    assert_eq!(result, "(a c)");
}
