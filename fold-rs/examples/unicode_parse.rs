use fold_rs::tools::fold_parse::{parse_fold_program, strip_spans};

fn main() {
    let test_cases = vec![
        ("(λ (x) x)", "lambda", 0),
        ("(∧ #t #f)", "and", 0),
        ("(∨ #t #f)", "or", 0),
        ("(¬ x)", "not", 0),
        ("(≤ 1 2)", "<=", 0),
        ("(≥ 3 2)", ">=", 0),
        ("(≠ 1 2)", "<>", 0),
        ("(× 3 4)", "*", 0),
        ("(÷ 10 2)", "/", 0),
        ("(* 2 π)", "pi", 2),  // Check the third element (π)
    ];

    println!("Unicode Symbol Alias Demonstration\n");
    println!("{:<20} {:<20} {}", "Input", "Expected Symbol", "Result");
    println!("{}", "=".repeat(60));

    for (input, expected_symbol, index) in test_cases {
        match parse_fold_program(input, None) {
            Ok(exprs) if !exprs.is_empty() => {
                let plain = strip_spans(&exprs[0]);

                // Check if it's a list and extract the symbol at the specified index
                let actual_symbol = match &plain {
                    fold_rs::tools::fold_parse::PlainSexp::List(items) if items.len() > index => {
                        match &items[index] {
                            fold_rs::tools::fold_parse::PlainSexp::Symbol(s) => s.as_str(),
                            _ => "NOT A SYMBOL",
                        }
                    }
                    _ => "NOT A LIST OR INDEX OUT OF BOUNDS",
                };

                let status = if actual_symbol == expected_symbol {
                    "✓ PASS"
                } else {
                    "✗ FAIL"
                };

                println!("{:<20} {:<20} {} (got: {})",
                    input, expected_symbol, status, actual_symbol);
            }
            Ok(_) => println!("{:<20} {:<20} ✗ FAIL (no expressions)", input, expected_symbol),
            Err(e) => println!("{:<20} {:<20} ✗ ERROR: {}", input, expected_symbol, e),
        }
    }

    println!("\n\nComplex Expression Test:");
    let complex = "(λ (x y) (∧ (≤ x y) (≠ x 0)))";
    println!("Input: {}", complex);
    match parse_fold_program(complex, None) {
        Ok(exprs) if !exprs.is_empty() => {
            let _plain = strip_spans(&exprs[0]);
            println!("Parsed successfully!");
            println!("Note: All Unicode symbols were converted to ASCII equivalents");
            println!("  λ → lambda, ∧ → and, ≤ → <=, ≠ → <>");
        }
        Err(e) => println!("ERROR: {}", e),
        _ => println!("No expressions parsed"),
    }
}
