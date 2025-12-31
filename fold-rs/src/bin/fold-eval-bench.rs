use std::env;
use std::hint::black_box;
use std::time::Instant;

use fold_rs::fabric::{EvalOutcome, eval_spanned};
use fold_rs::tools::{lower_program, parse_fold_program, primitives_env};

fn main() {
    let iterations = env::var("FOLD_BENCH_ITERS")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(10_000);

    let fuel: usize = env::var("FOLD_BENCH_FUEL")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1_000_000);

    println!();
    println!("=== Rust Evaluation Benchmarks ===");
    println!("Iterations per test: {}", iterations);
    println!("Fuel per evaluation: {}", fuel);
    println!();

    // =========================================================================
    // Parsing benchmarks
    // =========================================================================
    println!("--- Parsing ---");

    let simple_expr = "(+ 1 2)";
    let nested_expr = "(+ (* 2 3) (- 10 (/ 20 4)))";
    let deep_list = "(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)";
    let function_def = "(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))";
    let large_program = r#"
        (define (fib n)
          (if (< n 2)
              n
              (+ (fib (- n 1)) (fib (- n 2)))))
        (define (sum-list lst)
          (if (null? lst)
              0
              (+ (car lst) (sum-list (cdr lst)))))
        (define (map f lst)
          (if (null? lst)
              '()
              (cons (f (car lst)) (map f (cdr lst)))))
    "#;

    bench_parse("simple expr", simple_expr, iterations);
    bench_parse("nested expr", nested_expr, iterations);
    bench_parse("deep list (20)", deep_list, iterations);
    bench_parse("function def", function_def, iterations);
    bench_parse("large program", large_program, iterations);

    // =========================================================================
    // Lowering benchmarks
    // =========================================================================
    println!();
    println!("--- Lowering (Parse + Lower) ---");

    bench_lower("simple expr", simple_expr, iterations);
    bench_lower("nested expr", nested_expr, iterations);
    bench_lower("function def", function_def, iterations);
    bench_lower("large program", large_program, iterations);

    // =========================================================================
    // Evaluation benchmarks
    // =========================================================================
    println!();
    println!("--- Evaluation (Full Pipeline) ---");

    // Simple arithmetic
    bench_eval("arithmetic: (+ 1 2)", "(+ 1 2)", iterations, fuel);
    bench_eval(
        "nested arith: (+ (* 2 3) (- 10 4))",
        "(+ (* 2 3) (- 10 4))",
        iterations,
        fuel,
    );

    // Variable lookup
    bench_eval(
        "let binding (1 var)",
        "(let ((x 42)) x)",
        iterations,
        fuel,
    );
    bench_eval(
        "let binding (3 vars)",
        "(let ((x 1) (y 2) (z 3)) (+ x (+ y z)))",
        iterations,
        fuel,
    );
    bench_eval(
        "let binding (5 vars)",
        "(let ((a 1) (b 2) (c 3) (d 4) (e 5)) (+ a (+ b (+ c (+ d e)))))",
        iterations,
        fuel,
    );
    bench_eval(
        "nested let (3 deep)",
        "(let ((x 1)) (let ((y 2)) (let ((z 3)) (+ x (+ y z)))))",
        iterations,
        fuel,
    );

    // Function calls
    bench_eval(
        "lambda call (0 args)",
        "((lambda () 42))",
        iterations,
        fuel,
    );
    bench_eval(
        "lambda call (1 arg)",
        "((lambda (x) (+ x 1)) 10)",
        iterations,
        fuel,
    );
    bench_eval(
        "lambda call (3 args)",
        "((lambda (x y z) (+ x (+ y z))) 1 2 3)",
        iterations,
        fuel,
    );
    bench_eval(
        "nested calls (3 deep)",
        "((lambda (x) ((lambda (y) ((lambda (z) (+ x (+ y z))) 3)) 2)) 1)",
        iterations,
        fuel,
    );

    // Conditionals
    bench_eval(
        "if true branch",
        "(if #t 1 2)",
        iterations,
        fuel,
    );
    bench_eval(
        "if false branch",
        "(if #f 1 2)",
        iterations,
        fuel,
    );
    bench_eval(
        "nested if (3 deep)",
        "(if #t (if #t (if #t 1 2) 3) 4)",
        iterations,
        fuel,
    );

    // Closure capture
    bench_eval(
        "closure capture (1 var)",
        "(let ((x 10)) ((lambda (y) (+ x y)) 5))",
        iterations,
        fuel,
    );
    bench_eval(
        "closure capture (3 vars)",
        "(let ((a 1) (b 2) (c 3)) ((lambda (x) (+ a (+ b (+ c x)))) 4))",
        iterations,
        fuel,
    );

    // Recursion (fix)
    println!();
    println!("--- Recursion ---");

    // Simple recursion - countdown using fix
    bench_eval(
        "countdown(10)",
        "((fix countdown (lambda (n) (if (<= n 0) 0 (countdown (- n 1))))) 10)",
        iterations,
        fuel,
    );
    bench_eval(
        "countdown(50)",
        "((fix countdown (lambda (n) (if (<= n 0) 0 (countdown (- n 1))))) 50)",
        iterations,
        fuel,
    );
    bench_eval(
        "countdown(100)",
        "((fix countdown (lambda (n) (if (<= n 0) 0 (countdown (- n 1))))) 100)",
        iterations,
        fuel,
    );

    // Factorial using fix
    bench_eval(
        "factorial(5)",
        "((fix fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) 5)",
        iterations,
        fuel,
    );
    bench_eval(
        "factorial(10)",
        "((fix fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) 10)",
        iterations,
        fuel,
    );

    // Fibonacci (exponential - tests call overhead)
    bench_eval(
        "fib(10)",
        "((fix fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) 10)",
        iterations / 10, // Fewer iterations due to exponential complexity
        fuel,
    );
    bench_eval(
        "fib(15)",
        "((fix fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) 15)",
        iterations / 100,
        fuel,
    );

    // List operations
    println!();
    println!("--- List Operations ---");

    bench_eval(
        "cons x10",
        "(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 '()))))))))))",
        iterations,
        fuel,
    );
    bench_eval(
        "car/cdr chain",
        "(let ((lst (cons 1 (cons 2 (cons 3 '()))))) (+ (car lst) (car (cdr lst))))",
        iterations,
        fuel,
    );

    // Length via recursion using fix
    bench_eval(
        "length(10)",
        r#"((fix len (lambda (lst) (if (null? lst) 0 (+ 1 (len (cdr lst))))))
            (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 '())))))))))))"#,
        iterations,
        fuel,
    );

    // Sum list via recursion using fix
    bench_eval(
        "sum-list(10)",
        r#"((fix sum (lambda (lst) (if (null? lst) 0 (+ (car lst) (sum (cdr lst))))))
            (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 '())))))))))))"#,
        iterations,
        fuel,
    );

    // Environment stress test
    println!();
    println!("--- Environment Stress ---");

    bench_eval(
        "deep scope (5 nested lets)",
        "(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (let ((e 5)) (+ a (+ b (+ c (+ d e)))))))))",
        iterations,
        fuel,
    );

    bench_eval(
        "many lookups (same var)",
        "(let ((x 1)) (+ x (+ x (+ x (+ x (+ x (+ x (+ x (+ x (+ x x))))))))))",
        iterations,
        fuel,
    );

    bench_eval(
        "outer var access (deep)",
        "(let ((outer 100)) (let ((a 1)) (let ((b 2)) (let ((c 3)) (+ outer (+ a (+ b c)))))))",
        iterations,
        fuel,
    );

    // Symbol-heavy operations
    println!();
    println!("--- Symbol Operations ---");

    bench_eval(
        "symbol creation",
        "'foo",
        iterations,
        fuel,
    );
    bench_eval(
        "symbol comparison",
        "(eq? 'foo 'foo)",
        iterations,
        fuel,
    );
    bench_eval(
        "symbol->string",
        "(symbol->string 'hello-world)",
        iterations,
        fuel,
    );

    println!();
    println!("=== Benchmarks Complete ===");
}

fn bench_parse(name: &str, input: &str, iterations: usize) {
    bench(name, iterations, || {
        let result = parse_fold_program(input, None).expect("parse failed");
        black_box(result);
    });
}

fn bench_lower(name: &str, input: &str, iterations: usize) {
    bench(name, iterations, || {
        let parsed = parse_fold_program(input, None).expect("parse failed");
        let lowered = lower_program(&parsed).expect("lower failed");
        black_box(lowered);
    });
}

fn bench_eval(name: &str, input: &str, iterations: usize, fuel: usize) {
    // Pre-parse and lower once
    let parsed = parse_fold_program(input, None).expect("parse failed");
    let lowered = lower_program(&parsed).expect("lower failed");

    if lowered.is_empty() {
        println!("  {}: SKIPPED (no expressions)", name);
        return;
    }

    // Evaluate the last expression (or single expression)
    let expr = lowered.last().unwrap().clone();

    bench(name, iterations, || {
        let env = primitives_env();
        match eval_spanned(expr.clone(), env, fuel) {
            Ok(EvalOutcome::Done(value)) => {
                black_box(value);
            }
            Ok(EvalOutcome::Suspended { .. }) => {
                panic!("evaluation suspended (out of fuel)");
            }
            Err(e) => {
                panic!("evaluation error: {:?}", e);
            }
        }
    });
}

fn bench<F>(name: &str, iterations: usize, mut f: F)
where
    F: FnMut(),
{
    // Warm-up
    for _ in 0..iterations.min(100) {
        f();
    }

    let start = Instant::now();
    for _ in 0..iterations {
        f();
    }
    let elapsed = start.elapsed();

    let ns_total = elapsed.as_nanos() as f64;
    let us_per_op = ns_total / (iterations as f64 * 1000.0);
    let ms = ns_total / 1_000_000.0;

    if us_per_op >= 1000.0 {
        println!("  {}: {:.2}ms total, {:.2}ms/op", name, ms, us_per_op / 1000.0);
    } else if us_per_op >= 1.0 {
        println!("  {}: {:.2}ms total, {:.1}µs/op", name, ms, us_per_op);
    } else {
        let ns_per_op = ns_total / iterations as f64;
        println!("  {}: {:.2}ms total, {:.1}ns/op", name, ms, ns_per_op);
    }
}
