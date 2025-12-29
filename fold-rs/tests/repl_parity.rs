use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

fn should_run_parity() -> bool {
    matches!(env::var("FOLD_PARITY").as_deref(), Ok("1"))
}

fn find_scheme() -> Option<String> {
    if let Ok(cmd) = env::var("FOLD_SCHEME_CMD")
        && command_exists(&cmd)
    {
        return Some(cmd);
    }
    for cmd in ["scheme", "chez-scheme", "chezscheme", "petite"] {
        if command_exists(cmd) {
            return Some(cmd.to_string());
        }
    }
    None
}

fn command_exists(cmd: &str) -> bool {
    Command::new(cmd)
        .arg("--help")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok()
}

fn legacy_eval(expr: &str) -> Result<String, String> {
    let scheme = find_scheme().ok_or_else(|| "scheme not found".to_string())?;
    let mut script = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    script.push("tests");
    script.push("fixtures");
    script.push("legacy-eval.ss");
    let mut fold_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    fold_root.pop();

    let mut child = Command::new(scheme)
        .arg("--script")
        .arg(script)
        .env("FOLD_FUEL", "10000")
        .env("FOLD_ROOT", &fold_root)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|err| format!("failed to spawn scheme: {err}"))?;

    {
        let stdin = child.stdin.as_mut().ok_or("missing stdin")?;
        stdin
            .write_all(expr.as_bytes())
            .map_err(|err| format!("failed to write stdin: {err}"))?;
        stdin
            .write_all(b"\n")
            .map_err(|err| format!("failed to write newline: {err}"))?;
    }

    let output = child
        .wait_with_output()
        .map_err(|err| format!("failed to read scheme output: {err}"))?;
    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "legacy eval failed: status={}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            stdout.trim(),
            stderr.trim()
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn rust_eval(expr: &str) -> Result<String, String> {
    let bin = env!("CARGO_BIN_EXE_fold-repl");
    let output = Command::new(bin)
        .arg("--expr")
        .arg(expr)
        .arg("--fuel")
        .arg("10000")
        .output()
        .map_err(|err| format!("failed to run fold-repl: {err}"))?;
    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "rust eval failed: status={}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            stdout.trim(),
            stderr.trim()
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

#[test]
fn repl_parity_core_expressions() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    let cases = [
        "42",
        "\"hello\"",
        "#t",
        "#f",
        "(prim 'add 1 2)",
        "(prim 'add 1 2.5)",
        "(let ((x 2) (y 3)) (prim 'mul x y))",
        "(if #f 1 2)",
        "(prim 'list 1 2 3)",
        "'(1 2 3)",
        "(case (prim 'make-block 'True (prim 'bv-make 0) (prim 'vec-empty)) ((True) 1) ((False) 0))",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).expect("legacy eval failed");
        let rust = rust_eval(expr).expect("rust eval failed");
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_arithmetic() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    let cases = [
        // Basic arithmetic
        "(prim 'add 10 20)",
        "(prim 'sub 50 30)",
        "(prim 'mul 6 7)",
        "(prim 'div 100 4)",
        "(prim 'mod 17 5)",
        "(prim 'neg 42)",
        "(prim 'abs -99)",
        // Comparisons
        "(prim 'lt? 1 2)",
        "(prim 'lt? 2 1)",
        "(prim 'le? 5 5)",
        "(prim 'gt? 10 5)",
        "(prim 'ge? 3 3)",
        "(prim 'zero? 0)",
        "(prim 'zero? 1)",
        "(prim 'positive? 5)",
        "(prim 'negative? -5)",
        // Bitwise
        "(prim 'bitand 15 7)",
        "(prim 'bitor 8 4)",
        "(prim 'bitxor 15 9)",
        "(prim 'bitnot 0)",
        "(prim 'shl 1 4)",
        "(prim 'shr 16 2)",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_list_ops() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    let cases = [
        "(prim 'cons 1 2)",
        "(prim 'cons 'a '(b c))",
        "(prim 'car '(1 2 3))",
        "(prim 'cdr '(1 2 3))",
        "(prim 'null? '())",
        "(prim 'null? '(1))",
        // Use cons instead of dotted pair literal syntax
        "(prim 'pair? (prim 'cons 1 2))",
        "(prim 'pair? 42)",
        "(prim 'list 'a 'b 'c)",
        "(prim 'length '(1 2 3 4 5))",
        "(prim 'reverse '(1 2 3))",
        "(prim 'append '(1 2) '(3 4))",
        "(prim 'list-ref '(a b c d) 2)",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_string_ops() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    let cases = [
        "(prim 'string-length \"hello\")",
        "(prim 'string-ref \"hello\" 1)",
        "(prim 'string-append \"foo\" \"bar\")",
        "(prim 'substring \"hello world\" 0 5)",
        "(prim 'string=? \"abc\" \"abc\")",
        "(prim 'string=? \"abc\" \"def\")",
        "(prim 'string<? \"abc\" \"abd\")",
        "(prim 'string>? \"xyz\" \"abc\")",
        // Use integer->char instead of character literals
        "(prim 'list->string (prim 'list (prim 'integer->char 104) (prim 'integer->char 105)))",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_char_ops() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    // Use integer->char instead of character literals since Rust parser doesn't support #\x syntax
    let cases = [
        "(prim 'char->integer (prim 'integer->char 65))",
        "(prim 'integer->char 65)",
        "(prim 'char=? (prim 'integer->char 97) (prim 'integer->char 97))",
        "(prim 'char<? (prim 'integer->char 97) (prim 'integer->char 98))",
        "(prim 'char-alphabetic? (prim 'integer->char 90))",
        "(prim 'char-alphabetic? (prim 'integer->char 53))",
        "(prim 'char-numeric? (prim 'integer->char 55))",
        "(prim 'char-whitespace? (prim 'integer->char 32))",
        "(prim 'char-upcase (prim 'integer->char 97))",
        "(prim 'char-downcase (prim 'integer->char 65))",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_vector_ops() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    let cases = [
        "(prim 'vec-make 1 2 3)",
        "(prim 'vec-empty)",
        "(prim 'vec-ref (prim 'vec-make 'a 'b 'c) 1)",
        "(prim 'vec-length (prim 'vec-make 1 2 3 4))",
        "(prim 'vec->list (prim 'vec-make 'x 'y))",
        "(prim 'list->vec '(1 2 3))",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_bytevector_ops() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    // Test operations that return non-bytevector values (avoiding output format differences)
    let cases = [
        "(prim 'bv-length (prim 'bv-make 10))",
        "(prim 'bv-ref (prim 'bv-make 5 42) 2)",
        // Converting back to string normalizes the output
        "(prim 'utf8->string (prim 'bv-slice (prim 'string->utf8 \"hello\") 1 4))",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_type_predicates() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    let cases = [
        "(prim 'number? 42)",
        "(prim 'number? \"not a number\")",
        "(prim 'integer? 42)",
        "(prim 'integer? 3.14)",
        "(prim 'symbol? 'foo)",
        "(prim 'string? \"hello\")",
        // Use integer->char instead of character literal
        "(prim 'char? (prim 'integer->char 120))",
        "(prim 'boolean? #t)",
        "(prim 'list? '(1 2 3))",
        // Use cons instead of dotted pair literal
        "(prim 'list? (prim 'cons 1 2))",
        "(prim 'vector? (prim 'vec-make 1 2))",
        "(prim 'bytevector? (prim 'bv-make 5))",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_block_ops() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    let cases = [
        // Block creation and accessors
        "(prim 'block-tag (prim 'make-block 'MyTag (prim 'bv-make 0) (prim 'vec-empty)))",
        // Convert payload to string to avoid bytevector format differences
        "(prim 'utf8->string (prim 'block-payload (prim 'make-block 'Tag (prim 'string->utf8 \"data\") (prim 'vec-empty))))",
        "(prim 'bv-length (prim 'block-payload (prim 'make-block 'Tag (prim 'string->utf8 \"hello\") (prim 'vec-empty))))",
        // Block predicates
        "(prim 'block? (prim 'make-block 'X (prim 'bv-make 0) (prim 'vec-empty)))",
        "(prim 'block? 42)",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_hash_ops() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    // Note: sha256 primitive needs to be loaded in legacy eval
    // For now, test block hashing which should work
    let cases = [
        // Block hash (content-addressing) - same content = same hash
        "(prim 'eq? (prim 'hash-block (prim 'make-block 'X (prim 'bv-make 0) (prim 'vec-empty))) (prim 'hash-block (prim 'make-block 'X (prim 'bv-make 0) (prim 'vec-empty))))",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}

#[test]
fn repl_parity_closures_and_recursion() {
    if !should_run_parity() {
        return;
    }

    let Some(_) = find_scheme() else {
        return;
    };

    let cases = [
        // Simple closure
        "((fn (x) (prim 'add x 1)) 5)",
        // Nested closures
        "((fn (x) ((fn (y) (prim 'add x y)) 3)) 2)",
        // Fix for recursion (factorial) - Rust uses (fix name expr) syntax
        "((fix fact (fn (n) (if (prim 'le? n 1) 1 (prim 'mul n (fact (prim 'sub n 1)))))) 5)",
    ];

    for expr in cases {
        let legacy = legacy_eval(expr).unwrap_or_else(|_| panic!("legacy eval failed for: {}", expr));
        let rust = rust_eval(expr).unwrap_or_else(|_| panic!("rust eval failed for: {}", expr));
        assert_eq!(legacy, rust, "expr: {expr}");
    }
}
