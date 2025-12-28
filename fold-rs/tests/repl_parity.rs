use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

fn should_run_parity() -> bool {
    matches!(env::var("FOLD_PARITY").as_deref(), Ok("1"))
}

fn find_scheme() -> Option<String> {
    if let Ok(cmd) = env::var("FOLD_SCHEME_CMD") {
        if command_exists(&cmd) {
            return Some(cmd);
        }
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
