use std::process::Command;

#[test]
fn fold_repl_runs_expression() {
    let bin = env!("CARGO_BIN_EXE_fold-repl");
    let output = Command::new(bin)
        .arg("--expr")
        .arg("(prim 'add 1 2)")
        .output()
        .expect("run fold-repl");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout.trim(), "=> 3");
}
