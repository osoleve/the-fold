use std::fs;
use std::path::PathBuf;

use fold_rs::fabric::EvalOutcome;
use fold_rs::tools::{LoadError, RunError, run_fold_file};

struct TempPath {
    path: PathBuf,
}

impl Drop for TempPath {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

fn temp_path(name: &str) -> TempPath {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("target");
    path.push(name);
    TempPath { path }
}

#[test]
fn run_file_evaluates_last_expr() {
    let temp = temp_path("fold-run-ok.fold");
    fs::write(&temp.path, "1 (prim 'add 1 2)").expect("write test file");

    let outcome = run_fold_file(&temp.path, 10000).expect("run file");
    match outcome {
        EvalOutcome::Done(value) => match value {
            fold_rs::fabric::Value::Number(n) => assert_eq!(n, 3),
            other => panic!("expected number, got {:?}", other),
        },
        other => panic!("expected done, got {:?}", other),
    }
}

#[test]
fn run_file_out_of_fuel() {
    let temp = temp_path("fold-run-suspend.fold");
    // Use a recursive function that will exhaust fuel after prelude loads
    // The prelude is large, so give 100000 total fuel and recurse deeply to exhaust it
    fs::write(
        &temp.path,
        "((fix f (fn (n) (if (= n 0) 0 (f (- n 1))))) 1000000)",
    )
    .expect("write test file");

    let outcome = run_fold_file(&temp.path, 100000).expect("run file");
    match outcome {
        EvalOutcome::Suspended { .. } => {}
        other => panic!("expected suspended, got {:?}", other),
    }
}

#[test]
fn run_file_reports_parse_error() {
    let temp = temp_path("fold-run-bad.fold");
    fs::write(&temp.path, "(let ((x 1))").expect("write test file");

    let err = run_fold_file(&temp.path, 100).expect_err("expected error");
    match err {
        RunError::Load(LoadError::Parse { .. }) => {}
        other => panic!("expected parse error, got {:?}", other),
    }
}
