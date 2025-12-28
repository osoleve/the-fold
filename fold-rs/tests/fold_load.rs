use std::fs;
use std::path::PathBuf;

use fold_rs::core::Expr;
use fold_rs::tools::{load_fold_program, LoadError};

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
fn load_program_from_file() {
    let temp = temp_path("fold-load-ok.fold");
    fs::write(&temp.path, "(let ((x 1)) (prim 'add x 2))").expect("write test file");

    let exprs = load_fold_program(&temp.path).expect("load program");
    assert_eq!(exprs.len(), 1);

    match &exprs[0] {
        Expr::Let { bindings, .. } => {
            assert_eq!(bindings.len(), 1);
            assert_eq!(bindings[0].0, "x");
        }
        other => panic!("expected let, got {:?}", other),
    }
}

#[test]
fn load_reports_parse_errors() {
    let temp = temp_path("fold-load-bad.fold");
    fs::write(&temp.path, "(let ((x 1))").expect("write test file");

    let err = load_fold_program(&temp.path).expect_err("expected parse error");
    match err {
        LoadError::Parse { .. } => {}
        other => panic!("expected parse error, got {:?}", other),
    }
}
