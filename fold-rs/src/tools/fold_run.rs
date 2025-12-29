use std::fmt;
use std::path::Path;

use crate::fabric::{Env, EvalError, EvalOutcome, Expr, Value, eval_loop};
use crate::tools::fold_load::{LoadError, load_fold_program};

#[derive(Debug)]
pub enum RunError {
    Load(LoadError),
    Eval(EvalError),
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RunError::Load(err) => write!(f, "{err}"),
            RunError::Eval(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for RunError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            RunError::Load(err) => Some(err),
            RunError::Eval(err) => Some(err),
        }
    }
}

pub fn run_fold_file<P: AsRef<Path>>(path: P, fuel: usize) -> Result<EvalOutcome, RunError> {
    let exprs = load_fold_program(path).map_err(RunError::Load)?;
    let expr = sequence_exprs(exprs);
    let env = Env::new();
    eval_loop(expr, env, fuel).map_err(RunError::Eval)
}

fn sequence_exprs(exprs: Vec<Expr>) -> Expr {
    let mut iter = exprs.into_iter();
    let mut current = match iter.next() {
        Some(expr) => expr,
        None => return Expr::Value(Value::Nil),
    };
    for (index, next) in iter.enumerate() {
        let name = format!("#%fold-seq-{index}");
        current = Expr::Let {
            bindings: vec![(name, current)],
            body: Box::new(next),
        };
    }
    current
}
