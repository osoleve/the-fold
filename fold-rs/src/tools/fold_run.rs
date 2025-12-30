use std::fmt;
use std::path::Path;

use crate::fabric::{EvalOutcome, Expr, SpannedEvalError, SpannedExpr, Value, eval_spanned};
use crate::tools::fold_load::{LoadError, load_fold_program};
use crate::tools::prelude::prelude_env;

#[derive(Debug)]
pub enum RunError {
    Load(LoadError),
    Eval(SpannedEvalError),
    Prelude(String),
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RunError::Load(err) => write!(f, "{err}"),
            RunError::Eval(err) => write!(f, "{err}"),
            RunError::Prelude(err) => write!(f, "prelude error: {err}"),
        }
    }
}

impl std::error::Error for RunError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            RunError::Load(err) => Some(err),
            RunError::Eval(err) => Some(err),
            RunError::Prelude(_) => None,
        }
    }
}

pub fn run_fold_file<P: AsRef<Path>>(path: P, fuel: usize) -> Result<EvalOutcome, RunError> {
    let exprs = load_fold_program(path).map_err(RunError::Load)?;
    let expr = sequence_exprs(exprs);
    let env = prelude_env(fuel).map_err(RunError::Prelude)?;
    eval_spanned(expr, env, fuel).map_err(RunError::Eval)
}

fn sequence_exprs(exprs: Vec<SpannedExpr>) -> SpannedExpr {
    let mut iter = exprs.into_iter();
    let mut current = match iter.next() {
        Some(expr) => expr,
        None => return SpannedExpr::unspanned(Expr::Value(Value::Nil)),
    };
    for (index, next) in iter.enumerate() {
        let name = format!("#%fold-seq-{index}");
        current = SpannedExpr::new(
            Expr::Let {
                bindings: vec![(name, current)],
                body: Box::new(next),
            },
            None,
        );
    }
    current
}
