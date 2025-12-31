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
    if exprs.is_empty() {
        return SpannedExpr::unspanned(Expr::Value(Value::Nil));
    }

    // Collect defines and load expressions, treating them as "transparent"
    // Load expressions are evaluated but don't consume the result - their defines
    // should be visible to subsequent expressions
    let mut bindings: Vec<(String, SpannedExpr)> = Vec::new();
    let mut body_exprs: Vec<SpannedExpr> = Vec::new();
    let mut loads: Vec<SpannedExpr> = Vec::new();

    for expr in exprs {
        match &expr.expr {
            Expr::Define { name, value } => {
                bindings.push((name.clone(), (**value).clone()));
            }
            Expr::Load { .. } => {
                // Load expressions need special handling - their defines should propagate
                loads.push(expr);
            }
            _ => {
                body_exprs.push(expr);
            }
        }
    }

    // Build the body: sequence non-define, non-load expressions
    let body = if body_exprs.is_empty() {
        // If only defines/loads, return nil or last define's value
        if bindings.is_empty() {
            SpannedExpr::unspanned(Expr::Value(Value::Nil))
        } else {
            let last_name = bindings.last().unwrap().0.clone();
            SpannedExpr::unspanned(Expr::Var(last_name))
        }
    } else if body_exprs.len() == 1 {
        body_exprs.remove(0)
    } else {
        // Sequence the body expressions with let bindings
        let mut iter = body_exprs.into_iter();
        let mut current = iter.next().unwrap();
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
    };

    // Wrap body in let* for all defines
    let with_defines = if bindings.is_empty() {
        body
    } else {
        SpannedExpr::new(
            Expr::Let {
                bindings,
                body: Box::new(body),
            },
            None,
        )
    };

    // Prepend loads - each load is wrapped in a let that discards its value
    // but keeps the environment changes (via Load's special handling)
    // Actually, we need a different approach - loads should be evaluated first
    // and their defines should be part of the let bindings
    // For now, we'll extract defines from loads by inlining them
    if loads.is_empty() {
        with_defines
    } else {
        // Wrap loads as a sequence before the main body
        // Each load's result is discarded but its defines propagate
        let mut result = with_defines;
        for (i, load) in loads.into_iter().rev().enumerate() {
            result = SpannedExpr::new(
                Expr::Let {
                    bindings: vec![(format!("#%load-{i}"), load)],
                    body: Box::new(result),
                },
                None,
            );
        }
        result
    }
}
