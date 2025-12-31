use std::env;
use std::io::{self, Read};
use std::process;

use fold_rs::fabric::{EvalOutcome, Expr, SpannedExpr, Value, eval_spanned};
use fold_rs::tools::{
    format_value, lower_expr, lower_program, parse_fold_expr, parse_fold_program, prelude_env,
    run_fold_file,
};

fn main() {
    let mut args = env::args().skip(1);
    let mut expr_arg: Option<String> = None;
    let mut file_arg: Option<String> = None;
    let mut fuel: usize = 1_000_000;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--expr" => {
                expr_arg = args.next();
            }
            "--file" => {
                file_arg = args.next();
            }
            "--fuel" => {
                if let Some(value) = args.next() {
                    fuel = value.parse().unwrap_or(fuel);
                }
            }
            "--help" | "-h" => {
                print_help();
                return;
            }
            other => {
                eprintln!("Unknown argument: {other}");
                print_help();
                process::exit(2);
            }
        }
    }

    if expr_arg.is_some() && file_arg.is_some() {
        eprintln!("--expr and --file are mutually exclusive");
        process::exit(2);
    }

    let outcome = if let Some(expr) = expr_arg {
        run_expr_source(&expr, fuel)
    } else if let Some(file) = file_arg {
        run_fold_file(&file, fuel).map_err(|err| err.to_string())
    } else {
        let mut input = String::new();
        if io::stdin().read_to_string(&mut input).is_err() {
            eprintln!("Failed to read stdin");
            process::exit(1);
        }
        run_program_source(&input, fuel)
    };

    match outcome {
        Ok(EvalOutcome::Done(value)) => {
            println!("=> {}", format_value(&value));
        }
        Ok(EvalOutcome::Suspended { .. }) => {
            println!("[suspended - out of fuel]");
        }
        Err(message) => {
            eprintln!("ERROR: {message}");
            process::exit(1);
        }
    }
}

fn run_expr_source(source: &str, fuel: usize) -> Result<EvalOutcome, String> {
    let parsed = parse_fold_expr(source, Some("<expr>")).map_err(|err| err.to_string())?;
    let expr = lower_expr(&parsed).map_err(|err| err.to_string())?;
    run_expr(expr, fuel)
}

fn run_program_source(source: &str, fuel: usize) -> Result<EvalOutcome, String> {
    let parsed = parse_fold_program(source, Some("<stdin>")).map_err(|err| err.to_string())?;
    let exprs = lower_program(&parsed).map_err(|err| err.to_string())?;
    run_expr(sequence_exprs(exprs), fuel)
}

fn run_expr(expr: SpannedExpr, fuel: usize) -> Result<EvalOutcome, String> {
    // Load prelude with primitives and HOFs
    let env = prelude_env(fuel)?;
    eval_spanned(expr, env, fuel).map_err(|err| err.to_string())
}

fn sequence_exprs(exprs: Vec<SpannedExpr>) -> SpannedExpr {
    if exprs.is_empty() {
        return SpannedExpr::unspanned(Expr::Value(Value::Nil));
    }

    // Collect defines and convert them to let* bindings
    let mut bindings: Vec<(String, SpannedExpr)> = Vec::new();
    let mut body_exprs: Vec<SpannedExpr> = Vec::new();

    for expr in exprs {
        match &expr.expr {
            Expr::Define { name, value } => {
                bindings.push((name.clone(), (**value).clone()));
            }
            _ => {
                body_exprs.push(expr);
            }
        }
    }

    // Build the body: sequence non-define expressions
    let body = if body_exprs.is_empty() {
        // If only defines, return the last define's value
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
    if bindings.is_empty() {
        body
    } else {
        SpannedExpr::new(
            Expr::Let {
                bindings,
                body: Box::new(body),
            },
            None,
        )
    }
}

fn print_help() {
    println!("fold-repl --expr \"(prim 'add 1 2)\"");
    println!("fold-repl --file path/to/source.fold");
    println!("fold-repl --fuel 10000");
    println!("fold-repl < source.fold");
}
