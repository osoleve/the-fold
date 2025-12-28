use std::fmt;

use crate::fabric::{CaseArm, Expr, Value};
use crate::tools::fold_parse::{NumberLit, Span, Spanned, Sexp};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LowerError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for LowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.message, self.span)
    }
}

impl std::error::Error for LowerError {}

pub fn lower_expr(expr: &Spanned<Sexp>) -> Result<Expr, LowerError> {
    match &expr.value {
        Sexp::Number(NumberLit::Integer(n)) => Ok(Expr::Value(Value::Number(*n))),
        Sexp::Number(NumberLit::Float(n)) => Ok(Expr::Value(Value::Float(*n))),
        Sexp::String(s) => Ok(Expr::Value(Value::String(s.clone()))),
        Sexp::Bool(b) => Ok(Expr::Value(Value::Bool(*b))),
        Sexp::Symbol(sym) => Ok(Expr::Var(sym.clone())),
        Sexp::List(items) => lower_list(expr, items),
    }
}

pub fn lower_program(exprs: &[Spanned<Sexp>]) -> Result<Vec<Expr>, LowerError> {
    exprs.iter().map(lower_expr).collect()
}

fn lower_list(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    if items.is_empty() {
        return Ok(Expr::Value(Value::Nil));
    }
    if let Some(head_symbol) = symbol_name(&items[0]) {
        match head_symbol.as_str() {
            "quote" => return lower_quote(list_expr, items),
            "fn" => return lower_fn(list_expr, items),
            "let" => return lower_let(list_expr, items),
            "fix" => return lower_fix(list_expr, items),
            "if" => return lower_if(list_expr, items),
            "case" => return lower_case(list_expr, items),
            "prim" => return lower_prim(list_expr, items),
            "call" => return lower_call(list_expr, items),
            _ => {}
        }
    }

    let func = lower_expr(&items[0])?;
    let mut args = Vec::with_capacity(items.len().saturating_sub(1));
    for arg in &items[1..] {
        args.push(lower_expr(arg)?);
    }
    Ok(Expr::Call {
        func: Box::new(func),
        args,
    })
}

fn lower_quote(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    if items.len() != 2 {
        return Err(error(list_expr, "quote expects 1 argument"));
    }
    let datum = value_from_spanned(&items[1])?;
    Ok(Expr::Quote(datum))
}

fn lower_fn(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    if items.len() != 3 {
        return Err(error(list_expr, "fn expects (fn (params...) body)"));
    }
    let params = match &items[1].value {
        Sexp::List(list) => list
            .iter()
            .map(|param| match &param.value {
                Sexp::Symbol(name) => Ok(name.clone()),
                _ => Err(error(param, "fn params must be symbols")),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => return Err(error(&items[1], "fn expects a parameter list")),
    };
    let body = lower_expr(&items[2])?;
    Ok(Expr::Fn {
        params,
        body: Box::new(body),
    })
}

fn lower_let(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    if items.len() != 3 {
        return Err(error(list_expr, "let expects (let ((name expr) ...) body)"));
    }
    let bindings_list = match &items[1].value {
        Sexp::List(list) => list,
        _ => return Err(error(&items[1], "let expects a bindings list")),
    };
    let mut bindings = Vec::with_capacity(bindings_list.len());
    for binding in bindings_list {
        match &binding.value {
            Sexp::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].value {
                    Sexp::Symbol(sym) => sym.clone(),
                    _ => return Err(error(&pair[0], "let binding name must be a symbol")),
                };
                let expr = lower_expr(&pair[1])?;
                bindings.push((name, expr));
            }
            _ => return Err(error(binding, "let binding must be (name expr)")),
        }
    }
    let body = lower_expr(&items[2])?;
    Ok(Expr::Let {
        bindings,
        body: Box::new(body),
    })
}

fn lower_fix(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    if items.len() != 3 {
        return Err(error(list_expr, "fix expects (fix name expr)"));
    }
    let name = match &items[1].value {
        Sexp::Symbol(sym) => sym.clone(),
        _ => return Err(error(&items[1], "fix name must be a symbol")),
    };
    let value = lower_expr(&items[2])?;
    Ok(Expr::Fix {
        name,
        value: Box::new(value),
    })
}

fn lower_if(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    if items.len() != 4 {
        return Err(error(list_expr, "if expects (if test then else)"));
    }
    Ok(Expr::If {
        test: Box::new(lower_expr(&items[1])?),
        then_branch: Box::new(lower_expr(&items[2])?),
        else_branch: Box::new(lower_expr(&items[3])?),
    })
}

fn lower_case(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    if items.len() < 2 {
        return Err(error(list_expr, "case expects (case expr (pattern body) ...)"));
    }
    let scrutinee = lower_expr(&items[1])?;
    let mut arms = Vec::with_capacity(items.len().saturating_sub(2));
    for clause in &items[2..] {
        let clause_items = match &clause.value {
            Sexp::List(list) if list.len() == 2 => list,
            _ => return Err(error(clause, "case clause must be (pattern body)")),
        };
        let pattern_items = match &clause_items[0].value {
            Sexp::List(list) if !list.is_empty() => list,
            _ => return Err(error(&clause_items[0], "case pattern must be (tag vars...)")),
        };
        let tag = match &pattern_items[0].value {
            Sexp::Symbol(sym) => sym.clone(),
            _ => return Err(error(&pattern_items[0], "case tag must be a symbol")),
        };
        let mut vars = Vec::with_capacity(pattern_items.len().saturating_sub(1));
        for var in &pattern_items[1..] {
            match &var.value {
                Sexp::Symbol(sym) => vars.push(sym.clone()),
                _ => return Err(error(var, "case pattern vars must be symbols")),
            }
        }
        let body = lower_expr(&clause_items[1])?;
        arms.push(CaseArm::new(tag, vars, body));
    }
    Ok(Expr::Case {
        expr: Box::new(scrutinee),
        arms,
    })
}

fn lower_prim(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    if items.len() < 2 {
        return Err(error(list_expr, "prim expects (prim op args...)"));
    }
    let op = parse_prim_op(&items[1])?;
    let mut args = Vec::with_capacity(items.len().saturating_sub(2));
    for arg in &items[2..] {
        args.push(lower_expr(arg)?);
    }
    Ok(Expr::Prim { op, args })
}

fn lower_call(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    if items.len() < 2 {
        return Err(error(list_expr, "call expects (call func args...)"));
    }
    let func = lower_expr(&items[1])?;
    let mut args = Vec::with_capacity(items.len().saturating_sub(2));
    for arg in &items[2..] {
        args.push(lower_expr(arg)?);
    }
    Ok(Expr::Call {
        func: Box::new(func),
        args,
    })
}

fn parse_prim_op(op: &Spanned<Sexp>) -> Result<String, LowerError> {
    match &op.value {
        Sexp::Symbol(sym) => Ok(sym.clone()),
        Sexp::List(items) if items.len() == 2 => {
            if let Some(head) = symbol_name(&items[0]) {
                if head == "quote" {
                    if let Sexp::Symbol(sym) = &items[1].value {
                        return Ok(sym.clone());
                    }
                    return Err(error(&items[1], "prim op must be a symbol"));
                }
            }
            Err(error(op, "prim op must be a symbol or quoted symbol"))
        }
        _ => Err(error(op, "prim op must be a symbol or quoted symbol")),
    }
}

fn value_from_spanned(expr: &Spanned<Sexp>) -> Result<Value, LowerError> {
    match &expr.value {
        Sexp::Number(NumberLit::Integer(n)) => Ok(Value::Number(*n)),
        Sexp::Number(NumberLit::Float(n)) => Ok(Value::Float(*n)),
        Sexp::String(s) => Ok(Value::String(s.clone())),
        Sexp::Symbol(sym) => Ok(Value::Symbol(sym.clone())),
        Sexp::Bool(b) => Ok(Value::Bool(*b)),
        Sexp::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(value_from_spanned(item)?);
            }
            Ok(list_from_values(&values))
        }
    }
}

fn list_from_values(values: &[Value]) -> Value {
    let mut list = Value::Nil;
    for value in values.iter().rev() {
        list = Value::Pair(Box::new(value.clone()), Box::new(list));
    }
    list
}

fn symbol_name(expr: &Spanned<Sexp>) -> Option<String> {
    match &expr.value {
        Sexp::Symbol(sym) => Some(sym.clone()),
        _ => None,
    }
}

fn error(expr: &Spanned<Sexp>, message: &str) -> LowerError {
    error_at(expr, message)
}

fn error_at(expr: &Spanned<Sexp>, message: &str) -> LowerError {
    LowerError {
        message: message.to_string(),
        span: expr.span.clone(),
    }
}
