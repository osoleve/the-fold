use crate::fabric::{symbol::Symbol, value::Value};
use crate::tools::Span;

/// A source location span for error reporting.
/// Re-exported from tools for use in fabric.
pub use crate::tools::Span as SourceSpan;

/// An expression paired with an optional source location.
#[derive(Debug, Clone)]
pub struct SpannedExpr {
    pub expr: Expr,
    pub span: Option<Span>,
}

impl SpannedExpr {
    pub fn new(expr: Expr, span: Option<Span>) -> Self {
        Self { expr, span }
    }

    pub fn unspanned(expr: Expr) -> Self {
        Self { expr, span: None }
    }
}

impl From<Expr> for SpannedExpr {
    fn from(expr: Expr) -> Self {
        Self::unspanned(expr)
    }
}

#[derive(Debug, Clone)]
pub struct CaseArm {
    pub tag: Symbol,
    pub vars: Vec<Symbol>,
    pub body: Box<SpannedExpr>,
}

impl CaseArm {
    pub fn new(tag: Symbol, vars: Vec<Symbol>, body: SpannedExpr) -> Self {
        Self {
            tag,
            vars,
            body: Box::new(body),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Value(Value),
    Quote(Value),
    Fn {
        params: Vec<Symbol>,
        body: Box<SpannedExpr>,
    },
    Call {
        func: Box<SpannedExpr>,
        args: Vec<SpannedExpr>,
    },
    Let {
        bindings: Vec<(Symbol, SpannedExpr)>,
        body: Box<SpannedExpr>,
    },
    If {
        test: Box<SpannedExpr>,
        then_branch: Box<SpannedExpr>,
        else_branch: Box<SpannedExpr>,
    },
    Fix {
        name: Symbol,
        value: Box<SpannedExpr>,
    },
    Case {
        expr: Box<SpannedExpr>,
        arms: Vec<CaseArm>,
        else_body: Option<Box<SpannedExpr>>,
    },
    Prim {
        op: Symbol,
        args: Vec<SpannedExpr>,
    },
    Var(Symbol),
}
