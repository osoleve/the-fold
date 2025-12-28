use crate::core::{symbol::Symbol, value::Value};

#[derive(Debug, Clone)]
pub struct CaseArm {
    pub tag: Symbol,
    pub vars: Vec<Symbol>,
    pub body: Box<Expr>,
}

impl CaseArm {
    pub fn new(tag: Symbol, vars: Vec<Symbol>, body: Expr) -> Self {
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
        body: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    Let {
        bindings: Vec<(Symbol, Expr)>,
        body: Box<Expr>,
    },
    If {
        test: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Fix {
        name: Symbol,
        value: Box<Expr>,
    },
    Case {
        expr: Box<Expr>,
        arms: Vec<CaseArm>,
    },
    Prim {
        op: Symbol,
        args: Vec<Expr>,
    },
    Var(Symbol),
}
