use crate::fabric::{env::EnvRef, expr::Expr, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct Closure {
    pub params: Vec<Symbol>,
    pub body: Box<Expr>,
    pub env: EnvRef,
}

impl Closure {
    pub fn new(params: Vec<Symbol>, body: Expr, env: EnvRef) -> Self {
        Self {
            params,
            body: Box::new(body),
            env,
        }
    }
}
