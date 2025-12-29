use crate::fabric::{env::EnvRef, expr::SpannedExpr, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct Closure {
    pub params: Vec<Symbol>,
    pub body: Box<SpannedExpr>,
    pub env: EnvRef,
}

impl Closure {
    pub fn new(params: Vec<Symbol>, body: SpannedExpr, env: EnvRef) -> Self {
        Self {
            params,
            body: Box::new(body),
            env,
        }
    }
}
