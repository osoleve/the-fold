use std::fmt;

use crate::fabric::symbol::Symbol;

#[derive(Debug, Clone)]
pub enum EvalError {
    OutOfFuel,
    UnboundVariable(Symbol),
    InvalidExpression,
    FixRequiresFn,
    CaseRequiresBlock,
    NoMatchingClause(Symbol),
    PatternArityMismatch { expected: usize, got: usize },
    NotCallable,
    ArityMismatch { expected: usize, got: usize },
    TypeMismatch(&'static str),
    DivisionByZero,
    IndexOutOfBounds,
    UnknownPrimitive(Symbol),
    Unimplemented(&'static str),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::OutOfFuel => write!(f, "out of fuel"),
            EvalError::UnboundVariable(name) => write!(f, "unbound variable: {name}"),
            EvalError::InvalidExpression => write!(f, "invalid expression"),
            EvalError::FixRequiresFn => write!(f, "fix requires fn"),
            EvalError::CaseRequiresBlock => write!(f, "case requires block"),
            EvalError::NoMatchingClause(tag) => write!(f, "no matching clause: {tag}"),
            EvalError::PatternArityMismatch { expected, got } => {
                write!(f, "pattern arity mismatch: expected {expected}, got {got}")
            }
            EvalError::NotCallable => write!(f, "value is not callable"),
            EvalError::ArityMismatch { expected, got } => {
                write!(f, "arity mismatch: expected {expected}, got {got}")
            }
            EvalError::TypeMismatch(msg) => write!(f, "type mismatch: {msg}"),
            EvalError::DivisionByZero => write!(f, "division by zero"),
            EvalError::IndexOutOfBounds => write!(f, "index out of bounds"),
            EvalError::UnknownPrimitive(op) => write!(f, "unknown primitive: {op}"),
            EvalError::Unimplemented(feature) => write!(f, "unimplemented: {feature}"),
        }
    }
}

impl std::error::Error for EvalError {}
