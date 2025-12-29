use crate::fabric::{address::Address, block::Block, closure::Closure, symbol::Symbol};
use num_bigint::BigInt;

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    Float(f64),
    BigInt(BigInt),
    String(String),
    Symbol(Symbol),
    Bool(bool),
    Char(char),
    Bytevector(Vec<u8>),
    Address(Address),
    Pair(Box<Value>, Box<Value>),
    Vector(Vec<Value>),
    Closure(Closure),
    Block(Block),
    Nil,
}

impl Value {
    /// Check if this value is a BigInt
    pub fn is_bigint(&self) -> bool {
        matches!(self, Value::BigInt(_))
    }

    /// Try to extract a BigInt reference
    pub fn as_bigint(&self) -> Option<&BigInt> {
        match self {
            Value::BigInt(n) => Some(n),
            _ => None,
        }
    }

    /// Convert a Number to BigInt if needed for mixed arithmetic
    pub fn to_bigint(&self) -> Option<BigInt> {
        match self {
            Value::BigInt(n) => Some(n.clone()),
            Value::Number(n) => Some(BigInt::from(*n)),
            _ => None,
        }
    }
}
