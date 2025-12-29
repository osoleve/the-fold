use crate::fabric::{address::Address, block::Block, closure::Closure, symbol::Symbol};
use num_bigint::BigInt;
use num_rational::BigRational;

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    Float(f64),
    BigInt(BigInt),
    BigRational(BigRational),
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

    /// Check if this value is a BigRational
    pub fn is_bigrational(&self) -> bool {
        matches!(self, Value::BigRational(_))
    }

    /// Try to extract a BigRational reference
    pub fn as_bigrational(&self) -> Option<&BigRational> {
        match self {
            Value::BigRational(r) => Some(r),
            _ => None,
        }
    }

    /// Convert numeric types to BigRational for mixed arithmetic
    pub fn to_bigrational(&self) -> Option<BigRational> {
        match self {
            Value::BigRational(r) => Some(r.clone()),
            Value::BigInt(n) => Some(BigRational::from(n.clone())),
            Value::Number(n) => Some(BigRational::from(BigInt::from(*n))),
            _ => None,
        }
    }
}
