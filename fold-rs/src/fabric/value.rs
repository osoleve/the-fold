use crate::fabric::{address::Address, block::Block, closure::Closure, symbol::Symbol};
use num_bigint::BigInt;
use num_rational::BigRational;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Port {
    StringInput { content: String, position: usize },
    StringOutput { buffer: String },
    // FileInput and FileOutput can be added later
}

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
    /// Wrapped in Rc for cheap cloning during function application
    Closure(Rc<Closure>),
    /// First-class primitive function reference (e.g., +, cons, car)
    Primitive(Symbol),
    Block(Block),
    /// Mutable hashtable with interior mutability
    Hashtable(Rc<RefCell<HashMap<Value, Value>>>),
    /// Port for I/O operations (string or file)
    Port(Rc<RefCell<Port>>),
    /// Exception condition with kind, message, and irritants
    Condition {
        kind: Symbol,
        message: String,
        irritants: Vec<Value>,
    },
    Nil,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::BigInt(a), Value::BigInt(b)) => a == b,
            (Value::BigRational(a), Value::BigRational(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Bytevector(a), Value::Bytevector(b)) => a == b,
            (Value::Address(a), Value::Address(b)) => a == b,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Vector(a), Value::Vector(b)) => a == b,
            (Value::Closure(a), Value::Closure(b)) => Rc::ptr_eq(a, b),
            (Value::Primitive(a), Value::Primitive(b)) => a == b,
            (Value::Block(a), Value::Block(b)) => a == b,
            (Value::Hashtable(a), Value::Hashtable(b)) => Rc::ptr_eq(a, b),
            (Value::Port(a), Value::Port(b)) => Rc::ptr_eq(a, b),
            (
                Value::Condition {
                    kind: k1,
                    message: m1,
                    irritants: i1,
                },
                Value::Condition {
                    kind: k2,
                    message: m2,
                    irritants: i2,
                },
            ) => k1 == k2 && m1 == m2 && i1 == i2,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Hash the discriminant first
        std::mem::discriminant(self).hash(state);
        match self {
            Value::Number(n) => n.hash(state),
            Value::Float(f) => f.to_bits().hash(state),
            Value::BigInt(n) => n.hash(state),
            Value::BigRational(r) => {
                r.numer().hash(state);
                r.denom().hash(state);
            }
            Value::String(s) => s.hash(state),
            Value::Symbol(sym) => sym.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Char(c) => c.hash(state),
            Value::Bytevector(bytes) => bytes.hash(state),
            Value::Address(addr) => addr.hash(state),
            Value::Pair(car, cdr) => {
                car.hash(state);
                cdr.hash(state);
            }
            Value::Vector(values) => values.hash(state),
            Value::Closure(closure) => {
                // Use pointer address for hashing
                (Rc::as_ptr(closure) as usize).hash(state);
            }
            Value::Primitive(sym) => sym.hash(state),
            Value::Block(block) => block.hash(state),
            Value::Hashtable(ht) => {
                // Use pointer address for hashing
                (Rc::as_ptr(ht) as usize).hash(state);
            }
            Value::Port(port) => {
                // Use pointer address for hashing
                (Rc::as_ptr(port) as usize).hash(state);
            }
            Value::Condition {
                kind,
                message,
                irritants,
            } => {
                kind.hash(state);
                message.hash(state);
                irritants.hash(state);
            }
            Value::Nil => {}
        }
    }
}

impl Value {
    /// Create a Closure value (wraps in Rc for cheap cloning)
    pub fn closure(c: Closure) -> Self {
        Value::Closure(Rc::new(c))
    }

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
