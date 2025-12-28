use crate::core::{address::Address, block::Block, closure::Closure, symbol::Symbol};

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    Float(f64),
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
