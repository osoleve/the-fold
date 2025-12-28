use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::core::{symbol::Symbol, value::Value};

pub type EnvRef = Rc<RefCell<Env>>;

#[derive(Debug)]
pub struct Env {
    bindings: HashMap<Symbol, Value>,
    parent: Option<EnvRef>,
}

impl Env {
    pub fn new() -> EnvRef {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    pub fn with_parent(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: Some(parent),
        }))
    }

    pub fn extend<I>(parent: EnvRef, bindings: I) -> EnvRef
    where
        I: IntoIterator<Item = (Symbol, Value)>,
    {
        Rc::new(RefCell::new(Self {
            bindings: bindings.into_iter().collect(),
            parent: Some(parent),
        }))
    }

    pub fn insert(env: &EnvRef, name: Symbol, value: Value) {
        env.borrow_mut().bindings.insert(name, value);
    }

    pub fn lookup(env: &EnvRef, name: &Symbol) -> Option<Value> {
        let (value, parent) = {
            let scoped = env.borrow();
            (scoped.bindings.get(name).cloned(), scoped.parent.clone())
        };

        if value.is_some() {
            return value;
        }

        parent.and_then(|parent| Self::lookup(&parent, name))
    }
}
