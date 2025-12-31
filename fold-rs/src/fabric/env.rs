use std::cell::RefCell;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::fabric::{symbol::Symbol, value::Value};

pub type EnvRef = Rc<RefCell<Env>>;

#[derive(Debug)]
pub struct Env {
    /// FxHashMap is faster than std HashMap for small keys like Symbol (u32)
    bindings: FxHashMap<Symbol, Value>,
    parent: Option<EnvRef>,
}

impl Env {
    pub fn new() -> EnvRef {
        Rc::new(RefCell::new(Self {
            bindings: FxHashMap::default(),
            parent: None,
        }))
    }

    pub fn with_parent(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            bindings: FxHashMap::default(),
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

    /// Set a variable binding, searching up the parent chain.
    /// Returns true if the binding was found and updated, false otherwise.
    pub fn set(env: &EnvRef, name: &Symbol, value: Value) -> bool {
        // Check if this environment has the binding
        {
            let mut scoped = env.borrow_mut();
            if scoped.bindings.contains_key(name) {
                scoped.bindings.insert(*name, value);
                return true;
            }
        }

        // Not in this environment - check parent
        let parent = env.borrow().parent.clone();
        if let Some(parent) = parent {
            Self::set(&parent, name, value)
        } else {
            false
        }
    }
}
