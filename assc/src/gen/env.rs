use crate::grammar::Name;
use super::Place;
use super::ty::Ty;

use std::collections::HashMap;

#[derive(Debug, Clone, Hash)]
pub struct Bind {
    pub name: Name,
    pub ty: Ty,
    pub place: Place,
}

#[derive(Debug, Clone)]
pub struct Env {
    parent: Option<Box<Env>>,
    bindings: HashMap<Name, Bind>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            parent: None,
            bindings: HashMap::new(),
        }
    }

    pub fn derive(env: Env) -> Self {
        Self {
            parent: Some(Box::new(env)),
            bindings: HashMap::new(),
        }
    }

    pub fn pop(mut self) -> Option<Self> { self.parent.take().map(|x| *x) }

    pub fn resolve(&self, name: Name) -> Option<&Bind> {
        self.bindings.get(&name)
            .or_else(|| self.parent.as_ref().and_then(|par| par.resolve(name)))
    }

    pub fn insert(&mut self, bind: Bind) {
        self.bindings.insert(bind.name, bind);
    }
}
