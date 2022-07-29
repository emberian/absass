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

pub type Env = HashMap<Name, Bind>;
