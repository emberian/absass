use super::*;
use crate::grammar::{Expr, Program};

impl Gen for Program {
    fn gen(&self, cx: &mut Cx) -> Res {
        self.expr.gen(cx)
    }
}

impl Gen for Expr {
    fn gen(&self, cx: &mut Cx) -> Res {
        todo!()
    }
}
