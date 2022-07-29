use std::borrow::Borrow;
use std::collections::HashMap;
use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ProgramParser;

pub type Name = usize;

#[derive(Debug, Clone)]
pub struct Namespace {
    to_ident: HashMap<String, Name>,
    to_string: Vec<String>,
}

impl Namespace {
    pub fn new() -> Self {
        Namespace {
            to_ident: HashMap::new(),
            to_string: Vec::new(),
        }
    }

    pub fn len(&self) -> usize { self.to_string.len() }

    pub fn index_of<S>(&mut self, s: &S) -> Name
        where String: Borrow<S>, S: ToOwned<Owned=String> + ?Sized
    {
        let len_now = self.len() as Name;
        *self.to_ident.entry(s.to_owned()).or_insert_with(|| {
            self.to_string.push(s.to_owned());
            len_now
        })
    }

    pub fn name_of(&self, index: Name) -> Option<&str> {
        self.to_string.get(index).map(AsRef::as_ref)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Value {
    SInt(isize),
    UInt(usize),
    F64(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Assign, LOr, LAnd, Eq, Neq, Lt, Gt, Le, Ge, Or, Xor, And, Lsh, Rsh,
    Add, Sub, Mul, Div, Mod,
}

impl BinOp {
    pub fn from_rule(r: Rule) -> Option<Self> {
        match r {
            Rule::op_assign => Some(BinOp::Assign),
            Rule::op_logic_or => Some(BinOp::LOr),
            Rule::op_logic_and => Some(BinOp::LAnd),
            Rule::op_eq => Some(BinOp::Eq),
            Rule::op_neq => Some(BinOp::Neq),
            Rule::op_lt => Some(BinOp::Lt),
            Rule::op_gt => Some(BinOp::Gt),
            Rule::op_le => Some(BinOp::Le),
            Rule::op_ge => Some(BinOp::Ge),
            Rule::op_or => Some(BinOp::Or),
            Rule::op_xor => Some(BinOp::Xor),
            Rule::op_and => Some(BinOp::And),
            Rule::op_lsh => Some(BinOp::Lsh),
            Rule::op_rsh => Some(BinOp::Rsh),
            Rule::op_add => Some(BinOp::Add),
            Rule::op_sub => Some(BinOp::Sub),
            Rule::op_mul => Some(BinOp::Mul),
            Rule::op_div => Some(BinOp::Div),
            Rule::op_mod => Some(BinOp::Mod),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    Ident, Neg, Not, Cmp,
}

impl UnOp {
    pub fn from_rule(r: Rule) -> Option<Self> {
        match r {
            Rule::op_ident => Some(UnOp::Ident),
            Rule::op_neg => Some(UnOp::Neg),
            Rule::op_not => Some(UnOp::Not),
            Rule::op_cmp => Some(UnOp::Cmp),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Name(Name),
    Literal(Value),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
}

#[inline(always)]
fn child(par: Pair<'_, Rule>) -> Pair<'_, Rule> {
    par.into_inner().next().unwrap()
}

impl Expr {
    pub fn from_ex(ex: Pair<'_, Rule>, ns: &mut Namespace) -> Self {
        let rule = ex.as_rule();
        match rule {
            Rule::atom => Self::from_ex(child(ex), ns),
            Rule::literal => Self::from_ex(child(ex), ns),
            Rule::lit_int => Self::from_ex(child(ex), ns),

            Rule::lit_float => Expr::Literal(
                Value::F64(ex.as_str().parse().unwrap())
            ),

            Rule::lit_hex => Expr::Literal(
                Value::UInt(usize::from_str_radix(
                        &ex.as_str()[2..], 16
                ).unwrap())
            ),
            Rule::lit_oct => Expr::Literal(
                Value::UInt(usize::from_str_radix(
                        &ex.as_str()[2..], 8
                ).unwrap())
            ),
            Rule::lit_bin => Expr::Literal(
                Value::UInt(usize::from_str_radix(
                        &ex.as_str()[2..], 2
                ).unwrap())
            ),
            Rule::lit_dec => Expr::Literal(
                Value::UInt(ex.as_str().parse().unwrap())
            ),

            Rule::ident => Expr::Name(ns.index_of(ex.as_str())),

            Rule::unary => {
                let mut it = ex.into_inner().rev();
                let mut ex = Self::from_ex(it.next().unwrap(), ns);
                while let Some(op) = it.next() {
                    ex = Expr::UnOp(
                        UnOp::from_rule(op.as_rule()).unwrap(),
                        Box::new(ex),
                    );
                }
                ex
            },

            Rule::assign |
            Rule::logic_or |
            Rule::logic_and |
            Rule::rel_eq |
            Rule::rel_ineq |
            Rule::bit_or |
            Rule::bit_and |
            Rule::arith_term |
            Rule::arith_factor
            => {
                let mut it = ex.into_inner();
                let mut ex = Self::from_ex(it.next().unwrap(), ns);
                while let Some(op) = it.next() {
                    ex = Expr::BinOp(
                        Box::new(ex),
                        BinOp::from_rule(op.as_rule()).unwrap(),
                        Box::new(Self::from_ex(it.next().unwrap(), ns)),
                    );
                }
                ex
            },

            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub expr: Expr,
    pub ns: Namespace,
}

impl Program {
    pub fn from_parse(mut parse: Pairs<'_, Rule>) -> Self {
        Self::from_program(parse.next().unwrap())
    }

    pub fn from_program(pgm: Pair<'_, Rule>) -> Self {
        let mut ns = Namespace::new();
        let ex = Expr::from_ex(child(pgm), &mut ns);
        Self { expr: ex, ns }
    }
}
