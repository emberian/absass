pub mod bv;
pub mod insn;
pub mod imp;
pub mod env;
pub mod ty;
pub mod rall;
pub mod caco;

use insn::*;

use crate::{grammar::{self, Program, Stmts}, gen::rall::SP};
use env::Env;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExprCx {
    Read,
    Write,
}

#[derive(Debug)]
pub struct Cx {
    next_temp: Temp,
    next_block: usize,
    pub gcx: grammar::Context,
    pub env: Env,
    pub ecx: ExprCx,
    pub caco: caco::CallConv,
}

#[derive(Debug, Clone)]
pub struct Block {
    label: String,
    prelude: Vec<Line>,
    children: Vec<Block>,
    postlude: Vec<Line>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Life {
    Claim, Release,
}

impl ToAsm for Life {
    fn to_asm(&self) -> String {
        match self {
            Life::Claim => "CLAIM",
            Life::Release => "RELEASE",
        }.into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Dir {
    Entry, Exit
}

impl ToAsm for Dir {
    fn to_asm(&self) -> String {
        match self {
            Dir::Entry => "IN",
            Dir::Exit => "OUT",
        }.into()
    }
}

#[derive(Debug, Clone)]
pub enum Line {
    Insn(Insn),
    Label(String),
    Word(usize),
    WordExpr(String),
    Life(Life, Reg),
    Stack(Dir),
}

impl From<Insn> for Line {
    fn from(insn: Insn) -> Line {
        Line::Insn(insn)
    }
}

impl ToAsm for Line {
    fn to_asm(&self) -> String {
        match self {
            Line::Insn(i) => i.to_asm(),
            Line::Label(l) => format!("{}:", l),
            Line::Word(w) => format!(".WORD {};", w),
            Line::WordExpr(s) => format!(".WORD {};", s),
            Line::Life(l, r) => format!("# Life: {} R{}", l.to_asm(), r),
            Line::Stack(d) => format!("# Stack: {}", d.to_asm()),
        }
    }
}

impl Block {
    pub fn new(label: String) -> Self {
        Self::with_lines(label, Vec::new())
    }

    pub fn with_lines(label: String, lines: Vec<Line>) -> Self {
        Block {
            label,
            prelude: lines,
            children: Vec::new(),
            postlude: Vec::new(),
        }
    }

    pub fn before(mut self, line: Line) -> Self {
        self.prelude.push(line);
        self
    }

    pub fn after(mut self, line: Line) -> Self {
        self.postlude.push(line);
        self
    }

    pub fn child(mut self, block: Block) -> Self {
        self.children.push(block);
        self
    }

    // Mutating versions of the above
    pub fn add_before(&mut self, line: Line) {
        self.prelude.push(line);
    }

    pub fn add_after(&mut self, line: Line) {
        self.postlude.push(line);
    }

    pub fn add_child(&mut self, block: Block) {
        self.children.push(block);
    }

    pub fn load_into(self, cx: &mut Cx, dst: Place, src: Place) -> Self {
        use Place::*;

        fn mov(bk: Block, dst: Place, src: Place) -> Block {
            bk.after(Insn::Logic { src, dst, op: LogicOp::S }.into())
        }

        fn bxft(mut bk: Block, cx: &mut Cx, pl: Place, dst: bool) -> (Xft, Block) {
            match pl {
                Reg(r) => (Xft { reg: Reg(r), indirect: false, mode: AutoMode::None }, bk),
                Temp(t) => (Xft { reg: Temp(t), indirect: false, mode: AutoMode::None }, bk),
                Label(l) => {
                    let temp = cx.temp();
                    bk = bk.after(Insn::Transfer {
                        src: Xft { reg: Reg(rall::PC), indirect: true, mode: AutoMode::PostIncr },
                        dst: Xft { reg: temp.clone(), indirect: false, mode: AutoMode::None },
                    }.into())
                        .after(Line::WordExpr(l.clone()));
                    (Xft { reg: temp, indirect: true, mode: AutoMode::None }, bk)
                },
                Stack => if dst {
                    (Xft { reg: Place::Reg(SP), indirect: true, mode: AutoMode::PreDecr }, bk)
                } else {
                    (Xft { reg: Place::Reg(SP), indirect: true, mode: AutoMode::PostIncr }, bk)
                },
            }
        }


        match (dst, src) {
            (Reg(d), Reg(s)) => mov(self, Reg(d), Reg(s)),
            (Reg(d), Temp(s)) => mov(self, Reg(d), Temp(s)),
            (Temp(d), Reg(s)) => mov(self, Temp(d), Reg(s)),
            (Temp(d), Temp(s)) => mov(self, Temp(d), Temp(s)),
            (d, s) => {
                let (dxft, bk) = bxft(self, cx, d, true);
                let (sxft, bk) = bxft(bk, cx, s, false);
                bk.after(Insn::Transfer { src: sxft, dst: dxft }.into())
            }
        }
    }

    pub fn to_linear(&self) -> Vec<Line> {
        let mut lines = vec![Line::Label(self.label.clone())];
        lines.extend(self.prelude.iter().cloned());
        for child in &self.children {
            lines.extend(child.to_linear());
        }
        lines.extend(self.postlude.iter().cloned());
        lines
    }
}

pub type Top = Stmts;

impl Cx {
    pub fn new(pgm: Program) -> (Self, Top) {
        (Cx {
            next_temp: 0,
            next_block: 0,
            gcx: pgm.context,
            env: Env::new(),
            ecx: ExprCx::Read,
            caco: caco::CallConv::default(),
        }, pgm.stmts)
    }

    pub fn temp(&mut self) -> Place {
        let res = Place::Temp(self.next_temp);
        self.next_temp += 1;
        res
    }

    fn block_label(&mut self) -> String {
        let res = format!("B{}", self.next_block);
        self.next_block += 1;
        res
    }

    pub fn block(&mut self) -> Block {
        Block::new(self.block_label())
    }

    pub fn block_insns(&mut self, lines: Vec<Line>) -> Block {
        Block::with_lines(self.block_label(), lines)
    }
}

#[derive(Debug, Clone)]
pub struct Res {
    pub block: Option<Block>,
    pub place: Option<Place>,
}

pub trait Gen {
    fn gen(&self, cx: &mut Cx) -> Res;
}
