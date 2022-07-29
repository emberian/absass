pub mod bv;
pub mod insn;
pub mod imp;

use insn::*;

#[derive(Debug)]
pub struct Cx {
    next_temp: Temp,
    next_block: usize,
}

#[derive(Debug, Clone)]
pub struct Block {
    label: String,
    prelude: Vec<Line>,
    children: Vec<Block>,
    postlude: Vec<Line>,
}

#[derive(Debug, Clone)]
pub enum Line {
    Insn(Insn),
    Label(String),
    Word(usize),
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
            Line::Word(w) => format!(".WORD {}", w)
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

impl Cx {
    pub fn new() -> Self {
        Cx {
            next_temp: 0,
            next_block: 0,
        }
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

pub struct Res {
    pub block: Block,
    pub place: Option<Place>,
}

pub trait Gen {
    fn gen(&self, cx: &mut Cx) -> Res;
}
