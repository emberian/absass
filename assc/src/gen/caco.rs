use super::*;
use super::rall::{PC, SP, RA, A0, A_NUM};

#[derive(Debug, Clone)]
pub struct CallConv {
    regs: Vec<Reg>,
    sp: Reg,
}

#[derive(Debug)]
pub struct CaCoCx {
    reg_stack: Vec<Reg>,
    reg_claimed: Vec<Reg>,
    sp: Reg,
    block: Block,
    pushed: usize,
}

impl CallConv {
    pub fn new(sp: Reg) -> Self {
        Self {
            regs: Vec::new(),
            sp,
        }
    }

    pub fn begin(&self, cx: &mut Cx) -> CaCoCx {
        let bk = cx.block()
            .before(Line::Stack(Dir::Entry));
        CaCoCx {
            reg_stack: self.regs.clone(),
            reg_claimed: Vec::new(),
            sp: self.sp,
            block: bk,
            pushed: 0,
        }
    }

    pub fn regs(&self) -> &Vec<Reg> { &self.regs }
}

impl Default for CallConv {
    fn default() -> Self {
        // Note the stack modality--push these in reverse.
        let mut regs = Vec::new();
        for i in (0 .. A_NUM).rev() {
            regs.push(A0 + i);
        }
        CallConv {
            regs,
            sp: SP,
        }
    }
}

impl CaCoCx {
    pub fn arg(&mut self, res: Res, cx: &mut Cx) {
        let mut bk = res.block.unwrap_or_else(|| cx.block());
        let p = res.place.expect("invalid arg");
        if let Some(reg) = self.reg_stack.pop() {
            self.reg_claimed.push(reg);
            bk = bk
                .after(Line::Life(Life::Claim, reg))
                .load_into(
                    cx,
                    Place::Reg(reg),
                    p
                );
        } else {
            // if self.pushed == 0 {
            //     bk = bk.before(Line::Stack(Dir::Entry));
            // }
            self.pushed += 1;
            bk = bk.load_into(
                cx,
                Place::Stack,
                p
            );
        }
        self.block.add_child(bk);
    }

    pub fn end(self, cbl: Res, cx: &mut Cx) -> Res {
        let mut block = self.block;
        if let Some(bk) = cbl.block {
            block = block.child(bk);
        }
        // if self.pushed == 0 {
        //     block = block.before(Line::Stack(Dir::Entry));
        // }
        let p = cbl.place.expect("invalid callable");
        block = block
            .load_into(cx, Place::Stack, Place::Reg(RA))
            .load_into(cx, Place::Reg(RA), p)
            .after(Insn::Misc {
                op: MiscOp::Swap,
                a: Place::Reg(PC),
                b: Place::Reg(RA),
            }.into())
            .load_into(cx, Place::Reg(RA), Place::Stack);
        for _ in 0 .. self.pushed {
            block = block.load_into(cx, Place::Reg(A0 + 1), Place::Stack);
        }
        for reg in self.reg_claimed {
            block = block.after(Line::Life(Life::Release, reg));
        }
        block = block.after(Line::Stack(Dir::Exit));
        Res { block: Some(block), place: Some(Place::Reg(A0)) }
    }
}
