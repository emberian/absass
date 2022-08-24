use comfy_table::Table;

use crate::isa::*;

use std::io::Write;

pub struct Machine {
    pub regs: [Word; 16],
    pub memory: Vec<u8>,
    pub ivt: Reg,
    pub cycles: Word,
    pub insns: Word,
    pub bps: [Option<Word>; 16],
    pub sr10: Option<Word>,
}

impl Default for Machine {
    fn default() -> Self {
        Self {
            regs: Default::default(),
            memory: vec![0; 256],
            ivt: Default::default(),
            cycles: Default::default(),
            insns: Default::default(),
            bps: Default::default(),
            sr10: Default::default(),
        }
    }
}

impl std::fmt::Debug for Machine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut t = Table::new();
        t.add_row(self.regs[0..8].iter().map(|x| format!("{:04x}", x)));
        t.add_row(self.regs[8..].iter().map(|x| format!("{:04x}", x)));
        t.add_row(
            vec![
                (self.ivt as usize, "eh"),
                (self.cycles as usize, "cycs"),
                (self.insns as usize, "instret"),
                (self.sr10.unwrap_or(0) as usize, "sr10"),
            ]
            .iter()
            .map(|(x, lbl)| format!("{} = {:04x}", lbl, x)),
        );
        write!(f, "{}", t)
    }
}
#[derive(Debug, Hash, Clone, Copy)]
pub enum StepOut {
    Continue,
    Halt,
}

impl Machine {
    pub const FREQ: Word = 0xa55;

    pub fn pc(&self) -> usize {
        self.regs[0] as usize
    }

    pub fn reset(&mut self) {
        *self = Machine::default();
    }

    pub fn exec(&mut self, i: Insn) -> StepOut {
        use StepOut::*;
        #[cfg(debug_assertions)]
        println!("exec {}", i);

        let mut pc = self.pc() + 2;
        match i {
            Insn::Logic { src, dst, op } => {
                let mut res: Word = 0;
                for i in 0..(WORDSZ * 8) {
                    let ix =
                        (self.regs[src] & (1 << i)) >> i | ((self.regs[dst] & (1 << i)) >> i << 1);
                    res |= (((op & (1 << ix)) as Word) >> ix) << i;
                }
                self.regs[dst] = res;
            }
            Insn::Arith { src, dst, si, op } => {
                let src = if si { src as Word } else { self.regs[src] };
                self.regs[dst] = match op {
                    ArithOp::Add => self.regs[dst].wrapping_add(src),
                    // make the rest of the code wrapping
                    ArithOp::Sub => self.regs[dst].wrapping_sub(src),
                    ArithOp::Shl => self.regs[dst] << src,
                    ArithOp::Shr => self.regs[dst] >> src,
                    ArithOp::Asr => ((self.regs[dst] as i64) >> src) as Word,
                    ArithOp::Mul => self.regs[dst].wrapping_mul(src),
                    ArithOp::Div => self.regs[dst] / src,
                    ArithOp::Mod => self.regs[dst] % src,
                };
            }
            Insn::Compare {
                src,
                dst,
                eq,
                sn,
                gt,
                iv,
            } => {
                let e = eq && self.regs[src] == self.regs[dst];
                let g = gt && sn && ((self.regs[dst] as i64) > (self.regs[src] as i64))
                    || (gt && !sn && self.regs[dst] > self.regs[src]);
                self.regs[dst] = if iv { !(e || g) } else { e || g } as Word;
            }
            Insn::Move {
                src,
                dst,
                s_mode,
                s_deref,
                d_mode,
                d_deref,
            } => {
                self.regs[0] = pc as Word;
                let s = self.regs[src];
                let d = self.regs[dst];
                let s_val = if let MoveMode::Decr = s_mode {
                    self.regs[src] = s.wrapping_sub(WORDSZ);
                    s.wrapping_sub(WORDSZ)
                } else {
                    s
                };
                let s = if s_deref {
                    let mut buf = [0u8; WORDSZ as usize];
                    buf.copy_from_slice(
                        &self.memory[s_val as usize..s_val as usize + WORDSZ as usize],
                    );
                    let w = Word::from_be_bytes(buf);
                    #[cfg(debug_assertions)]
                    println!("read {:016x} from {:016x}", w, s_val);
                    w
                } else {
                    s
                };
                match s_mode {
                    MoveMode::Incr => {
                        self.regs[src] = self.regs[src].wrapping_add(WORDSZ);
                    }
                    MoveMode::DecrPost => {
                        self.regs[src] = self.regs[src].wrapping_sub(WORDSZ);
                    }
                    _ => (),
                }
                let d_val = if let MoveMode::Decr = d_mode {
                    self.regs[dst] = d.wrapping_sub(WORDSZ);

                    d.wrapping_sub(WORDSZ)
                } else {
                    d
                };
                if d_deref {
                    let bts = Word::to_be_bytes(s);
                    self.memory[d_val as usize..d_val as usize + WORDSZ as usize]
                        .copy_from_slice(&bts);
                    #[cfg(debug_assertions)]
                    println!("wrote {:016x} to {:016x}", s, d_val);
                } else {
                    self.regs[dst] = s;
                }
                match d_mode {
                    MoveMode::Incr => {
                        self.regs[dst] = self.regs[dst].wrapping_add(WORDSZ);
                    }
                    MoveMode::DecrPost => {
                        self.regs[dst] = self.regs[dst].wrapping_sub(WORDSZ);
                    }
                    _ => (),
                }

                self.cycles += 1;
                self.insns += 1;
                return Continue;
            }
            Insn::JumpLink { link, prog } => {
                self.regs[link] = pc as Word;
                pc = self.regs[prog] as usize;
            }
            Insn::JumpCond { offset, cond } => {
                if self.regs[cond] != 0 {
                    pc = pc.checked_add_signed(offset as isize).unwrap();
                }
            }
            Insn::SubWord { dst, index, bytes } => {
                let b = if bytes == 0 { 8 } else { bytes };
                let m = (1 << (8 * b)) - 1;
                let s = 8 * index;
                self.regs[dst] = (self.regs[dst] & (m << s)) >> s;
            }
            Insn::SysReg { write, reg, sr } => match sr {
                0 => {
                    if !write {
                        self.regs[reg] = STEPPING as Word;
                    }
                }
                1 => {
                    if !write {
                        self.regs[reg] = std::mem::size_of::<usize>() as Word * 8;
                    }
                }
                2 => {
                    if write {
                        self.ivt = self.regs[reg] as Reg;
                    } else {
                        self.regs[reg] = self.ivt as Word;
                    }
                }
                3 => {
                    return Halt;
                }
                4 => {
                    if write {
                        self.insns = self.regs[reg];
                    } else {
                        self.regs[reg] = self.insns;
                    }
                }
                5 => {
                    if write {
                        self.cycles = self.regs[reg];
                    } else {
                        self.regs[reg] = self.cycles;
                    }
                }
                6 => {
                    if !write {
                        self.regs[reg] = Self::FREQ;
                    }
                }
                7 => {
                    if !write {
                        self.regs[reg] = Extension::Multiplier as Word;
                    }
                }
                10 => {
                    if write {
                        self.sr10 = Some(self.regs[reg]);
                        print!("{}", char::from_u32(self.regs[reg] as u32).unwrap_or('?'));
                        std::io::stdout().flush().unwrap();
                    }
                }
                _ => (),
            },
            Insn::SmallImm { dst, val } => {
                self.regs[dst] = val as Word;
            }
            Insn::NotSure { value } => {
                panic!("tried to execute unknown instruction {:?}", value);
            }
        }

        self.regs[0] = pc as Word;
        self.cycles += 1;
        self.insns += 1;
        Continue
    }

    pub fn step(&mut self) -> StepOut {
        let i =
            Insn::decode((self.memory[self.pc()] as u16) << 8 | self.memory[self.pc() + 1] as u16);
        self.exec(i)
    }

    pub fn check_bps(&self) -> Option<u8> {
        for i in 0..self.regs.len() {
            if Some(self.regs[i]) == self.bps[i] {
                return Some(i as u8);
            }
        }
        None
    }

    pub fn run(&mut self) {
        while self.pc() + 1 < self.memory.len() {
            #[cfg(debug_assertions)]
            {
                for r in 0..16 {
                    print!("{:x}:{:016x} ", r, self.regs[r]);
                }
                println!();
            }
            match self.step() {
                StepOut::Halt => break,
                StepOut::Continue => (),
            }
        }
    }
}
