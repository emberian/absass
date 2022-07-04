use std::io::{BufRead, Read};

#[derive(Debug, Hash, Clone, Copy)]
pub enum MoveMode {
    Direct,
    Incr,
    Decr,
    Resv,
}
impl MoveMode {
    pub fn from_u8(val: u8) -> MoveMode {
        match val {
            0 => MoveMode::Direct,
            1 => MoveMode::Incr,
            2 => MoveMode::Decr,
            3 => MoveMode::Resv,
            _ => panic!("Invalid MoveMode value: {}", val),
        }
    }
}
pub type Reg = usize; // ruuuuust
pub type Word = u64;
const WORDSZ: Word = 8;

#[derive(Debug, Hash, Clone, Copy)]
pub enum ArithOp {
    Add,
    Sub,
    Shl,
    Shr,
    Asr,
    Mul,
    Div,
    Mod,
}
impl ArithOp {
    pub fn from_u8(val: u8) -> ArithOp {
        match val {
            0 => ArithOp::Add,
            1 => ArithOp::Sub,
            2 => ArithOp::Shl,
            3 => ArithOp::Shr,
            4 => ArithOp::Asr,
            5 => ArithOp::Mul,
            6 => ArithOp::Div,
            7 => ArithOp::Mod,
            _ => panic!("invalid arith op"),
        }
    }
}
#[derive(Debug, Hash, Clone, Copy)]
pub enum Insn {
    Logic {
        src: Reg,
        dst: Reg,
        op: u8,
    },
    Arith {
        src: Reg,
        dst: Reg,
        op: ArithOp,
    },
    Compare {
        src: Reg,
        dst: Reg,
        eq: bool,
        sn: bool,
        gt: bool,
        iv: bool,
    },
    Move {
        src: Reg,
        dst: Reg,
        s_mode: MoveMode,
        s_deref: bool,
        d_mode: MoveMode,
        d_deref: bool,
    },
}

impl Insn {
    pub fn encode(&self) -> u16 {
        match self {
            Insn::Logic { src, dst, op } => {
                *dst as u16 | ((*src as u16) << 4) | ((*op as u16) << 8) | (0x1 << 12)
            }
            Insn::Arith { src, dst, op } => {
                *dst as u16 | ((*src as u16) << 4) | ((*op as u16) << 8) | (0x2 << 12)
            }
            Insn::Compare {
                src,
                dst,
                eq,
                sn,
                gt,
                iv,
            } => {
                *dst as u16
                    | ((*src as u16) << 4)
                    | ((*eq as u16) << 8)
                    | ((*gt as u16) << 9)
                    | ((*sn as u16) << 10)
                    | ((*iv as u16) << 11)
                    | (0x3 << 12)
            }
            Insn::Move {
                src,
                dst,
                s_mode,
                s_deref,
                d_mode,
                d_deref,
            } => {
                *dst as u16
                    | ((*src as u16) << 4)
                    | ((*s_mode as u16) << 11)
                    | ((*s_deref as u16) << 13)
                    | ((*d_deref as u16) << 10)
                    | ((*d_mode as u16) << 8)
                    | (0x4 << 12)
            }
        }
    }

    pub fn decode(val: u16) -> Insn {
        match val & 0xf000 {
            0x1000 => {
                let src = (val & 0xf0) >> 4;
                let dst = val & 0xf;
                let op = (val & 0xf00) >> 8;
                Insn::Logic {
                    src: src as Reg,
                    dst: dst as Reg,
                    op: op as u8,
                }
            }
            0x2000 => {
                let src = (val & 0xf0) >> 4;
                let dst = val & 0xf;
                let op = (val & 0xf00) >> 8;
                Insn::Arith {
                    src: src as Reg,
                    dst: dst as Reg,
                    op: ArithOp::from_u8(op as u8),
                }
            }
            0x3000 => {
                let src = (val & 0xf0) >> 4;
                let dst = val & 0xf;
                let eq = (val & 0x100) >> 8;
                let sn = (val & 0x200) >> 10;
                let gt = (val & 0x400) >> 9;
                let iv = (val & 0x800) >> 11;
                Insn::Compare {
                    src: src as Reg,
                    dst: dst as Reg,
                    eq: eq != 0,
                    sn: sn != 0,
                    gt: gt != 0,
                    iv: iv != 0,
                }
            }
            0x4000..=0x7000 => {
                let src = (val & 0xf0) >> 4;
                let dst = val & 0xf;
                let s_mode = (val & 0x800) >> 11;
                let s_deref = (val & 0x1000) >> 12;
                let d_mode = (val & 0x400) >> 10;
                let d_deref = (val & 0x200) >> 9;
                Insn::Move {
                    src: src as Reg,
                    dst: dst as Reg,
                    s_mode: MoveMode::from_u8(s_mode as u8),
                    s_deref: s_deref != 0,
                    d_mode: MoveMode::from_u8(d_mode as u8),
                    d_deref: d_deref != 0,
                }
            }
            _ => panic!("invalid insn"),
        }
    }
}

struct Machine {
    regs: [Word; 16],
    memory: Vec<u8>,
    pc: usize,
}
impl Machine {
    fn step(&mut self, i: Insn) {
        let pc = self.pc + 2;
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
            Insn::Arith { src, dst, op } => {
                self.regs[dst] = match op {
                    ArithOp::Add => self.regs[dst].wrapping_add(self.regs[src]),
                    // make the rest of the code wrapping
                    ArithOp::Sub => self.regs[dst].wrapping_sub(self.regs[src]),
                    ArithOp::Shl => self.regs[dst] << self.regs[src],
                    ArithOp::Shr => self.regs[dst] >> self.regs[src],
                    ArithOp::Asr => ((self.regs[dst] as i64) >> self.regs[src]) as Word,
                    ArithOp::Mul => self.regs[dst].wrapping_mul(self.regs[src]),
                    ArithOp::Div => self.regs[dst] / self.regs[src],
                    ArithOp::Mod => self.regs[dst] % self.regs[src],
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
                self.regs[0] = self.pc as Word;
                let s = self.regs[src];
                let d = self.regs[dst];
                let s_val = if let MoveMode::Decr = s_mode {
                    self.regs[src] = s.wrapping_sub(WORDSZ);
                    s.wrapping_sub(WORDSZ)
                } else if let MoveMode::Resv = s_mode {
                    !0
                } else {
                    s
                };
                let d_val = if let MoveMode::Decr = d_mode {
                    self.regs[dst] = d.wrapping_sub(WORDSZ);

                    d.wrapping_sub(WORDSZ)
                } else if let MoveMode::Resv = d_mode {
                    !0
                } else {
                    d
                };
                let s = if s_deref {
                    let mut buf = [0u8; 8];
                    buf.copy_from_slice(
                        &self.memory[s_val as usize..s_val as usize + WORDSZ as usize],
                    );
                    Word::from_le_bytes(buf)
                } else {
                    s
                };
                if d_deref {
                    let bts = Word::to_le_bytes(s);
                    self.memory[d_val as usize..d_val as usize + WORDSZ as usize]
                        .copy_from_slice(&bts);
                } else {
                    self.regs[dst] = s;
                }
                if let MoveMode::Incr = s_mode {
                    self.regs[src] = self.regs[src].wrapping_add(WORDSZ);
                }
                if let MoveMode::Incr = d_mode {
                    self.regs[dst] = self.regs[dst].wrapping_add(WORDSZ);
                }

                return;
            }
        }

        self.regs[0] = pc as Word;
    }

    pub fn run(&mut self) {
        while self.pc < self.memory.len() {
            let i =
                Insn::decode((self.memory[self.pc] as u16) << 8 | self.memory[self.pc + 1] as u16);
            self.step(i);
        }
    }
}

fn main() {
    match std::env::args().nth(1).as_deref() {
        Some("test") => {
            let mut m = Machine {
                regs: [0; 16],
                memory: vec![0; 0x10000],
                pc: 0,
            };
            m.step(Insn::Logic {
                src: 1,
                dst: 1,
                op: 0xf,
            });
            assert_eq!(m.regs[1], !0);
            m.regs[3] = 0x8;

            m.step(Insn::Move {
                src: 1,
                dst: 3,
                s_mode: MoveMode::Direct,
                s_deref: false,
                d_mode: MoveMode::Direct,
                d_deref: true,
            });
            assert_eq!(m.memory[0x8 as usize], 0xff);
        }
        Some("disasm") => {
            let mut insn_buf = [0u8; 2];
            let mut i = std::io::stdin().lock();
            while let Ok(2) = i.read(&mut insn_buf) {
                println!(
                    "{:?}",
                    Insn::decode((insn_buf[0] as u16) << 8 | insn_buf[1] as u16)
                );
            }
        }
        Some("dishex") => {
            for l in std::io::stdin().lock().lines() {
                let l = l.unwrap();
                let l = l.trim_start_matches("0x");

                let iv = Insn::decode(u16::from_str_radix(l, 16).unwrap());
                println!("{:?}", iv);
            }
        }

        Some(_) => eprintln!("no such command!"),
        None => eprintln!("no command given!"),
    }
}
