use std::io::{BufRead, Read};

const STEPPING: usize = 0;

#[derive(Debug, Hash, Clone, Copy)]
pub enum MoveMode {
    Direct,
    Incr,
    Decr,
    DecrPost,
}
impl MoveMode {
    pub fn from_u8(val: u8) -> MoveMode {
        match val {
            0 => MoveMode::Direct,
            1 => MoveMode::Incr,
            2 => MoveMode::Decr,
            3 => MoveMode::DecrPost,
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
    SysReg {
        write: bool,
        reg: Reg,
        sr: u8,
    },
    NotSure {
        value: u16,
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
            Insn::SysReg {
                write,
                reg,
                sr,
            } => {
                (0xau16 << 12)
                    | ((*write as u16) << 12)
                    | ((*reg as u16) << 8)
                    | (*sr as u16)
            }
            Insn::NotSure {
                value,
            } => {
                *value
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
            0xa000..=0xb000 => {
                let write = (val & 0x1000) >> 12;
                let reg = (val & 0xf00) >> 8;
                let sr = val & 0xff;
                Insn::SysReg {
                    write: write != 0,
                    reg: reg as Reg,
                    sr: sr as u8,
                }
            }
            x => Insn::NotSure { value: x },
        }
    }
}

struct Machine {
    regs: [Word; 16],
    memory: Vec<u8>,
    ivt: Reg,
}

#[derive(Debug, Hash, Clone, Copy)]
pub enum StepOut {
    Continue,
    Halt,
}

impl Machine {
    fn pc(&self) -> usize { self.regs[0] as usize }

    fn step(&mut self, i: Insn) -> StepOut {
        use StepOut::*;
        println!("exec {:?}", i);

        let pc = self.pc() + 2;
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
                self.regs[0] = pc as Word;
                let s = self.regs[src];
                let d = self.regs[dst];
                let s_val = if let MoveMode::Decr = s_mode {
                    self.regs[src] = s.wrapping_sub(WORDSZ);
                    s.wrapping_sub(WORDSZ)
                } else {
                    s
                };
                let d_val = if let MoveMode::Decr = d_mode {
                    self.regs[dst] = d.wrapping_sub(WORDSZ);

                    d.wrapping_sub(WORDSZ)
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
                match s_mode {
                    MoveMode::Incr => { self.regs[src] = self.regs[src].wrapping_add(WORDSZ); }
                    MoveMode::DecrPost => { self.regs[src] = self.regs[src].wrapping_sub(WORDSZ); }
                    _ => ()
                }
                match d_mode {
                    MoveMode::Incr => { self.regs[dst] = self.regs[dst].wrapping_add(WORDSZ); }
                    MoveMode::DecrPost => { self.regs[dst] = self.regs[dst].wrapping_sub(WORDSZ); }
                    _ => ()
                }

                return Continue;
            }
            Insn::SysReg {
                write,
                reg,
                sr
            } => {
                match sr {
                    0 => if !write { self.regs[reg] = STEPPING as Word; }
                    1 => if !write { self.regs[reg] = std::mem::size_of::<usize>() as Word * 8; }
                    2 => if write {
                        self.ivt = self.regs[reg] as Reg;
                    } else {
                        self.regs[reg] = self.ivt as Word;
                    }
                    3 => { return Halt; }
                    _ => ()
                }
            }
            Insn::NotSure {
                value,
            } => {
                panic!("tried to execute unknown instruction {:?}", value);
            }
        }

        self.regs[0] = pc as Word;
        Continue
    }

    pub fn run(&mut self) {
        while self.pc() < self.memory.len() {
            println!("pc {:?}", self.pc());
            let i =
                Insn::decode((self.memory[self.pc()] as u16) << 8 | self.memory[self.pc() + 1] as u16);
            if let StepOut::Halt = self.step(i) { break; }
        }
    }
}

fn main() {
    match std::env::args().nth(1).as_deref() {
        Some("test") => {
            let mut m = Machine {
                regs: [0; 16],
                memory: vec![0; 0x10000],
                ivt: 0,
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
        },
        Some("run") => {
            let mut m = Machine {
                regs: [0; 16],
                memory: Vec::new(),
                ivt: 0,
            };
            if let Some(filename) = std::env::args().nth(2) {
                let mut rdr = std::fs::File::open(filename).expect("opening program file");
                rdr.read_to_end(&mut m.memory);
            } else {
                std::io::stdin().read_to_end(&mut m.memory);
            }
            m.run();
        },
        Some("disasm") => {
            let mut insn_buf = [0u8; 2];
            let mut i = std::io::stdin().lock();
            while let Ok(2) = i.read(&mut insn_buf) {
                println!(
                    "{:?}",
                    Insn::decode((insn_buf[0] as u16) << 8 | insn_buf[1] as u16)
                );
            }
        },
        Some("dishex") => {
            for l in std::io::stdin().lock().lines() {
                let l = l.unwrap();
                let l = l.trim_start_matches("0x");

                let iv = Insn::decode(u16::from_str_radix(l, 16).unwrap());
                println!("{:?}", iv);
            }
        },

        Some(_) => eprintln!("no such command!"),
        None => eprintln!("no command given!"),
    }
}
