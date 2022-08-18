#![feature(mixed_integer_ops)]

use std::io::{BufRead, Read};

pub const STEPPING: usize = 0;

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
pub const WORDSZ: Word = 8;

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

    pub fn brief(&self) -> &'static str {
        use ArithOp::*;
        match self {
            Add => "ADD",
            Sub => "SUB",
            Shl => "SHL",
            Shr => "SHR",
            Asr => "ASR",
            Mul => "MUL",
            Div => "DIV",
            Mod => "MOD",
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
        si: bool,
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
    JumpLink {
        prog: Reg,
        link: Reg,
    },
    JumpCond {
        offset: i8,
        cond: Reg,
    },
    SubWord {
        dst: Reg,
        index: u8,
        bytes: u8,
    },
    SysReg {
        write: bool,
        reg: Reg,
        sr: u8,
    },
    SmallImm {
        dst: Reg,
        val: u8,
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
            Insn::Arith { src, dst, si, op } => {
                let si = if *si { 1 << 11 } else { 0 };
                *dst as u16 | ((*src as u16) << 4) | ((*op as u16) << 8) | si | (0x2 << 12)
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
            Insn::JumpLink {
                prog,
                link,
            } => {
                *link as u16
                    | ((*prog as u16) << 4)
                    | (0x9 << 12)
            }
            Insn::JumpCond {
                offset,
                cond,
            } => {
                *offset as u16
                    | ((*cond as u16) << 8)
                    | (0x12 << 11)
            }
            Insn::SubWord {
                dst,
                index,
                bytes
            } => {
                *dst as u16
                    | ((*index as u16) << 4)
                    | ((*bytes as u16) << 8)
                    | (0x13 << 11)
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
            },
            Insn::SmallImm { dst, val } => {
                (0xc000 << 12) | ((*val as u16) << 4) | (*dst as u16)
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
                let op = (val & 0x700) >> 8;
                let si = val & 0x800;
                Insn::Arith {
                    src: src as Reg,
                    dst: dst as Reg,
                    si: si != 0,
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
            0x8000 => {
                let offset = (val & 0xff) as i8;
                let cond = (val & 0xf00) >> 8;
                Insn::JumpCond { offset, cond: cond as Reg }
            }
            0x9000 => match val & 0x0800 {
                0 => {
                    let link = val & 0xf;
                    let prog = (val & 0xf0) >> 4;
                    Insn::JumpLink { prog: prog as Reg, link: link as Reg }
                },
                _ => {
                    let dst = val & 0xf;
                    let index = (val & 0xf) >> 4;
                    let bytes = (val & 0x700) >> 8;
                    Insn::SubWord {
                        dst: dst as Reg,
                        index: index as u8,
                        bytes: bytes as u8,
                    }
                },
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
            },
            0xc000 => {
                let reg = val & 0xf;
                let imm = (val & 0x0ff0) >> 4;
                Insn::SmallImm {
                    dst: reg as Reg,
                    val: imm as u8,
                }
            }
            x => Insn::NotSure { value: x },
        }
    }

    pub fn brief(&self) -> Option<&'static str> {
        match self {
            Insn::Logic { op, .. } => Some(match op {
                0b0000 => "F",
                0b0001 => "NOR",
                0b0010 => "NCI",
                0b0011 => "ND",
                0b0100 => "NI",
                0b0101 => "NS",
                0b0110 => "XOR",
                0b0111 => "NAN",
                0b1000 => "AND",
                0b1001 => "XNR",
                0b1010 => "S",
                0b1011 => "IMP",
                0b1100 => "D",
                0b1101 => "CI",
                0b1110 => "OR",
                0b1111 => "T",
                _ => "??",
            }),
            Insn::Arith { op, .. } => Some(op.brief()),
            Insn::Compare { eq, gt, sn, iv, .. } => Some(match (eq, gt, sn, iv) {
                (false, false, false, false) => ".F1",
                (true, false, false, false) => ".E1",
                (false, true, false, false) => ".AB",
                (true, true, false, false) => ".AE",
                (false, false, true, false) => ".F2",
                (true, false, true, false) => ".E2",
                (false, true, true, false) => ".GT",
                (true, true, true, false) => ".GE",
                (false, false, false, true) => ".T1",
                (true, false, false, true) => ".N1",
                (false, true, false, true) => ".BE",
                (true, true, false, true) => ".BL",
                (false, false, true, true) => ".T2",
                (true, false, true, true) => ".N2",
                (false, true, true, true) => ".LT",
                (true, true, true, true) => ".LE",
            }),
            Insn::Move { .. } => Some("XF"),
            Insn::JumpLink { .. } => Some("JL"),
            Insn::JumpCond { .. } => Some("JC"),
            Insn::SubWord { .. } => Some("SWO"),
            Insn::SysReg { .. } => Some("SR"),
            Insn::SmallImm { .. } => Some("SI"),
            Insn::NotSure { .. } => None,
        }
    }

    pub fn color(&self) -> (u8, u8, u8) {
        match self {
            Insn::Logic { .. } => (255, 0, 0),
            Insn::Arith { .. } => (255, 255, 0),
            Insn::Compare { .. } => (0, 255, 0),
            Insn::Move { .. } => (0, 0, 0),
            Insn::JumpLink { .. } => (127, 0, 0),
            Insn::JumpCond { .. } => (127, 127, 0),
            Insn::SubWord { .. } => (127, 0, 127),
            Insn::SysReg { .. } => (255, 0, 255),
            Insn::SmallImm { .. } => (0, 255, 255),
            Insn::NotSure { .. } => (255, 255, 255),
        }
    }
}

#[derive(Default)]
pub struct Machine {
    regs: [Word; 16],
    memory: Vec<u8>,
    ivt: Reg,
    cycles: Word,
    insns: Word,
}

#[derive(Debug, Hash, Clone, Copy)]
pub enum StepOut {
    Continue,
    Halt,
}

#[repr(usize)]
#[derive(Debug, Hash, Clone, Copy)]
pub enum Extension {
    Multiplier = 1,
}

impl Machine {
    pub const FREQ: Word = 0xa55;

    pub fn pc(&self) -> usize { self.regs[0] as usize }

    pub fn step(&mut self, i: Insn) -> StepOut {
        use StepOut::*;
        println!("exec {:?}", i);

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

                self.cycles += 1;
                self.insns += 1;
                return Continue;
            }
            Insn::JumpLink {
                link, prog
            } => {
                self.regs[link] = pc as Word;
                pc = self.regs[prog] as usize;
            }
            Insn::JumpCond {
                offset,
                cond,
            } => {
                if self.regs[cond] != 0 {
                    pc = pc.checked_add_signed(offset as isize).unwrap();
                }
            }
            Insn::SubWord {
                dst,
                index,
                bytes,
            } => {
                let b = if bytes == 0 { 8 } else { bytes };
                let m = (1 << (8 * b)) - 1;
                let s = 8 * index;
                self.regs[dst] =
                    (self.regs[dst] & (m << s)) >> s;
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
                    4 => if write {
                        self.insns = self.regs[reg];
                    } else {
                        self.regs[reg] = self.insns;
                    }
                    5 => if write {
                        self.cycles = self.regs[reg];
                    } else {
                        self.regs[reg] = self.cycles;
                    }
                    6 => if !write { self.regs[reg] = Self::FREQ; }
                    7 => if !write { self.regs[reg] = Extension::Multiplier as Word; }
                    10 => if write { println!("{}", char::from_u32(self.regs[reg] as u32).unwrap_or('?')); }
                    _ => ()
                }
            }
            Insn::SmallImm { dst, val } => {
                self.regs[dst] = val as Word;
            }
            Insn::NotSure {
                value,
            } => {
                panic!("tried to execute unknown instruction {:?}", value);
            }
        }

        self.regs[0] = pc as Word;
        self.cycles += 1;
        self.insns += 1;
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

pub fn main() {
    match std::env::args().nth(1).as_deref() {
        Some("test") => {
            let mut m = Machine::default();
            m.memory = vec![0u8; 0x10000];
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
            let mut m = Machine::default();
            if let Some(filename) = std::env::args().nth(2) {
                let mut rdr = std::fs::File::open(filename).expect("opening program file");
                rdr.read_to_end(&mut m.memory).unwrap();
            } else {
                std::io::stdin().read_to_end(&mut m.memory).expect("reading program file");
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

        Some("gen_opcode") => {
            match std::env::args().nth(2).as_deref() {
                Some("html") => {
                    println!("<html>");
                    println!("<head><style type=\"text/css\">.valid {{ background-color: #0f03; }} table {{ border-collapse: collapse; }} table td {{ border: 1px solid #000; }}</style></head>");
                    println!("<body><table><tr>");
                    for insn in 0 ..= std::u16::MAX {
                        if insn != 0 && insn % 256 == 0 {
                            println!("</tr></tr>")
                        }
                        let ins = Insn::decode(insn);
                        match ins.brief() {
                            None => println!("<td></td><!--{:04x}-->", insn),
                            Some(b) => println!("<td class=\"valid\">{}</td><!--{:04x}-->", b, insn),
                        }
                    }
                    println!("</tr></table></body></html>");
                },
                Some("ppm") => {
                    println!("P3");
                    println!("256 256 255");
                    for insn in 0 ..= std::u16::MAX {
                        let insn = Insn::decode(insn);
                        let col = insn.color();
                        println!("{} {} {}", col.0, col.1, col.2);
                    }
                },
                Some(_) => {
                    eprintln!("bad format; try html, ppm");
                },
                None => {
                    eprintln!("no format argument");
                },
            }
        }

        Some(_) => eprintln!("no such command!"),
        None => eprintln!("no command given!"),
    }
}
