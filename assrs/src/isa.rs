pub const STEPPING: usize = 0;

#[derive(Debug, Hash, Clone, Copy, enum_utils::IterVariants)]
pub enum MoveMode {
    Direct,
    Incr,
    DecrPost,
    Decr,
}
impl MoveMode {
    pub fn all() -> impl Iterator<Item = MoveMode> {
        MoveMode::iter()
    }
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

impl std::fmt::Display for Insn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_asm())
    }
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
            Insn::JumpLink { prog, link } => *link as u16 | ((*prog as u16) << 4) | (0x9 << 12),
            Insn::JumpCond { offset, cond } => {
                *offset as u16 | ((*cond as u16) << 8) | (0x12 << 11)
            }
            Insn::SubWord { dst, index, bytes } => {
                *dst as u16 | ((*index as u16) << 4) | ((*bytes as u16) << 8) | (0x13 << 11)
            }
            Insn::SysReg { write, reg, sr } => {
                (0xau16 << 12) | ((*write as u16) << 12) | ((*reg as u16) << 8) | (*sr as u16)
            }
            Insn::SmallImm { dst, val } => (0xc000 << 12) | ((*val as u16) << 4) | (*dst as u16),
            Insn::NotSure { value } => *value,
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
                let s_mode = (val & 0x1800) >> 11;
                let s_deref = val & 0x2000;
                let d_mode = (val & 0x300) >> 8;
                let d_deref = val & 0x400;
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
                Insn::JumpCond {
                    offset,
                    cond: cond as Reg,
                }
            }
            0x9000 => match val & 0x0800 {
                0 => {
                    let link = val & 0xf;
                    let prog = (val & 0xf0) >> 4;
                    Insn::JumpLink {
                        prog: prog as Reg,
                        link: link as Reg,
                    }
                }
                _ => {
                    let dst = val & 0xf;
                    let index = (val & 0xf) >> 4;
                    let bytes = (val & 0x700) >> 8;
                    Insn::SubWord {
                        dst: dst as Reg,
                        index: index as u8,
                        bytes: bytes as u8,
                    }
                }
            },
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
            0xc000 => {
                let reg = val & 0xf;
                let imm = (val & 0x0ff0) >> 4;
                Insn::SmallImm {
                    dst: reg as Reg,
                    val: imm as u8,
                }
            }
            _ => Insn::NotSure { value: val },
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
                (false, false, false, false) => "CMP.F1",
                (true, false, false, false) => "CMP.E1",
                (false, true, false, false) => "CMP.AB",
                (true, true, false, false) => "CMP.AE",
                (false, false, true, false) => "CMP.F2",
                (true, false, true, false) => "CMP.E2",
                (false, true, true, false) => "CMP.GT",
                (true, true, true, false) => "CMP.GE",
                (false, false, false, true) => "CMP.T1",
                (true, false, false, true) => "CMP.N1",
                (false, true, false, true) => "CMP.BE",
                (true, true, false, true) => "CMP.BL",
                (false, false, true, true) => "CMP.T2",
                (true, false, true, true) => "CMP.N2",
                (false, true, true, true) => "CMP.LT",
                (true, true, true, true) => "CMP.LE",
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

    pub fn to_asm(&self) -> String {
        match *self {
            Insn::Logic { src, dst, .. } => format!("{} R{}, R{}", self.brief().unwrap(), dst, src),
            Insn::Arith { op, src, dst, si } => {
                if !si {
                    format!("{} R{}, R{}", op.brief(), dst, src)
                } else {
                    format!("{} R{}, {}", op.brief(), dst, src)
                }
            }
            Insn::Compare { src, dst, .. } => {
                format!("{} R{}, R{}", self.brief().unwrap(), dst, src)
            }
            Insn::Move {
                src,
                dst,
                s_mode,
                s_deref,
                d_mode,
                d_deref,
            } => {
                fn operand(r: usize, m: MoveMode) -> String {
                    use MoveMode::*;
                    match m {
                        Decr => format!("--R{}", r),
                        Incr => format!("R{}++", r),
                        Direct => format!("R{}", r),
                        DecrPost => format!("R{}--", r),
                    }
                }
                format!(
                    "XF {}{}, {}{}",
                    if d_deref { "*" } else { "" },
                    operand(dst, d_mode),
                    if s_deref { "*" } else { "" },
                    operand(src, s_mode)
                )
            }
            Insn::JumpLink { prog, link } => format!("JAL R{}, R{}", prog, link),
            Insn::JumpCond { cond, offset } => format!("JC R{}, {}", cond, offset),
            Insn::SubWord { dst, index, bytes } => {
                format!("SWO R{}[{}..{}]", dst, index, index+bytes)
            }
            Insn::SysReg { write, reg, sr } => {
                if write {
                    format!("SR.W R{}, {}", reg, sr)
                } else {
                    format!("SR.R R{}, {}", reg, sr)
                }
            }
            Insn::SmallImm { dst, val } => format!("SI R{}, {}", dst, val),
            Insn::NotSure { value } => {
                format!(".BYTE 0x{:x}\n.BYTE 0x{:x}", (value & 0xff00) >> 8, value & 0xff)
            }
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

#[repr(usize)]
#[derive(Debug, Hash, Clone, Copy)]
pub enum Extension {
    Multiplier = 1,
}
