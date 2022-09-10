pub const STEPPING: usize = 0;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, enum_utils::IterVariants)]
pub enum MoveMode {
    Direct,
    Incr,
    Decr,
    DecrPost,
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
pub type Word = u16;
pub type IWord = i16;
pub const WORDSZ: Word = 2;

#[derive(Debug, Hash, Clone, Copy)]
pub enum ArithOp {
    Add,
    Sub,
    Shl,
    Shr,
    Asr,
    Rol,
    Ror,
    Neg,
}
impl ArithOp {
    pub fn from_u8(val: u8) -> ArithOp {
        match val {
            0 => ArithOp::Add,
            1 => ArithOp::Sub,
            2 => ArithOp::Shl,
            3 => ArithOp::Shr,
            4 => ArithOp::Asr,
            5 => ArithOp::Rol,
            6 => ArithOp::Ror,
            7 => ArithOp::Neg,
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
            Rol => "ROL",
            Ror => "ROR",
            Neg => "NEG",
        }
    }
}
#[derive(Debug, Hash, Clone, Copy)]
pub enum Insn<Reg> {
    DistinguishedExceptionGenerator,
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
    Misc {
        op: MiscOp,
        a: Reg,
        b: Reg,
    },
    ShortBranch {
        cond: Reg,
        imm: bool,
        val: Reg,
        inv: bool,
        gt: bool,
    },
    JumpNZ {
        offset: i8,
        cond: Reg,
    },
    SubWord {
        dst: Reg,
        index: u8,
        bytes: u8,
        sign_extend: bool,
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

pub trait RegEnc: std::fmt::Display + Copy {
    fn reg(r: u16) -> Self;
    fn imm(r: u16) -> Self;
    fn enc(self) -> u16;
}
impl RegEnc for u8 {
    fn reg(r: u16) -> Self {
        r as Self
    }
    fn imm(r: u16) -> Self {
        r as Self
    }
    fn enc(self) -> u16 {
        self as u16
    }
}
impl RegEnc for usize {
    fn reg(r: u16) -> Self {
        r as Self
    }
    fn imm(r: u16) -> Self {
        r as Self
    }
    fn enc(self) -> u16 {
        self as u16
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, enum_utils::IterVariants)]
pub enum MiscOp {
    Swap,
    Mul,
    Div,
    Mod,
    LoadR,
    StoreR,
    Loop,
    LoopI,
}

impl<R: RegEnc> std::fmt::Display for Insn<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_asm())
    }
}
impl MiscOp {
    pub fn from_u8(val: u8) -> MiscOp {
        match val {
            0 => MiscOp::Swap,
            1 => MiscOp::Mul,
            2 => MiscOp::Div,
            3 => MiscOp::Mod,
            4 => MiscOp::LoadR,
            5 => MiscOp::StoreR,
            6 => MiscOp::Loop,
            7 => MiscOp::LoopI,
            _ => panic!("invalid misc op"),
        }
    }

    fn brief(&self) -> &'static str {
        use MiscOp::*;
        match self {
            Swap => "SWAP",
            Mul => "MUL",
            Div => "DIV",
            Mod => "MOD",
            LoadR => "LOADR",
            StoreR => "STORER",
            Loop => "LOOP",
            LoopI => "LOOPI",
        }
    }
}
impl<R: RegEnc> Insn<R> {
    pub fn encode(&self) -> u16 {
        match self {
            Insn::DistinguishedExceptionGenerator => 0,
            Insn::Logic { src, dst, op } => {
                dst.enc() | (src.enc() << 4) | ((*op as u16) << 8) | (0x1 << 12)
            }
            Insn::Arith { src, dst, si, op } => {
                let si = if *si { 1 << 11 } else { 0 };
                dst.enc() | (src.enc() << 4) | ((*op as u16) << 8) | si | (0x2 << 12)
            }
            Insn::Compare {
                src,
                dst,
                eq,
                sn,
                gt,
                iv,
            } => {
                dst.enc()
                    | (src.enc() << 4)
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
                dst.enc()
                    | (src.enc() << 4)
                    | (*d_deref as u16) << 10
                    | ((*d_mode as u16) << 8)
                    | (*s_deref as u16) << 13
                    | ((*s_mode as u16) << 11)
                    | (0x4 << 12)
            }
            Insn::Misc { a, b, op } => a.enc() | b.enc() << 4 | (*op as u16) << 8 | (0xe << 12),
            Insn::JumpNZ { offset, cond } => {
                (*offset as u8) as u16 | (cond.enc() << 8) | (0x8 << 12)
            }
            Insn::SubWord {
                dst,
                index,
                bytes,
                sign_extend,
            } => {
                dst.enc()
                    | ((*index as u16) << 4)
                    | ((*bytes as u16) << 8)
                    | ((*sign_extend as u16) << 11)
                    | (0x9 << 12)
            }
            Insn::ShortBranch {
                cond,
                imm,
                val,
                inv,
                gt,
            } => {
                cond.enc()
                    | val.enc() << 4
                    | (*imm as u16) << 8
                    | (*inv as u16) << 9
                    | (*gt as u16) << 10
            }
            Insn::SysReg { write, reg, sr } => {
                (0xa << 12) | ((*write as u16) << 12) | (reg.enc() << 8) | (*sr as u16)
            }
            Insn::SmallImm { dst, val } => (0xc << 12) | ((*val as u16) << 4) | (dst.enc()),
            Insn::NotSure { value } => *value,
        }
    }

    pub fn decode(val: u16) -> Insn<R> {
        match val & 0xf000 {
            0 if val == 0 => Insn::DistinguishedExceptionGenerator,
            0x1000 => {
                let src = (val & 0xf0) >> 4;
                let dst = val & 0xf;
                let op = (val & 0xf00) >> 8;
                Insn::Logic {
                    src: R::reg(src),
                    dst: R::reg(dst),
                    op: op as u8,
                }
            }
            0x2000 => {
                let src = (val & 0xf0) >> 4;
                let dst = val & 0xf;
                let op = (val & 0x700) >> 8;
                let si = val & 0x800;
                Insn::Arith {
                    src: R::reg(src),
                    dst: R::reg(dst),
                    si: si != 0,
                    op: ArithOp::from_u8(op as u8),
                }
            }
            0x3000 => {
                let src = (val & 0xf0) >> 4;
                let dst = val & 0xf;
                let eq = val & 0x100;
                let gt = val & 0x200;
                let sn = val & 0x400;
                let iv = val & 0x800;
                Insn::Compare {
                    src: R::reg(src),
                    dst: R::reg(dst),
                    eq: eq != 0,
                    sn: sn != 0,
                    gt: gt != 0,
                    iv: iv != 0,
                }
            }
            0x4000..=0x7000 => {
                let dst = val & 0xf;
                let src = (val & 0xf0) >> 4;
                let d_mode = (val >> 8) & 0x3;
                let d_deref = (val >> 10) & 0x1;
                let s_mode = (val >> 11) & 0x3;
                let s_deref = val >> 13 & 0x1;
                Insn::Move {
                    src: R::reg(src),
                    dst: R::reg(dst),
                    s_mode: MoveMode::from_u8(s_mode as u8),
                    s_deref: s_deref != 0,
                    d_mode: MoveMode::from_u8(d_mode as u8),
                    d_deref: d_deref != 0,
                }
            }
            0x8000 => {
                let offset = (val & 0xff) as i8;
                let cond = (val & 0xf00) >> 8;
                Insn::JumpNZ {
                    offset,
                    cond: R::reg(cond),
                }
            }
            0x9000 => {
                let dst = val & 0xf;
                let index = (val & 0xf0) >> 4;
                let bytes = (val & 0x700) >> 8;
                let sign_extend = (val & 0x800) != 0;
                Insn::SubWord {
                    dst: R::reg(dst),
                    index: index as u8,
                    bytes: bytes as u8,
                    sign_extend,
                }
            }
            0xe000 => match val & 0x800 {
                0 => {
                    let a = val & 0xf;
                    let b = (val & 0xf0) >> 4;
                    let op = (val & 0x700) >> 8;
                    Insn::Misc {
                        a: R::reg(a),
                        b: R::reg(b),
                        op: MiscOp::from_u8(op as u8),
                    }
                }
                1 => {
                    let cond = val & 0xf;
                    let valu = (val & 0xf0) >> 4;
                    let s = ((val >> 8) & 0x1) != 0;
                    let inv = ((val >> 9) & 0x1) != 0;
                    let gt = ((val >> 10) & 0x1) != 0;

                    Insn::ShortBranch {
                        cond: R::reg(cond),
                        val: if s { R::imm(valu) } else { R::reg(valu) },
                        inv,
                        gt,
                        imm: s,
                    }
                }
                _ => unreachable!(),
            },
            0xa000..=0xb000 => {
                let write = (val & 0x1000) >> 12;
                let reg = (val & 0xf00) >> 8;
                let sr = val & 0xff;
                Insn::SysReg {
                    write: write != 0,
                    reg: R::reg(reg),
                    sr: sr as u8,
                }
            }
            0xc000 => {
                let reg = val & 0xf;
                let imm = (val & 0x0ff0) >> 4;
                Insn::SmallImm {
                    dst: R::reg(reg),
                    val: imm as u8,
                }
            }
            _ => Insn::NotSure { value: val },
        }
    }

    pub fn brief(&self) -> Option<&'static str> {
        match self {
            Insn::DistinguishedExceptionGenerator => Some("EXC"),
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
            Insn::JumpNZ { .. } => Some("JNZ"),
            Insn::SubWord { .. } => Some("SWO"),
            Insn::ShortBranch { inv, gt, .. } => Some(match (inv, gt) {
                (false, false) => "SBR.Z",
                (true, false) => "SBR.NZ",
                (false, true) => "SBR.GZ",
                (true, true) => "SBR.LZ",
            }),
            Insn::SysReg { .. } => Some("SR"),
            Insn::SmallImm { .. } => Some("SI"),
            Insn::Misc { op, .. } => Some(op.brief()),
            Insn::NotSure { .. } => None,
        }
    }

    pub fn to_asm(&self) -> String {
        match *self {
            Insn::DistinguishedExceptionGenerator => "EXC".into(),
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
            Insn::ShortBranch { cond, imm, val, .. } => {
                format!(
                    "{} R{}, {}",
                    self.brief().unwrap(),
                    cond,
                    if imm {
                        val.enc().to_string()
                    } else {
                        format!("R{}", val)
                    }
                )
            }
            Insn::Move {
                src,
                dst,
                s_mode,
                s_deref,
                d_mode,
                d_deref,
            } => {
                fn operand(r: u16, m: MoveMode) -> String {
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
                    operand(dst.enc(), d_mode),
                    if s_deref { "*" } else { "" },
                    operand(src.enc(), s_mode)
                )
            }
            Insn::Misc { a, b, op } => format!("{} R{}, R{}", op.brief(), a, b),
            Insn::JumpNZ { cond, offset } => format!("JNZ R{}, {}", cond, offset),
            Insn::SubWord {
                dst,
                index,
                bytes,
                sign_extend,
            } => {
                format!(
                    "SWO{} R{}[{}..{}]",
                    if sign_extend { ".X" } else { "" },
                    dst,
                    index,
                    index + bytes
                )
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
                format!(
                    ".BYTE 0x{:02x}, 0x{:02x}",
                    (value & 0xff00) >> 8,
                    value & 0xff
                )
            }
        }
    }

    pub fn color(&self) -> (u8, u8, u8) {
        match self {
            Insn::DistinguishedExceptionGenerator => (42, 69, 7),
            Insn::Logic { .. } => (255, 0, 0),
            Insn::Arith { .. } => (255, 255, 0),
            Insn::Compare { .. } => (0, 255, 0),
            Insn::Move { .. } => (0, 0, 0),
            Insn::Misc { .. } => (127, 0, 0),
            Insn::ShortBranch { .. } => (69, 69, 69),
            Insn::JumpNZ { .. } => (127, 127, 0),
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
