pub trait ToAsm {
    fn to_asm(&self) -> String;
}

pub trait ToCode {
    fn to_code(&self) -> Option<u16>;
}

#[EnumRepr(type = "u8", implicit = true)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogicOp {
    False,
    NOr,
    NConvImp,
    ND,
    NImp,
    NS,
    XOr,
    NAnd,
    And,
    XNOr,
    S,
    Imp,
    D,
    ConvImp,
    Or,
    True,
}

impl LogicOp {
    pub fn uses_src(&self) -> bool {
        match self {
            LogicOp::D | LogicOp::ND | LogicOp::False | LogicOp::True => false,
            _ => true,
        }
    }

    pub fn reads_dst(&self) -> bool {
        match self {
            LogicOp::S | LogicOp::NS | LogicOp::False | LogicOp::True => false,
            _ => true,
        }
    }
}

#[EnumRepr(type = "u8", implicit = true)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl ToAsm for ArithOp {
    fn to_asm(&self) -> String {
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
        }.into()
    }
}

#[EnumRepr(type = "u8", implicit = true)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AutoMode {
    None,
    PostIncr,
    PreDecr,
    PostDecr,
}

impl AutoMode {
    pub fn decorate(&self, reg: &str) -> String {
        match self {
            AutoMode::None => reg.into(),
            AutoMode::PostIncr => format!("{}+", reg),
            AutoMode::PreDecr => format!("-{}", reg),
            AutoMode::PostDecr => format!("{}-", reg),
        }
    }

    pub fn modifies(&self) -> bool {
        *self != AutoMode::None
    }
}

pub type Reg = usize;
pub type SysReg = usize;
pub type Temp = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Place {
    Reg(Reg),
    Temp(Temp),
    Label(String),
}

impl ToAsm for Place {
    fn to_asm(&self) -> String {
        match self {
            Place::Reg(r) => format!("R{}", r),
            // Not valid, but useful for debugging
            Place::Temp(t) => format!("%{}", t),
            Place::Label(s) => s.clone(),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Xft {
    pub reg: Place,
    pub indirect: bool,
    pub mode: AutoMode,
}

impl ToAsm for Xft {
    fn to_asm(&self) -> String {
        format!("{}{}",
                if self.indirect { "*" } else { "" },
                self.mode.decorate(&self.reg.to_asm())
        )
    }
}

#[derive(Debug, Clone, Hash)]
pub enum Insn {
    Logic { src: Place, dst: Place, op: LogicOp },
    Arith { src: Place, dst: Place, op: ArithOp },
    // This opcode sharing makes more sense than, say, adding an Imm variant to Place (since that's
    // not valid in most places), or adding a boolean to Arith (which, depending on a Reg as an
    // immediate, is quite hacky).
    ArithImm { dst: Place, imm: u8, op: ArithOp },
    Compare { src: Place, dst: Place, eq: bool, gt: bool, sn: bool, iv: bool },
    Transfer { src: Xft, dst: Xft },
    SysReg { reg: Place, sr: SysReg, write: bool },
    JumpCond { reg: Place, offset: i8 },
    JumpLink { prog: Place, link: Place },
    SubWord { dst: Place, byte_ix: u8, bytes: u8 },
    Unknown(u16),
}

impl ToAsm for Insn {
    fn to_asm(&self) -> String {
        use Insn::*;
        match self {
            Logic { src, dst, op } => match op {
                // Special forms: ZERO and ONE
                LogicOp::False => format!("ZERO {}", dst.to_asm()),
                LogicOp::True => format!("ONE {}", dst.to_asm()),
                // MOV
                LogicOp::S => format!("MOV {}, {}", dst.to_asm(), src.to_asm()),
                // NOT
                LogicOp::ND => format!("NOT {}", dst.to_asm()),
                // Standard bitwise
                LogicOp::And => format!("AND {}, {}", dst.to_asm(), src.to_asm()),
                LogicOp::Or => format!("OR {}, {}", dst.to_asm(), src.to_asm()),
                LogicOp::XOr => format!("XOR {}, {}", dst.to_asm(), src.to_asm()),
                LogicOp::NAnd => format!("NAND {}, {}", dst.to_asm(), src.to_asm()),
                LogicOp::NOr => format!("NOR {}, {}", dst.to_asm(), src.to_asm()),
                LogicOp::XNOr => format!("XNOR {}, {}", dst.to_asm(), src.to_asm()),
                // Everything else
                op => format!("BIT {}, {}, {}", dst.to_asm(), src.to_asm(), op.repr()),
            },
            Arith { src, dst, op } => format!("{} {}, {}", op.to_asm(), dst.to_asm(), src.to_asm()),
            ArithImm { imm, dst, op } => format!("{} {}, ${}", op.to_asm(), dst.to_asm(), imm),
            Compare { src, dst, eq, gt, sn, iv } => format!("CMP.{}{}{}{} {}, {}",
                                                            if *eq { "E" } else { "" },
                                                            if *gt { "G" } else { "" },
                                                            if *sn { "S" } else { "" },
                                                            if *iv { "I" } else { "" },
                                                            dst.to_asm(),
                                                            src.to_asm()
            ),
            Transfer { src, dst } => format!("XF {}, {}", dst.to_asm(), src.to_asm()),
            SysReg { reg, sr, write } => format!("SR {}, {}, {}", if *write { "W" } else { "R" }, reg.to_asm(), sr),
            JumpCond { reg, offset } => format!("JC {}, {}", reg.to_asm(), offset),
            JumpLink { prog, link } => format!("JAL {}, {}", link.to_asm(), prog.to_asm()),
            SubWord { dst, byte_ix, bytes } => format!("SWO {}, {}, {}", dst.to_asm(), byte_ix, bytes),
            Unknown(i) => format!(".BYTE {}, {}", (i >> 8) & 0xff, i & 0xff),
        }
    }
}
