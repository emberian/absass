use super::*;
use super::env::Bind;
use crate::grammar::{Expr, Value, BinOp, UnOp};

impl Gen for Expr {
    fn gen(&self, cx: &mut Cx) -> Res {
        match self {
            Expr::Name(nm) => {
                let name = cx.ns.name_of(*nm).expect("unexpected unnamed variable");
                if cx.ecx == ExprCx::Read {
                    panic!("use of uninitialized name {}, var {:?}", nm, name);
                }
                let mut bk: Option<Block> = None;
                let pl = cx.env.entry(*nm).or_insert_with(|| {
                    bk = Some(
                        Block::new(name.into())
                        .before(Line::Word(0))
                    );
                    Bind { name: *nm, ty: (), place: Place::Label(name.into()) }
                }).place.clone();
                Res { block: bk, place: Some(pl) }
            },
            Expr::Literal(v) => {
                let uval = match v {
                    Value::SInt(i) => *i as usize,
                    Value::UInt(u) => *u,
                    Value::F64(_) => unimplemented!(),
                };
                let temp = cx.temp();
                let bk = cx.block()
                    .before(Insn::Transfer {
                        src: Xft {
                            reg: Place::Reg(0),
                            indirect: true,
                            mode: AutoMode::PostIncr,
                        },
                        dst: Xft {
                            reg: temp.clone(),
                            indirect: false,
                            mode: AutoMode::None,
                        },
                    }.into())
                    .before(Line::Word(uval));
                Res { block: Some(bk), place: Some(temp) }
            },
            Expr::BinOp(l, o, r) => {
                let lr = {
                    if *o == BinOp::Assign {
                        let old_ecx = cx.ecx;
                        cx.ecx = ExprCx::Write;
                        let res = l.gen(cx);
                        cx.ecx = old_ecx;
                        res
                    } else {
                        l.gen(cx)
                    }
                };
                let rr = r.gen(cx);
                let mut bk = cx.block();
                if let Some(b) = lr.block {
                    bk = bk.child(b);
                }
                if let Some(b) = rr.block {
                    bk = bk.child(b);
                }
                // Most operations modify left in place, so copy to a temp
                let temp = cx.temp();
                let rp = rr.place.expect("invalid rvalue");
                let lp = lr.place.expect("invalid lvalue");
                bk = bk
                    .load_into(cx, temp.clone(), lp.clone());
                match o {
                    BinOp::Assign => {
                        bk = bk
                            .load_into(cx, lp, rp);
                    },
                    BinOp::LOr | BinOp::Or => {
                        bk = bk
                            .after(Insn::Logic {
                                src: rp,
                                dst: temp.clone(),
                                op: LogicOp::Or,
                            }.into());
                    },
                    BinOp::LAnd | BinOp::And => {
                        bk = bk
                            .after(Insn::Logic {
                                src: rp,
                                dst: temp.clone(),
                                op: LogicOp::And,
                            }.into());
                    },
                    BinOp::Eq => {
                        bk = bk
                            .after(Insn::Compare {
                                src: rp,
                                dst: temp.clone(),
                                eq: true,
                                gt: false,
                                sn: false,
                                iv: false,
                            }.into());
                    },
                    BinOp::Neq => {
                        bk = bk
                            .after(Insn::Compare {
                                src: rp,
                                dst: temp.clone(),
                                eq: true,
                                gt: false,
                                sn: false,
                                iv: true,
                            }.into());
                    },
                    BinOp::Lt => {
                        bk = bk
                            .after(Insn::Compare {
                                src: rp,
                                dst: temp.clone(),
                                eq: true,
                                gt: true,
                                sn: false,
                                iv: true,
                            }.into());
                    },
                    BinOp::Gt => {
                        bk = bk
                            .after(Insn::Compare {
                                src: rp,
                                dst: temp.clone(),
                                eq: false,
                                gt: true,
                                sn: false,
                                iv: false,
                            }.into());
                    },
                    BinOp::Le => {
                        bk = bk
                            .after(Insn::Compare {
                                src: rp,
                                dst: temp.clone(),
                                eq: false,
                                gt: true,
                                sn: false,
                                iv: true,
                            }.into());
                    },
                    BinOp::Ge => {
                        bk = bk
                            .after(Insn::Compare {
                                src: rp,
                                dst: temp.clone(),
                                eq: true,
                                gt: true,
                                sn: false,
                                iv: false,
                            }.into());
                    },
                    BinOp::Xor => {
                        bk = bk
                            .after(Insn::Logic {
                                src: rp,
                                dst: temp.clone(),
                                op: LogicOp::XOr,
                            }.into());
                    },
                    BinOp::Lsh => {
                        bk = bk
                            .after(Insn::Arith {
                                src: rp,
                                dst: temp.clone(),
                                op: ArithOp::Shl,
                            }.into());
                    },
                    BinOp::Rsh => {
                        bk = bk
                            .after(Insn::Arith {
                                src: rp,
                                dst: temp.clone(),
                                op: ArithOp::Shr,
                            }.into());
                    },
                    BinOp::Add => {
                        bk = bk
                            .after(Insn::Arith {
                                src: rp,
                                dst: temp.clone(),
                                op: ArithOp::Add,
                            }.into());
                    },
                    BinOp::Sub => {
                        bk = bk
                            .after(Insn::Arith {
                                src: rp,
                                dst: temp.clone(),
                                op: ArithOp::Sub,
                            }.into());
                    },
                    BinOp::Mul => {
                        bk = bk
                            .after(Insn::Arith {
                                src: rp,
                                dst: temp.clone(),
                                op: ArithOp::Mul,
                            }.into());
                    },
                    BinOp::Div => {
                        bk = bk
                            .after(Insn::Arith {
                                src: rp,
                                dst: temp.clone(),
                                op: ArithOp::Div,
                            }.into());
                    },
                    BinOp::Mod => {
                        bk = bk
                            .after(Insn::Arith {
                                src: rp,
                                dst: temp.clone(),
                                op: ArithOp::Mod,
                            }.into());
                    },
                }
                Res { block: Some(bk), place: Some(temp) }
            },
            Expr::UnOp(u, e) => {
                let er = e.gen(cx);
                let mut bk = cx.block();
                if let Some(b) = er.block {
                    bk = bk.child(b);
                }
                let ep = er.place.expect("invalid unop rvalue");
                match u {
                    UnOp::Ident => (),
                    UnOp::Neg => todo!(),
                    UnOp::Not => todo!(),
                    UnOp::Cmp => {
                        bk = bk
                            .after(Insn::Logic {
                                src: Place::Reg(0),
                                dst: ep.clone(),
                                op: LogicOp::ND,
                            }.into());
                    },
                }
                Res { block: Some(bk), place: Some(ep) }
            },
        }
    }
}
