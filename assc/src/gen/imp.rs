use super::*;
use super::env::Bind;
use super::rall::PC;
use crate::grammar::{Expr, Value, BinOp, UnOp, Stmt, Binding};

impl Gen for Expr {
    fn gen(&self, cx: &mut Cx) -> Res {
        match self {
            Expr::Name(nm) => {
                let name = cx.gcx.var_ns.name_of(*nm).expect("unexpected unnamed variable");
                if !cx.env.contains_key(nm) && cx.ecx == ExprCx::Read {
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
                            reg: Place::Reg(PC),
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
                    if let BinOp::Assign = o {
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
                if let BinOp::Assign = o {
                    return Res {
                        block: Some(bk.load_into(cx, lp, rp.clone())),
                        place: Some(rp),
                    };
                }
                bk = bk
                    .load_into(cx, temp.clone(), lp.clone());
                match o {
                    BinOp::Assign => unreachable!(),

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
                            .after(Insn::Misc {
                                b: rp,
                                a: temp.clone(),
                                op: MiscOp::Mul,
                            }.into());
                    },
                    BinOp::Div => {
                        bk = bk
                            .after(Insn::Misc {
                                b: rp,
                                a: temp.clone(),
                                op: MiscOp::Div,
                            }.into());
                    },
                    BinOp::Mod => {
                        bk = bk
                            .after(Insn::Misc {
                                b: rp,
                                a: temp.clone(),
                                op: MiscOp::Mod,
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
            Expr::Branch(cond, ift, iff) => {
                let condr = cond.gen(cx);
                let (condb, condp) = (
                    condr.block,
                    condr.place.expect("invalid cond place"),
                );
                let iftr = ift.gen(cx);
                let (mut iftb, iftp) = (
                    iftr.block.unwrap_or_else(|| cx.block()),
                    iftr.place,
                );
                let iffr = iff.gen(cx);
                let (mut iffb, iffp) = (
                    iffr.block.unwrap_or_else(|| cx.block()),
                    iffr.place,
                );
                let mut rp: Option<Place> = None;
                let mut whole = cx.block();
                let mut test = condb.unwrap_or_else(|| cx.block());
                test = test
                    .after(Insn::JumpCond {
                        reg: condp,
                        offset: Offset::Expr(format!("{} - $ - 2", iftb.label)),
                    }.into())
                    .load_into(cx,
                               Place::Reg(PC),
                               Place::Label(iffb.label.clone())
                    );
                let end = cx.block();
                match (iftp, iffp) {
                    (Some(tp), Some(fp)) => {
                        let res = cx.temp();
                        iftb = iftb.load_into(cx, res.clone(), tp);
                        iffb = iffb.load_into(cx, res.clone(), fp);
                        rp = Some(res);
                    },
                    (None, None) => (),
                    _ => panic!("cannot unify result/non-result branches"),
                }
                whole = whole
                    .child(test)
                    .child(iftb
                           .load_into(cx, Place::Reg(PC), Place::Label(end.label.clone())))
                    .child(iffb)
                    .child(end);
                Res { block: Some(whole), place: rp }
            },
            Expr::Block(stmts, ex) => {
                let mut bk = cx.block();
                for stmt in &stmts.0[..] {
                    let res = stmt.gen(cx);
                    if let Some(b) = res.block {
                        bk = bk.child(b);
                    }
                }
                let exr = ex.gen(cx);
                if let Some(b) = exr.block {
                    bk = bk.child(b);
                }
                Res { block: Some(bk), place: exr.place }
            },
            Expr::Call(cbl, args) => {
                let cblr = cbl.gen(cx);
                let mut cacocx = { cx.caco.clone() }.begin(cx);
                for arg in args {
                    let argr = arg.gen(cx);
                    cacocx.arg(argr, cx);
                }
                cacocx.end(cblr, cx)
            },
            Expr::Null => {
                Res { block: None, place: None }
            },
        }
    }
}

impl Gen for Stmt {
    fn gen(&self, cx: &mut Cx) -> Res {
        match self {
            Stmt::Expr(e) => {
                let exr = e.gen(cx);
                Res { block: exr.block, place: None }
            },
            Stmt::Decl(ty, bnds) => {
                // TODO: type sizes
                let mut bk = cx.block();
                // Storage first...
                for Binding(nm, _) in bnds {
                    bk = bk
                        .before(
                            Line::Label(cx.gcx.var_ns.name_of(*nm).expect("unexpected unnamed variable").to_string())
                        )
                        .before(
                            Line::Word(0)
                        );
                }
                // ... init after
                for Binding(nm, init) in bnds {
                    if let Some(ex) = init {
                        let exr = ex.gen(cx);
                        if let Some(b) = exr.block {
                            bk = bk
                                .child(b
                                       .load_into(cx,
                                                  Place::Label(cx.gcx.var_ns.name_of(*nm).expect("unexpected unnamed variable").to_string()),
                                                  exr.place.expect("invalid initializer")
                                        )
                                       );
                        }
                    }
                }
                Res { block: Some(bk), place: None }
            }
        }
    }
}

impl Gen for Stmts {
    fn gen(&self, cx: &mut Cx) -> Res {
        let mut bk = cx.block();
        for st in &self.0[..] {
            let sr = st.gen(cx);
            if let Some(b) = sr.block {
                bk = bk.child(b);
            }
        }
        Res { block: Some(bk), place: None }
    }
}
