#![recursion_limit = "100000"]

use self::Observable::*;
use self::SymbolicValue::*;
use assrs::*;
use comfy_table::presets::UTF8_FULL;
use joinery::Joinable;
use std::collections::HashMap;
use std::io::prelude::*;
use std::rc::Rc;
// everything else about the interpreter is bone standard for an abstract domain.
#[derive(Clone, Eq, Hash, PartialOrd, Ord, Debug)]
enum SymbolicValue {
    // start value of a register, possibly offset. we assume nothing about these except RegInit(x, z) == RegInit(y, w) iff x == y && z == w
    RegInit(usize, isize),

    // start value of memory. we assume nothing about these except MemInit(x,z) == MemInit(y,w) iff x == y && z sym_eq w
    MemInit(usize, Rc<SymbolicValue>),
    Incr(Rc<SymbolicValue>),
    Decr(Rc<SymbolicValue>),
    MemVal(Rc<SymbolicValue>, Rc<SymbolicValue>),
    RegVal(usize, Rc<SymbolicValue>, usize),
}

impl PartialEq for SymbolicValue {
    fn eq(&self, other: &Self) -> bool {
        self.sym_eq(other)
    }
}
impl SymbolicValue {
    fn sym_eq(&self, other: &SymbolicValue) -> bool {
        self.sym_eq_msg(other, &mut Vec::new())
    }
    fn sym_eq_msg(&self, other: &SymbolicValue, msg: &mut Vec<u8>) -> bool {
        let r = match (&*self, &*other) {
            (RegInit(r0, off1), RegInit(r1, off2)) => r0 == r1 && off1 == off2,
            (MemInit(m0, iv0), MemInit(m1, iv1)) => m0 == m1 && iv0.sym_eq(iv1),
            (Incr(a), Incr(b)) => a.sym_eq(&b),
            (Decr(a), Decr(b)) => a.sym_eq(&b),
            (MemVal(a0, a1), MemVal(b0, b1)) => a0.sym_eq(&b0) && a1.sym_eq(&b1),
            (RegVal(r0, a, t0), RegVal(r1, b, t1)) => r0 == r1 && a.sym_eq(&b) && t0 == t1,
            (RegInit(r0, _off), RegVal(r1, guess, _t0)) => r0 == r1 && self.sym_eq(&guess),
            (RegVal(..), RegInit(..)) => other.sym_eq(self),
            (MemInit(..), MemVal(_addr, guess)) => todo!(),
            (MemVal(..), MemInit(..)) => other.sym_eq(self),
            (MemInit(..), _) | (_, MemInit(..)) => false,
            (_, _) => false,
        };
        if r {
            if msg.capacity() != 0 {
                writeln!(msg, "{} === {}", self, other).unwrap();
            }
        } else {
            if msg.capacity() != 0 {
                writeln!(msg, "{} =/= {}", self, other).unwrap();
            }
        }
        r
    }
}
impl std::fmt::Display for SymbolicValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*Symmer::default().shrink(&Rc::new(self.clone())) {
            RegInit(v, off) => write!(
                f,
                "R{}{}{}",
                v,
                if *off > 0 { "+" } else { "" },
                if *off != 0 {
                    off.to_string()
                } else {
                    "".into()
                }
            ),
            MemInit(v, orig) => write!(f, "M{}(*{})", v, orig),
            Incr(v) => write!(f, "({}+ 1)", v),
            Decr(v) => write!(f, "({}- 1)", v),
            MemVal(addr, best_guess) => write!(f, "rd({}@{})", best_guess, addr),
            RegVal(r, best_guess, t) => write!(f, "R{}({}@{})", r, best_guess, t),
        }
    }
}

#[derive(Debug, Clone)]
enum Observable {
    GetReg(usize),
    SetReg(usize, Rc<SymbolicValue>),
    SetMem(Rc<SymbolicValue>, Rc<SymbolicValue>),
    LoadMem(Rc<SymbolicValue>),
}

#[derive(Clone, Eq, Hash)]
struct FinalState {
    regs: [Rc<SymbolicValue>; 16],
    memory: Vec<(Rc<SymbolicValue>, Rc<SymbolicValue>)>,
}
impl PartialEq for FinalState {
    fn eq(&self, other: &Self) -> bool {
        self.equivalent(other, &mut Vec::new())
    }
}
impl FinalState {
    fn structural_eq(&self, other: &Self) -> bool {
        self.regs == other.regs && self.memory == other.memory
    }
}
#[derive(Debug, Clone)]
struct Symmer {
    regs: [Rc<SymbolicValue>; 16],
    observe: Vec<Observable>,
    mems: usize,
}
impl Symmer {
    fn load(&mut self, addr: &Rc<SymbolicValue>) -> Rc<SymbolicValue> {
        self.observe.push(LoadMem(addr.clone()));
        for eff in self.observe.iter().rev() {
            if let SetMem(old_addr, val) = eff {
                if old_addr.sym_eq(&addr) {
                    return Rc::new(MemVal(addr.clone(), val.clone()));
                }
            }
        }
        self.mems += 1;
        Rc::new(MemVal(
            addr.clone(),
            Rc::new(MemInit(self.mems - 1, addr.clone())),
        ))
    }

    fn store(&mut self, addr: &Rc<SymbolicValue>, val: &Rc<SymbolicValue>) {
        match (addr, &**val) {
            (addr, MemInit(_, startval)) if addr.sym_eq(startval) => return,
            _ => (),
        }

        self.observe.push(SetMem(addr.clone(), val.clone()));
    }

    fn incr(&self, val: &Rc<SymbolicValue>) -> Rc<SymbolicValue> {
        match &**val {
            Decr(v) => v.clone(),
            RegInit(r, off) => Rc::new(RegInit(*r, off + 1)),
            _ => Rc::new(Incr(val.clone())),
        }
    }
    fn decr(&self, val: &Rc<SymbolicValue>) -> Rc<SymbolicValue> {
        match &**val {
            Incr(v) => v.clone(),
            RegInit(r, off) => Rc::new(RegInit(*r, off - 1)),
            _ => Rc::new(Decr(val.clone())),
        }
    }
    fn setreg(&mut self, r: usize, val: &Rc<SymbolicValue>) {
        self.observe.push(SetReg(r, val.clone()));
        self.regs[r] = val.clone();
    }

    fn getreg(&mut self, r: usize) -> Rc<SymbolicValue> {
        self.observe.push(GetReg(r));
        if let RegInit(..) = *self.regs[r] {
            return self.regs[r].clone();
        }
        Rc::new(RegVal(r, self.regs[r].clone(), self.observe.len()))
    }

    fn correct(&mut self, i: Instance) {
        use assrs::MoveMode::*;
        let s = self.getreg(i.src);
        let s_val = if let MoveMode::Decr = i.s_mode {
            let s_dec = self.decr(&s);
            self.setreg(i.src, &s_dec);
            s_dec
        } else {
            s.clone()
        };

        let s = if i.s_deref { self.load(&s_val) } else { s };

        match i.s_mode {
            Incr => {
                let n = self.incr(&s);
                self.setreg(i.src, &n)
            }
            DecrPost => {
                let n = self.decr(&s);
                self.setreg(i.src, &n)
            }
            Decr | Direct => (),
        }

        let d_val = self.getreg(i.dst);
        let d_val = match i.d_mode {
            Decr => self.decr(&d_val),
            _ => d_val,
        };

        if i.d_deref {
            self.store(&d_val, &s)
        } else {
            self.setreg(i.dst, &s)
        }
        let d_val = self.getreg(i.dst);
        match i.d_mode {
            DecrPost => self.setreg(i.dst, &self.decr(&d_val)),
            Incr => self.setreg(i.dst, &self.incr(&d_val)),
            Decr | Direct => (),
        }
    }

    fn shrink(&self, val: &Rc<SymbolicValue>) -> Rc<SymbolicValue> {
        match &**val {
            MemInit(..) | RegInit(..) => val.clone(),
            Incr(i) => self.incr(&self.shrink(i)),
            Decr(i) => self.decr(&self.shrink(i)),
            MemVal(_, guess) => self.shrink(guess),
            RegVal(_, guess, _) => self.shrink(guess),
        }
    }

    fn incorrect(&mut self, i: Instance, fix: bool) {
        use assrs::MoveMode::*;
        let sp = self.getreg(i.src);
        let mut dl = self.getreg(i.dst);

        let sp_out = match i.s_mode {
            Direct => sp.clone(),
            Incr => self.incr(&sp),
            DecrPost | Decr => self.decr(&sp),
        };
        let sp_in = if let Decr = i.s_mode {
            self.decr(&sp)
        } else {
            sp.clone()
        };
        if fix {
            if i.dst == i.src {
                dl = sp_out.clone();
            }
        }
        let mut dl_out = match i.d_mode {
            Direct => dl.clone(),
            Incr => self.incr(&dl),
            DecrPost | Decr => self.decr(&dl),
        };
        let dl_in = if let Decr = i.d_mode {
            self.decr(&dl)
        } else {
            dl.clone()
        };

        let val_to_store = if i.s_deref {
            self.load(&sp_in)
        } else {
            sp_out.clone()
        };

        if i.d_deref {
            self.store(&dl_in, &val_to_store);
        } else {
            dl_out = match i.d_mode {
                Direct => val_to_store.clone(),
                Incr => self.incr(&val_to_store),
                DecrPost | Decr => self.decr(&val_to_store),
            };
        }
        self.setreg(i.src, &sp_out);
        self.setreg(i.dst, &dl_out);
    }

    fn final_state(&self) -> FinalState {
        let mut effective_writes: Vec<(Rc<_>, Rc<_>)> = Vec::new();
        for o in &self.observe {
            match o {
                SetMem(addr, val) => {
                    let addr = self.shrink(addr);
                    if !effective_writes
                        .iter()
                        .any(|(e_addr, _)| addr.sym_eq(e_addr))
                    {
                        effective_writes.push((addr.clone(), self.shrink(val)));
                    }
                }
                _ => (),
            }
        }
        FinalState {
            regs: self.regs.clone().map(|r| self.shrink(&r)),
            memory: effective_writes,
        }
    }
}

impl FinalState {
    fn equivalent(&self, other: &FinalState, msg: &mut Vec<u8>) -> bool {
        let mut out = true;

        for reg in 0..16 {
            writeln!(
                msg,
                "examine R{} (us = {}, them = {})",
                reg, self.regs[reg], other.regs[reg]
            )
            .unwrap();
            if !self.regs[reg].sym_eq_msg(&other.regs[reg], msg) {
                writeln!(
                    msg,
                    "R{} differed (us = {}, them = {})",
                    reg, self.regs[reg], other.regs[reg],
                )
                .unwrap();
                out = false;
            }
        }

        let sm: HashMap<_, _> = self.memory.clone().into_iter().collect();
        let om: HashMap<_, _> = other.memory.clone().into_iter().collect();

        for us in &sm {
            if let Some(v) = om.get(us.0) {
                if !us.1.sym_eq(v) {
                    writeln!(
                        msg,
                        "Memory at {} differed (us = {}, them = {})",
                        us.0, us.1, v,
                    )
                    .unwrap();
                    out = false;
                } else {
                    writeln!(
                        msg,
                        "examined addr {} in (them = {}, us = {}) :gucci:",
                        us.0, v, us.1
                    )
                    .unwrap();
                }
            } else {
                writeln!(msg, "Memory at {} was not present in the other state", us.0,).unwrap();
                out = false;
            }
        }
        for us in &om {
            if let Some(v) = sm.get(us.0) {
                if !us.1.sym_eq(v) {
                    writeln!(
                        msg,
                        "Memory at {} differed (them = {}, us = {})",
                        us.0, us.1, v,
                    )
                    .unwrap();
                    out = false;
                } else {
                    writeln!(
                        msg,
                        "examined addr {} in (them = {}, us = {}) :gucci:",
                        us.0, v, us.1
                    )
                    .unwrap();
                }
            } else {
                writeln!(msg, "Memory at {} was not present in the other state", us.0,).unwrap();
                out = false;
            }
        }
        if out {
            assert!(sm == om);
            writeln!(msg, "/compgood").unwrap();
        }

        if !out {
            writeln!(msg, "/compfail").unwrap();
        }
        out
    }
}

impl std::fmt::Debug for FinalState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let regs = self
            .regs
            .iter()
            .enumerate()
            .filter(|(i, r)| match &***r {
                RegInit(r, 0) if r == i => false,
                _ => true,
            })
            .collect::<Vec<_>>();
        f.debug_struct("FinalState")
            .field("regs", &regs)
            .field("memory", &self.memory)
            .finish()
    }
}
impl std::fmt::Display for FinalState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use comfy_table::*;

        let mut i = 0;
        let mut regs_tb = Table::new();
        regs_tb
            .set_content_arrangement(ContentArrangement::Dynamic)
            .load_preset(UTF8_FULL)
            .set_width(150);

        let mut cur_row = Row::new();
        for (rn, reg) in self.regs.iter().enumerate() {
            match &**reg {
                RegInit(r, val) if *r == rn && *val == 0 => {}
                _ => {
                    cur_row.add_cell(Cell::new(format!("R{} = {}", rn, reg)));
                    i += 1;
                }
            }

            if i == 4 {
                let mut nr = Row::new();
                std::mem::swap(&mut nr, &mut cur_row);
                regs_tb.add_row(nr);
                i = 0;
            }
        }
        if cur_row.cell_count() != 0 {
            regs_tb.add_row(cur_row);
        }
        let mut cur_row = Row::new();
        i = 0;
        for (addr, val) in &self.memory {
            cur_row.add_cell(Cell::new(format!("Mem[{}] := {}", addr, val)));
            i += 1;
            if i == 4 {
                let mut nr = Row::new();
                std::mem::swap(&mut nr, &mut cur_row);
                regs_tb.add_row(nr);
                i = 0;
            }
        }
        if cur_row.cell_count() != 0 {
            regs_tb.add_row(cur_row);
        }

        regs_tb.discover_columns();

        regs_tb.fmt(f)
    }
}

impl Default for Symmer {
    fn default() -> Symmer {
        Symmer {
            regs: std::array::from_fn(|i| Rc::new(RegInit(i, 0))),
            observe: vec![],
            mems: 0,
        }
    }
}
#[derive(Copy, PartialEq, Eq, Clone)]
struct Instance {
    src: usize,
    dst: usize,
    d_deref: bool,
    d_mode: MoveMode,
    s_deref: bool,
    s_mode: MoveMode,
}
impl Instance {
    fn to_inst(self) -> assrs::Insn<u8> {
        let Instance {
            src,
            dst,
            d_deref,
            d_mode,
            s_deref,
            s_mode,
        } = self;

        assrs::Insn::Move {
            src: src as u8,
            dst: dst as u8,
            s_mode,
            s_deref,
            d_mode,
            d_deref,
        }
    }
}
impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_inst().fmt(f)
    }
}
impl std::fmt::Debug for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use MoveMode::*;
        fn operand(r: usize, m: MoveMode) -> String {
            match m {
                Decr => format!("--R{}", r),
                Incr => format!("R{}++", r),
                Direct => format!("R{}", r),
                DecrPost => format!("R{}--", r),
            }
        }
        write!(
            f,
            "XF {}{}, {}{}",
            if self.d_deref { "*" } else { "" },
            operand(self.dst, self.d_mode),
            if self.s_deref { "*" } else { "" },
            operand(self.src, self.s_mode)
        )
    }
}

/// A set of instructions with various fields configured.
struct InstQuantifier {
    which_dst_indirs: Vec<bool>,
    which_src_indirs: Vec<bool>,
    which_src_modes: Vec<MoveMode>,
    which_dst_modes: Vec<MoveMode>,
    which_src_regs: Vec<usize>,
    which_dst_regs: Vec<usize>,
}
impl InstQuantifier {
    fn all() -> InstQuantifier {
        InstQuantifier {
            which_dst_indirs: vec![true, false],
            which_src_indirs: vec![true, false],
            which_src_modes: MoveMode::all().collect(),
            which_dst_modes: MoveMode::all().collect(),
            which_src_regs: (0..16).collect(),
            which_dst_regs: (0..16).collect(),
        }
    }

    fn direct_dst(self) -> Self {
        InstQuantifier {
            which_dst_indirs: vec![false],
            ..self
        }
    }
    fn direct_src(self) -> Self {
        InstQuantifier {
            which_src_indirs: vec![false],
            ..self
        }
    }
    fn no_src_move(self) -> Self {
        InstQuantifier {
            which_dst_modes: vec![MoveMode::Direct],
            ..self
        }
    }

    fn into_iter(self) -> impl Iterator<Item = Instance> {
        itertools::iproduct!(
            self.which_src_indirs,
            self.which_src_modes,
            self.which_dst_indirs,
            self.which_dst_modes,
            self.which_src_regs,
            self.which_dst_regs
        )
        .map(
            |(src_indir, src_mode, dst_indir, dst_mode, src_reg, dst_reg)| Instance {
                d_deref: dst_indir,
                s_deref: src_indir,
                d_mode: dst_mode,
                s_mode: src_mode,
                src: src_reg,
                dst: dst_reg,
            },
        )
    }
}

fn correct_isnt_incorrect() -> (String, bool) {
    let mut solutions = vec![];
    let mut dissolutions = vec![];
    let mut bad = 0;
    let mut good = vec![];
    let mut s: Vec<u8> = vec![];

    for inst in same_reg(InstQuantifier::all().into_iter()) {
        let mut tr_g = Symmer::default();
        let mut tr_b = Symmer::default();
        tr_g.correct(inst);
        tr_b.incorrect(inst, false);
        solutions.push((inst, tr_g.clone()));
        dissolutions.push((inst, tr_b.clone()));

        let sp_diff = !tr_g.regs[inst.src].sym_eq(&tr_b.regs[inst.src]);
        let dl_diff = !tr_g.regs[inst.dst].sym_eq(&tr_b.regs[inst.dst]);
        if sp_diff || dl_diff {
            bad += 1;
            writeln!(&mut s, "Generated an incorrect instance {:?}\nCorrect version computed:\n\tR{r} = {:?}\n\teffs = {:?}\n\nIncorrect version computed:\n\tR{r} = {:?}\n\teffs = {:?}\n===============================================================================\n", 
                inst,
                tr_g.shrink(&tr_g.regs[inst.src]),
                tr_g.observe,
                tr_b.shrink(&tr_b.regs[inst.src]),
                tr_b.observe,
                r=inst.src,
            ).unwrap();
        } else {
            good.push((inst, tr_g.regs[inst.src].clone()));
        }
    }

    for g in &good {
        writeln!(&mut s, "{:?} agreed: {:?} ", g.0, g.1).unwrap();
    }

    writeln!(&mut s, "{} / {} cases were bad", bad, bad + good.len()).unwrap();
    (String::from_utf8(s).unwrap(), bad != 38)
}

fn direct_dsts_ignore_src_mode_same_reg() -> (String, bool) {
    let mut bad = 0;
    let mut good = vec![];
    let mut s: Vec<u8> = vec![];

    for inst in same_reg(InstQuantifier::all().direct_dst().direct_src().into_iter()) {
        let mut reference_behavior = Symmer::default();
        reference_behavior.correct(inst);

        for smode in MoveMode::all() {
            let mut sm_bhv = Symmer::default();
            sm_bhv.correct(Instance {
                s_mode: smode,
                ..inst
            });
            let mut subs = vec![];
            if !sm_bhv
                .final_state()
                .equivalent(&reference_behavior.final_state(), &mut subs)
            {
                bad += 1;
                writeln!(&mut s, "Generated an incorrect instance {1:?} which does not match {0:?} because\n{2:?}\n====================================================\n", 
                    inst,
                    Instance{s_mode:smode,..inst}, String::from_utf8_lossy(&subs)).unwrap();
            } else {
                good.push(inst);
            }
        }
    }

    writeln!(
        &mut s,
        "{} / {} cases were bad ({} extra encodings)",
        bad,
        bad + good.len(),
        (bad as f32 + good.len() as f32).log2() - 1.
    )
    .unwrap();
    (String::from_utf8(s).unwrap(), bad != 20)
}

fn same_reg(i: impl Iterator<Item = Instance>) -> impl Iterator<Item = Instance> {
    i.filter(|i| i.src == i.dst && i.src == 7)
}

fn quotient<I: IntoIterator<Item = Instance>>(
    insts: I,
    mut model: impl FnMut(&mut Symmer, Instance),
    desc: impl Into<String>,
    expect_classes: usize,
) -> QuotientReport {
    let mut classes = HashMap::new();
    for inst in insts {
        let mut tr = Symmer::default();
        model(&mut tr, inst);
        let final_st = tr.final_state();

        let class = classes.entry(final_st.clone()).or_insert(Vec::new());
        class.push(inst);
    }

    // shrink classes
    println!("shrink classes");
    let mut msg = vec![];
    let mut new_classes: HashMap<FinalState, Vec<Instance>> = HashMap::new();
    'outer: for (rep, insts) in &classes {
        for (new_rep, new_insts) in &mut new_classes {
            let mut submsg = vec![];
            if rep.equivalent(new_rep, &mut submsg) {
                writeln!(msg, "WARN WARN WARN equivalence isn't! \n{:?} \nand\n{:?}\n claim to be equivalent but were different classes ({:?} and {:?}). Reason:\n{}",
                rep,new_rep, insts.clone().join_with(";").to_string(), new_insts.clone().join_with(";").to_string(), String::from_utf8_lossy(&submsg)).unwrap();
                new_insts.extend(insts);
                continue 'outer;
            }
        }
        new_classes.insert(rep.clone(), insts.clone());
    }

    assert!(new_classes == classes);

    for (representative_state, instances) in &new_classes {
        writeln!(
            &mut msg,
            "Class with representative action\n{0:?}\n{0}\nhas {1} instances:",
            representative_state,
            instances.len()
        )
        .unwrap();
        for inst in instances {
            writeln!(&mut msg, "\t{:?}", inst).unwrap();
        }
    }

    writeln!(&mut msg, "{} total classes", new_classes.len()).unwrap();

    QuotientReport {
        convenient_classes: new_classes
            .iter()
            .map(|(r, i)| {
                (
                    i.iter()
                        .map(|i| ((i.to_inst().encode() & 0x3f) >> 8) as u8)
                        .collect(),
                    format!("{}", r),
                )
            })
            .collect(),
        surprised: new_classes.len() != expect_classes,

        rich_classes: new_classes,
        log: String::from_utf8_lossy(&msg).into(),
        desc: desc.into(),
    }
}

struct QuotientReport {
    convenient_classes: Vec<(Vec<u8>, String)>,
    rich_classes: HashMap<FinalState, Vec<Instance>>,
    desc: String,
    log: String,
    surprised: bool,
}

impl std::fmt::Display for QuotientReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.surprised {
            writeln!(f, "WARNING: {}: {} classes were generated\n{}", self.desc, self.rich_classes.len(), self.log)
        } else {
            writeln!(f, "{}: {} classes generated, as expected.", self.desc, self.rich_classes.len())
        }
    }
}
/*
fn html_quotient(q: QuotientReport) -> String {
    use typed_html::{dom::DOMTree, *};
    let revix = q
        .convenient_classes
        .iter()
        .map(|(c, s)| c.iter().map(|c| (*c, s.clone())))
        .flatten()
        .collect::<HashMap<_, _>>();
    let mut doc: DOMTree<String> = html! {
        <html>
            <head><title>"ANAL XF REPORT"</title></head>
            <body>
            <h1>"ANAL XF REPORT"</h1>
            <p>{text!("This report represents {} equivalences classes of all autoautomodifying XF instructions according to their semantics in the {} model",
        q.convenient_classes.len(), q.desc)}</p>

            <table>
            <tr><th colspan=4>"Direct"</th><th colspan=4>"Indirect"</th></tr>
            <tr><th>"R"</th><th>"R+"</th><th>"R-"</th><th>"-R"</th><th>"R"</th><th>"R+"</th><th>"R-"</th><th>"-R"</th></tr>
            { (0..8).map(|rowctr|

            html!{<tr>
                {
                (0..8).map(|colctr| {
                    html!{<td>{revix.get(&((rowctr as u8) << 3 | (colctr as u8))).unwrap_or(&"missing".into())}</td>}
            }).chain(vec!(html!{<td colspan="4">"Direct"</td>},html!{<td colspan="4">"Indirect"</td>})

            )}</tr> }

        )} </table>
        </body>
        </html>
    };
    doc.to_string()
}
*/
fn main() {
    let verbose = std::env::args().nth(1) == Some("-v".into());

    let (report, surprised) = correct_isnt_incorrect();
    if surprised || verbose {
        println!("{}", report);
        println!("/correct isnt incorrect");
    };
    let (report, surprised) = direct_dsts_ignore_src_mode_same_reg();
    if surprised || verbose {
        println!("{}", report);
        println!("/direct dsts ignore");
    };
    //let (report, surprised) = quotient(InstQuantifier::all().into_iter(), Symmer::correct, 12305);
    if surprised || verbose {
        //println!("{}", report)
    };

    let rep = quotient(
        same_reg(InstQuantifier::all().into_iter()),
        Symmer::correct,
        "correct",
        0,
    );
    if rep.surprised || verbose {
        println!("{}", rep);
        println!("/correct");
    };
    let rep = quotient(
        same_reg(InstQuantifier::all().into_iter()),
        |sym, i| sym.incorrect(i, false),
        "unfixed incorrect",
        0,
    );
    if rep.surprised || verbose {
        println!("{}", rep);
        println!("/incorrect unfixed");
    };
    let rep = quotient(
        same_reg(InstQuantifier::all().into_iter()),
        |sym, i| sym.incorrect(i, true),
        "fixed incorrect",
        0,
    );
    if rep.surprised || verbose {
        println!("{}", rep);
        println!("/incorrect fixed");
    };
}

