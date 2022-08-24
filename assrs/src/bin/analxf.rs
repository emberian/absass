use self::Observable::*;
use self::SymbolicValue::*;
use assrs::*;
use comfy_table::presets::UTF8_FULL;
use std::fmt::Pointer;
use std::io::prelude::*;
use std::rc::Rc;
// everything else about the interpreter is bone standard for an abstract domain.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
enum SymbolicValue {
    // start value of a register. we assume nothing about these except RegInit(x, z) == RegInit(y, w) iff x == y && z == w
    RegInit(usize, isize),

    // start value of memory. we assume nothing about these except MemInit(x) == MemInit(y) iff x == y
    MemInit(usize),
    Incr(Rc<SymbolicValue>),
    Decr(Rc<SymbolicValue>),
    MemVal(Rc<SymbolicValue>, Rc<SymbolicValue>),
    RegVal(usize, Rc<SymbolicValue>, usize),
}

impl SymbolicValue {
    fn sym_eq(&self, other: &SymbolicValue) -> bool {
        match (&*self, &*other) {
            (RegInit(r0, off1), RegInit(r1, off2)) => r0 == r1 && off1 == off2,
            (MemInit(m0), MemInit(m1)) => m0 == m1,
            (Incr(a), Incr(b)) => a.sym_eq(&b),
            (Decr(a), Decr(b)) => a.sym_eq(&b),
            (MemVal(a0, a1), MemVal(b0, b1)) => a0.sym_eq(&b0) && a1.sym_eq(&b1),
            (RegVal(r0, a, t0), RegVal(r1, b, t1)) => r0 == r1 && a.sym_eq(&b) && t0 == t1,
            (RegInit(r0, _off), RegVal(r1, guess, _t0)) => r0 == r1 && self.sym_eq(&guess),
            (RegVal(..), RegInit(..)) => other.sym_eq(self),
            (MemInit(_t0), MemVal(_addr, guess)) => self.sym_eq(&guess),
            (MemVal(..), MemInit(..)) => other.sym_eq(self),
            (MemInit(_), _) => false,
            (_, MemInit(_)) => false,
            (_, _) => false,
        }
    }
}
impl std::fmt::Debug for SymbolicValue {
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
            MemInit(v) => write!(f, "M{}", v),
            Incr(v) => write!(f, "({:?} + 1)", v),
            Decr(v) => write!(f, "({:?} - 1)", v),
            MemVal(addr, best_guess) => write!(f, "rd({:?} @ {:?})", best_guess, addr),
            RegVal(r, best_guess, t) => write!(f, "R{}({:?} @ {:?})", r, best_guess, t),
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

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
struct FinalState {
    regs: [Rc<SymbolicValue>; 16],
    memory: Vec<(Rc<SymbolicValue>, Rc<SymbolicValue>)>,
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
        Rc::new(MemVal(addr.clone(), Rc::new(MemInit(self.mems - 1))))
    }

    fn store(&mut self, addr: &Rc<SymbolicValue>, val: &Rc<SymbolicValue>) {
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
        let s = self.getreg(i.sp);
        let s_val = if let MoveMode::Decr = i.sp_mode {
            let s_dec = self.decr(&s);
            self.setreg(i.sp, &s_dec);
            s_dec
        } else {
            s.clone()
        };

        let s = if i.src_indir { self.load(&s_val) } else { s };

        match i.sp_mode {
            Incr => {
                let n = self.incr(&s);
                self.setreg(i.sp, &n)
            }
            DecrPost => {
                let n = self.decr(&s);
                self.setreg(i.sp, &n)
            }
            Decr | Direct => (),
        }

        let d_val = self.getreg(i.dl);
        let d_val = match i.dl_mode {
            Decr => self.decr(&d_val),
            _ => d_val,
        };

        if i.dst_indir {
            self.store(&d_val, &s)
        } else {
            self.setreg(i.dl, &s)
        }
        let d_val = self.getreg(i.dl);
        match i.dl_mode {
            DecrPost => self.setreg(i.dl, &self.decr(&d_val)),
            Incr => self.setreg(i.dl, &self.incr(&d_val)),
            Decr | Direct => (),
        }
    }

    fn shrink(&self, val: &Rc<SymbolicValue>) -> Rc<SymbolicValue> {
        match &**val {
            MemInit(_) | RegInit(..) => val.clone(),
            Incr(i) => self.incr(&self.shrink(i)),
            Decr(i) => self.decr(&self.shrink(i)),
            MemVal(_, guess) => self.shrink(guess),
            RegVal(_, guess, _) => self.shrink(guess),
        }
    }

    fn incorrect(&mut self, i: Instance) {
        use assrs::MoveMode::*;
        let sp = self.getreg(i.sp);
        let dl = self.getreg(i.dl);

        let sp_out = match i.sp_mode {
            Direct => sp.clone(),
            Incr => self.incr(&sp),
            DecrPost | Decr => self.decr(&sp),
        };
        let sp_in = if let Decr = i.sp_mode {
            self.decr(&sp)
        } else {
            sp.clone()
        };
        let mut dl_out = match i.dl_mode {
            Direct => dl.clone(),
            Incr => self.incr(&dl),
            DecrPost | Decr => self.decr(&dl),
        };
        let dl_in = if let Decr = i.dl_mode {
            self.decr(&dl)
        } else {
            dl.clone()
        };

        let val_to_store = if i.src_indir {
            self.load(&sp_in)
        } else {
            sp_out.clone()
        };

        if i.dst_indir {
            self.store(&dl_in, &val_to_store);
        } else {
            dl_out = match i.dl_mode {
                Direct => val_to_store.clone(),
                Incr => self.incr(&val_to_store),
                DecrPost | Decr => self.decr(&val_to_store),
            };
        }
        self.setreg(i.sp, &sp_out);
        self.setreg(i.dl, &dl_out);
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
            if !self.regs[reg].sym_eq(&other.regs[reg]) {
                writeln!(
                    msg,
                    "R{} differed (us = {:?}, them = {:?})",
                    reg, self.regs[reg], other.regs[reg],
                )
                .unwrap();
                out = false;
                break;
            }
        }

        let mut sm = self.memory.clone();
        sm.sort_by_key(|(addr, _)| addr.clone());
        let mut om = other.memory.clone();
        om.sort_by_key(|(addr, _)| addr.clone());

        for (us, them) in sm.into_iter().zip(om.into_iter()) {
            if !us.0.sym_eq(&them.0) || !us.1.sym_eq(&them.1) {
                writeln!(
                    msg,
                    "final memory state differs (us = {:?}, them = {:?})",
                    us, them
                )
                .unwrap();
                out = false;
            }
        }

        if !out {
            writeln!(msg, "our effs: {:?}\nees effs:{:?}", self, other).unwrap();
        }
        out
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
            if let RegInit(r, val) = &**reg {
                if *r != rn || *val != 0 {
                    cur_row.add_cell(Cell::new(format!("R{} = {:?}", rn, reg)));
                    i += 1;
                }
                if i == 4 {
                    let mut nr = Row::new();
                    std::mem::swap(&mut nr, &mut cur_row);
                    println!("{:?}", nr);
                    regs_tb.add_row(nr);
                    i = 0;
                }
            }
        }
        if cur_row.cell_count() != 0 {
            regs_tb.add_row(cur_row);
        }
        let mut cur_row = Row::new();
        i = 0;
        for (addr, val) in &self.memory {
            cur_row.add_cell(Cell::new(format!("Mem[{:?}] := {:?}", addr, val)));
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
#[derive(Copy, Clone)]
struct Instance {
    sp: usize,
    dl: usize,
    dst_indir: bool,
    dl_mode: MoveMode,
    src_indir: bool,
    sp_mode: MoveMode,
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
            if self.dst_indir { "*" } else { "" },
            operand(self.dl, self.dl_mode),
            if self.src_indir { "*" } else { "" },
            operand(self.sp, self.sp_mode)
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
                dst_indir,
                src_indir,
                dl_mode: dst_mode,
                sp_mode: src_mode,
                sp: src_reg,
                dl: dst_reg,
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
        tr_b.incorrect(inst);
        solutions.push((inst, tr_g.clone()));
        dissolutions.push((inst, tr_b.clone()));

        let sp_diff = !tr_g.regs[inst.sp].sym_eq(&tr_b.regs[inst.sp]);
        let dl_diff = !tr_g.regs[inst.dl].sym_eq(&tr_b.regs[inst.dl]);
        if sp_diff || dl_diff {
            bad += 1;
            writeln!(&mut s, "Generated an incorrect instance {:?}\nCorrect version computed:\n\tR{r} = {:?}\n\teffs = {:?}\n\nIncorrect version computed:\n\tR{r} = {:?}\n\teffs = {:?}\n===============================================================================\n", 
                inst,
                tr_g.shrink(&tr_g.regs[inst.sp]),
                tr_g.observe,
                tr_b.shrink(&tr_b.regs[inst.sp]),
                tr_b.observe,
                r=inst.sp,
            ).unwrap();
        } else {
            good.push((inst, tr_g.regs[inst.sp].clone()));
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
                sp_mode: smode,
                ..inst
            });
            if !sm_bhv
                .final_state()
                .equivalent(&reference_behavior.final_state(), &mut s)
            {
                bad += 1;
                writeln!(&mut s, "Generated an incorrect instance {1:?} which does not match {0:?}\n====================================================\n", 
                    inst,
                    Instance{sp_mode:smode,..inst}).unwrap();
            } else {
                good.push(inst);
            }
        }
    }

    writeln!(
        &mut s,
        "{} / {} cases were bad ({}/4 = {})",
        bad,
        bad + good.len(),
        bad,
        bad as f32 / 4f32
    )
    .unwrap();
    (String::from_utf8(s).unwrap(), bad != 20)
}

fn same_reg(i: impl Iterator<Item = Instance>) -> impl Iterator<Item = Instance> {
    i.filter(|i| i.sp == i.dl && i.sp == 7)
}

fn quotient<I: IntoIterator<Item = Instance>>(
    insts: I,
    mut model: impl FnMut(&mut Symmer, Instance),
) -> (String, bool) {
    let mut msg = vec![];
    let mut classes = std::collections::HashMap::new();
    for inst in insts {
        let mut tr = Symmer::default();
        model(&mut tr, inst);
        let final_st = tr.final_state();
        let class = classes.entry(final_st.clone()).or_insert(Vec::new());
        class.push(inst);
        assert!(final_st.equivalent(classes.get_key_value(&final_st).unwrap().0, &mut msg));
    }

    for (representative_state, instances) in &classes {
        writeln!(
            &mut msg,
            "Class with representative action\n{}\nhas {} instances:",
            representative_state,
            instances.len()
        )
        .unwrap();
        for inst in instances {
            writeln!(&mut msg, "\t{:?}", inst).unwrap();
        }
    }

    writeln!(&mut msg, "{} total classes", classes.len()).unwrap();

    (String::from_utf8(msg).unwrap(), classes.len() != 10081)
}

fn main() {
    let verbose = std::env::args().nth(1) == Some("-v".into());

    let (report, surprised) = correct_isnt_incorrect();
    if surprised || verbose {
        println!("{}", report)
    };
    let (report, surprised) = direct_dsts_ignore_src_mode_same_reg();
    if surprised || verbose {
        println!("{}", report)
    };
    let (report, surprised) = quotient(InstQuantifier::all().into_iter(), Symmer::correct);
    if surprised || verbose {
        println!("{}", report)
    };

    let (report, surprised) =
        quotient(same_reg(InstQuantifier::all().into_iter()), Symmer::correct);
    if surprised || verbose {
        println!("{}", report)
    };
    let (report, surprised) = quotient(
        same_reg(InstQuantifier::all().into_iter()),
        Symmer::incorrect,
    );
    if surprised || verbose {
        println!("{}", report)
    };
}
