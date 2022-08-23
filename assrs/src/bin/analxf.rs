use self::Observable::*;
use self::SymbolicValue::*;
use assrs::*;
use std::rc::Rc;

// everything else about the interpreter is bone standard for an abstract domain.
enum SymbolicValue {
    // start value of a register. we assume nothing about these except RegInit(x) == RegInit(y) iff x == y
    RegInit(usize),

    // start value of memory. we assume nothing about these except MemInit(x) == MemInit(y) iff x == y
    MemInit(usize),
    Incr(Rc<SymbolicValue>),
    Decr(Rc<SymbolicValue>),
    MemVal(Rc<SymbolicValue>, Rc<SymbolicValue>),
    RegVal(usize, Rc<SymbolicValue>, usize),
}

impl std::fmt::Debug for SymbolicValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RegInit(v) => write!(f, "R{}", v),
            MemInit(v) => write!(f, "M{}", v),
            Incr(v) => write!(f, "({:?} + 1)", v),
            Decr(v) => write!(f, "({:?} - 1)", v),
            MemVal(addr, best_guess) => write!(f, "rd({:?} @ {:?})", best_guess, addr),
            RegVal(r, best_guess, t) => write!(f, "R{}({:?} @ {:?})", r, best_guess, t),
        }
    }
}

#[derive(Debug,Clone)]
enum Observable {
    GetReg(usize),
    SetReg(usize),
    SetMem(Rc<SymbolicValue>, Rc<SymbolicValue>),
    LoadMem(Rc<SymbolicValue>),
}
#[derive(Debug,Clone)]
struct Trace {
    regs: [Rc<SymbolicValue>; 16],
    effs: Vec<Observable>,
    mems: usize,
}
impl Trace {
    fn load(&mut self, addr: &Rc<SymbolicValue>) -> Rc<SymbolicValue> {
        self.effs.push(LoadMem(addr.clone()));
        for eff in self.effs.iter().rev() {
            if let SetMem(old_addr, val) = eff {
                if self.sym_eq(old_addr, addr) {
                    return Rc::new(MemVal(addr.clone(), val.clone()));
                }
            }
        }
        self.mems += 1;
        Rc::new(MemVal(addr.clone(), Rc::new(MemInit(self.mems-1))))
    }

    fn store(&mut self, addr: &Rc<SymbolicValue>, val: &Rc<SymbolicValue>) {
        self.effs.push(SetMem(addr.clone(), val.clone()));
    }

    fn incr(&self, val: &Rc<SymbolicValue>) -> Rc<SymbolicValue> {
        match &**val {
            Decr(v) => v.clone(),
            _ => Rc::new(Incr(val.clone())),
        }
    }
    fn decr(&self, val: &Rc<SymbolicValue>) -> Rc<SymbolicValue> {
        match &**val {
            Incr(v) => v.clone(),
            _ => Rc::new(Decr(val.clone())),
        }
    }
    fn setreg(&mut self, r: usize, val: &Rc<SymbolicValue>) {
        self.effs.push(SetReg(r));
        self.regs[r] = val.clone();
    }

    fn getreg(&mut self, r: usize) -> Rc<SymbolicValue> {
        self.effs.push(GetReg(r));
        if let RegInit(_) = *self.regs[r] {
            return self.regs[r].clone()
        }
        Rc::new(RegVal(r, self.regs[r].clone(), self.effs.len()))
    }

    fn correct(&mut self, i: Instance) {
        use assrs::MoveMode::*;
        let sp = self.getreg(i.sp);
        let presp = match i.sp_mode {
            Decr => self.decr(&sp),
            _ => sp.clone(),
        };

        let val_to_store = if i.src_indir {
            self.load(&presp)
        } else {
            sp.clone()
        };

        match i.sp_mode {
            Incr => {
                let n = self.incr(&presp);
                self.setreg(i.sp, &n)

            },
            DecrPost => {
                let n = self.decr(&presp);
                self.setreg(i.sp, &n)
            }
            Decr|Direct => (),
        }

        let dl = self.getreg(i.dl);
        let predl = match i.dl_mode {
            Decr => self.decr( &dl),
            _ => dl.clone(),
        };

        if i.dst_indir {
            self.store(&predl, &val_to_store)
        } else {
            self.setreg(i.dl, &val_to_store)
        }
        match i.dl_mode {
            DecrPost => {
                self.setreg(i.dl, &self.decr( &dl))
            }
            Incr => { self.setreg(i.dl, &self.incr(&dl)) },
            Decr | Direct => (),
        }
    }

    fn shrink(&self, val: &Rc<SymbolicValue>) -> Rc<SymbolicValue> {
        match &**val {
            MemInit(_) | RegInit(_) => val.clone(),
            Incr(i) => self.incr(&self.shrink(i)),
            Decr(i) => self.decr(&self.shrink(i)),
            MemVal(_, guess) => self.shrink(guess),
            RegVal(_, guess, _) => self.shrink(guess),
        }
    }

    fn sym_eq(&self, a: &Rc<SymbolicValue>, b: &Rc<SymbolicValue>) -> bool {
        match (&*self.shrink(a), &*self.shrink(b)) {
            (RegInit(r0), RegInit(r1)) => r0 == r1,
            (MemInit(m0), MemInit(m1)) => m0 == m1,
            (Incr(a), Incr(b)) => self.sym_eq(a, b),
            (Decr(a), Decr(b)) => self.sym_eq(a, b),
            (MemVal(a0, a1), MemVal(b0, b1)) => self.sym_eq(a0, b0) && self.sym_eq(a1, b1),
            (RegVal(r0, a, t0), RegVal(r1, b, t1)) => r0 == r1 && self.sym_eq(a, b) && t0 == t1,
            (RegInit(r0), RegVal(r1, guess, _t0)) => r0 == r1 && self.sym_eq(a, guess),
            (RegVal(..), RegInit(..)) => self.sym_eq(b, a),
            (MemInit(_t0), MemVal(_addr, guess)) => self.sym_eq(a, guess),
            (MemVal(..), MemInit(..)) => self.sym_eq(b, a),
            (MemInit(_), _) => false,
            (_, MemInit(_)) => false,
            (_, _) => false,
        }
    }
    

    fn incorrect(&mut self, i: Instance) {
        use assrs::MoveMode::*;
        let sp = self.getreg(i.sp);
        let dl = self.getreg(i.dl);

        let sp_out = match i.sp_mode {
            Direct => sp.clone(),
            Incr => self.incr( &sp),
            DecrPost | Decr => self.decr( &sp),
        };
        let sp_in = if let Decr = i.sp_mode {
            self.decr(&sp)
        } else {
            sp.clone()
        };
        let mut dl_out = match i.dl_mode {
            Direct => dl.clone(),
            Incr => self.incr( &dl),
            DecrPost | Decr => self.decr( &dl),
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
                Incr => self.incr( &val_to_store),
                DecrPost | Decr => self.decr( &val_to_store),
            };
        }
        self.setreg(i.sp, &sp_out);
        self.setreg(i.dl, &dl_out);
    }
}

impl Default for Trace {
    fn default() -> Trace {
        Trace {
            regs: std::array::from_fn(|i| Rc::new(RegInit(i))),
            effs: vec![],
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
        InstQuantifier { which_dst_indirs: vec![true,false], which_src_indirs: vec![true,false], which_src_modes: MoveMode::all().collect(), which_dst_modes: MoveMode::all().collect(), which_src_regs: (0..16).collect(), which_dst_regs: (0..16).collect() }
    }

    fn direct_dst(self) -> Self {
        InstQuantifier { which_dst_indirs: vec![false], ..self }
    }
    fn into_iter(self) -> impl Iterator<Item=Instance> {
        itertools::iproduct!(self.which_dst_indirs, self.which_src_indirs, self.which_src_modes, self.which_dst_modes, self.which_src_regs, self.which_dst_regs).map(|(dst_indir, src_indir, src_mode, dst_mode, src_reg, dst_reg)| Instance { dst_indir, src_indir, dl_mode: dst_mode, sp_mode: src_mode, sp: src_reg, dl: dst_reg })
    }
}

fn main() {
    let mut solutions = vec![];
    let mut dissolutions = vec![];
    let mut bad = 0;
    let mut good = vec![];
    for dst_indir in vec![false, true] {
        for dl_mode in MoveMode::all() {
            for src_indir in vec![false, true] {
                for sp_mode in MoveMode::all() {
                    let inst = Instance {
                        sp: 4,
                        dl: 4,
                        dst_indir,
                        dl_mode,
                        src_indir,
                        sp_mode,
                    };
                    let mut tr_g = Trace::default();
                    let mut tr_b = Trace::default();
                    tr_g.correct(inst);
                    tr_b.incorrect(inst);
                    solutions.push((inst, tr_g.clone()));
                    dissolutions.push((inst, tr_b.clone()));

                    let sp_diff = !tr_g.sym_eq(&tr_g.regs[inst.sp], &tr_b.regs[inst.sp]);
                    let dl_diff = !tr_g.sym_eq(&tr_g.regs[inst.dl], &tr_b.regs[inst.dl]);
                    if sp_diff || dl_diff {
                        bad += 1;
                        println!("Generated an incorrect instance {:?}\nCorrect version computed:\n\tR4 = {:?}\n\teffs = {:?}\n\nIncorrect version computed:\n\tR4 = {:?}\n\teffs = {:?}\n===============================================================================\n", 
                            inst,
                            tr_g.shrink(&tr_g.regs[inst.sp]),
                            tr_g.effs, 
                            tr_b.shrink(&tr_b.regs[inst.sp]),
                            tr_b.effs);
                    } else { good.push(inst); }
                }
            }
        }
    }

    for g in  good { println!("{:?} agreed", g); }
    println!("{} / 64 cases were bad", bad);
}
