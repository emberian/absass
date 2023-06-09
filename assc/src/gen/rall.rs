use super::*;

use std::collections::{HashMap, HashSet};

// Well-known registers
pub const PC: Reg = 0;
pub const SP: Reg = 1;
pub const FP: Reg = 2;
pub const RA: Reg = 3;
pub const A0: Reg = 4;
pub const A_NUM: Reg = 6;
pub const T0: Reg = 10;
pub const T_NUM: Reg = 6;

#[allow(dead_code)]
pub const fn is_arg(reg: Reg) -> bool { reg >= A0 && (reg - A0) < A_NUM }
#[allow(dead_code)]
pub const fn is_temp(reg: Reg) -> bool { reg >= T0 && (reg - T0) < T_NUM }

#[derive(Debug, Clone, Copy, Hash)]
pub struct Span(pub usize, pub usize, pub Option<Reg>);

impl Span {
    pub fn new(start: usize, stop: usize) -> Self {
        Span(start, stop, None)
    }

    pub fn reg(&self) -> &Option<Reg> { &self.2 }
    pub fn reg_mut(&mut self) -> &mut Option<Reg> { &mut self.2 }

    pub fn overlaps(&self, other: &Span) -> Option<Span> {
        let (ls, ee) = (self.0.max(other.0), self.1.min(other.1));
        if ls <= ee {
            Some(Span(ls, ee, None))
        } else {
            None
        }
    }

    pub fn contains(&self, t: usize, endpoint: bool) -> bool {
        t >= self.0 && (
            t < self.1 || (endpoint && t == self.1)
        )
    }
}

#[derive(Debug, Clone, Hash)]
pub struct RegDesc {
    pub reg: Reg,
    pub in_use: bool,
    pub must_save: bool,
    pub saved: bool,
    pub allowed: bool,
    pub uses: Vec<usize>,
    pub defs: Vec<usize>,
    pub spans: Vec<Span>,
}

impl RegDesc {
    pub fn denied(r: Reg) -> Self {
        RegDesc {
            reg: r,
            in_use: false,
            must_save: true,
            saved: false,
            allowed: false,
            uses: Vec::new(),
            defs: Vec::new(),
            spans: Vec::new(),
        }
    }

    pub fn caller_saved(r: Reg) -> Self {
        RegDesc {
            reg: r,
            in_use: false,
            must_save: false,
            saved: false,
            allowed: true,
            uses: Vec::new(),
            defs: Vec::new(),
            spans: Vec::new(),
        }
    }

    pub fn callee_saved(r: Reg) -> Self {
        RegDesc {
            reg: r,
            in_use: false,
            must_save: true,
            saved: false,
            allowed: true,
            uses: Vec::new(),
            defs: Vec::new(),
            spans: Vec::new(),
        }
    }

    pub fn span_mut_starting_at(&mut self, t: usize) -> Option<&mut Span> {
        let spans = self.spans.clone();  // XXX for debugging only!
        for spn in self.spans.iter_mut() {
            if t == spn.0 { return Some(spn); }
        }
        println!("WARN: no span found at time {} for reg {}; defs {:?}, uses {:?}, spans {:?}", t, self.reg, self.defs, self.uses, spans);
        None
    }

    pub fn span_mut_at(&mut self, t: usize, endpoint: bool) -> Option<&mut Span> {
        for spn in self.spans.iter_mut() {
            if t < spn.0 { return None; }
            if t < spn.1 || (endpoint && t == spn.1) { return Some(spn); }
        }
        None
    }

    pub fn span_at(&self, t: usize, endpoint: bool) -> Option<&Span> {
        for spn in self.spans.iter() {
            if t < spn.0 { return None; }
            if t < spn.1 || (endpoint && t == spn.1) { return Some(spn); }
        }
        None
    }

    pub fn live_at(&self, t: usize) -> bool {
        self.span_at(t, true).is_some()
    }

    pub fn defined_at(&self, t: usize) -> bool {
        self.defs.binary_search(&t).is_ok()
    }

    pub fn used_at(&self, t: usize) -> bool {
        self.uses.binary_search(&t).is_ok()
    }

    pub fn dies_at(&self, t: usize) -> bool {
        for spn in &self.spans {
            if t == spn.1 { return true; }
            if t < spn.0 { return false; }
        }
        false
    }

    pub fn overlaps_any(&self, span: &Span) -> Option<Span> {
        for spn in &self.spans {
            if let Some(_overlap) = spn.overlaps(span) {
                return Some(*spn);
            }
        }
        None
    }
}

pub type Pool = Vec<Reg>;

#[derive(Debug, Clone)]
pub struct RAlloc {
    pub descs: HashMap<Reg, RegDesc>,
    pub open: Pool,
    pub used: Pool,
    pub denied: Pool,
    pub stack: Reg,
}

#[derive(Debug, Clone, Copy, Hash)]
pub enum Taken {
    Free(Reg),
    Save(Reg),
    Spill(Reg),
    Nothing,
}

impl RAlloc {
    pub fn new(stack: Reg) -> Self {
        RAlloc {
            descs: HashMap::new(),
            open: Pool::new(),
            used: Pool::new(),
            denied: Pool::new(),
            stack,
        }
    }

    pub fn add(&mut self, rd: RegDesc) {
        let reg = rd.reg;
        if rd.allowed {
            if rd.in_use {
                self.used.push(reg);
            } else {
                self.open.push(reg);
            }
        } else {
            self.denied.push(reg);
        }
        self.descs.insert(reg, rd);
    }

    pub fn desc(&self, reg: Reg) -> Option<&RegDesc> {
        self.descs.get(&reg)
    }

    pub fn reorder(&mut self) {
        self.open.sort_unstable_by_key(|x| !self.descs[x].must_save);
    }

    pub fn deny(&mut self, reg: Reg) {
        let rd = self.descs.get_mut(&reg).unwrap();
        if rd.allowed {
            rd.allowed = false;
            self.open.remove(self.open.iter().position(|&r| r == reg).unwrap());
            self.denied.push(reg);
        }
    }

    pub fn allow(&mut self, reg: Reg) {
        let rd = self.descs.get_mut(&reg).unwrap();
        if !rd.allowed {
            rd.allowed = true;
            self.denied.remove(self.denied.iter().position(|&r| r == reg).unwrap());
            self.open.push(reg);
        }
    }

    pub fn take(&mut self) -> Taken {
        if let Some(top) = self.open.pop() {
            self.used.push(top);
            let desc = self.desc(top).unwrap();
            if desc.must_save {
                Taken::Save(top)
            } else {
                Taken::Free(top)
            }
        } else {
            if self.used.is_empty() {
                Taken::Nothing
            } else  {
                let used = self.used.remove(0);
                Taken::Spill(used)
            }
        }
    }

    pub fn give(&mut self, reg: Reg) {
        self.used.remove(self.used.iter().position(|&r| r == reg).unwrap());
        self.open.push(reg);
        self.reorder();
    }
}

impl Default for RAlloc {
    fn default() -> Self {
        let mut ra = RAlloc::new(SP);

        ra.add(RegDesc::denied(PC));
        ra.add(RegDesc::denied(SP));
        ra.add(RegDesc::denied(FP));
        ra.add(RegDesc::denied(RA));

        for i in 0 .. A_NUM {
            ra.add(RegDesc::caller_saved(A0 + i));
        }

        for i in 0 .. T_NUM {
            ra.add(RegDesc::callee_saved(T0 + i));
        }

        ra.reorder();
        ra
    }
}

#[derive(Debug, Clone)]
pub struct StackDesc {
    pub spans: Vec<(Span, HashSet<Reg>)>,
}

impl StackDesc {
    pub fn new() -> Self {
        Self {
            spans: Vec::new(),
        }
    }

    pub fn add_span(&mut self, spn: Span) {
        self.spans.push((spn, HashSet::new()));
    }

    pub fn in_span(&self, time: usize) -> Option<usize> {
        for (idx, (spn, _)) in self.spans.iter().enumerate() {
            if spn.contains(time, false) {
                return Some(idx);
            }
        }
        None
    }

    pub fn add_reg(&mut self, spidx: usize, reg: Reg) {
        self.spans[spidx].1.insert(reg);
    }

    pub fn iter<'s>(&'s self) -> impl Iterator<Item=(Span, Vec<Reg>)> + 's {
        self.spans.iter()
            .map(|(sp, hs)| {
                let mut ordered = hs.iter().cloned().collect::<Vec<_>>();
                ordered.sort();
                (*sp, ordered)
            })
    }
}

#[derive(Debug, Clone)]
pub struct RaCx {
    pub ra: RAlloc,
    pub time: Vec<(usize, Line)>,
    pub insertions: HashMap<usize, Vec<Line>>,
    pub temp_descs: HashMap<Temp, RegDesc>,
    pub reg_descs: HashMap<Reg, RegDesc>,
    pub stack: StackDesc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Usage {
    Def, Use,
}

impl RaCx {
    pub fn new(ra: RAlloc, lines: Vec<Line>) -> Self {
        RaCx {
            ra,
            time: lines.iter().cloned().enumerate().collect(),
            insertions: HashMap::new(),
            temp_descs: HashMap::new(),
            reg_descs: HashMap::new(),
            stack: StackDesc::new(),
        }
    }

    fn analyze_place(&mut self, time: usize, place: &Place, usage: Usage) {
        if let Place::Temp(t) = place {
            let desc = self.temp_descs.entry(*t).or_insert_with_key(|&temp| {
                RegDesc::caller_saved(temp as Reg)
            });
            (if usage == Usage::Def {
                &mut desc.defs
            } else {
                &mut desc.uses
            }).push(time);
        }
    }

    fn analyze_line(&mut self, time: usize, line: &Line) {
        use Usage::*;

        match line {
            Line::Insn(insn) => match insn {
                Insn::Logic { src, dst, op } => {
                    self.analyze_place(time, dst, Def);
                    if op.reads_dst() {
                        self.analyze_place(time, dst, Use);
                    }
                    if op.uses_src() {
                        self.analyze_place(time, src, Use);
                    }
                },
                Insn::Arith { src, dst, .. } => {
                    self.analyze_place(time, dst, Def);
                    self.analyze_place(time, dst, Use);
                    self.analyze_place(time, src, Use);
                },
                Insn::ArithImm { dst, .. } => {
                    self.analyze_place(time, dst, Def);
                    self.analyze_place(time, dst, Use);
                },
                Insn::Compare { src, dst, .. } => {
                    self.analyze_place(time, dst, Def);
                    self.analyze_place(time, dst, Use);
                    self.analyze_place(time, src, Use);
                },
                Insn::Transfer { src, dst } => {
                    // A little trickier, because indirects are uses, not defs
                    if dst.indirect {
                        self.analyze_place(time, &dst.reg, Use);
                        // ... but any AutoMode also causes a Def
                        if dst.mode.modifies() {
                            self.analyze_place(time, &dst.reg, Def);
                        }
                    } else {
                        self.analyze_place(time, &dst.reg, Def);
                    }
                    self.analyze_place(time, &src.reg, Use);
                    if src.mode.modifies() {
                        self.analyze_place(time, &src.reg, Def);
                    }
                },
                Insn::SysReg { reg, write, .. } => {
                    // write is from the perspective of the SR
                    self.analyze_place(time, reg, if *write { Use } else { Def });
                },
                Insn::JumpCond { reg, .. } => {
                    self.analyze_place(time, reg, Use);
                },
                Insn::Misc { a, b, .. } => {
                    self.analyze_place(time, a, Def);
                    self.analyze_place(time, a, Use);
                    self.analyze_place(time, b, Use);
                },
                Insn::SubWord { dst, .. } => {
                    self.analyze_place(time, dst, Use);
                    self.analyze_place(time, dst, Def);
                },
                Insn::Unknown(_) => (),  // all bets are off
            },
            Line::Life(life, reg) => {
                let desc = self.reg_descs.entry(*reg).or_insert_with(
                    || self.ra.descs.get(&reg).unwrap().clone()
                );
                (match life {
                    Life::Claim => &mut desc.defs,
                    Life::Release => &mut desc.uses,
                }).push(time);
            },
            _ => (),
        }
    }

    fn insert(&mut self, time: usize) -> &mut Vec<Line> {
        self.insertions.entry(time).or_default()
    }

    fn def_use_pass(&mut self) {
        let timeline = self.time.clone();
        for (time, line) in &timeline {
            self.analyze_line(*time, &line);
        }
    }

    fn stack_pass(&mut self) {
        let mut depth = 0;
        let mut start = None;
        let mut last_time = None;

        let timeline = self.time.clone();
        for (time, line) in timeline {
            last_time = Some(time);
            if let Line::Stack(dir) = line {
                match dir {
                    Dir::Entry => {
                        if depth == 0 {
                            start = Some(time);
                        }
                        depth += 1;
                    },
                    Dir::Exit => {
                        if depth == 0 {
                            panic!("stack save discipline violation at time {}", time);
                        }
                        depth -= 1;
                        if depth == 0 {
                            self.stack.add_span(Span::new(start.unwrap(), time));
                            start = None;
                        }
                    },
                }
            }
        }
        
        if depth > 0 {
            println!("WARN: save stack nonzero ({}) at exit, closing span", depth);
            self.stack.add_span(Span::new(start.unwrap(), last_time.unwrap() + 1));
        }
    }

    fn span_pass(&mut self) {
        use Usage::*;

        for (temp, desc) in
            self.temp_descs.values_mut()
            .map(|v| (true, v))
            .chain(
                self.reg_descs.values_mut()
                .map(|v| (false, v))
            )
        {
            let kind = if temp { "temp" } else { "reg" };
            let mut times: Vec<_> = desc.uses.iter().map(|&t| (Use, t)).collect();
            times.extend(desc.defs.iter().map(|&t| (Def, t)));
            // A stable sort preserves Use/Def ordering on the same t
            times.sort_by_key(|(_, t)| *t);

            let mut last_use: Option<usize> = None;
            let mut last_def: Option<usize> = None;
            let mut last_time: Option<usize> = None;

            for (ev, time) in times {
                last_time = Some(time);
                match ev {
                    Use => {
                        if last_def.is_none() {
                            println!("WARN: use before def, {} {} at time {}", kind, desc.reg, time);
                        }
                        last_use = Some(time);
                    },
                    Def => {
                        if let Some(birth) = last_def {
                            // The last use killed the last span
                            if let Some(death) = last_use {
                                desc.spans.push(Span::new(birth, death));
                            } else {
                                println!("WARN: unused value, {} {} at time {}", kind, desc.reg, time);
                                // This span is still needed by reg_pass anyway
                                desc.spans.push(Span::new(birth, time));
                            }
                        }
                        last_def = Some(time);
                    },
                }
            }

            // Treat the end of the program as a def to cap off the last use
            if let Some(birth) = last_def {
                if let Some(death) = last_use {
                    if birth < death {
                        desc.spans.push(Span::new(birth, death));
                    } else {
                        println!("WARN: unused value at program end, {} {} at time {}", kind, desc.reg, birth);
                        // Push a span anyway, since reg_pass needs it--just say it goes beyond the
                        // end time.
                        let end = last_time.unwrap() + 1;
                        desc.spans.push(Span::new(birth, end));
                    }
                } else {
                    // If we're here, the desc was defined at least once but never used.
                    // This is a rather degenerate case, but reg_pass still needs a span anyway.
                    println!("WARN: unused value at program end, {} {} at time {}", kind, desc.reg, birth);
                    desc.spans.push(Span::new(birth, birth + 1));
                }
            }
        }
    }

    fn reg_pass(&mut self) {
        let mut event_times: Vec<usize> = self.temp_descs.values()
            .flat_map(|desc| desc.uses.iter()   // uses
                      .chain(desc.defs.iter())  // defs
                      .cloned()  // &usize -> usize
                      .chain(desc.spans.iter().map(|sp| sp.0))  // births
                      .chain(desc.spans.iter().map(|sp| sp.1))  // deaths
            )
            .collect();
        event_times.sort();
        event_times.dedup();

        #[derive(Debug, Clone, Copy, Hash)]
        struct Bind {
            reg: Reg,
            saved: bool,
        }
        let mut bindings: HashMap<Temp, Bind> = HashMap::new();

        #[derive(Debug, Clone, Copy, Hash)]
        struct Fence {
            reg: Reg,
            ready: bool
        }
        let mut save_fence: Vec<Fence> = Vec::new();

        let mut descs = self.temp_descs.clone();  // FIXME: split borrows better
        let stack = self.ra.stack;

        for time in event_times {
            // println!("time {}:", time);
            for temp_desc in descs.values_mut() {
                // println!("time {}: temp {}:", time, temp_desc.reg);
                if temp_desc.dies_at(time) {
                    println!("time {}: temp {} dies", time, temp_desc.reg);
                    let bind = bindings.remove(&(temp_desc.reg as Temp));
                    if bind.is_none() {
                        println!("span check: temp descriptor is {:?}", temp_desc);
                        panic!("time {}: temp {} not found in bindings {:?}", time, temp_desc.reg, bindings);
                    }
                    let bind = bind.unwrap();
                    println!("time {}: temp {} releases reg {} from bindings {:?}", time, temp_desc.reg, bind.reg, bindings);
                    self.ra.give(bind.reg);
                    if bind.saved {
                        for fence in save_fence.iter_mut() {
                            if fence.reg == bind.reg {
                                fence.ready = true;
                                break;
                            }
                        }
                        println!("\tprevious value was saved/spilled, stack now {:?}", save_fence);
                        while let Some(top) = save_fence.last() {
                            if !top.ready { break; }
                            let top = save_fence.pop().unwrap();
                            println!("\tpop reg {}", top.reg);
                            self.insert(time + 1).push(Line::Insn(Insn::Transfer {
                                src: Xft {
                                    reg: Place::Reg(stack),
                                    indirect: true,
                                    mode: AutoMode::PostIncr,
                                },
                                dst: Xft {
                                    reg: Place::Reg(top.reg),
                                    indirect: false,
                                    mode: AutoMode::None,
                                },
                            }));
                        }
                    }
                }
                if temp_desc.defined_at(time) {
                    println!("time {}: temp {} is born", time, temp_desc.reg);
                    let span = temp_desc.span_at(time, false).unwrap();
                    let mut unusable = Vec::new();
                    let bind = loop {
                        let conflicts = |reg: Reg| {
                            if let Some(desc) = self.reg_descs.get(&reg) {
                                desc.overlaps_any(span).is_some()
                            } else {
                                false
                            }
                        };
                        break match self.ra.take() {
                            Taken::Free(reg) => {
                                if conflicts(reg) { unusable.push(reg); continue; }
                                Bind { reg, saved: false }
                            },
                            Taken::Save(reg) | Taken::Spill(reg) => {
                                if conflicts(reg) { unusable.push(reg); continue; }
                                if let Some(ix) = self.stack.in_span(time) {
                                    self.stack.add_reg(ix, reg);
                                    println!("spilled reg {} saved to span {} ({:?})", reg, ix, self.stack.spans[ix].0);
                                    Bind { reg, saved: false }
                                } else {
                                    self.insert(time).push(Line::Insn(Insn::Transfer {
                                        src: Xft {
                                            reg: Place::Reg(reg),
                                            indirect: false,
                                            mode: AutoMode::None,
                                        },
                                        dst: Xft {
                                            reg: Place::Reg(stack),
                                            indirect: true,
                                            mode: AutoMode::PreDecr,
                                        },
                                    }));
                                    save_fence.push(Fence { reg, ready: false });
                                    println!("\tsaved/spilled, stack is now {:?}", save_fence);
                                    Bind { reg, saved: true }
                                }
                            },
                            Taken::Nothing => panic!("Failed to allocate a register"),
                        };
                    };
                    for un in unusable {
                        self.ra.give(un);
                    }
                    bindings.insert(temp_desc.reg as Temp, bind);
                    println!("time {}: temp {} inserted as reg {} into bindings {:?}", time, temp_desc.reg, bind.reg, bindings);
                    *temp_desc.span_mut_starting_at(time).unwrap().reg_mut() = Some(bind.reg);
                }
            }
        }

        self.temp_descs = descs;
    }

    fn respill_pass(&mut self) {
        let stack = self.ra.stack;

        let spills = self.stack.iter().collect::<Vec<_>>();
        for (sp, regs) in spills {
            for reg in regs.iter().copied().rev() {
                self.insert(sp.0).push(Insn::Transfer {
                    src: Xft {
                        reg: Place::Reg(reg),
                        indirect: false,
                        mode: AutoMode::None,
                    },
                    dst: Xft {
                        reg: Place::Reg(stack),
                        indirect: true,
                        mode: AutoMode::PreDecr,
                    },
                }.into());
            }
            for reg in regs.iter().copied() {
                self.insert(sp.1).push(Insn::Transfer {
                    src: Xft {
                        reg: Place::Reg(stack),
                        indirect: true,
                        mode: AutoMode::PostIncr,
                    },
                    dst: Xft {
                        reg: Place::Reg(reg),
                        indirect: false,
                        mode: AutoMode::None,
                    },
                }.into());
            }
        }
    }

    pub fn allocate(&mut self) {
        self.def_use_pass();
        self.span_pass();
        self.stack_pass();
        self.reg_pass();
        self.respill_pass();
    }

    fn rewrite_place(&self, time: usize, pl: Place, usage: Usage) -> Place {
        if let Place::Temp(t) = pl {
            let desc = &self.temp_descs[&t];
            let span = desc.span_at(time, usage == Usage::Use);
            if span.is_none() {
                println!("span check: no span at time {} while rewriting place {:?} usage {:?}", time, pl, usage);
            }
            let span = span.unwrap();
            Place::Reg(span.reg().unwrap())
        } else {
            pl
        }
    }

    fn rewrite_line(&self, time: usize, line: Line) -> Line {
        use Usage::*;

        if let Line::Insn(insn) = line {
            Line::Insn(match insn {
                Insn::Logic { src, dst, op } =>
                    Insn::Logic {
                        src: self.rewrite_place(time, src, Use),
                        dst: self.rewrite_place(time, dst, Def),
                        op,
                    },
                Insn::Arith { src, dst, op } =>
                    Insn::Arith {
                        src: self.rewrite_place(time, src, Use),
                        dst: self.rewrite_place(time, dst, Def),
                        op,
                    },
                Insn::ArithImm { dst, imm, op } =>
                    Insn::ArithImm {
                        dst: self.rewrite_place(time, dst, Def),
                        imm, op,
                    },
                Insn::Compare { src, dst, eq, gt, sn, iv } =>
                    Insn::Compare {
                        src: self.rewrite_place(time, src, Use),
                        dst: self.rewrite_place(time, dst, Def),
                        eq, gt, sn, iv,
                    },
                Insn::Transfer { src, dst } =>
                    Insn::Transfer {
                        src: Xft {
                            reg: self.rewrite_place(time, src.reg, Use),
                            .. src
                        },
                        dst: Xft {
                            reg: self.rewrite_place(time, dst.reg,
                                                    if dst.indirect {
                                                        Use
                                                    } else {
                                                        Def
                                                    }),
                            .. dst
                        },
                    },
                Insn::SysReg { reg, sr, write } =>
                    Insn::SysReg {
                        reg: self.rewrite_place(time, reg, if write { Use } else { Def }),
                        sr, write,
                    },
                Insn::JumpCond { reg, offset } =>
                    Insn::JumpCond {
                        reg: self.rewrite_place(time, reg, Use),
                        offset,
                    },
                Insn::Misc { op, a, b } =>
                    Insn::Misc {
                        a: self.rewrite_place(time, a, Def),
                        b: self.rewrite_place(time, b, Use),
                        op,
                    },
                Insn::SubWord { dst, byte_ix, bytes } =>
                    Insn::SubWord {
                        dst: self.rewrite_place(time, dst, Def),
                        byte_ix, bytes,
                    },
                Insn::Unknown(i) => Insn::Unknown(i),
            })
        } else {
            line
        }
    }

    pub fn to_linear(&self) -> Vec<Line> {
        self.time.iter()
            .flat_map(|(time, line)| {
                let ins = self.insertions.get(time);
                let rewritten = self.rewrite_line(*time, line.clone());
                if let Some(insertions) = ins {
                    let mut ins = insertions.clone();
                    ins.push(rewritten);
                    ins
                } else {
                    vec![rewritten]
                }
            })
            .collect()
    }
}
