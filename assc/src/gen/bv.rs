pub trait BitVec {
    fn bits(&self) -> usize;
    fn first_set(&self) -> Option<usize>;
    fn set_to(&mut self, idx: usize, bit: bool);
    fn is_set(&self, idx:usize) -> bool;

    fn set(&mut self, idx: usize) { self.set_to(idx, true); }
    fn clear(&mut self, idx: usize) { self.set_to(idx, false); }

    fn take(&mut self) -> Option<usize> {
        self.first_set().map(|bit| {
            self.clear(bit);
            bit
        })
    }
}

pub struct BV64(u64);

impl BitVec for BV64 {
    fn bits(&self) -> usize { 64 }
    fn first_set(&self) -> Option<usize> {
        let tz = self.0.trailing_zeros();
        if tz == 64 {
            None
        } else {
            Some(tz as usize)
        }
    }
    fn set_to(&mut self, idx: usize, bit: bool) {
        if bit {
            self.0 |= 1 << idx;
        } else {
            self.0 &= !(1 << idx);
        }
    }
    fn is_set(&self, idx: usize) -> bool { self.0 & (1 << idx) != 0 }
}

pub struct BV<B>(Vec<B>);

impl<B: BitVec> BitVec for BV<B> {
    fn bits(&self) -> usize { self.0.len() * self.0[0].bits() }
    fn first_set(&self) -> Option<usize> {
        let bs = self.0[0].bits();
        for (idx, elem) in self.0.iter().enumerate() {
            if let Some(bit) = elem.first_set() {
                return Some(bs * idx + bit);
            }
        }
        None
    }
    fn set_to(&mut self, idx: usize, bit: bool) {
        let bs = self.0[0].bits();
        let (idx, subbit) = (idx / bs, idx % bs);
        self.0[idx].set_to(subbit, bit);
    }
    fn is_set(&self, idx: usize) -> bool {
        let bs = self.0[0].bits();
        let (idx, subbit) = (idx / bs, idx % bs);
        self.0[idx].is_set(subbit)
    }
}


