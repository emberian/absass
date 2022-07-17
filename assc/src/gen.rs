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
