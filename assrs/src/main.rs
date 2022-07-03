#[derive(Debug,Hash,Clone,Copy)]
enum MoveMode { Direct, Incr, Decr, Resv }
#[derive(Debug,Hash,Clone,Copy)]
pub enum Insn {
    Logic { src : Reg, dst : Reg , op: u8 },
    Arith { src: Reg, dst: Reg, op: ArithOp},
    Compare { src: Reg, dst: Reg, eq: bool, sn:bool,gt:bool,iv:bool },
    Move { src: Reg, dst: Reg, s_mode:MoveMode, s_deref:bool,d_mode:MoveMode,d_deref:bool},
}

struct Machine {
    regs: [Word;16]
}
impl Machine {
    fn step(&mut self, i: Insn) {
        match i {
            Insn::Logic {src,dst,op} => {
                let mut res = 0;
                for i in 0..32 {
                    let ix = (self.regs[src] & (1<<i))>>i | ((self.regs[dst]&(1<<i))>>i<<1;
                    res |= ((self.op & (1<<ix)) >>ix) << i;
                }
                self.regs[dst] = res
            }
        }
    }
}
fn main() {
    println!("Hello, world!");
}
