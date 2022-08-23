#![feature(mixed_integer_ops)]

mod isa;
mod sim;

pub use isa::*;
pub use sim::*;

#[cfg_attr(test, test)]
pub fn runtest() {
    let mut m = Machine::default();
    m.memory = vec![0u8; 0x10000];
    m.exec(Insn::Logic {
        src: 1,
        dst: 1,
        op: 0xf,
    });
    assert_eq!(m.regs[1], !0);
    m.regs[3] = 0x8;

    m.exec(Insn::Move {
        src: 1,
        dst: 3,
        s_mode: MoveMode::Direct,
        s_deref: false,
        d_mode: MoveMode::Direct,
        d_deref: true,
    });
    assert_eq!(m.memory[0x8 as usize], 0xff);
}
