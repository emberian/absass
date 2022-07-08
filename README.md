# ABSOLUTE ASS

A computer! Our features:

- A compact and uniform 16-bit instruction format
- A pleasant RISC load-store design, we think, we're not really sure as while we are educated, we are still so ignorant.
- Word-size agnostic. Want a 4-bit CPU? Go for it! 64 bit? Sure!
- A suite of poorly-tested implementations that occasionally make their way onto an FPGA
- A generalized bitwise instruction that can perform any of the 16 2-input boolean functions

Planned features:

- Memory protection scheme
- Interrupts and I/O interfaces

Here you'll find...

- [`doc`](./doc), with a [specification of the instruction set](./doc/isa-insns.txt) as well as a [description of the calling convention](./doc/calling-convention.txt).
- [`myhdl`](./myhdl), an implementation in MyHDL (surprise!) of a core.
- [`spinal_ass`](./spinal_ass), an implmentation in SpinalHDL of a core.
- [`src`](./src), an outdated partial implementation in Chisel of a core.
- [`assrs`](./assrs), an emulator in Rust that also has some utilities such as a disassembler and instruction encoder
- [`assembler`](./assembler), an assembler in MoonScript that supports labels and a few handy instruction aliases like "LI" to load immediate.
