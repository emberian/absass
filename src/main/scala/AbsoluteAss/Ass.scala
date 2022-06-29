// See README.md for license details.

package absass

import chisel3._
import chisel3.util._

class LogicUnit(val ws: Int) extends Module {
  val io = IO(new Bundle {
    val p = Input(UInt(ws.W))
    val q = Input(UInt(ws.W))
    val op = Input(UInt(4.W))
    val out = Output(UInt(ws.W))
  })

  val (l_f :: l_nor :: l_nci :: l_np :: l_nmi :: l_nq ::
    l_xor :: l_nand :: l_and :: l_xnor :: l_q :: l_mi ::
    l_p :: l_ci :: l_or :: l_t :: Nil) = Enum(16)

  io.out := 0.U // otherwise the impossible untaken switch default makes us think the wire is uninitialized

  switch(io.op) {
    is(l_f) { io.out := 0.U }
    is(l_nor) { io.out := ~(io.p | io.q) }
    is(l_nci) { io.out := io.q & ~io.p }
    is(l_np) { io.out := ~io.p }
    is(l_nmi) { io.out := io.p & ~io.q }
    is(l_nq) { io.out := ~io.q }
    is(l_xor) { io.out := io.p ^ io.q }
    is(l_nand) { io.out := ~(io.p & io.q) }
    is(l_and) { io.out := io.p & io.q }
    is(l_xnor) { io.out := ~(io.p ^ io.q) }
    is(l_q) { io.out := io.q }
    is(l_mi) { io.out := (io.p & io.q) | ~io.p }
    is(l_p) { io.out := io.p }
    is(l_ci) { io.out := io.p | ~io.q }
    is(l_or) { io.out := io.p | io.q }
    is(l_t) { io.out := Fill(ws, 1.U) }
  }
}

class ArithUnit(val ws: Int) extends Module {
  val io = IO(new Bundle {
    val d = Input(UInt(ws.W))
    val s = Input(UInt(ws.W))
    val op = Input(UInt(3.W))
    val out = Output(UInt(ws.W))
  })

  val (l_add :: l_sub :: l_shl :: l_shr :: l_asr :: l_mul :: l_div :: l_mod :: Nil) =
    Enum(8)

  val signed_max = (1.U << (ws - 1) - 1)

  io.out := 0.U
  switch(io.op) {
    is(l_add) { io.out := io.d + io.s }
    is(l_sub) { io.out := io.d - io.s }
    is(l_shl) { io.out := io.d << io.s }
    is(l_shr) { io.out := io.d >> io.s }
    is(l_asr) {
      when(io.s < ws.U) { io.out := (io.d.asSInt >> io.s).asUInt }
        .otherwise { io.out := Fill(ws, io.d(ws - 1)) }
    }
    is(l_mul) { io.out := io.d * io.s }
    is(l_div) {
      when(io.s =/= 0.U) { io.out := io.d / io.s }.otherwise {
        io.out := Fill(ws, 1.U)
      }
    }
    is(l_mod) {
      when(io.s =/= 0.U) { io.out := io.d % io.s }.otherwise { io.out := 0.U }
    }
  }
}

class ComparisonUnit(val ws: Int) extends Module {
  val io = IO(new Bundle {
    val d = Input(UInt(ws.W))
    val s = Input(UInt(ws.W))
    val eq = Input(Bool())
    val gt = Input(Bool())
    val sn = Input(Bool())
    val iv = Input(Bool())
    val out = Output(Bool())
  })

  val eq = io.eq && io.d === io.s
  val gt =
    (io.gt && io.sn && (io.d.asSInt > io.s.asSInt) || (io.gt && !io.sn && io.d > io.s))
  val cnd = eq || gt
  when(io.iv) { io.out := !cnd }.otherwise { io.out := cnd }
}

class CPU(val ws: Int) extends Module {
  val Word = UInt(ws.W)
  val Insn = UInt(16.W)
  val io = IO(new Bundle {
    val insn_addr = Decoupled(Word)
    val insn_content = Flipped(Decoupled(Insn))

    val halt = Input(Bool())

    val reg = Flipped(Decoupled(UInt(4.W)))
    val reg_content = Decoupled(Word)
  })

  val (s_idle :: s_fetch :: s_execute :: s_writeback :: Nil) = Enum(4)

  val state = RegInit(s_idle)

  val arith = Module(new ArithUnit(ws))
  val logic = Module(new LogicUnit(ws))
  val compare = Module(new ComparisonUnit(ws))

  val regs = SyncReadMem(16, Word)

  val pc_reg = 0.U

  val pc = Reg(Word)
  val inst = Reg(UInt(16.W))

  val dl = Reg(Word)
  val sp = Reg(Word)

  io.insn_addr.valid := false.B
  io.insn_addr.bits := 0.U
  io.insn_content.ready := false.B
  compare.io.d := 0.U
  compare.io.s := 0.U
  compare.io.eq := 0.U
  compare.io.gt := 0.U
  compare.io.sn := 0.U
  compare.io.iv := 0.U
  logic.io.p := 0.U
  logic.io.q := 0.U
  logic.io.op := 0.U
  arith.io.d := 0.U
  arith.io.s := 0.U
  arith.io.op := 0.U

  io.reg.ready := true.B

  when(io.reg.valid) {
    io.reg_content.bits := regs(io.reg.bits)
    io.reg_content.valid := true.B
  }.otherwise {
    io.reg_content.bits := DontCare
    io.reg_content.valid := false.B
  }

  when(!io.halt && state === s_idle) {
    state := s_fetch
  }

  when(state === s_fetch) {
    pc := regs(pc_reg)
    io.insn_addr.bits := pc
    io.insn_addr.valid := true.B
    io.insn_content.ready := true.B
    when(io.insn_content.valid) {
      inst := io.insn_content.bits
      io.insn_addr.valid := false.B
      io.insn_content.ready := false.B
      state := s_execute
      pc := pc + 2.U
    }
  }.otherwise {
    io.insn_content.ready := false.B
    io.insn_addr.valid := false.B
  }

  val result = Reg(Word)

  when(state === s_execute) {
    dl := inst(3, 0)
    sp := inst(7, 4)

    printf(p"executing $inst, pc = $pc, regs = ${regs(0)} ${regs(1)} ${regs(2)}\n")

    when(inst(15, 14) === "b01".U) {
      // data transfer
    }.otherwise {
      switch(inst(15, 12)) {
        is("b0001".U) {
          logic.io.p := regs(dl)
          logic.io.q := regs(sp)
          logic.io.op := inst(11, 8)
          result := logic.io.out
        }
        is("b0010".U) {
          arith.io.d := regs(dl)
          arith.io.s := regs(sp)
          arith.io.op := inst(10, 8)
          result := arith.io.out
        }
        is("b0011".U) {
          compare.io.d := regs(dl)
          compare.io.s := regs(sp)
          compare.io.eq := inst(8)
          compare.io.gt := inst(9)
          compare.io.sn := inst(10)
          compare.io.iv := inst(11)
          result := compare.io.out
        }
        is("b1000".U) {
          // conditional
          val offset = inst(7, 0)
          val cmp = inst(11, 8)
          when(regs(cmp) =/= 0.U) {
            pc := (pc.asSInt + offset.asSInt).asUInt
          }
          result := regs(dl) // ew
        }
        is("b1001".U) {
          result := pc
          pc := regs(sp)
        }
      }
    }

    state := s_writeback
  }

  when(state === s_writeback) {
    dl := inst(3, 0)
    printf(p"writing back $inst, dl = $dl, result = $result, jump to $pc\n")
    regs(dl) := result
    regs(pc_reg) := pc
    when(io.halt) {
      state := s_idle
    }.otherwise {
      state := s_fetch
    }
  }
}

class CPUWrapper extends Module {
  val io = IO(new Bundle {
    val red_led0 = Output(Bool())
    val red_led1 = Output(Bool())
    val red_led2 = Output(Bool())
    val cpu_running = Input(Bool())
  })

  val cpu = Module(new CPU(4))

  val insns = VecInit(0x1322.U, 0x1a21.U, 0x82fc.U)

  cpu.io.insn_content.bits := 0.U
  cpu.io.insn_content.valid := false.B
  cpu.io.halt := !io.cpu_running
  cpu.io.insn_addr.ready := true.B

  val fetch_ready = RegInit(false.B)

  fetch_ready := cpu.io.insn_addr.valid
  when(fetch_ready) {
    cpu.io.insn_content.bits := insns(cpu.io.insn_addr.bits >> 1)
    cpu.io.insn_content.valid := true.B
  }

  when(cpu.io.insn_content.ready) {
    io.red_led0 := 1.U
  }.otherwise {
    io.red_led0 := 0.U
  }

  cpu.io.reg.bits := 1.U
  cpu.io.reg.valid := true.B
  cpu.io.reg_content.ready := true.B

  when(cpu.io.reg_content.valid) {
    io.red_led2 := cpu.io.reg_content.bits
  }.otherwise {
    io.red_led2 := DontCare
  }

  val ctr = Counter(12_000_000)
  ctr.inc()
  io.red_led1 := ctr.value < 6_000_000.U
}

import chisel3.stage.ChiselStage

object AssDriver extends App {
  (new ChiselStage).emitVerilog(new CPUWrapper, args)
}
