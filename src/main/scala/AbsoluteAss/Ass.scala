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
    l_xor :: l_nand :: l_and :: l_xnor :: l_p :: l_mi ::
    l_q :: l_ci :: l_or :: l_t :: Nil) = Enum(16)

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
    is(l_p) { io.out := io.p }
    is(l_mi) { io.out := (io.p & io.q) | ~io.p }
    is(l_q) { io.out := io.q }
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
        .otherwise { io.out := Fill(ws, io.d(ws-1)) }
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
    val cnd = Reg(Bool())
    val out = Output(UInt(ws.W))
  })

  io.cnd := 0.U
  when(io.eq && io.d === io.s) { io.cnd := 1.U }
  when(io.gt) {
    when(io.sn) {
      when(io.d.asSInt > io.s.asSInt) { io.cnd := 1.U }
    }.otherwise {
      when(io.d > io.s) { io.cnd := 1.U }
    }
  }
  when(io.iv) { io.cnd := !io.cnd }
  when(io.cnd) { io.out := 1.U }.otherwise { io.out := 0.U }
}

class CPU(val ws: Int) extends Module {
  val Word = UInt(ws.W)
  val io = IO(new Bundle {
    val mem_addr = Decoupled(Word)
    val mem_content = Flipped(Decoupled(Word))
    val is_write = Output(Bool())
    val halt = Input(Bool())
  })

  val arith = new ArithUnit(ws)
  val logic = new LogicUnit(ws)
  val compare = new ComparisonUnit(ws)

  val regs = Mem(16, Word)

  val pc_reg = 0.U
  val pc = 0.U
  val inst = 0.U

  val dl = 0.U
  val sp = 0.U

  when (!io.halt) {
    pc := regs(pc_reg)
    io.mem_addr.bits := pc
    io.is_write := false.B
    io.mem_addr.valid := true.B

    io.mem_content.ready := true.B
    when (io.mem_content.valid) {
      inst := io.mem_content.bits
      io.mem_addr.valid := false.B

      pc := pc + 2.U
      dl := inst(3, 0)
      sp := inst(7, 4)

      when (inst(15, 14) === "b01".U) {
        // data transfer
      }.otherwise {
        switch (inst(15, 12)) {
          is("b0001".U) {
            logic.io.p := regs(dl)
            logic.io.q := regs(sp)
            logic.io.op := inst(11, 8)
            regs(dl) := logic.io.out
          }
          is("b0010".U) {
            arith.io.d := regs(dl)
            arith.io.s := regs(sp)
            arith.io.op := inst(10, 8)
            regs(dl) := arith.io.out
          }
          is("b0011".U) {
            compare.io.d := regs(dl)
            compare.io.s := regs(sp)
            compare.io.eq := inst(8)
            compare.io.gt := inst(9)
            compare.io.sn := inst(10)
            compare.io.iv := inst(11)
            regs(dl) := compare.io.out
          }
          is("b1000".U) {
            // conditional
            val offset = inst(7, 0)
            val cmp = inst(11, 8)
            when(regs(cmp) =/= 0.U) {
              pc := pc + offset
            }
          }
          is("b1001".U) {
            regs(dl) := pc
            regs(pc_reg) := regs(sp)
          }
        }
      }
    }

  }
}

/** Compute GCD using subtraction method. Subtracts the smaller from the larger
  * until register y is zero. value in register x is then the GCD
  */
class GCD extends Module {
  val io = IO(new Bundle {
    val value1 = Input(UInt(16.W))
    val value2 = Input(UInt(16.W))
    val loadingValues = Input(Bool())
    val outputGCD = Output(UInt(16.W))
    val outputValid = Output(Bool())
  })

  val x = Reg(UInt())
  val y = Reg(UInt())

  when(x > y) { x := x - y }
    .otherwise { y := y - x }

  when(io.loadingValues) {
    x := io.value1
    y := io.value2
  }

  io.outputGCD := x
  io.outputValid := y === 0.U
}

import chisel3.stage.ChiselStage

object GCDDriver extends App {
  (new ChiselStage).emitVerilog(new GCD, args)
}
