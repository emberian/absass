package absass
import spinal.core._
import spinal.lib._

import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import spinal.core.sim.SimPublic

class LogicUnit(val ws: Int) extends Component {
  val io = new Bundle {
    val p = in UInt (ws bits)
    val q = in UInt (ws bits)
    val op = in UInt (4 bits)
    val go = in Bool ()
    val rdy = out Bool ()
    val res = out UInt (ws bits)
  }
  io.res.setAsReg()
  val rdy = RegInit(False)
  io.rdy := rdy
  when(io.go) {
    for (bit <- 0 until ws) {
      io.res(bit) := io.op(
        io.q(bit).asUInt(2 bits) | (io.p(bit).asUInt(1 bits) << 1)
      )
    }
    rdy := True
  } otherwise {
    io.rdy := False
    rdy := False
  }

}

object ArithOps extends SpinalEnum {
  val l_add, l_sub, l_shl, l_shr, l_asr, l_mul, l_div, l_mod =
    newElement()
}
import ArithOps._
class ArithUnit(val ws: Int, val fancy: Boolean) extends Module {
  val io = new Bundle {
    val d = in UInt (ws bits)
    val s = in UInt (ws bits)
    val op = in(ArithOps())
    val res = out UInt (ws bits)
    val go = in Bool ()
    val ready = out Bool ()
  }

  val signed_max = (1 << (ws - 1) - 1)
  io.res := 0
  when(io.go) {
    switch(io.op) {
      is(l_add) { io.res := io.d + io.s }
      is(l_sub) { io.res := io.d - io.s }
      is(l_shl) { io.res := io.d |<< io.s }
      is(l_shr) { io.res := io.d |>> io.s }
      is(l_asr) {
        when(io.s < ws) { io.res := io.d >> io.s }
          .otherwise { io.res.setAllTo(io.d(ws - 1)) }
      }
      is(l_mul) {
        if (fancy) { io.res := (io.d * io.s).resize(ws bits) }
      }
      is(l_div) {
        when(io.s =/= 0) { if (fancy) { io.res := io.d / io.s } }.otherwise {
          io.res.setAll()
        }
      }
      is(l_mod) {
        when(io.s =/= 0) { if (fancy) { io.res := io.d % io.s } }.otherwise {
          io.res := 0
        }
      }
    }
    io.ready := True
  } otherwise {
    io.ready := False
  }
}

class ComparisonUnit(val ws: Int) extends Module {
  val io = new Bundle {
    val d = in UInt (ws bits)
    val s = in UInt (ws bits)
    val eq = in Bool ()
    val gt = in Bool ()
    val sn = in Bool ()
    val iv = in Bool ()
    val res = out Bool ()
    val go = in Bool ()
    val rdy = out Bool ()
  }
  val rdy = RegInit(False)
  val cnd = RegInit(False)
  io.rdy := rdy
  when(io.go) {
    val eq = io.eq && io.d === io.s
    val gt =
      (io.gt && io.sn && (io.d.asSInt > io.s.asSInt) || (io.gt && !io.sn && io.d > io.s))
    cnd := eq || gt

    when(io.iv) { io.res := !cnd }.otherwise { io.res := cnd }
    rdy := True
  } otherwise {
    rdy := False
    cnd := False
    io.rdy := False
    io.res.assignDontCare()
  }
}

case class DebugPort(ws: Int) extends Bundle with IMasterSlave {
  val cur_stage = Stages()
  val insn = UInt(16 bits)
  val sstep_insn = Bool()
  val sstep = Bool()

  val halt = Bool()

  override def asMaster(): Unit = {
    out(cur_stage)
    in(halt, insn, sstep_insn, sstep)
  }
}
