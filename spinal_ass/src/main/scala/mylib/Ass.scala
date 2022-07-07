/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package absass

import spinal.core._
import spinal.lib._

import scala.util.Random

//Hardware definition
class LogicUnit(val ws: Int) extends Component {
  val io = new Bundle {
    val p = in UInt (ws bits)
    val q = in UInt (ws bits)
    val op = in UInt (4 bits)
    val res = out UInt (ws bits)
  }

  for (bit <- 0 until ws) {
    io.res(bit) := io.op(
      io.q(bit).asUInt(2 bits) | (io.p(bit).asUInt(1 bits) << 1)
    )
  }
}

object ArithOps extends SpinalEnum {
  val l_add, l_sub, l_shl, l_shr, l_asr, l_mul, l_div, l_mod =
    newElement()

}
class ArithUnit(val ws: Int, val fancy: Boolean) extends Module {
  val io = new Bundle {
    val d = in UInt (ws bits)
    val s = in UInt (ws bits)
    val op = in(ArithOps())
    val res = out UInt (ws bits)
  }

  val signed_max = (1 << (ws - 1) - 1)

  switch(io.op) {
    is(ArithOps.l_add) { io.res := io.d + io.s }
    is(ArithOps.l_sub) { io.res := io.d - io.s }
    is(ArithOps.l_shl) { io.res := io.d |<< io.s }
    is(ArithOps.l_shr) { io.res := io.d |>> io.s }
    is(ArithOps.l_asr) {
      when(io.s < ws) { io.res := io.d >> io.s }
        .otherwise { io.res.setAllTo(io.d(ws - 1)) }
    }
    is(ArithOps.l_mul) {
      if (fancy) { io.res := (io.d * io.s).resize(ws bits) }
    }
    is(ArithOps.l_div) {
      when(io.s =/= 0) { if (fancy) { io.res := io.d / io.s } }.otherwise {
        io.res.setAll()
      }
    }
    is(ArithOps.l_mod) {
      when(io.s =/= 0) { if (fancy) { io.res := io.d % io.s } }.otherwise {
        io.res := 0
      }
    }
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
  }

  val eq = io.eq && io.d === io.s
  val gt =
    (io.gt && io.sn && (io.d.asSInt > io.s.asSInt) || (io.gt && !io.sn && io.d > io.s))
  val cnd = eq || gt
  when(io.iv) { io.res := !cnd }.otherwise { io.res := cnd }
}

object Stages extends SpinalEnum {
  val s_idle, s_fetch, s_execute, s_writeback = newElement()
}

class CPU(val ws: Int, val fancy: Boolean) extends Module {
  val Word = UInt(ws bits)
  val Insn = UInt(16 bits)
  val io = new Bundle {
    val insn_addr = master Stream (Word)
    val insn_content = slave Stream (Insn)

    val mem_addr = master Stream (Word)
    val read_port = slave Stream (Word)
    val write_port = master Stream (Word)
    val mem_is_write = out Bool ()

    val halt = in Bool ()
  }

  val dbg = new Bundle {
    val reg = slave Stream (UInt(4 bits))
    val reg_content = master Stream (UInt(ws bits))

    val cur_stage = out(Stages())
  }

  val state = RegInit(Stages.s_idle)

  val arith = new ArithUnit(ws, fancy)
  val logic = new LogicUnit(ws)
  val compare = new ComparisonUnit(ws)

  arith.io.d := 0
  arith.io.s := 0
  arith.io.op := ArithOps.l_add

  logic.io.p.assignDontCare()
  logic.io.q.assignDontCare()
  logic.io.op.assignDontCare()

  compare.io.d.assignDontCare()
  compare.io.s.assignDontCare()
  compare.io.iv.assignDontCare()
  compare.io.gt.assignDontCare()
  compare.io.eq.assignDontCare()
  compare.io.sn.assignDontCare()

  val regs = Vec(Reg(UInt(ws bits)), 16)

  val pc_reg = 0

  val pc = Reg(Word)
  val inst = Reg(UInt(16 bits))

  val dl = Reg(UInt(4 bits))
  val sp = Reg(UInt(4 bits))

  io.insn_addr.valid := False
  io.insn_addr.payload := 0
  io.insn_content.ready := False

  io.mem_addr.valid := False
  io.mem_addr.payload := 0
  io.write_port.valid := False
  io.write_port.payload := 0
  io.mem_is_write := False
  io.read_port.ready := False
  dbg.reg.ready := True
  dbg.cur_stage := state

  when(dbg.reg.valid) {
    dbg.reg_content.payload := regs(dbg.reg.payload)
    dbg.reg_content.valid := True
  }.otherwise {
    dbg.reg_content.payload.assignDontCare()
    dbg.reg_content.valid := False
  }

  when(!io.halt && state === Stages.s_idle) {
    state := Stages.s_fetch
  }

  when(state === Stages.s_fetch) {
    pc := regs(pc_reg) + 2
    io.insn_addr.payload := regs(pc_reg)
    io.insn_addr.valid := True
    io.insn_content.ready := True
    when(io.insn_content.valid) {
      inst := io.insn_content.payload
      io.insn_addr.valid := False
      io.insn_content.ready := False
      state := Stages.s_execute
    }
  }.otherwise {
    io.insn_content.ready := False
    io.insn_addr.valid := False
  }

  val result = Reg(Word)

  result := 0

  when(state === Stages.s_execute) {
    dl := inst(3 downto 0)
    sp := inst(7 downto 4)

    printf(s"executing ${inst}, pc = ${regs(0)}\n")

    when(inst(15 downto 14) === 1) {
      // data transfer
      val dindir = inst(10)
      val dmode = inst(9 downto 8)
      val sindir = inst(13)
      val smode = inst(12 downto 11)

      val scont = Reg(Word) init (0)
      val sready = Reg(Bool()) init (False)
      val dready = Reg(Bool()) init (False)
      val pc_stowed = Reg(Bool()) init (False)

      regs(pc_reg) := pc
      pc_stowed := True

      when(pc_stowed) {
        dready := False
        sready := False
        when(sindir) {
          val pres = Reg(Bool()) init (False)
          val sc = 0
          when(smode === 2) {
            regs(sp) := regs(sp) - (ws / 8); sc := regs(sp) - (ws / 8)
          } otherwise { sc := regs(sp) }
          io.mem_addr.valid := True
          io.mem_addr.payload := sc
          io.read_port.ready := True
          io.mem_is_write := False
          when(io.read_port.valid) {
            switch(smode) {
              is(1) { regs(sp) := regs(sp) + (ws / 8) }
              is(3) { regs(sp) := regs(sp) - (ws / 8) }
            }
            scont := io.read_port.payload
            sready := True
          }
        }.otherwise {
          switch(smode) {
            is(2) { scont := regs(sp) - (ws / 8) }
            is(0) { scont := regs(sp) }
            is(3) { scont := regs(sp); regs(sp) := regs(sp) - (ws / 8) }
            is(1) { scont := regs(sp); regs(sp) := regs(sp) + (ws / 8) }
          }
          sready := True
        }
        when(sready) {
          io.mem_addr.valid := False
          io.read_port.ready := False
          when(dindir) {
            val daddr = 0
            when(dmode === 2) {
              regs(dl) := regs(dl) - (ws / 8); daddr := regs(dl) - (ws / 8)
            } otherwise { daddr := regs(dl) }
            io.mem_addr.valid := True
            io.mem_addr.payload := daddr
            io.write_port.valid := True
            io.mem_is_write := True
            io.write_port.payload := scont
            when(io.write_port.ready) {
              switch(dmode) {
                is(1) { regs(dl) := regs(dl) + (ws / 8) }
                is(3) { regs(dl) := regs(dl) - (ws / 8) }
              }

              dready := True
            }
          }.otherwise {
            switch(dmode) {
              is(0) { regs(dl) := scont }
              is(1) { regs(dl) := scont + (ws / 8) }
              is(2) { regs(dl) := scont - (ws / 8) }
              is(3) { regs(dl) := scont - (ws / 8) }
            }
            dready := True
          }
        }

        when(dready) {
          io.write_port.valid := False
          io.mem_addr.valid := False
          io.mem_is_write := False
          pc_stowed := False
          when(io.halt) {
            state := Stages.s_idle
          }.otherwise {
            state := Stages.s_fetch
          }
        }
      }
    }.otherwise {
      switch(inst(15 downto 12)) {
        is(1) {
          logic.io.q := regs.read(dl)
          logic.io.p := regs.read(sp)
          logic.io.op := inst(11 downto 8)
          result := logic.io.res
          printf(s"logic op=${inst(11 downto 8)}\n")
        }
        is(2) {
          arith.io.d := regs.read(dl)
          arith.io.s := regs.read(sp)
          arith.io.op.assignFromBits(inst(10 downto 8).asBits)
          result := arith.io.res
        }
        is(3) {
          compare.io.d := regs.read(dl)
          compare.io.s := regs.read(sp)
          compare.io.eq := inst(8)
          compare.io.gt := inst(9)
          compare.io.sn := inst(10)
          compare.io.iv := inst(11)
          result := compare.io.res.asUInt(ws bits)
        }
        is(8) {
          // conditional
          val offset = inst(7 downto 0)
          val cmp = inst(11 downto 8)
          when(regs(cmp) =/= 0) {
            pc := (pc.asSInt + offset.resize(ws bits).asSInt).asUInt
          }
          result := regs.read(dl) // ew
        }
        is(9) {
          result := pc
          pc := regs.read(sp)
        }
      }
      state := Stages.s_writeback
    }

  }

  regs.allowOverride
  when(state === Stages.s_writeback) {
    dl := inst(3 downto 0)
    printf(
      s"writing back $inst, dl = $dl, jump to $pc, units logic=${logic.io.res}, arith=${arith.io.res}, compare=${compare.io.res}\n"
    )
    regs.write(dl, result)
    regs.write(pc_reg, pc)
    when(io.halt) {
      state := Stages.s_idle
    }.otherwise {
      state := Stages.s_fetch
    }
  }
}

object Icestick {
  def main(args: Array[String]) {
    SpinalVerilog(new CPU(4, false))
  }
}

//Generate the MyTopLevel's VHDL
object MyTopLevelVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CPU(16, true))
  }
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object MySpinalConfig
    extends SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)
    )

//Generate the MyTopLevel's Verilog using the above custom configuration.
object MyTopLevelVerilogWithCustomConfig {
  def main(args: Array[String]) {
    MySpinalConfig.generateVerilog(new CPU(4, false))
  }
}
