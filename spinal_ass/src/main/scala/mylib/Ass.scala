package absass

import spinal.core._
import spinal.lib._

import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import spinal.core.sim.SimPublic

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
  }

  val eq = io.eq && io.d === io.s
  val gt =
    (io.gt && io.sn && (io.d.asSInt > io.s.asSInt) || (io.gt && !io.sn && io.d > io.s))
  val cnd = eq || gt
  when(io.iv) { io.res := !cnd }.otherwise { io.res := cnd }
}

object Stages extends SpinalEnum {
  val s_idle, s_fetch, s_execute, s_writeback = newElement()
  defaultEncoding = binaryOneHot
}
import Stages._

case class DebugPort(ws: Int) extends Bundle with IMasterSlave {
  val reg_addr = UInt(4 bits)
  val reg_r = UInt(ws bits)
  val reg_w = UInt(ws bits)
  val wren = Bool()
  val rgen = Bool()
  val cur_stage = Stages()
  val insn = UInt(16 bits)
  val sstep_insn = Bool()
  val sstep = Bool()

  val halt = Bool()

  override def asMaster(): Unit = {
    out(cur_stage, reg_r)
    in(halt, insn, sstep_insn, sstep, reg_addr, reg_w, wren, rgen)
  }
}

case class CPUIO(val ws: Int) extends Bundle with IMasterSlave {
  val insn_addr = Flow(UInt(ws bits))
  val insn_content = Stream(UInt(16 bits))

  val mem_addr = Flow(UInt(ws bits))
  val read_port = Stream(UInt(ws bits))
  val write_port = Stream(UInt(ws bits))
  val mem_is_write = Bool()

  override def asMaster(): Unit = {
    out(mem_is_write)
    master(insn_addr, mem_addr, write_port)
    slave(insn_content, read_port)
  }
}

class CPU(val ws: Int, val fancy: Boolean) extends Module {
  val io = master(CPUIO(ws))
  val dbg = master(DebugPort(ws))

  val arith = new ArithUnit(ws, fancy)
  val logic = new LogicUnit(ws)
  val compare = new ComparisonUnit(ws)

  arith.io.d.assignDontCare()
  arith.io.s.assignDontCare()
  arith.io.op.assignDontCare()
  arith.io.go := False

  logic.io.p.assignDontCare()
  logic.io.q.assignDontCare()
  logic.io.op.assignDontCare()

  compare.io.d.assignDontCare()
  compare.io.s.assignDontCare()
  compare.io.iv.assignDontCare()
  compare.io.gt.assignDontCare()
  compare.io.eq.assignDontCare()
  compare.io.sn.assignDontCare()

  val pc_reg = 0

  val pc = Reg(UInt(ws bits))
  val inst = Reg(UInt(16 bits))

  val dl = Reg(UInt(4 bits))
  val sp = Reg(UInt(4 bits))
  val dl_v = Reg(UInt(ws bits))
  val sp_v = Reg(UInt(ws bits))

  io.insn_addr.valid := False
  io.insn_addr.payload := 0
  io.insn_content.ready := False

  io.mem_addr.valid := False
  io.mem_addr.payload := 0
  io.write_port.valid := False
  io.write_port.payload := 0
  io.mem_is_write := False
  io.read_port.ready := False
  dbg.cur_stage.setAsReg()

  /** Architectural registers */
  val ivt = Reg(UInt(ws bits)) init (0)
  val halt = RegInit(True)
  val inst_ret = Reg(UInt(ws bits)) init (0)
  val cycl_ct = Reg(UInt(ws bits)) init (0)

  cycl_ct := cycl_ct + 1
  when(dbg.halt) { halt := dbg.halt }
  when(dbg.halt.fall) {
    halt := False
  }

  val regs = Mem(UInt(ws bits), 16) init (Vec(U(0, ws bits), 16))

  val reg_addr = UInt(4 bits)
  val reg_w = UInt(ws bits)
  val reg_r = UInt(ws bits)
  val wren = Bool
  val rgen = Bool

  reg_addr.assignDontCare()
  reg_w.assignDontCare()
  wren := False
  rgen := False

  val reg_r_wire = regs.readWriteSync(reg_addr, reg_w, rgen, wren)

  reg_r := reg_r_wire
  dbg.reg_r := reg_r_wire

  when(halt) {
    reg_w := dbg.reg_w
    wren := dbg.wren
    reg_addr := dbg.reg_addr
    rgen := dbg.rgen
  } otherwise {}

  // register every memory operand because, well, you know,
  val dl_sp_access = RegInit(False)
  val dl_sp_writeback = RegInit(False)
  val sr_access = RegInit(False)
  val sr_writeback = RegInit(False)
  val cond_access = RegInit(False)
  val cond_addr = Reg(UInt(4 bits))
  val sr_addr = Reg(UInt(4 bits))
  val old_dst_val = Reg(UInt(ws bits))
  val sr_out = Reg(UInt(ws bits))
  val cond = Reg(UInt(ws bits))
  val other_res = Reg(UInt(ws bits))
  val other_writeback = RegInit(False)
  val pc_writeback = RegInit(False)

  val reg_access = new StateMachine {
    val watch: State = new State with EntryPoint {
      whenIsActive {
        when(dl_sp_access) {
          goto(rd_dl_sp)
        }
        when(dl_sp_writeback) {
          goto(wr_dl_sp)
        }
        when(sr_access) {
          goto(rd_sr)
        }
        when(sr_writeback) {
          goto(wr_sr)
        }
        when(cond_access) {
          goto(rd_cond)
        }
        when(other_writeback) {
          goto(wr_other)
        }
        when(pc_writeback) {
          goto(wr_pc)
        }
      }
    }

    val rd_dl_sp: State = new State {
      val didsp = RegInit(False)
      val diddl = RegInit(False)
      whenIsActive {
        rgen := True
        when(!diddl) {
          reg_addr := dl
          diddl := True
        } otherwise {
          when(!didsp) {
            dl_v := reg_r
            reg_addr := sp
            didsp := True
          } otherwise {
            sp_v := reg_r
            didsp := False
            diddl := False
            dl_sp_access := False
            goto(watch)
          }
        }
      }
    }

    val wr_dl_sp: State = new State {
      val didsp = RegInit(False)
      val diddl = RegInit(False)
      whenIsActive {
        rgen := True
        wren := True
        when(!diddl) {
          reg_addr := dl
          reg_w := dl_v
          diddl := True
        } otherwise {
          when(!didsp) {
            reg_addr := sp
            reg_w := sp_v
            didsp := True
          } otherwise {
            didsp := False
            diddl := False
            dl_sp_writeback := False
            goto(watch)
          }
        }
      }
    }

    val rd_sr = new State {
      onEntry { rgen := True; reg_addr := sr_addr }
      whenIsActive {
        old_dst_val := reg_r
        sr_access := False
        goto(watch)
      }
    }
    val wr_sr = new State {
      whenIsActive {
        wren := True; rgen := True; reg_addr := sr_addr; reg_w := sr_out
        sr_writeback := False
        goto(watch)
      }
    }
    val rd_cond = new State {
      onEntry { rgen := True; reg_addr := cond_addr }
      whenIsActive {
        cond := reg_r
        cond_access := False
        goto(watch)
      }
    }
    val wr_other = new State {
      onEntry { wren := True; rgen := True; reg_addr := dl; reg_w := other_res }
      whenIsActive {
        other_writeback := False
        goto(watch)
      }
    }
    val wr_pc = new State {
      onEntry { wren := True; rgen := True; reg_addr := pc_reg; reg_w := pc }
      whenIsActive {
        pc_writeback := False
        goto(watch)
      }
    }
  }

  reg_access.setEncoding(binaryOneHot)

  val ctrl = new StateMachine {
    val idle: State = new State with EntryPoint {
      onEntry { dbg.cur_stage := s_idle }
      whenIsActive {
        when(!halt) {
          goto(fetch)
        } otherwise {
          when(dbg.sstep_insn) {
            goto(execute)
            inst := dbg.insn
          } otherwise {
            when(dbg.sstep) {
              goto(fetch)
            }
          }
        }
      }

      val fetch = new State {
        onEntry { reg_addr := pc_reg }
        whenIsActive {
          dbg.cur_stage := s_fetch
          pc := reg_r + 2
          io.insn_addr.payload := reg_r
          io.insn_addr.valid := True
          io.insn_content.ready := True
          inst := io.insn_content.payload
          when(io.insn_content.valid) {
            dl := io.insn_content.payload(3 downto 0)
            sp := io.insn_content.payload(7 downto 4)
            dl_sp_access := True
            io.insn_addr.valid := False
            io.insn_content.ready := False
            goto(execute)
          }
        }
      }
      val did_other = RegInit(False)

      val execute: State = new StateFsm({

        val exec_ctrl = new StateMachine {
          val wait_regs: State = new State with EntryPoint {
            whenIsActive {
              when(!dl_sp_access) {
                goto(decode)
              }
            }
          }
          val decode: State = new State {
            whenIsActive {
              when(inst(15 downto 14) === 1) {
                goto(do_xf)
              }.otherwise {
                when(inst(15 downto 13) === 5) {
                  goto(do_sr)
                }.otherwise {
                  goto(others)
                }
              }
            }
          }

          val do_xf: State = new StateFsm({
            val xf_ctrl = new StateMachine {
              val dindir = inst(10)
              val dmode = inst(9 downto 8)
              val sindir = inst(13)
              val smode = inst(12 downto 11)
              val dl_cp = Reg(UInt(ws bits))
              val sp_cp = Reg(UInt(ws bits))
              val start: State = new State with EntryPoint {
                onEntry { pc_writeback := True }
                whenIsActive {
                  dl_cp := dl_v
                  sp_cp := sp_v
                  when(sindir) {
                    goto(src_mem)
                  } otherwise {
                    goto(src_reg)
                  }
                }
              }
              val srcval = Reg(UInt(ws bits))
              val src_mem: State = new State {
                onEntry {
                  when(smode === 2) {
                    sp_cp := sp_v - (ws / 8)
                  }
                }
                onExit {
                  switch(smode) {
                    is(1) { sp_cp := sp_cp + (ws / 8) }
                    is(3) { sp_cp := sp_cp - (ws / 8) }
                  }
                }
                whenIsActive {
                  io.mem_addr.valid := True
                  io.mem_addr.payload := sp_cp
                  io.read_port.ready := True
                  io.mem_is_write := False
                  when(io.read_port.valid) {
                    srcval := io.read_port.payload

                    when(dindir) {
                      goto(dst_mem)
                    } otherwise {
                      goto(dst_reg)
                    }
                  }
                }
              }
              val src_reg: State = new State {
                whenIsActive {
                  switch(smode) {
                    is(2) { srcval := sp_cp - (ws / 8) }
                    is(0) { srcval := sp_cp }
                    is(3) {
                      srcval := sp_cp; sp_cp := sp_cp - (ws / 8)
                    }
                    is(1) {
                      srcval := sp_cp; sp_cp := sp_cp + (ws / 8)
                    }
                  }

                  when(dindir) {
                    goto(dst_mem)
                  } otherwise {
                    goto(dst_reg)
                  }
                }
              }

              val dst_mem: State = new State {
                val daddr = 0

                onEntry {
                  when(dmode === 2) {
                    dl_cp := dl_cp - (ws / 8)
                  }
                }
                onExit {
                  switch(dmode) {
                    is(1) { dl_cp := dl_cp + (ws / 8) }
                    is(3) { dl_cp := dl_cp - (ws / 8) }
                  }
                }
                whenIsActive {
                  io.mem_addr.valid := True
                  io.mem_addr.payload := dl_cp
                  io.write_port.valid := True
                  io.mem_is_write := True
                  io.write_port.payload := srcval
                  when(io.write_port.ready) {
                    goto(finish)
                  }
                }
              }

              val dst_reg = new State {
                whenIsActive {
                  switch(dmode) {
                    is(0) { dl_cp := srcval }
                    is(1) { dl_cp := srcval + (ws / 8) }
                    is(2) { dl_cp := srcval - (ws / 8) }
                    is(3) { dl_cp := srcval - (ws / 8) }
                  }
                  goto(finish)

                }
              }
              val finish: State = new State {
                onEntry {
                  dl_v := dl_cp; sp_v := sp_cp; dl_sp_writeback := True
                }
                // TODO: can this cycle be skipped?
                // i think the nested exits might cause enough
                // time to pass such that we don't need to pause here.
                whenIsActive { when(!dl_sp_writeback) { exit() } }
              }
            }
            xf_ctrl.setEncoding((binaryOneHot))
            xf_ctrl

          }) {
            whenCompleted {
              exit()
            }
          }

          val do_sr: State = new State {
            val dst = inst(11 downto 8)
            val wr_sr = inst(12)

            onEntry { sr_addr := dst; sr_access := True }
            whenIsActive {
              when(!sr_access) {
                switch(inst(7 downto 0)) {
                  is(0) {
                    // TODO: register a number
                    sr_out := U(0, ws bits)
                  }
                  is(1) {
                    sr_out := U(ws, ws bits)
                  }
                  is(2) {
                    when(wr_sr) {
                      ivt := old_dst_val
                    } otherwise {
                      sr_out := ivt
                    }
                  }
                  is(3) {
                    when(wr_sr) {
                      halt := old_dst_val =/= 0
                    }
                  }
                  is(4) {
                    when(wr_sr) {
                      inst_ret := old_dst_val
                    }.otherwise { sr_out := inst_ret }
                  }
                  is(5) {
                    when(wr_sr) {
                      cycl_ct := old_dst_val
                    }.otherwise { sr_out := cycl_ct }
                  }
                  is(6) {
                    when(!wr_sr) {
                      sr_out := U(234, ws bits)
                      // FIXME? 240MHz / 1024Hz = 234.375, 0.1% error
                    }
                  }
                }
                exit()
              }
            }
          }

          val others: State = new State {
            val cond_src = inst(11 downto 8)
            onEntry {
              cond_access := True; cond_addr := cond_src; did_other := True
            }
            whenIsActive {
              switch(inst(15 downto 12)) {
                is(1) {
                  logic.io.q := dl_v
                  logic.io.p := sp_v
                  logic.io.op := inst(11 downto 8)
                  other_res := logic.io.res
                  exit()
                  printf(s"logic op=${inst(11 downto 8)}\n")
                }
                is(2) {
                  arith.io.d := dl_v
                  arith.io.s := Mux(inst(11), sp, sp_v)
                  arith.io.op.assignFromBits(inst(10 downto 8).asBits)
                  arith.io.go := True
                  when(arith.io.ready) {
                    other_res := arith.io.res
                    exit()
                  }
                }
                is(3) {
                  compare.io.d := dl_v
                  compare.io.s := sp_v
                  compare.io.eq := inst(8)
                  compare.io.gt := inst(9)
                  compare.io.sn := inst(10)
                  compare.io.iv := inst(11)
                  other_res := compare.io.res.asUInt(ws bits)
                  exit()
                }
                is(8) {
                  // conditional
                  val offset = inst(7 downto 0)
                  when(!cond_access) {
                    when(cond =/= 0) {
                      pc := (pc.asSInt + offset.resize(ws bits).asSInt).asUInt
                    }
                    other_res := dl_v
                  }
                  exit()
                }
                is(9) {
                  // JAL
                  other_res := pc
                  pc := sp_v
                  exit()
                }
                is(12) {
                  other_res := inst(11 downto 4).resized
                  exit()
                }
              }
            }
          }

          val arith_res = new State {
            whenIsActive {
              other_res := arith.io.res
              exit()
            }
          }
        }

        exec_ctrl.setEncoding(binaryOneHot)
        exec_ctrl
      }) {
        whenIsActive { dbg.cur_stage := s_execute }

        whenCompleted {
          when(did_other) {
            other_writeback := True
            did_other := False
          }
          goto(instret)
        }
      }

      val instret: State = new State {
        onEntry { pc_writeback := True }
        whenIsActive {
          dbg.cur_stage := s_writeback
          inst_ret := inst_ret + 1
          when(!pc_writeback) {
            when(halt) {
              goto(idle)
            }.otherwise {
              goto(fetch)
            }
          }
        }
      }
    }
  }
}
