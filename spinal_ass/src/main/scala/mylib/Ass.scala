package absass

import spinal.core._
import spinal.lib._

import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import spinal.core.sim.SimPublic

//Hardware definition
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

object Stages extends SpinalEnum {
  val s_idle, s_fetch, s_execute, s_writeback = newElement()
  defaultEncoding = binaryOneHot
}
import Stages._
import RegOps._

class CPU(val ws: Int, val fancy: Boolean) extends Module {
  val WB = ws / 8
  val io = new Bundle {
    val io = master(CPUIO(ws))
    val dbg = master(DebugPort(ws))
    val regs = slave(RegBus(ws))
  } simPublic()
  val dbg = io.dbg
  val regs = io.regs
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
  logic.io.go := False

  compare.io.d.assignDontCare()
  compare.io.s.assignDontCare()
  compare.io.iv.assignDontCare()
  compare.io.gt.assignDontCare()
  compare.io.eq.assignDontCare()
  compare.io.sn.assignDontCare()
  compare.io.go := False

  regs.sp.assignDontCare()
  regs.dl.assignDontCare()
  regs.reg_addr.assignDontCare()
  regs.sr_addr.assignDontCare()
  regs.sp_out.assignDontCare()
  regs.dl_out.assignDontCare()
  regs.other.assignDontCare()
  regs.op.assignDontCare()
  regs.go := False
  regs.pc.assignDontCare()

  val sr_out = Reg(UInt(ws bits))
  regs.sr_out := sr_out

  val pc_reg = 0

  val pc = Reg(UInt(ws bits))
  val inst = Reg(UInt(16 bits))

  val dl = Reg(UInt(4 bits))
  val sp = Reg(UInt(4 bits))
  regs.dl := dl
  regs.sp := sp
  val dl_v = Reg(UInt(ws bits))
  val sp_v = Reg(UInt(ws bits))

  io.io.insn_addr.valid := False
  io.io.insn_addr.payload := 0
  io.io.insn_content.ready := False

  io.io.mem_addr.valid := False
  io.io.mem_addr.payload := 0
  io.io.write_port.valid := False
  io.io.write_port.payload := 0
  io.io.mem_is_write := False
  io.io.read_port.ready := False
  dbg.cur_stage.setAsReg()

  /** Architectural registers */
  val ivt = Reg(UInt(ws bits)) init (0)
  val halt = RegInit(True)
  val inst_ret = Reg(UInt(ws bits)) init (0)
  val cycl_ct = Reg(UInt(ws bits)) init (0)

  cycl_ct := cycl_ct + 1
  // dbg.regs.assignDontCare()
  when(dbg.halt) { halt := dbg.halt }
  when(dbg.halt.fall) {
    halt := False
  }

  val ctrl = new StateMachine {
    val idle: State = new State with EntryPoint {
      onEntry { dbg.cur_stage := s_idle }
      whenIsActive {
        regs.reg_addr := pc_reg
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
        whenIsActive {
          dbg.cur_stage := s_fetch
          pc := regs.reg_r + 2
          io.io.insn_addr.payload := regs.reg_r
          io.io.insn_addr.valid := True
          io.io.insn_content.ready := True
          inst := io.io.insn_content.payload
          when(io.io.insn_content.valid) {
            dl := io.io.insn_content.payload(3 downto 0)
            sp := io.io.insn_content.payload(7 downto 4)
            io.io.insn_addr.valid := False
            io.io.insn_content.ready := False
            goto(execute)
          }
        }
      }
      val did_other = RegInit(False)
      val other_res = Reg(UInt(ws bits))

      when(did_other) {
        regs.op := other_writeback
        regs.go := True
        regs.other := other_res
        when(regs.busy.fall) {
          did_other := False
        }
      }
      val dl_cp = Reg(UInt(ws bits))
      val sp_cp = Reg(UInt(ws bits))

      val execute: State = new StateFsm({

        val exec_ctrl = new StateMachine {
          val wait_regs: State = new State with EntryPoint {
            whenIsActive {
              regs.op := dl_sp_access
              regs.go := True

              when(regs.busy.fall) {
                dl_v := regs.dl_v
                sp_v := regs.sp_v
                dl_cp := regs.dl_v
                sp_cp := regs.sp_v

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
          }

          val do_xf: State = new StateFsm({
            val xf_ctrl = new StateMachine {
              val dindir = inst(10)
              val dmode = inst(9 downto 8)
              val sindir = inst(13)
              val smode = inst(12 downto 11)
              val dl_in = Reg(UInt(ws bits))
              val sp_in = Reg(UInt(ws bits))
              val sp_inc = sp_cp + WB
              val sp_dec = sp_cp - WB
              val dl_inc = dl_cp + WB
              val dl_dec = dl_cp - WB

              val start: State = new State with EntryPoint {
                whenIsActive {
                  regs.op := pc_writeback
                  regs.go := True
                  dl_in := dl_cp
                  sp_in := sp_cp

                  switch(smode) {
                    is(0) { sp_cp_out := sp_cp }
                    is(1) { sp_cp_out := sp_inc }
                    is(2) { sp_in := sp_dec; sp_cp_out := sp_dec }
                    is(3) { sp_cp_out := sp_dec }
                  }

                  switch(dmode) {
                    is(0) { dl_cp_out := dl_cp }
                    is(1) { dl_cp_out := dl_inc }
                    is(2) {
                      dl_in := dl_dec; dl_cp_out := dl_dec
                    }
                    is(3) { dl_cp_out := dl_dec }
                  }

                  when(sindir) {
                    goto(src_mem)
                  } otherwise {
                    goto(src_reg)
                  }
                }
              }
              val srcval = Reg(UInt(ws bits))

              val sp_cp_out = Reg(UInt(ws bits))
              val dl_cp_out = Reg(UInt(ws bits))

              val src_mem: State = new State {
                whenIsActive {
                  when(sp === dl) {
                    // there is a hazard that we can correct this cycle
                    // before it can be observed. refresh the dl registers
                    // to reflect an input of sp_in.
                    dl_in := sp_in
                    switch(dmode) {
                      is(0) { dl_cp_out := sp_in }
                      is(1) { dl_cp_out := sp_in + WB }
                      is(2) { dl_in := dl_dec; dl_cp_out := sp_in - WB }
                      is(3) { dl_cp_out := sp_in - WB }
                    }
                  }
                  io.io.mem_addr.valid := True
                  io.io.mem_addr.payload := sp_in
                  io.io.read_port.ready := True
                  io.io.mem_is_write := False
                  when(io.io.read_port.valid) {
                    srcval := io.io.read_port.payload

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
                  srcval := sp_in
                  when(sp === dl) {
                    // there is a hazard that we can correct this cycle
                    // before it can be observed. refresh the dl registers
                    // to reflect an input of sp_in.
                    dl_in := sp_in
                    switch(dmode) {
                      is(0) { dl_cp_out := sp_in }
                      is(1) { dl_cp_out := sp_in + WB }
                      is(2) { dl_in := dl_dec; dl_cp_out := sp_in - WB }
                      is(3) { dl_cp_out := sp_in - WB }
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

                whenIsActive {
                  io.io.mem_addr.valid := True
                  io.io.mem_addr.payload := dl_in
                  io.io.write_port.valid := True
                  io.io.mem_is_write := True
                  io.io.write_port.payload := srcval
                  when(io.io.write_port.ready) {
                    goto(finish)
                  }
                }
              }

              val dst_reg = new State {
                whenIsActive {
                  switch(dmode) {
                    is(0) { dl_cp_out := srcval }
                    is(1) { dl_cp_out := srcval + (ws / 8) }
                    is(2) { dl_cp_out := srcval }
                    is(3) { dl_cp_out := srcval - (ws / 8) }
                  }
                  goto(finish)
                }
              }
              val finish: State = new State {
                // TODO: can this cycle be skipped?
                // i think the nested exits might cause enough
                // time to pass such that we don't need to pause here.
                whenIsActive {
                  regs.dl_out := dl_cp_out
                  regs.sp_out := sp_cp_out
                  regs.op := dl_sp_writeback
                  regs.go := True
                  exit()
                }
              }
            }
            xf_ctrl.setEncoding((binaryOneHot))
            xf_ctrl

          }) {
            whenCompleted {
              when(!regs.busy) { exit() }
            }
          }

          val sr_dst = inst(11 downto 8)

          val do_sr: State = new State {
            val wr_sr = inst(12)
            whenIsActive {
              regs.sr_addr := sr_dst
              regs.op := sr_access
              regs.go := True

              when(regs.busy.fall) {
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
                      ivt := regs.sr_v
                    } otherwise {
                      sr_out := ivt
                    }
                  }
                  is(3) {
                    when(wr_sr) {
                      halt := regs.sr_v =/= 0
                    }
                  }
                  is(4) {
                    when(wr_sr) {
                      inst_ret := regs.sr_v
                    }.otherwise { sr_out := inst_ret }
                  }
                  is(5) {
                    when(wr_sr) {
                      cycl_ct := regs.sr_v
                    }.otherwise { sr_out := cycl_ct }
                  }
                  is(6) {
                    when(!wr_sr) {
                      sr_out := U(234, ws bits)
                      // FIXME? 240MHz / 1024Hz = 234.375, 0.1% error
                    }
                  }
                }
                goto(flush)
              }
            }
          }

          val flush: State = new State {
            whenIsActive {
              regs.go := True
              regs.sr_addr := sr_dst
              regs.op := sr_writeback
              regs.sr_out := sr_out
              exit()
            }
          }

          val logic_res = Reg(UInt(ws bits))
          val compare_res = Reg(UInt(ws bits))
          val arith_res = Reg(UInt(ws bits))

          val others: State = new State {
            val cond_src = inst(11 downto 8)
            whenIsActive {
              regs.op := cond_access
              regs.go := True
              regs.reg_addr := cond_src

              switch(inst(15 downto 12)) {
                is(1) {
                  logic.io.q := dl_v
                  logic.io.p := sp_v
                  logic.io.op := inst(11 downto 8)
                  logic.io.go := True
                  when(logic.io.rdy) {
                    logic_res := logic.io.res
                    printf(s"logic op=${inst(11 downto 8)}\n")
                    goto(logic_done)
                  }
                }
                is(2) {
                  arith.io.d := dl_v
                  arith.io.s := Mux(inst(11), sp, sp_v)
                  arith.io.op.assignFromBits(inst(10 downto 8).asBits)
                  arith.io.go := True
                  when(arith.io.ready) {
                    arith_res := arith.io.res
                    goto(logic_done)
                  }
                }
                is(3) {
                  compare.io.d := dl_v
                  compare.io.s := sp_v
                  compare.io.eq := inst(8)
                  compare.io.gt := inst(9)
                  compare.io.sn := inst(10)
                  compare.io.iv := inst(11)
                  compare.io.go := True
                  when(compare.io.rdy) {
                    compare_res := compare.io.res.asUInt(ws bits)
                    goto(compare_done)
                  }
                }
                is(8) {
                  // conditio.ional
                  val offset = inst(7 downto 0)
                  when(regs.busy.fall) {
                    when(regs.cond =/= 0) {
                      pc := (pc.asSInt + offset.resize(ws bits).asSInt).asUInt
                    }
                    did_other := True
                    other_res := dl_v
                    exit()
                  }
                }
                is(9) {
                  // JAL
                  did_other := True
                  other_res := pc
                  pc := sp_v
                  exit()
                }
                is(12) {
                  did_other := True
                  other_res := inst(11 downto 4).resized
                  exit()
                }
              }
            }
          }

          val logic_done = new State {
            whenIsActive {
              other_res := logic_res
              did_other := True
              exit()
            }
          }
          val arith_done = new State {
            whenIsActive {
              other_res := arith_res
              did_other := True
              exit()
            }
          }
          val compare_done = new State {
            whenIsActive {
              other_res := compare_res
              did_other := True
              exit()
            }
          }

        }

        exec_ctrl.setEncoding(binaryOneHot)
        exec_ctrl
      }) {
        whenIsNext { dbg.cur_stage := s_execute }

        whenCompleted {
          when(!regs.busy) {
            goto(instret)
          }
        }
      }

      val instret: State = new State {
        whenIsNext { dbg.cur_stage := s_writeback }
        whenIsActive {
          inst_ret := inst_ret + 1
          regs.op := pc_writeback
          regs.go := True
          when(regs.busy.fall) {
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
  ctrl.setEncoding(binaryOneHot)
}
