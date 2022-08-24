package absass

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

object RegOps extends SpinalEnum(binaryOneHot) {
  val skip, dl_sp_access, dl_sp_writeback, sr_access, sr_writeback, cond_access,
      other_writeback, pc_writeback =
    newElement()
}

case class RegBus(ws: Int) extends Bundle with IMasterSlave {
  val sp = UInt(4 bits)
  val dl = UInt(4 bits)
  val sp_v = UInt(ws bits)
  val dl_v = UInt(ws bits)
  val sr_v = UInt(ws bits)
  val cond = UInt(ws bits)

  val pc = UInt(ws bits)

  val op = RegOps()
  val go = Bool()
  val busy = Bool()

  val reg_addr = UInt(4 bits)
  val sr_addr = UInt(4 bits)
  val sr_out = UInt(ws bits)
  val sp_out = UInt(ws bits)
  val dl_out = UInt(ws bits)
  val other = UInt(ws bits)

  val reg_r = UInt(ws bits)

  def asMaster() = {
    in(sp, dl, pc, op, go, reg_addr, sr_addr, sr_out, sp_out, dl_out, other)
    out(sp_v, dl_v, sr_v, cond, busy, reg_r)
  }
}

class RegAccess(ws: Int) extends Component {
  val regs = Mem(UInt(ws bits), 16) init (Vec(U(0, ws bits), 16))

  val io = master(RegBus(ws))

  val reg_addr = UInt(4 bits)

  val reg_w = UInt(ws bits)
  val reg_r = UInt(ws bits)
  val wren = Bool()
  val rgen = Bool()

  reg_addr.assignDontCare()
  reg_w.assignDontCare()
  reg_r.assignDontCare()
  wren := False
  rgen := False
  io.busy := True
  val reg_r_wire = regs.readWriteSync(reg_addr, reg_w, rgen, wren)


  io.sp_v.setAsReg()
  io.dl_v.setAsReg()
  io.sr_v.setAsReg()
  io.cond.setAsReg()

  io.reg_r.assignDontCare()
  val sp_out = Reg(UInt(ws bits))
  val dl_out = Reg(UInt(ws bits))
  val reg_access = new StateMachine {
    val watch: State = new State with EntryPoint {
      whenIsActive {
        io.busy := False
        import RegOps._
        wren := False
        rgen := True
        reg_addr := io.reg_addr
        io.reg_r := reg_r_wire
        sp_out := io.sp_out
        dl_out := io.dl_out
        when(io.go.rise) {
          switch(io.op) {
            is(dl_sp_access) {
              goto(rd_dl_sp)
            }
            is(dl_sp_writeback) {
              goto(wr_dl_sp)
            }
            is(sr_access) {
              goto(rd_sr)
            }
            is(sr_writeback) {
              goto(wr_sr)
            }
            is(cond_access) {
              goto(rd_cond)
            }
            is(other_writeback) {
              goto(wr_other)
            }
            is(pc_writeback) {
              goto(wr_pc)
            }
          }
        }
      }
    }
    val rd_dl_sp: State = new State {
      val didsp = RegInit(False)
      val diddl = RegInit(False)
      whenIsActive {
        rgen := True
        when(!didsp) {
          reg_addr := io.sp
          didsp := True
        } otherwise {
          when(!diddl) {
            io.sp_v := reg_r
            reg_addr := io.dl
            didsp := True
          } otherwise {
            io.dl_v := reg_r
            didsp := False
            diddl := False
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

        when(!didsp) {
          reg_addr := io.sp
          reg_w := sp_out
          didsp := True
        } otherwise {
          when(!diddl) {
            reg_addr := io.dl
            reg_w := dl_out
            diddl := True
          } otherwise {
            didsp := False
            diddl := False
            goto(watch)
          }
        }
      }
    }

    val rd_sr = new State {
      whenIsActive {
        Alt(
          { () =>
            rgen := True
            reg_addr := io.sr_addr
          },
          { () =>
            io.sr_v := reg_r
            goto(watch)
          }
        )
      }
    }
    val wr_sr = new State {
      whenIsActive {
        wren := True; rgen := True; reg_addr := io.sr_addr; reg_w := io.sr_out
        goto(watch)
      }
    }
    val rd_cond = new State {
      whenIsActive {
        Alt(
          { () =>
            reg_addr := io.reg_addr
            rgen := True
          },
          { () =>
            io.cond := reg_r
            goto(watch)
          }
        )
      }
    }
    val wr_other = new State {
      whenIsActive {
        wren := True; rgen := True; reg_addr := io.dl; reg_w := io.other
        goto(watch)
      }
    }
    val wr_pc = new State {
      whenIsActive {
        wren := True; rgen := True; reg_addr := 0; reg_w := io.pc
        goto(watch)
      }
    }
  }

  reg_access.setEncoding(binaryOneHot)
}

import RegOps._
object RegSim {
  def main(args: Array[String]) {

    SimConfig.withWave.doSim(new RegAccess(16)) { regs =>
      regs.clockDomain.forkStimulus(period = 2)

      regs.clockDomain.waitRisingEdge()
      regs.io.dl #= 1
      regs.io.other #= 2
      regs.io.go #= true
      regs.io.op #= other_writeback
      regs.clockDomain.waitRisingEdge()
      regs.io.go#=false
      waitUntil(!regs.io.busy.toBoolean)
      regs.io.reg_addr #= 1
      regs.clockDomain.waitRisingEdge(2)
      assert(regs.io.reg_r.toBigInt == 2)
    }
  }
}