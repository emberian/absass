package absass

import spinal.core._
import spinal.lib._

import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

/** Memory-only platform. */
class TrivialPlat(val ws: Int, val memsz: Int) extends Component {
  val io = slave(CPUIO(ws))

  val cpu = io
  val mem = Mem(UInt(ws bits), memsz)
  mem.setTechnology(ramBlock)

  val insn_addr = Reg(UInt(ws bits))
  val mem_addr = Reg(UInt(ws bits))

  val res = Reg(UInt(ws bits))
  val insn_res = Reg(UInt(16 bits))

  val addrbits = log2Up(memsz)

  cpu.insn_content.valid := False
  cpu.insn_content.payload.assignDontCare()
  cpu.read_port.valid := False
  cpu.read_port.payload.assignDontCare()
  cpu.write_port.ready := False

  val bus_switch = new StateMachine {
    val watch: State = new State with EntryPoint {
      whenIsActive {
        insn_addr := cpu.insn_addr.payload
        mem_addr := cpu.mem_addr.payload

        when(cpu.insn_addr.valid) {
          goto(fetch_insn)
        } otherwise {
          when(cpu.mem_addr.valid) {
            goto(do_mem)
          }
        }
      }
    }

    val fetch_insn: State = new State {
      whenIsActive {
        cpu.insn_content.valid := True
        cpu.insn_content.payload := insn_res
        when(cpu.insn_content.ready) {
          goto(watch)
        }
      }
    }
    val do_mem: State = new State {
      whenIsActive {
        when(cpu.mem_is_write) {
          cpu.write_port.ready := True
          when(cpu.write_port.valid) {
            goto(watch)
          }
        } otherwise {
          cpu.read_port.valid := True
          cpu.read_port.payload := res
          when(cpu.read_port.ready) {
            goto(watch)
          }
        }
      }
    }
  }
  bus_switch.setEncoding(binaryOneHot)

  mem.readSyncMixedWidth(
    insn_addr.resize(if (16 == ws) addrbits else addrbits + 1),
    insn_res,
    bus_switch.isEntering(bus_switch.fetch_insn)
  )

  res := mem.readWriteSyncMixedWidth(
    mem_addr.resize(log2Up(memsz)),
    cpu.write_port.payload,
    !bus_switch.isEntering(bus_switch.fetch_insn),
    cpu.mem_is_write
  )
}
