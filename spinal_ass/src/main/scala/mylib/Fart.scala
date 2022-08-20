package absass

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

object FartCmds extends SpinalEnum {
  val rst, readReg, writeReg, readMem, writeMem, execInsn, step, run =
    newElement()

  defaultEncoding = binaryOneHot
}

import FartCmds._

class Fart(ws: Int) extends Component {
  val Byte = UInt(8 bits)
  val Word = UInt(ws bits)

  val BPW = ws / 8

  val io = new Bundle {
    val rd = master Stream (Byte)
    val wr = slave Stream (Byte)
    val txd = out Bool ()
    val rxd = in Bool ()
    val synced = out Bool ()
    val txClk = out Bool ()
    val rxClk = out Bool ()
    val noticeMeSenpai = out Bool ()

    val ass = slave(CPUIO(ws))
  }

  io.ass.dbg.reg.valid := False
  io.ass.dbg.reg.payload := 0x0
  io.ass.dbg.reg_content.ready := False
  io.ass.dbg.reg_write := 0x0
  io.ass.dbg.reg_dir_out := False
  io.ass.dbg.sstep_insn := False

  io.ass.insn_addr.ready := False
  io.ass.insn_content.payload := 0x0
  io.ass.insn_content.valid := False
  io.ass.mem_addr.ready := False
  io.ass.read_port.valid := False
  io.ass.read_port.payload := 0x0
  io.ass.write_port.ready := False

  io.ass.halt.setAsReg() init (True)

  val uart = new Uart
  io.txClk := uart.dbg.txClk
  io.rxClk := uart.dbg.rxClk

  io.synced.setAsReg() init (False)
  io.noticeMeSenpai.setAsReg() init (False)

  uart.io.rxd := io.rxd
  io.txd := uart.io.txd

  val payload = Reg(Byte) init (0)
  uart.io.wr.valid := False
  uart.io.wr.payload := payload
  uart.io.rd.ready := False

  io.rd.valid := False
  io.rd.payload := 0x0
  io.wr.ready := False

  val m = new StateMachine {
    val scream: State = new State with EntryPoint {
      whenIsActive {
        io.synced := False
        uart.io.rd.ready := True
        when(uart.io.rd.valid) {
          handshake_fifo.io.push.valid := True
          bytes_seen := 1
          goto(handshake)
        }
      }
    }

    val handshake_fifo = StreamFifo(UInt(8 bits), 4)
    val magic = Vec(U(80), U(65), U(82), U(67))
    val magic2 = Vec(U(80), U(65), U(82), U(67))
    val bytes_seen = Reg(UInt(2 bits))
    val compare_cursor = Reg(UInt(2 bits))

    handshake_fifo.io.push.valid := False
    handshake_fifo.io.pop.ready := False
    handshake_fifo.io.push.payload := uart.io.rd.payload

    val handshake: State = new State {
      whenIsActive {
        uart.io.rd.ready := True
        when(uart.io.rd.valid) {
          handshake_fifo.io.push.valid := True
          bytes_seen := bytes_seen + 1
          when(bytes_seen === 3) {
            compare_cursor := 0
            goto(compare_pop)
          }
        }
      }
    }

    val failing_value = RegInit(U(0, 8 bits))
    val latched = Reg(Byte) init (0)
    val compare_got = Reg(Byte)
    val compare_pop: State = new State {
      whenIsActive {
        handshake_fifo.io.pop.ready := True
        when(handshake_fifo.io.pop.valid) {
          failing_value := handshake_fifo.io.pop.payload
          goto(compare_ram)
        }
      }
    }

    val compare_ram: State = new State {
      whenIsActive {
        compare_got := magic(compare_cursor).resized
        payload := compare_cursor.resize(8)
        goto(compare_wr)
      }
    }

    val compare_wr: State = new State {
      whenIsActive {
        compare_cursor := compare_cursor + 1
        when(compare_got =/= failing_value) {
          io.noticeMeSenpai := True
          uart.io.wr.valid := True
          goto(fail)
        } otherwise {
          when(compare_cursor === 3) {
            ack_cursor := 3
            goto(ack_ram)
          } otherwise {
            goto(compare_pop)
          }
        }
      }
    }

    val ack_cursor = Reg(UInt(2 bits))
    val ack_got = Reg(Byte)

    val ack_ram: State = new State {
      whenIsActive {
        payload := magic2(ack_cursor).resized
        goto(ack)
      }
    }

    val ack: State = new State {
      whenIsActive {
        uart.io.wr.valid := True
        ack_cursor := ack_cursor - 1

        when(uart.io.wr.ready) {
          when(ack_cursor === 0) {
            io.synced := True
            goto(find_cmd)
          } otherwise {
            goto(ack_ram)
          }
        }
      }
    }

    val fail: State = new State {
      whenIsActive {
        handshake_fifo.io.pop.ready := True
        uart.io.wr.valid := True
        uart.io.wr.payload := failing_value
        when(uart.io.wr.ready && !handshake_fifo.io.pop.valid) {
          goto(scream)
        }
      }
    }

    val cmd = Reg(FartCmds()) init (rst)

    val find_cmd: State = new State {
      whenIsActive {
        uart.io.rd.ready := True
        when(uart.io.rd.valid) {
          cmd.assignFromBits(uart.io.rd.payload.asBits)
          goto(handle)
        }
      }
    }

    val handle: State = new State {
      whenIsActive {
        switch(cmd) {
          is(rst) {
            goto(scream)
          }
          is(readReg) {
            goto(read_reg_num)
          }
          is(writeReg) {
            goto(write_reg_num)
          }
          is(execInsn) {
            goto(read_insn)
          }
          is(step) {
            goto(step_s)
          }
          is(run) {
            goto(run_s)
          }
        }
      }
    }

    val reg_to_op = Reg(UInt(4 bits))

    val word_to_write = Reg(Word)
    val word_reading = Reg(Word)

    val read_reg_num: State = new State {
      whenIsActive {
        uart.io.rd.ready := True
        when(uart.io.rd.valid) {
          reg_to_op := uart.io.rd.payload(3 downto 0)
          goto(read_reg)
        }
      }
    }

    val read_reg: State = new State {
      whenIsActive {
        io.ass.dbg.reg.valid := True
        io.ass.dbg.reg.payload := reg_to_op
        io.ass.dbg.reg_content.ready := True
        io.ass.dbg.reg_dir_out := False
        when(io.ass.dbg.reg_content.valid) {
          word_to_write := io.ass.dbg.reg_content.payload
          goto(write_one_word)
        }
      }
    }

    val word_bytes = Reg(UInt(log2Up(BPW) bits)) init (0)

    val write_one_word: State = new State {
      onEntry { word_bytes := 0 }
      whenIsActive {
        uart.io.wr.valid := True
        uart.io.wr.payload := word_to_write(7 downto 0)
        when(uart.io.wr.ready) {
          word_bytes := word_bytes + 1
          word_to_write := (word_to_write >> 8).resized
          when(word_bytes === BPW - 1) {
            goto(find_cmd)
          }
        }
      }
    }

    val write_reg_num: State = new State {
      whenIsActive {
        uart.io.rd.ready := True
        when(uart.io.rd.valid) {
          reg_to_op := uart.io.rd.payload(3 downto 0)
          goto(write_reg_read_one_word)
        }
      }
    }

    val write_reg_read_one_word: State = new State {
      onEntry { word_bytes := 0; word_reading := 0 }
      whenIsActive {
        uart.io.rd.ready := True
        when(uart.io.rd.valid) {
          word_bytes := word_bytes + 1
          word_reading := (word_reading << 8).resize(
            ws
          ) | uart.io.rd.payload.resized
          when(word_bytes === BPW - 1) {
            goto(write_reg_finish)
          }
        }
      }
    }

    val write_reg_finish: State = new State {
      whenIsActive {
        io.ass.dbg.reg.valid := True
        io.ass.dbg.reg.payload := reg_to_op
        io.ass.dbg.reg_write := word_to_write
        io.ass.dbg.reg_dir_out := True
        when(io.ass.dbg.reg.ready) {
          goto(find_cmd)
        }
      }
    }

    val insn_to_exec = Reg(UInt(16 bits))
    val read_one = RegInit(False)
    val read_insn: State = new State {
      onEntry { read_one := False; io.ass.halt := True }
      whenIsActive {
        uart.io.rd.ready := True
        when(uart.io.rd.valid) {
          when(!read_one) {
            insn_to_exec(7 downto 0) := uart.io.rd.payload
            read_one := True
          } otherwise {
            insn_to_exec(15 downto 8) := uart.io.rd.payload
            goto(exec_insn)
          }
          word_to_write := uart.io.rd.payload.resized
          goto(write_one_word)
        }
      }
    }
    io.ass.dbg.insn := insn_to_exec

    val exec_insn: State = new State {
      whenIsActive {
        when(io.ass.dbg.cur_stage === Stages.s_idle) {
          io.ass.dbg.sstep_insn := True
        }
      }
    }
    val step_s: State = new State {
      whenIsActive {}
    }

    val run_s: State = new State {
      whenIsActive {}
    }

    val stream: State = new State {
      whenIsActive {
        io.rd << uart.io.rd
        uart.io.wr.valid := True
        uart.io.wr.payload := 0x6b
      }
    }
  }
  m.setEncoding(binaryOneHot)
}
