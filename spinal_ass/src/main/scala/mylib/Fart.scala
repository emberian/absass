package absass

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

object FartCmds extends SpinalEnum(binaryOneHot) {
  val rst, readReg, writeReg, readMem, writeMem, execInsn, step, run =
    newElement()
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
    val rst_cpu = out Bool ()

    val ass = slave(DebugPort(ws))
  }

  io.rst_cpu := False
  io.ass.halt.setAsReg

  val dbg = new Bundle {
    val synced = out Bool ()
    val txClk = out Bool ()
    val rxClk = out Bool ()
    val noticeMeSenpai = out Bool ()
    val waitResp = in Bool ()
  }

  io.ass.reg.valid := False
  io.ass.reg.payload := 0x0
  io.ass.reg_content.ready := False
  io.ass.reg_write := 0x0
  io.ass.reg_dir_out := False
  io.ass.sstep_insn := False

  val uart = new Uart
  dbg.txClk := uart.dbg.txClk
  dbg.rxClk := uart.dbg.rxClk

  dbg.synced.setAsReg() init (False)
  dbg.noticeMeSenpai.setAsReg() init (False)

  uart.io.rxd := io.rxd
  io.txd := uart.io.txd

  val payload = Reg(Byte) init (0)
  uart.io.wr.valid := False
  uart.io.wr.payload := payload
  val rd_ready = Reg(Bool) init (False)
  uart.io.rd.ready := rd_ready

  io.rd.valid := False
  io.rd.payload := 0x0
  io.wr.ready := False

  val m = new StateMachine {
    val scream: State = new State with EntryPoint {
      whenIsActive {
        dbg.synced := False
        rd_ready := True
        when(uart.io.rd.valid) {
          handshake_fifo.io.push.valid := True
          bytes_seen := 1
          goto(handshake)
        }
      }
    }

    val handshake_fifo = StreamFifo(UInt(8 bits), 4)
    val magic = Vec(U(80), U(65), U(82), U(67))
    val magic2 = Vec(U(67), U(82), U(65), U(80))
    val bytes_seen = Reg(UInt(2 bits))
    val compare_cursor = Reg(UInt(2 bits))

    handshake_fifo.io.push.valid := False
    handshake_fifo.io.pop.ready := False
    handshake_fifo.io.push.payload := uart.io.rd.payload

    val handshake: State = new State {
      whenIsActive {
        rd_ready := True
        when(uart.io.rd.valid) {
          handshake_fifo.io.push.valid := True
          bytes_seen := bytes_seen + 1
          when(bytes_seen === 3) {
            compare_cursor := 0
            goto(cmp)
          }
        }
      }
    }

    val failing_value = RegInit(U(0, 8 bits))
    val latched = Reg(Byte) init (0)
    val compare_got = Reg(Byte)

    val compare_failed = RegInit(False)

    val compare_fsm =
      new StateMachine {
        val compare_pop: State = new State with EntryPoint {
          whenIsActive {
            handshake_fifo.io.pop.ready := True
            compare_got := magic(compare_cursor).resized
            payload := compare_cursor.resize(8)

            when(handshake_fifo.io.pop.valid) {
              failing_value := handshake_fifo.io.pop.payload
              goto(compare_wr)
            }
          }
        }

        val compare_res = Reg(Bool)
        val compare_wr: State = new State {
          whenIsActive {
            compare_cursor := compare_cursor + 1
            when(compare_got =/= failing_value) {
              goto(compare_bad)
            } otherwise { goto(compare_good) }
          }
        }
        val compare_bad: State = new State {
          whenIsActive {
            dbg.noticeMeSenpai := True
            uart.io.wr.valid := True
            compare_failed := True
            exit()
          }
        }
        val compare_good: State = new State {
          whenIsActive {
            when(compare_cursor === 3) {
              ack_cursor := 8

              compare_failed := False
              exit()
            } otherwise {
              goto(compare_pop)
            }
          }
        }
      }
    compare_fsm.setEncoding(binaryOneHot)

    val cmp: State = new StateFsm(compare_fsm) {
      whenCompleted {
        when(compare_failed) { goto(fail) } otherwise {
          goto(ack_ram)
        }
      }
    }

    val ack_cursor = Reg(UInt(4 bits))
    val ack_got = Reg(Byte)
    val word_bytes = Reg(UInt(log2Up(BPW) bits)) init (0)

    val ack_ram: State = new State {
      whenIsActive {
        switch(ack_cursor) {
          is(1) { payload := magic2(3).resized }
          is(2) { payload := magic2(2).resized }
          is(4) { payload := magic2(1).resized }
          is(8) { payload := magic2(0).resized }
        }
        when(!dbg.waitResp) {
          goto(ack)
        }
      }
    }

    val ack: State = new State {
      whenIsActive {
        uart.io.wr.valid := True

        when(uart.io.wr.ready) {
          ack_cursor := (ack_cursor >> 1).resized

          when(ack_cursor === 1) {
            dbg.synced := True
            goto(handle)
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

    val handle: State = new StateFsm({
      val m = new StateMachine {

        val find_cmd: State = new State with EntryPoint {
          whenIsActive {
            rd_ready := True
            word_bytes := BPW - 1
            word_bytes_2 := BPW - 1

            when(uart.io.rd.valid) {
              cmd.assignFromBits(uart.io.rd.payload.asBits)
              goto(entry)
            }
          }
        }

        val entry: State = new State {
          whenIsActive {
            switch(cmd) {
              is(rst) {
                io.rst_cpu := True
                exit()
              }
              is(readReg) {
                goto(read_reg)
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
              default {
                exit()
              }
            }
          }
        }

        val reg_to_op = Reg(UInt(4 bits))

        val word_to_write = Reg(Word)
        val word_reading = Reg(Word)

        val read_reg: State = new StateFsm(new StateMachine {
          val read_reg_num: State = new State with EntryPoint {
            whenIsActive {
              rd_ready := True
              when(uart.io.rd.valid) {
                reg_to_op := uart.io.rd.payload(3 downto 0)
                goto(read_reg)
              }
            }
          }

          val read_reg: State = new State {
            whenIsActive {
              io.ass.reg.valid := True
              io.ass.reg.payload := reg_to_op
              io.ass.reg_content.ready := True
              io.ass.reg_dir_out := False
              when(io.ass.reg_content.valid) {
                word_to_write := io.ass.reg_content.payload
                exit()
              }
            }
          }
        }) {
          whenCompleted {
            goto(write_one_word)
          }
        }

        val write_one_word: State = new State {
          whenIsActive {
            uart.io.wr.valid := True
            uart.io.wr.payload := word_to_write(7 downto 0)
            when(uart.io.wr.ready) {
              word_bytes := (word_bytes - 1).resized
              word_to_write := (word_to_write >> 8).resized
              when(word_bytes === 0) {
                goto(find_cmd)
              }
            }
          }
        }

        val write_reg_num: State = new State {
          whenIsActive {
            rd_ready := True
            when(uart.io.rd.valid) {
              reg_to_op := uart.io.rd.payload(3 downto 0)
              goto(write_reg_read_one_word)
            }
          }
        }

        val word_bytes_2 = Reg(UInt(log2Up(BPW) bits)) init (0)
        val write_reg_read_one_word: State = new State {
          onEntry { word_reading := 0 }
          whenIsActive {
            rd_ready := True
            when(uart.io.rd.valid) {
              word_bytes_2 := (word_bytes_2 - 1).resized
              word_reading := (word_reading << 8).resize(
                ws
              ) | uart.io.rd.payload.resized
              when(word_bytes_2 === 0) {
                goto(write_reg_finish)
              }
            }
          }
        }

        val write_reg_finish: State = new State {
          whenIsActive {
            io.ass.reg.valid := True
            io.ass.reg.payload := reg_to_op
            io.ass.reg_write := word_to_write
            io.ass.reg_dir_out := True
            when(io.ass.reg.ready) {
              goto(find_cmd)
            }
          }
        }

        val insn_to_exec = Reg(UInt(16 bits))
        val read_one = RegInit(False)
        val read_insn: State = new State {
          onEntry { read_one := False; io.ass.halt := True }
          whenIsActive {
            rd_ready := True
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
        io.ass.insn := insn_to_exec

        val exec_insn: State = new State {
          whenIsActive {
            when(io.ass.cur_stage === Stages.s_idle) {
              io.ass.sstep_insn := True
            }
          }
        }
        val step_s: State = new State {
          whenIsActive {}
        }

        val run_s: State = new State {
          whenIsActive {}
        }

      }; m.setEncoding(binaryOneHot); m
    }) {
      whenCompleted { goto(scream) }
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
