package absass

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

class Fart extends Component {
  val Byte = UInt(8 bits)

  val io = new Bundle {
    val rd = master Stream (Byte)
    val wr = slave Stream (Byte)
    val txd = out Bool ()
    val rxd = in Bool ()
    val synced = out Bool ()
    val txClk = out Bool ()
    val rxClk = out Bool ()
    val noticeMeSenpai = out Bool ()
  }

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

    val cmd = Reg(UInt(8 bits)) init (0)

    val find_cmd: State = new State {
      whenIsActive {  }
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
