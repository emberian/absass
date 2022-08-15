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
  io.noticeMeSenpai := uart.dbg.noticeMeSenpai
  val sync = Reg(Bool()) init (False)

  io.synced := sync

  uart.io.rxd := io.rxd
  io.txd := uart.io.txd

  uart.io.wr.valid := False
  uart.io.wr.payload := 0x0

  uart.io.rd.ready := False

  io.rd.valid := False
  io.rd.payload := 0x0
  io.wr.ready := False

  val m = new StateMachine {
    val scream: State = new State with EntryPoint {
      whenIsActive {
        io.noticeMeSenpai := True
        sync := False
        uart.io.rd.ready := True
        when(uart.io.rd.valid) {
          handshake_fifo.io.push.valid := True
          handshake_fifo.io.push.payload := uart.io.rd.payload
          goto(handshake)
        }
      }
    }

    val handshake_fifo = StreamFifo(UInt(8 bits), 4)
    val magic = Vec(U(80), U(65), U(82), U(67))
    val bytes_seen = Reg(UInt(2 bits))

    handshake_fifo.io.push.valid := False
    handshake_fifo.io.push.payload := 0
    handshake_fifo.io.pop.ready := False

    val handshake: State = new State {
      onEntry { bytes_seen := 1 }
      whenIsActive {
        uart.io.rd.ready := True
        when(uart.io.rd.valid) {
          bytes_seen := bytes_seen + 1
          handshake_fifo.io.push.valid := True
          handshake_fifo.io.push.payload := uart.io.rd.payload

          when(bytes_seen === 3) {
            goto(compare)
          }
        }
      }
    }

    val compare: State = new State {
      onEntry { bytes_seen := 0 }
      whenIsActive {
        handshake_fifo.io.pop.ready := True
        when(handshake_fifo.io.pop.valid) {
          bytes_seen := bytes_seen + 1
          when(magic(bytes_seen) =/= handshake_fifo.io.pop.payload) {
            goto(scream)
          } otherwise {
            when(bytes_seen === 3) {
              goto(ack)
            }
          }
        }
      }
      onExit {}
    }

    val ack: State = new State {
      onEntry { bytes_seen := 3; sync := True }
      whenIsActive {
        uart.io.wr.valid := True
        uart.io.wr.payload := magic(bytes_seen).resized
        when(uart.io.wr.ready) {
          bytes_seen := bytes_seen - 1
          when(bytes_seen === 0) {
            goto(find_cmd)
          }
        }
      }
    }

    val cmd = Reg(UInt(8 bits)) init (0)

    val find_cmd: State = new State {
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
}
