package absass

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class Ftdi extends Component {
  val Byte = UInt(8 bits)
  val io = new Bundle {
    val rd = master Stream (Byte)
    val wr = slave Stream (Byte)
  }

  val ftdi = new Bundle {
    val txd = out Bool ()
    val rxd = in Bool ()
  }

  val spiclk = new ClockDomain(ftdi.tck, clockDomain.reset, frequency = 6 MHz)

  val spiDomain = new ClockingArea(spiclk) {
    val buf = Reg(Byte) init (0) addTag (crossClockDomain)
    val buf_rdy = Reg(Bool) init (False) addTag (crossClockDomain)

    val main = new StateMachine {
      val bits_sent = Reg(UInt(5 bits)) init (0)
      val fairness = Reg(Bool()) init (False)
      val cmdbuf = Reg(UInt(24 bits)) init (0)

      val start = new State with EntryPoint {
        onEntry {
          bits_sent := 0; buf := 0; wr.ready := True
        }
        whenIsActive {
          buf_rdy := False
          when(rd.ready) {
            when(fairness) {
              when(wr.valid) {
                goto(startWrite)
                // they had their chance
                fairness := False
              } otheriwse {
                goto(startRead)
              }
            } otherwise {
              // it our turn! but be nice next time
              fairness := True
              goto(startRead)
            }
          } otherwise {
            when(wr.valid) {
              goto(startWrite)
            }
          }
        }
      }

      val startWrite = new State {
        onEntry {
          wr.ready := False; cmdbuf(23 downto 16) := 0x22;
          cmdbuf(15 downto 8) := 7; cmdbuf(7 downto 0) := wr.payload
        }
        whenIsActive {
          // cross our fingers about this clock domain?
          ftdi.tdo := cmdbuf(23)
          cmdbuf := cmdbuf << 1
          bits_sent := bits_sent + 1
          when(bits_sent === 23) {
            goto(start)
          }
        }
      }

      val startRead = new State {
        onEntry {
          cmdbuf(23 downto 16) := 0x2A
          cmdbuf(15 downto 8) := 7
        }

        whenIsActive {
            ftdi.tdo := cmdbuf(23)
            cmdbuf := cmdbuf << 1
            bits_sent := bits_sent + 1
            when(bits_sent === 15) {
              goto(continueRead)
            }
        }
      }

      val continueRead = new State {
        onEntry { bits_sent := 0 }
        whenIsActive {
            buf := (buf << 1) | ftdi.tdi
            bits_sent := bits_sent + 1
            when (bits_sent === 7) {
                buf_rdy := True
                goto(start)
            }
        }
      }
    }
  }

  when(spiDomain.buf_rdy) {
    rd.payload := spiDomain.buf
    rd.valid := True
  }
}
