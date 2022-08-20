package absass

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

/** UART appropriate for communicating with Arduino SoftwareSerial.
  *
  * Will not transmit if we are receiving from the Arduino; it does not support
  * full duplex transfer and will miss the transmission if we do not stall until
  * the Arduino is ready.
  */
class Uart extends Component {
  val Byte = UInt(8 bits)
  val io = new Bundle {
    val rd = master Stream (Byte)
    val wr = slave Stream (Byte)
    val txd = out Bool ()
    val rxd = in Bool ()
  }

  val dbg = new Bundle {
    val txClk = out Bool () simPublic ()
    val rxClk = out Bool () simPublic ()
    val noticeMeSenpai = out Bool ()
    val stalled = out Bool ()
  }

  val stalled = RegInit(False)
  dbg.stalled := stalled

  io.wr.ready := False

  val baud = 9600 Hz

  val txClk = RegInit(False)
  val rxClk = RegInit(False)

  dbg.txClk := txClk
  dbg.rxClk := rxClk

  val sync_clk = RegInit(True)

  dbg.noticeMeSenpai := RegNext(sync_clk)

  val rctl = new Area {
    val currentFreq = ClockDomain.current.frequency.getValue.toBigDecimal
    val baudCtr = Reg(UInt(16 bits)) init (0)
    val factor = (currentFreq / (baud.toBigDecimal)).toInt
    println(f"it takes $factor cycles to toggle the clock")
    val rdctr = Reg(UInt(log2Up(factor) + 2 bits)) init (1)
    val wrctr = Reg(UInt(log2Up(factor) bits)) init (0)
    // we thus sample in the middle of the baud period
    rdctr := rdctr - 1
    when(!io.rxd && sync_clk) {
      rdctr := factor + (factor / 2)
      rxClk := False
      sync_clk := False
    }
    wrctr := wrctr + 1
    when(rdctr === 0) { rxClk := !rxClk; rdctr := factor - 1 }
    when(wrctr === factor - 1) {
      txClk := !txClk; wrctr := 0; baudCtr := baudCtr + 1
    }
  }

  val rxar = new Area {
    val sent = RegInit(False)
    val rx = new StateMachine {
      val recvbf = Reg(UInt(8 bits)) init (0)
      val bits_recvd = Reg(UInt(3 bits)) init (0)

      io.rd.valid := sent
      io.rd.payload := recvbf

      when(io.rd.ready && sent) {
        sent := False
      }

      val idle: State = new State with EntryPoint {
        whenIsActive {
          when(!sync_clk) {
            rxClk := False
            goto(rd)
          }
        }
      }

      val rd = new State {
        // god i hope these clock phases are good enough
        onEntry { bits_recvd := 0 }
        whenIsActive {
          when(rxClk.edge) {
            bits_recvd := bits_recvd + 1
            recvbf(7) := io.rxd
            recvbf(6 downto 0) := recvbf(7 downto 1)

            when(bits_recvd === 7) {
              sent := True
              stalled := sent
              goto(stop)
            }
          }
        }
      }

      val stop = new State {
        whenIsActive { when(rxClk.rise) { goto(idle) } }
        onExit { sync_clk := True }
      }
    }
  }

  val txar = new Area {
    val tx = new StateMachine {
      val bits_sent = Reg(UInt(4 bits)) init (0)
      val dataw = Reg(UInt(10 bits)) init (0)
      val latest_bit = Reg(Bool) init (True)

      io.txd := latest_bit

      val idle: State = new State with EntryPoint {
        whenIsActive {
          io.wr.ready := True
          when(io.wr.valid) {
            dataw(0) := False
            latest_bit := True // line kept high when not in use
            dataw(9) := True
            dataw(8 downto 1) := io.wr.payload
            goto(wr)
          }
        }
      }

      val wr = new State {
        val go = Reg(Bool) init (False)

        onEntry { go := False }
        onExit { bits_sent := 0 }
        whenIsActive {
          when(txClk.edge) {
            go := True
            when(go) {
              latest_bit := dataw(0)
              dataw(8 downto 0) := dataw(9 downto 1)
              dataw(9) := False // so the wave readout is nicer
              bits_sent := bits_sent + 1
              when(bits_sent === 9) {
                goto(idle)
              }
            }
          }
        }
      }
    }
  }
}

object FtdiSim {
  def main(args: Array[String]) {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(
          defaultClockDomainFrequency = FixedFrequency(0.125 MHz)
        )
      )
      .doSim(new Uart) { ftdi =>
        ftdi.io.wr.valid #= false
        ftdi.io.rxd #= true
        ftdi.io.rd.ready #= false
        ftdi.clockDomain.forkStimulus(period = 80000)
        ftdi.clockDomain.waitFallingEdge()

        fork {
          while (true) {
            assert(ftdi.dbg.stalled.toBoolean == false)
            ftdi.clockDomain.waitFallingEdge()
          }
        }

        for (_ <- 0 until 10) {

          val r = Random.nextInt(256)
          println(f"uat($r)")
          ftdi.io.wr.payload #= r
          waitUntil(ftdi.io.wr.ready.toBoolean)
          ftdi.io.wr.valid #= true
          ftdi.clockDomain.waitRisingEdge()
          ftdi.io.wr.valid #= false
          println("waiting tx low")
          waitUntil(!ftdi.io.txd.toBoolean)
          sleep(2)
          var prev = ftdi.dbg.txClk.toBoolean
          var data = 0
          for (i <- 0 until 8) {
            println("wait edge")
            waitUntil(ftdi.dbg.txClk.toBoolean != prev)
            prev = ftdi.dbg.txClk.toBoolean
            sleep(2)
            println(f"sampling ${ftdi.io.txd.toBoolean.toInt}")
            data = data | (ftdi.io.txd.toBoolean.toInt << i)
          }
          // skip the stop bit
          waitUntil(ftdi.dbg.txClk.toBoolean != prev)
          sleep(2)
          assert(ftdi.io.txd.toBoolean == true)
          waitUntil(!ftdi.dbg.txClk.toBoolean)

          println(f"$data == $r ?")
          assert(data == r)
        }
        for (_ <- 0 until 10) {
          ftdi.clockDomain.waitRisingEdge()
          val r = Random.nextInt(256)
          println(f"uar($r)")
          ftdi.io.rd.ready #= false
          println("sending start bit")
          waitUntil(ftdi.dbg.txClk.toBoolean)
          waitUntil(!ftdi.dbg.txClk.toBoolean)
          ftdi.io.rxd #= false

          var data = r
          for (i <- 0 until 4) {
            waitUntil(ftdi.dbg.txClk.toBoolean)
            println(f"wiggling out a ${data & 1}")
            ftdi.io.rxd #= (data & 1) == 1
            data = data >> 1
            waitUntil(!ftdi.dbg.txClk.toBoolean)
            println(f"wiggling out a ${data & 1}")
            ftdi.io.rxd #= (data & 1) == 1
            data = data >> 1
          }

          waitUntil(ftdi.dbg.txClk.toBoolean)
          ftdi.io.rxd #= true
          waitUntil(!ftdi.dbg.txClk.toBoolean)
          assert(ftdi.io.rd.valid.toBoolean)
          ftdi.io.rd.ready #= true
          ftdi.clockDomain.waitRisingEdge()
          val res = ftdi.io.rd.payload.toBigInt
          ftdi.clockDomain.waitFallingEdge()
          ftdi.io.rd.ready #= false

          println(f"$res == $r ?")
          assert(res == r)
        }
      }
  }
}

object FtdiV {
  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      defaultClockDomainFrequency = FixedFrequency(450 MHz)
    )
      .generate(new Uart)
  }
}
