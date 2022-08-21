package absass

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

object UartSim {
  def recv_byte(txd: Bool, txClk: Bool): Int = {
    recv_byte(txd, txClk, None)
  }
  def recv_byte(txd: Bool, txClk: Bool, samp: Bool): Int = {
    recv_byte(txd, txClk, Some(samp))
  }

  def recv_byte(txd: Bool, txClk: Bool, samp: Option[Bool]): Int = {
    waitUntil(!txd.toBoolean)
    samp.map(_ #= true)

    var prev = txClk.toBoolean
    var data = 0
    for (i <- 0 until 8) {
      println(f"wait edge (start = ${prev})")
      waitUntil(txClk.toBoolean != prev)
      samp.map(_ #= false)

      prev = txClk.toBoolean
      sleep(3)
      samp.map(_ #= true)
      println(f"sampling (clock was ${prev}) ${txd.toBoolean.toInt}")
      data = data | (txd.toBoolean.toInt << i)
      sleep(1)
      samp.map(_ #= false)
    }
    // skip the stop bit
    waitUntil(txClk.toBoolean != prev)
    waitUntil(txClk.toBoolean == prev)
    return data
  }
  def send_byte(rxd: Bool, txClk: Bool, byte: Int) {
    val prev = txClk.toBoolean
    waitUntil(txClk.toBoolean != prev)
    rxd #= false

    var data = byte
    for (i <- 0 until 4) {
      waitUntil(txClk.toBoolean == prev)
      println(f"wiggling out a ${data & 1}")
      rxd #= (data & 1) == 1
      data = data >> 1
      waitUntil(txClk.toBoolean != prev)
      println(f"wiggling out a ${data & 1}")
      rxd #= (data & 1) == 1
      data = data >> 1
    }
    waitUntil(txClk.toBoolean == prev)
    rxd #= true
    waitUntil(txClk.toBoolean != prev)
  }
}

/** UART appropriate for communicating with Arduino SoftwareSerial.
  *
  * Will not transmit if we are receiving from the Arduino; it does not support
  * full duplex transfer and will miss the transmission if we do not stall until
  * the Arduino is ready.
  */
class Uart extends Component {
  val io = new Bundle {
    val rd = master Stream (UInt(8 bits))
    val wr = slave Stream (UInt(8 bits))
    val txd = out Bool ()
    val rxd = in Bool ()
  }

  val dbg = new Bundle {
    val txClk = out Bool () simPublic ()
    val rxClk = out Bool () simPublic ()
    val noticeMeSenpai = in Bool () simPublic ()
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

  val rctl = new Area {
    val currentFreq = ClockDomain.current.frequency.getValue.toBigDecimal
    val baudCtr = Reg(UInt(16 bits)) init (0)
    val factor = (currentFreq / (baud.toBigDecimal)).toInt
    println(f"it takes $factor cycles to toggle the clock")
    val rdctr = Reg(UInt(log2Up(factor) + 2 bits)) init (1)
    val wrctr = Reg(UInt(log2Up(factor) bits)) init (0)
    rdctr := rdctr - 1
    wrctr := wrctr - 1

    when(rdctr === 0) { rxClk := !rxClk; rdctr := factor - 1 }
    when(wrctr === 0) {
      txClk := !txClk; wrctr := factor - 1; baudCtr := baudCtr + 1
    }

    when(!io.rxd && sync_clk) {
      // we thus sample in the middle of the baud period
      // after waiting one baud for the start bit to finish.
      rdctr := factor + (factor / 2)
      rxClk := False
      sync_clk := False
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
        whenIsActive { when(rxClk.edge) { goto(idle) } }
        onExit { sync_clk := True }
      }
    }
    rx.setEncoding(binaryOneHot)
  }

  val txar = new Area {
    val tx = new StateMachine {
      val bits_unsent = Reg(UInt(4 bits)) init (0)
      val dataw = Reg(UInt(10 bits)) init (0)
      val latest_bit = Reg(Bool) init (True)

      io.txd := latest_bit

      val idle: State = new State with EntryPoint {
        whenIsActive {
          io.wr.ready := True
          dataw(0) := False
          dataw(9) := True
          dataw(8 downto 1) := io.wr.payload
          bits_unsent := 10

          when(io.wr.valid) {
            goto(wr)
          }
        }
      }

      val wr = new State {
        onEntry { rctl.wrctr := 0 }
        whenIsActive {
          when(txClk.edge) {
            latest_bit := dataw(0)
            dataw(8 downto 0) := dataw(9 downto 1)
            dataw(9) := True
            bits_unsent := bits_unsent - 1
            when(bits_unsent === 0) {
              goto(idle)
            }
          }
        }
      }
    }
    tx.setEncoding(binaryOneHot)
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
        ftdi.clockDomain.forkStimulus(period = 2)
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
          val data = UartSim.recv_byte(ftdi.io.txd, ftdi.dbg.txClk)
          println(f"$data == $r ?")
          assert(data == r)
        }
        for (_ <- 0 until 10) {
          ftdi.clockDomain.waitRisingEdge()
          val r = Random.nextInt(256)
          println(f"uar($r)")
          ftdi.io.rd.ready #= false

          UartSim.send_byte(ftdi.io.rxd, ftdi.dbg.txClk, r)
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
