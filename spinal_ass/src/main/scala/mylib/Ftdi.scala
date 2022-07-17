package absass

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

class Ftdi(val fifo_sz: Int) extends Component {
  val Byte = UInt(8 bits)
  val io = new Bundle {
    val rd = master Stream (Byte)
    val wr = slave Stream (Byte)
    val txd = out Bool ()
    val rxd = in Bool ()
  }

  val dbg = new Bundle {
    val baudClk = out Bool ()
    val noticeMeSenpai = in Bool ()
  }

  io.txd := True // active low
  io.wr.ready := False
  val baud = 9600 Hz

  val baudClk = RegInit(False)
  val rxClk = RegInit(False)

  dbg.baudClk := baudClk

  val rdfifo = StreamFifo(
    dataType = Byte,
    depth = fifo_sz
  )
  rdfifo.io.push.valid := False
  rdfifo.io.push.payload.assignDontCare()

  rdfifo.io.pop >> io.rd

  val sync_clk = Reg(Bool()) init (False) addTag (crossClockDomain)

  val rctl = new Area {
    val currentFreq = ClockDomain.current.frequency.getValue.toBigDecimal
    val baudCtr = Reg(UInt(16 bits)) init (0)
    val factor = (currentFreq / (2 * baud.toBigDecimal)).toInt
    println(f"it takes $factor cycles to toggle the clock")
    // multiply by two to ensure one edge per baud
    val rdctr = Reg(UInt(log2Up(factor) bits)) init (0)
    val wrctr = Reg(UInt(log2Up(factor) bits)) init (0)
    // we thus sample in the middle of the baud period
    when(!io.rxd && sync_clk) {
      rdctr := factor / 2
      rxClk := False
    } otherwise {
      rdctr := rdctr + 1
    }
    wrctr := wrctr + 1
    when(rdctr === factor - 1) { rxClk := !rxClk; rdctr := 0 }
    when(wrctr === factor - 1) {
      baudClk := !baudClk; wrctr := 0; baudCtr := baudCtr + 1
    }
  }

  val rxar = new Area {
    val sent = RegInit(False)
    when(rdfifo.io.push.ready && sent) {
      rdfifo.io.push.valid := False; sent := False
    }

    val rx = new StateMachine {
      val recvbf = Reg(UInt(8 bits)) init (0)
      val bits_recvd = Reg(UInt(3 bits)) init (0)

      val idle: State = new State with EntryPoint {
        whenIsActive {
          when(!io.rxd) {
            goto(rd)
          }
        }
      }

      val rd = new State {
        // god i hope these clock phases are good enough
        onEntry { bits_recvd := 0 }
        onExit { sync_clk := True }
        whenIsActive {
          when(rxClk.rise) {
            bits_recvd := bits_recvd + 1
            recvbf(7) := io.rxd
            recvbf(6 downto 0) := recvbf(7 downto 1)

            when(bits_recvd === 7) {
              rdfifo.io.push.valid := True
              sent := True
              // what does a stall do? if sent = true, we stalled.
              rdfifo.io.push.payload := recvbf
              goto(idle)
            }
          }
        }
      }
    }
  }

  val txar = new Area {
    val tx = new StateMachine {
      val bits_sent = Reg(UInt(4 bits)) init (0)
      val dataw = Reg(UInt(10 bits)) init (0)

      val idle: State = new State with EntryPoint {
        onEntry { io.wr.ready := True }

        whenIsActive {
          when(io.wr.valid) { goto(wr) }
        }
        onExit { io.wr.ready := False }
      }

      val wr = new State {
        val latest_bit = Reg(Bool) init (False)
        onEntry {
          dataw(0) := False
          latest_bit := True // line kept high when not in use
          dataw(9) := True
          dataw(8 downto 1) := io.wr.payload
          bits_sent := 0
        }
        whenIsActive {
          when(baudClk.rise) {
            latest_bit := dataw(0)
            io.txd := dataw(0)
            dataw(8 downto 0) := dataw(9 downto 1)
            dataw(9) := False // so the wave readout is nicer
            bits_sent := bits_sent + 1
            when(bits_sent === 9) {
              goto(idle)
            }
          } otherwise {
            io.txd := latest_bit
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
          defaultClockDomainFrequency = FixedFrequency(0.5 MHz)
        )
      )
      .doSim(new Ftdi(2)) { ftdi =>
        ftdi.clockDomain.forkStimulus(period = 2000)
        for (_ <- 0 until 10) {
          ftdi.dbg.noticeMeSenpai #= false

          ftdi.clockDomain.waitRisingEdge()
          val r = 0xff
          println(f"uat($r)")
          ftdi.io.wr.payload #= r
          ftdi.io.wr.valid #= true
          waitUntil(ftdi.io.wr.ready.toBoolean)
          ftdi.clockDomain.waitRisingEdge()
          ftdi.io.wr.valid #= false
          println("waiting tx low")
          waitUntil(!ftdi.io.txd.toBoolean)
          println("waiting baudclk low")
          waitUntil(!ftdi.dbg.baudClk.toBoolean)
          var data = 0
          for (i <- 0 until 8) {
            println("waiting baudclk high")
            waitUntil(ftdi.dbg.baudClk.toBoolean)
            sleep(2)
            ftdi.dbg.noticeMeSenpai #= true
            println(f"sampling ${ftdi.io.txd.toBoolean.toInt}")
            data = data | (ftdi.io.txd.toBoolean.toInt << i)
            println("waiting baduclk toggle")
            waitUntil(!ftdi.dbg.baudClk.toBoolean)
            ftdi.dbg.noticeMeSenpai #= false
          }
          // skip the stop bit
          waitUntil(ftdi.dbg.baudClk.toBoolean)
          assert(ftdi.io.txd.toBoolean == true)
          waitUntil(!ftdi.dbg.baudClk.toBoolean)

          println(f"$data == $r ?")
          assert(data == r)
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
      .generate(new Ftdi(3))
  }
}
