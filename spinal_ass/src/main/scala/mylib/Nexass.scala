package absass

import spinal.core._
import spinal.lib._

import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

class HRam extends Bundle {
  val cs = in Bool ()
  val rst = out Bool ()
  val ckn = out Bool ()
  val rw = out Bool ()
  val dq = inout UInt (8 bits)
}

object Const {
  val FPGAFREQ = 240 MHz
}

class Debounce(dur: TimeNumber) extends Component {
  val io = new Bundle {
    val crappy = in Bool ()
    val pressed = out Bool ()
  }

  val factor = (ClockDomain.current.frequency.getValue * dur).toBigInt
  val counter = Reg(UInt(log2Up(factor) bits)) init (0)
  val cycs_high = Reg(UInt(log2Up(factor) bits)) init (0)

  io.pressed.setAsReg() init (False)

  when(counter =/= 0) {
    counter := counter - 1
    when(io.crappy) {
      cycs_high := cycs_high + 1
    }
  } otherwise {
    io.pressed := cycs_high > factor / 2
  }
  when(io.crappy.edge && counter === 0) {
    counter := factor - 1
    cycs_high := 0
  }
}

case class Nexlink() extends Bundle {
  val led = out UInt (4 bits)
  val dipsw = in UInt (4 bits)
  val pb0 = in Bool ()
  val pb1 = in Bool ()
  val txd = out Bool ()
  val rxd = in Bool ()
  // val hr0 = new HRam()
  // val hr1 = new HRam()
}

class Nexass extends Component {
  val ws = 16
  val io = new Bundle {
    val link = Nexlink()
  }

  val reset_fart = new Debounce(500 ms)
  reset_fart.io.crappy := !io.link.pb0

  val reset_cpu = new Debounce(500 ms)
  reset_cpu.io.crappy := !io.link.pb1

  val fart = new ResetArea(reset_fart.io.pressed, false) {
    val fart = new Fart(ws)

    fart.dbg.waitResp := False
  }

  io.link.led(0) := io.link.pb0

  io.link.led(1) := True

  io.link.led(2) := !fart.fart.dbg.synced

  io.link.led(3) := !fart.fart.dbg.noticeMeSenpai

  fart.fart.io.rxd := io.link.rxd

  val cpu = new ResetArea(reset_cpu.io.pressed || fart.fart.io.rst_cpu, false) {
    val ass = new CPU(ws, true)

    val plat = new TrivialPlat(ws, 1024)

    ass.io <> plat.cpu
  }

  fart.fart.io.ass <> cpu.ass.dbg

  io.link.txd := RegNext(!fart.fart.io.txd) addTag (crossClockDomain)
  // INVERSION: we're feeding into an NPN BJT which inverts the voltage
  // as it shifts 1.8V to 5V for the Arduino to sense.
}

class TopLvl extends Component {
  noIoPrefix()
  val io = Nexlink()

  val osc = new OSC_CORE(1)
  osc.io.HFOUTEN := True
  val core_clk =
    ClockDomain(
      osc.io.HFCLKOUT,
      False,
      frequency = FixedFrequency(Const.FPGAFREQ)
    )

  val top = new ClockingArea(core_clk) {
    val nexass = new Nexass
    nexass.io.link <> io
  }
}

class OSC_CORE(val div: Int) extends BlackBox {
  addGeneric("HF_CLK_DIV", div)
  addGeneric("HF_OSC_EN", "ENABLED")
  addGeneric("LF_OUTPUT_EN", "ENABLED")

  val io = new Bundle {
    val HFCLKOUT = out Bool ()
    val LFCLKOUT = out Bool ()
    val HFOUTEN = in Bool ()
  }
  noIoPrefix()
}

object Nexass {
  def main(args: Array[String]) {
    SpinalVerilog(new TopLvl).printPruned()
  }
}
