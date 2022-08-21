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

class Nexass extends Component {
  val io = new Bundle {
    val led = out UInt (4 bits)
    val dipsw = in UInt (4 bits)
    val gsrn = in Bool ()
    val pushbutton0 = in Bool ()
    val pushbutton1 = in Bool ()
    val pmod0_1 = in Bool ()
    val pmod0_2 = out Bool ()
    // val hr0 = new HRam()
    // val hr1 = new HRam()
  }

  noIoPrefix()

  val core_rst = False

  val fart_rst = Reg(Bool()) addTag (crossClockDomain)
  val cpu_rst = Reg(Bool()) addTag (crossClockDomain)

  val osc = new OSC_CORE(1)
  osc.io.HFOUTEN := True
  val core_clk =
    ClockDomain(
      osc.io.HFCLKOUT,
      core_rst,
      frequency = FixedFrequency(Const.FPGAFREQ)
    )

  io.led := 15

  val top = new ClockingArea(core_clk) {
    val reset_fart = new Debounce(500 ms)
    reset_fart.io.crappy := !io.pushbutton0

    fart_rst := reset_fart.io.pressed

    val reset_cpu = new Debounce(500 ms)
    reset_cpu.io.crappy := !io.pushbutton1

    val fart = new ResetArea(reset_fart.io.pressed, false) {
      val fart = new Fart(16)

      fart.io.rd.ready := False
      fart.io.wr.payload.assignDontCare()
      fart.io.wr.valid := False
      fart.dbg.waitResp := False
    }

    io.led(0) := io.pushbutton0

    io.led(1) := True

    io.led(2) := !fart.fart.dbg.synced

    io.led(3) := !fart.fart.dbg.noticeMeSenpai

    fart.fart.io.rxd := io.pmod0_1

    val cpu = new ResetArea(reset_cpu.io.pressed || fart.fart.io.rst_cpu, false) {
      val ass = new CPU(16, true)
    }

    fart.fart.io.ass <> cpu.ass.dbg

    cpu_rst := reset_cpu.io.pressed

    io.pmod0_2 := RegNext(!fart.fart.io.txd) addTag (crossClockDomain)
    // INVERSION: we're feeding into an NPN BJT which inverts the voltage
    // as it shifts 1.8V to 5V for the Arduino to sense.
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
    SpinalVerilog(new Nexass).printPruned()
  }
}
