package absass

import spinal.core._
import spinal.lib._

class HRam extends Bundle {
  val cs = in Bool ()
  val rst = out Bool ()
  val ckn = out Bool ()
  val rw = out Bool ()
  val dq = inout UInt (8 bits)
}

class Nexass extends Component {
  val io = new Bundle {
    val led = out UInt (4 bits)
    val rgb0 = out UInt (3 bits)
    val rgb1 = out UInt (3 bits)
    val dipsw = in UInt (4 bits)
    val gsrn = in Bool ()
    val pushbutton0 = in Bool ()
    val pushbutton1 = in Bool ()
    val ftdi_txd_uart = out Bool ()
    val ftdi_rxd_uart = in Bool ()
    // val hr0 = new HRam()
    // val hr1 = new HRam()
  }

  noIoPrefix()

  val core_rst = False

  val top = new Area {
    val osc = new OSC_CORE(1)
    osc.io.HFOUTEN := True
    val core_clk =
      ClockDomain(
        osc.io.HFCLKOUT,
        core_rst,
        frequency = FixedFrequency(240.79 MHz)
      )
  }

  io.led := 15

  val clocked = new ClockingArea(top.core_clk) {

    val ctr = Reg(UInt(3 bits))
    io.rgb0 := ctr
    io.rgb1 := 0

    val ftdi = new Ftdi(2)

    ftdi.io.rxd := io.ftdi_rxd_uart
    io.ftdi_txd_uart := ftdi.io.txd

    ftdi.io.wr << ftdi.io.rd

    new SlowArea(8 Hz) {
      ctr := ctr + 1
    }

    val _slow = new SlowArea(2 Hz) {
      val p = Reg(Bool()) init (False)
      io.led(0) := p

      p := !p

      io.led(1) := !ftdi.io.rd.valid

      io.led(2) := io.ftdi_rxd_uart

      io.led(3) := io.ftdi_txd_uart
    }

  }
  /*
  val blinky = new ClockingArea(top.core_clk) {
    // val cpu = new CPU(16, false);
    val div = Reg(UInt(8 bits))
    val shift = Reg(UInt(10 bits))
    val prev_pb0 = Reg(Bool()) init (False)
    val pwmd = Reg(UInt(10 bits))
    val act = Reg(Bool()) init (False)
    val dir = Reg(Bool()) init (False)

    io.led := pwmd(3 downto 0)
    io.rgb0 := pwmd(6 downto 4)
    io.rgb1 := pwmd(9 downto 7)

    div := div + 1
    when(div(0)) {
      pwmd := 0;
    }.otherwise {
      pwmd := shift;
    }
    when(div(7)) {
      when(shift(9)) {
        dir := True
      }.otherwise {
        dir := False
      }

      when(act) {
        when(shift === 0) {
          shift := 1
        }
          .otherwise {
            when(dir) {
              shift := shift.rotateLeft(1)
            }.otherwise {
              shift := shift.rotateRight(1)
            }
          }
        act := False
      }
      prev_pb0 := io.pushbutton0
    }

    when(!io.pushbutton0 && !prev_pb0) {
      act := True
    }
  }*/
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
    SpinalVerilog(new Nexass)
  }
}
