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

class Nexass extends Component {
  val io = new Bundle {
    val led = out UInt (4 bits)
    val rgb0 = out UInt (3 bits)
    val rgb1 = out UInt (3 bits)
    val dipsw = in UInt (4 bits)
    val gsrn = in Bool ()
    val pushbutton0 = in Bool ()
    val pushbutton1 = in Bool ()
    val pmod0_1 = out Bool ()
    val pmod0_2 = in Bool ()
    val pmod0_3 = out Bool ()
    val pmod0_4 = out Bool ()
    val pmod1_1 = out Bool ()
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
    val rxClk = Reg(Bool()) init (False)

    val ctr = Reg(UInt(3 bits))
    io.rgb0 := 7
    io.rgb1 := 7

    val fart = new Fart
    val pshd = Reg(Bool) init (False)

    fart.io.rxd := io.pmod0_2

    fart.io.rd.ready := False
    fart.io.wr.payload.assignDontCare()
    fart.io.wr.valid := False

    rxClk := fart.io.rxClk
    io.pmod0_4 := rxClk
    new SlowArea(8 Hz) {
      ctr := ctr + 1
      pshd := pshd || io.pushbutton0
    }

    val _slow = new Area {
      val p = Reg(Bool()) init (False)
      p := ctr < 4
      io.led(0) := True

      io.led(1) := False

      io.led(2) := !fart.io.synced

      io.led(3) := False
    }
  }

  io.pmod0_1 := clocked.fart.io.txd
  io.pmod0_3 := clocked.fart.io.txClk
  io.pmod1_1 := True
  // io.pmod0_4 := rxClk
  /*g
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

object FifoSim {
  def main(args: Array[String]) {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(defaultClockDomainFrequency = FixedFrequency(240.79 MHz))
      )
      .doSim(new Component {
        val io = new Bundle {
          val finished = out Bool ()
        }
        val bytes_seen = Reg(UInt(2 bits)) init (0)
        val fifo = StreamFifo(UInt(8 bits), 4)
        val magic =
          Vec(U(80, 8 bits), U(65, 8 bits), U(82, 8 bits), U(67, 8 bits))
        val st = Reg(Bool()) init (False)
        val bad = Reg(Bool()) init (False)
        val finish = Reg(Bool()) init (False)
        io.finished := finish
        fifo.io.push.valid := False
        fifo.io.push.payload.assignDontCare
        fifo.io.pop.ready := False

        when(!st) {
          fifo.io.push.valid := True
          fifo.io.push.payload := magic(bytes_seen)
          bytes_seen := bytes_seen + 1
          when(bytes_seen === 3) {
            // do NOT SET pop ready here!!! it's a wire not a register!!!
            st := True
            bytes_seen := 0
          }
        } otherwise {
          fifo.io.pop.ready := True
          when(fifo.io.pop.valid) {
            bytes_seen := bytes_seen + 1
            when(magic(bytes_seen) =/= fifo.io.pop.payload) {
              bad := True
            } otherwise {
              when(bytes_seen === 3) {
                finish := True
              }
            }
          }
        }
      }) { dut =>
        dut.clockDomain.forkStimulus(2)
        sleep(1000)
        assert(dut.io.finished.toBoolean)
      }
  }
}

object NexassSim {
  def main(args: Array[String]) {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(defaultClockDomainFrequency = FixedFrequency(240.79 MHz))
      )
      .doSim(new Fart) { fart =>
        fart.clockDomain.forkStimulus(period = 10)
        fart.clockDomain.waitFallingEdge()
        sleep(1000)
        fart.clockDomain.waitRisingEdge()
        for (c <- List('P', 'A', 'R', 'C')) {

          val r = c.toInt
          println(f"uar($r)")
          println("sending start bit")
          waitUntil(!fart.uart.dbg.txClk.toBoolean)
          waitUntil(fart.uart.dbg.txClk.toBoolean)
          fart.io.rxd #= false
          waitUntil(fart.uart.dbg.txClk.toBoolean)
          waitUntil(!fart.uart.dbg.txClk.toBoolean)

          var data = r
          for (i <- 0 until 8) {
            waitUntil(fart.uart.dbg.txClk.toBoolean)
            println(f"wiggling out a ${data & 1}")
            fart.io.rxd #= (data & 1) == 1
            data = data >> 1
            waitUntil(!fart.uart.dbg.txClk.toBoolean)
          }

          waitUntil(fart.uart.dbg.txClk.toBoolean)
          fart.io.rxd #= true
          waitUntil(!fart.uart.dbg.txClk.toBoolean)
          sleep(10000)
        }
      }

  }
}
