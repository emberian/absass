package absass

import spinal.core._
import spinal.lib._

import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

object DebounceSim {
  def main(args: Array[String]) {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(defaultClockDomainFrequency = FixedFrequency(20 Hz))
      )
      .doSim(new Debounce(250 ms)) { db =>
        {
          db.clockDomain.forkStimulus(2)
          db.clockDomain.waitRisingEdge()
          db.io.crappy #= false
          sleep(2)
          db.io.crappy #= true
          waitUntil(db.io.pressed.toBoolean)
          db.io.crappy #= false
          waitUntil(!db.io.pressed.toBoolean)
        }
      }
  }
}

object FifoSim {
  def main(args: Array[String]) {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(defaultClockDomainFrequency =
          FixedFrequency(Const.FPGAFREQ)
        )
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

object FartSim {
  def main(args: Array[String]) {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(defaultClockDomainFrequency =
          FixedFrequency(Const.FPGAFREQ / 500)
        )
      )
      .doSim(new Fart) { fart =>
        fart.clockDomain.forkStimulus(period = 10)
        fart.io.rxd #= true
        fart.clockDomain.waitFallingEdge()
        sleep(1000)
        fart.clockDomain.waitRisingEdge()
        waitUntil(fart.uart.dbg.txClk.toBoolean)
        waitUntil(!fart.uart.dbg.txClk.toBoolean)

        for (c <- List('P', 'A', 'R', 'C')) {

          val r = c.toInt
          println(f"uar($r)")
          println("sending start bit")
          fart.io.rxd #= false

          var data = r
          for (i <- 0 until 4) {
            waitUntil(fart.uart.dbg.txClk.toBoolean)
            println(f"wiggling out a ${data & 1}")
            fart.io.rxd #= (data & 1) == 1
            data = data >> 1
            waitUntil(!fart.uart.dbg.txClk.toBoolean)
            println(f"wiggling out a ${data & 1}")
            fart.io.rxd #= (data & 1) == 1
            data = data >> 1
          }

          waitUntil(fart.uart.dbg.txClk.toBoolean)
          fart.io.rxd #= true
          waitUntil(!fart.uart.dbg.txClk.toBoolean)
        }
        waitUntil(fart.io.synced.toBoolean)
      }

  }
}
