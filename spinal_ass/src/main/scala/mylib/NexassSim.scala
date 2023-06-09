package absass

import spinal.core._
import spinal.lib._

import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

object Alt {
  def apply(a: () => Unit, b: () => Unit) = {
    val _delay = RegInit(False)
    when(!_delay) {
      a()
      _delay := True
    }
    when(_delay) {
      b()
      _delay := False
    }
  }
}

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
          FixedFrequency(Const.FPGAFREQ / 1000)
        )
      )
      .doSim(new Fart(8)) { fart =>
        fart.clockDomain.forkStimulus(2)
        fart.io.rxd #= true
        fart.uart.dbg.noticeMeSenpai #= false
        fart.dbg.waitResp #= false
        sleep(1)
        for (c <- List('P', 'A', 'R', 'C')) {
          UartSim.send_byte(fart.io.rxd, fart.uart.dbg.txClk, c.toInt)
        }
        for (c <- List('C', 'R', 'A', 'P')) {
          // fart.dbg.waitResp #= false
          val got = UartSim
            .recv_byte(
              fart.io.txd,
              fart.uart.dbg.txClk,
              fart.uart.dbg.noticeMeSenpai
            )
            .toInt

          println(f"${got.toChar} == $c?")
          assert(
            got == c.toInt
          )
        }
        waitUntil(fart.dbg.synced.toBoolean)
      }
  }
}

object MemTest {
  def main(args: Array[String]) {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(defaultClockDomainFrequency = FixedFrequency(1 Hz))
      )
      .doSim(new Component {
        val m =
          Mem(Bool, List(False, False, True, False, True, False, True, False))
        val v = Reg(Bool)
        val ctr = Reg(UInt(2 bits)) init (3)
        val addr = UInt(3 bits)
        val wrval = False
        val memen = Bool
        val wren = Bool
        ctr := ctr + 1
        switch(ctr) {
          is(0) {
            addr := 0
            wrval := True
            memen := True
            wren := True
          }
          is(1) {
            addr := 3
            wren := False
            memen := True
          }
          is(2) {
            addr := 0
            wrval := False
            memen := True
            wren := True
          }
          is(3) {
            addr := 6
            wren := True
            memen := True
            wrval := True
          }
        }
        val w = m.readWriteSync(addr, wrval, memen, wren)
        v := w
      }) { dut =>
        dut.clockDomain.forkStimulus(2)
        dut.clockDomain.waitRisingEdge()
        sleep(30)
      }
  }

}
object NexassSim {
  def main(args: Array[String]) {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(defaultClockDomainFrequency =
          FixedFrequency(Const.FPGAFREQ / 1000)
        )
      )
      .doSim(new Nexass) { nexass =>
        nexass.clockDomain.forkStimulus(2)

      }
  }
}
