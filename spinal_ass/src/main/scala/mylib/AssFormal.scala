package absass

import spinal.core._
import spinal.core.formal._

// You need SymbiYosys to be installed.
// See https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Formal%20verification/index.html#installing-requirements
object LogicUnitFormal {
  def main(args: Array[String]) {
    FormalConfig.withBMC(10).doVerify(new Component {
      val dut = FormalDut(new LogicUnit(4))

      // Ensure the formal test start with a reset
      assumeInitial(clockDomain.isResetActive)

      // Provide some stimulus
      
      anyseq(dut.io.p)
      anyseq(dut.io.q)
      anyseq(dut.io.op)

      val word = UInt(4 bits)
      for (i <- 0 until 4) {
        word(i) := dut.io.op(dut.io.p(i).asUInt | (dut.io.q(i).asUInt << 1))
      }
      // Check the state initial value and increment
      assert(dut.io.res === word)
    })
  }
}
