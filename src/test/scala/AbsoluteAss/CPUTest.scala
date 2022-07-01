package absass

import chisel3._
import chisel3.stage.ChiselStage
import chiseltest._
import treadle.TreadleTester
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import firrtl.AnnotationSeq
import firrtl.annotations.Annotation
import firrtl.stage.FirrtlSourceAnnotation

class CPUTest extends AnyFreeSpec with ChiselScalatestTester {
  def exec_insn(tester: TreadleTester, i: Int) = {
    tester.expect("io_insn_content_ready", 1)
    tester.poke("io_insn_content_bits", i)
    tester.poke("io_insn_content_valid", 1)
    tester.step()
    tester.poke("io_insn_content_valid", 0)
    while (tester.peek("dbg_cur_stage") != 1) { tester.step() }
  }
  def pc(t: TreadleTester): BigInt = {
    t.expect("io_insn_addr_valid", 1)
    t.peek("io_insn_addr_bits")
  }

  "CPU executes some instruction" in {
    test(new CPU(4)) { cpu =>
      val f = (new ChiselStage).emitFirrtl(new CPU(4))
      val s = AnnotationSeq(Seq(FirrtlSourceAnnotation(f)))
      val tester = TreadleTester(s)

      tester.poke("io_halt", 0)
      tester.step(1)

      assert(pc(tester) == 0)
      exec_insn(tester, 0x1f11)
      
      assert(tester.peekMemory("regs", 1) == 0xf)
      tester.pokeMemory("regs", 2, 0xf)
      exec_insn(tester, 0x1a23)
      assert(tester.peekMemory("regs", 3) == 0xf)
    }
  }
}

class WrapperTest extends AnyFreeSpec with ChiselScalatestTester {
  "CPUWrapper does something reasonable" in {
    test(new CPUWrapper) { cpu =>
      val f = (new ChiselStage).emitFirrtl(new CPUWrapper)
      val s = AnnotationSeq(Seq(FirrtlSourceAnnotation(f)))
      val tester = TreadleTester(s)
      println(f)

      tester.poke("cpu.io.cpu_running", 1)

      tester.step(24)

      assert(tester.peek("io.red_led2") == 1)
      assert(tester.peekMemory("cpu.regs", 2) == 0xf)
    }
  }
}
