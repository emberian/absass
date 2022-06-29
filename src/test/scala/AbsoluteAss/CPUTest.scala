package absass

import chisel3._
import chiseltest._
import chiseltest.iotesters.PeekPokeTester
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

class CPUTest extends AnyFreeSpec with ChiselScalatestTester {
  "CPU executes one instruction" in {
    test(new CPU(4)).runPeekPoke(new PeekPokeTester(_) {
      val cpu = dut
      poke(cpu.io.halt, false)
      step(1)

      expect(cpu.io.insn_addr.valid, true)
      expect(cpu.io.insn_addr.bits, 0)
      expect(cpu.io.insn_content.ready, true)
      poke(cpu.io.insn_content.bits, "b0001111100000001".U)
      poke(cpu.io.insn_content.valid, true)

      step(5)

      assert(peekAt(cpu.regs, 1) == 0xF)
    })
  }
}

class WrapperTest extends AnyFreeSpec with ChiselScalatestTester {
  "CPUWrapper does something reasonable" in {
    test(new CPUWrapper).runPeekPoke(new PeekPokeTester(_) {
      val cpu = dut.cpu
    
      poke(dut.io.cpu_running, true)

      step(16)
    
      assert(peek(dut.io.red_led2) == 1)
      assert(peekAt(cpu.regs, 1) == 0xF)
    })
  }
}