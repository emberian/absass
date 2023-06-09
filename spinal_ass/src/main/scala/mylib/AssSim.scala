package absass

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random
import spinal.lib.slave
import spinal.lib.master

object LogicSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new LogicUnit(1)) { dut =>
      // Fork a process to generate the reset and the clock on the dut

      val truth_tables = List(
        (0, "0000"),
        (1, "0001"),
        (2, "0010"),
        (3, "0011"),
        (4, "0100"),
        (5, "0101"),
        (6, "0110"),
        (7, "0111"),
        (8, "1000"),
        (9, "1001"),
        (10, "1010"),
        (11, "1011"),
        (12, "1100"),
        (13, "1101"),
        (14, "1110"),
        (15, "1111")
      )

      for ((logic_op, result_string) <- truth_tables) {
        var ix = 0
        for ((p, q) <- List((1, 1), (1, 0), (0, 1), (0, 0))) {
          dut.io.p #= p
          dut.io.q #= q
          dut.io.op #= logic_op

          sleep(1)
          val expected = if (result_string(ix) == '0') { 0 }
          else { 1 }
          ix += 1
          assert(
            dut.io.res.toInt == expected,
            s"${dut.io.res.toInt} == $expected for p=$p op=$logic_op q=$q"
          )
        }
      }
    }
  }
}

object ArithSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new ArithUnit(16, true)) { arith =>
      def fix(x: Int): Int = {
        if (x >= 32768) { x - 65536 }
        else { x }
      }

      def test_unit(
          aunit: ArithUnit,
          op: ArithOps.E,
          c: (Int, Int) => Int
      ) = {
        val r = new scala.util.Random()
        for (_ <- 1 until 10000) {
          val a = r.nextInt(65535)
          val b = r.nextInt(65535)
          var exp = c(a, b) % 65536
          if (exp < 0) {
            exp = 65536 + exp;
          }

          aunit.io.op #= op
          aunit.io.d #= a;
          aunit.io.s #= b;
          sleep(1)
          val real = aunit.io.res.toInt
          val sop =
            List("+", "-", "<<", ">>", ">>>", "*", "/", "%")(
              op.position
            )
          if (real != BigInt(exp)) {
            println(s"bad bad bad! $a $sop $b == $real (wanted $exp)")
          }

          assert(
            aunit.io.res.toInt == exp,
            s"$a $sop $b == $exp ? result: ${aunit.io.res.toInt}"
          );
        }
      }

      test_unit(arith, ArithOps.l_add, (a: Int, b: Int) => a + b)
      test_unit(arith, ArithOps.l_sub, (a: Int, b: Int) => a - b)
      /*test_unit(
        arith,
        ArithOps.l_shr,
        (a: Int, b: Int) =>
          if (b > 15) { 0 }
          else { a << b }
      )

      test_unit(
        arith,
        ArithOps.l_shr,
        (a: Int, b: Int) =>
          if (b > 15) { 0 }
          else { a >> b }
      )
      test_unit(
        arith,
        ArithOps.l_asr,
        (a: Int, b: Int) =>
          if (b > 15) {
            if (a > 32767) { 0xffff }
            else { 0 }
          } else { fix(a) >>> b }
      )*/
      test_unit(arith, ArithOps.l_mul, (a: Int, b: Int) => a * b)
      test_unit(
        arith,
        ArithOps.l_div,
        (a: Int, b: Int) =>
          if (b == 0) { 0xffff }
          else { a / b }
      )
      test_unit(
        arith,
        ArithOps.l_mod,
        (a: Int, b: Int) =>
          if (b == 0) { 0 }
          else { a % b }
      )
    }
  }
}

object ComparisonSim {
  def test_unit(
      aunit: ComparisonUnit,
      eq: Boolean,
      gt: Boolean,
      sn: Boolean,
      iv: Boolean,
      c: (Int, Int) => Boolean
  ) = {
    val r = new scala.util.Random()
    val bi = { b: Boolean =>
      if (b) { 1 }
      else { 0 }
    }
    for (_ <- 1 until 10000) {
      val a = r.nextInt(65535)
      val b = r.nextInt(65535)
      val exp = c(a, b)

      aunit.io.d #= (a);
      aunit.io.s #= (b);
      aunit.io.eq #= (eq);
      aunit.io.gt #= (gt)
      aunit.io.sn #= (sn)
      aunit.io.iv #= (iv)
      sleep(10)
      val res = aunit.io.res

      val num = bi(iv) << 3 | bi(sn) << 2 | bi(gt) << 1 | bi(eq)
      val sop =
        List(
          "F",
          "==",
          "U>",
          "U>=",
          "F",
          "==",
          ">",
          ">=",
          "T",
          "!=",
          "U<=",
          "U<",
          "T",
          "!=",
          "<",
          "<="
        )(num)
      val resb = res.toBoolean
      if (resb != exp) {
        println(s"bad bad bad! $a $sop $b == $resb (wanted $exp)")
        assert(false)
      }

      assert(aunit.io.res.toBoolean == exp)
    }
  }

  def fix(x: Int): Int = {
    if (x >= 32768) { x - 65536 }
    else { x }
  }

  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new ComparisonUnit(16)) { l =>
      test_unit(l, true, false, false, false, (a: Int, b: Int) => a == b)
      test_unit(l, true, false, false, true, (a: Int, b: Int) => a != b)
      test_unit(l, false, true, false, false, (a: Int, b: Int) => a > b)
      test_unit(l, true, true, false, false, (a: Int, b: Int) => a >= b)
      test_unit(l, false, true, false, true, (a: Int, b: Int) => a < b)
      test_unit(l, true, true, false, true, (a: Int, b: Int) => a <= b)
      test_unit(
        l,
        false,
        true,
        true,
        false,
        (a: Int, b: Int) => fix(a) > fix(b)
      )
      test_unit(
        l,
        true,
        true,
        true,
        false,
        (a: Int, b: Int) => fix(a) >= fix(b)
      )
      test_unit(l, false, true, true, true, (a: Int, b: Int) => fix(a) < fix(b))
      test_unit(l, true, true, true, true, (a: Int, b: Int) => fix(a) <= fix(b))
    }
  }
}

class CPUWrap(ws: Int, fancy: Boolean) extends Component {
  val io = new Bundle { val dbg = master(DebugPort(ws)) }
  val cpu = new CPU(ws, fancy)
  val regs = new RegAccess(ws)
  val mem = new TrivialPlat(ws, 256)
  regs.io <> cpu.io.regs
  mem.io <> cpu.io.io
  io.dbg <> cpu.io.dbg
}

object CPUSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new CPUWrap(16, true)) { wrap =>
      wrap.clockDomain.forkStimulus(period = 2)
      val cpu = wrap.cpu
      def exec_insn(c: CPU, i: Int, exp_pc: Int) = {
        c.dbg.sstep #= true
        waitUntil(c.io.io.insn_content.ready.toBoolean == true)
        assert(c.io.io.insn_addr.payload.toBigInt == exp_pc)
        c.io.io.insn_content.payload #= i
        sleep(1)
        c.io.io.insn_content.valid #= true
        waitUntil(c.dbg.cur_stage.toBigInt == 1)
        c.dbg.sstep #= false
        sleep(2)
        c.io.io.insn_content.valid #= false
        waitUntil(c.dbg.cur_stage.toBigInt == 0)
        c.clockDomain.waitRisingEdge()
      }
      def pc(c: CPU): BigInt = {
        // assert(c.io.insn_addr.valid == 1)
        c.io.io.insn_addr.payload.toBigInt
      }

      cpu.io.dbg.halt #= true
      cpu.io.io.insn_content.valid #= false
      cpu.io.io.insn_content.payload #= 0
      cpu.io.io.read_port.valid #= false
      cpu.io.io.read_port.payload #= 0
      cpu.io.io.write_port.ready #= false
      cpu.io.dbg.sstep_insn #= false
      cpu.io.dbg.sstep #= false

      wrap.clockDomain.waitRisingEdge(3)

      exec_insn(cpu, 0x1f11, 0)

      assert(wrap.regs.regs(1).toBigInt == 0xff)
      wrap.regs.regs(2) #= 0xff
      exec_insn(cpu, 0x1a23, 2)
      assert(wrap.regs.regs(3).toBigInt == 0xff)

      exec_insn(cpu, 0xcafe, 4)
      assert(wrap.regs.regs(0xe).toBigInt == 0xaf)
      wrap.regs.regs(1) #= 0x0
      exec_insn(cpu, 0x4d11, 6)
    }
  }
}

object TestAll {
  def main(args: Array[String]) {
    LogicSim.main(args)
    ArithSim.main(args)
    ComparisonSim.main(args)
    CPUSim.main(args)
    FtdiSim.main(args)
  }
}
