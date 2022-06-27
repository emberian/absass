package absass

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

class ArithTest extends AnyFreeSpec with ChiselScalatestTester {
  def test_unit(
      aunit: ArithUnit,
      op: UInt,
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

      aunit.io.op.poke(op);
      aunit.io.d.poke(a);
      aunit.io.s.poke(b);
      val real = aunit.io.out.peekInt()
      val sop =
        List("+", "-", "<<", ">>", ">>>", "*", "/", "%")(op.litValue().toInt)
      if (real != BigInt(exp)) {
        println(s"bad bad bad! $a $sop $b == $real (wanted $exp)")
        assert(false)
      }

      aunit.io.out.expect(exp);
    }
  }

  "Addition" in {
    test(new ArithUnit(16)) { l =>
      test_unit(l, l.l_add, (a: Int, b: Int) => a + b)
    }
  }

  "Subtraction" in {
    test(new ArithUnit(16)) { l =>
      test_unit(l, l.l_sub, (a: Int, b: Int) => a - b)
    }
  }

  "Shift left" in {
    test(new ArithUnit(16)) { l =>
      test_unit(
        l,
        l.l_shl,
        (a: Int, b: Int) =>
          if (b > 15) { 0 }
          else { a << b }
      )
    }
  }
  "Shift right" in {
    test(new ArithUnit(16)) { l =>
      test_unit(
        l,
        l.l_shr,
        (a: Int, b: Int) =>
          if (b > 15) { 0 }
          else { a >> b }
      )
    }
  }
  "Arithmetic shift right" in {
    test(new ArithUnit(16)) { l =>
      test_unit(
        l,
        l.l_asr,
        (a: Int, b: Int) =>
          a >>> b
      )
    }
  }
  "Multiplication" in {
    test(new ArithUnit(16)) { l =>
      test_unit(l, l.l_mul, (a: Int, b: Int) => a * b)
    }
  }
  "Division" in {
    test(new ArithUnit(16)) { l =>
      test_unit(
        l,
        l.l_div,
        (a: Int, b: Int) =>
          if (b == 0) { 0xffff }
          else { a / b }
      )
    }
  }
  "Modulus" in {
    test(new ArithUnit(16)) { l =>
      test_unit(
        l,
        l.l_mod,
        (a: Int, b: Int) =>
          if (b == 0) { 0 }
          else { a % b }
      )
    }
  }
}
