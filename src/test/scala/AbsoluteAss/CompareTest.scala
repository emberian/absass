package absass

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

class CompareTest extends AnyFreeSpec with ChiselScalatestTester {
  def test_unit(
      aunit: ComparisonUnit,
      eq: Boolean,
      gt: Boolean,
      sn: Boolean,
      iv: Boolean,
      c: (Int, Int) => Boolean
  ) = {
    val r = new scala.util.Random()
    val bi = { b: Boolean => if (b) { 1 } else { 0 } }
    for (_ <- 1 until 10000) {
      val a = r.nextInt(65535)
      val b = r.nextInt(65535)
      val exp = c(a, b)

      aunit.io.d.poke(a);
      aunit.io.s.poke(b);
      aunit.io.eq.poke(eq.B);
      aunit.io.gt.poke(gt.B)
      aunit.io.sn.poke(sn.B)
      aunit.io.iv.poke(iv.B)
      val res = aunit.io.out.peekInt()

      val num = bi(iv) << 3 | bi(sn) << 2  | bi(gt) << 1 | bi(eq)
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
      val resb = res == 1
      if (resb != exp) {
        println(s"bad bad bad! $a $sop $b == $resb (wanted $exp)")
        assert(false)
      }

      aunit.io.out.expect(if (exp) { 1.U } else { 0.U });
    }
  }

  def fix(x: Int): Int = {
    if (x >= 32768) { x - 65536 } else { x }
  }
  "Equality" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, true, false, false, false, (a: Int, b: Int) => a == b)
    }
  }

  "Inequality" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, true, false, false, true, (a: Int, b: Int) => a != b)
    }
  }

  "Above" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, false, true, false, false, (a: Int, b: Int) => a > b)
    }
  }

  "Above equal" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, true, true, false, false, (a: Int, b: Int) => a >= b)
    }
  }
  "Below" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, false, true, false, true, (a: Int, b: Int) => a < b)
    }
  }

  "Below equal" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, true, true, false, true, (a: Int, b: Int) => a <= b)
    }
  }
  "Greater" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, false, true, true, false, (a: Int, b: Int) => fix(a) > fix(b))
    }
  }

  "Greater equal" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, true, true, true, false, (a: Int, b: Int) => fix(a) >= fix(b))
    }
  }
  "Less" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, false, true, true, true, (a: Int, b: Int) => fix(a) < fix(b))
    }
  }

  "Less equal" in {
    test(new ComparisonUnit(16)) { l =>
      test_unit(l, true, true, true, true, (a: Int, b: Int) => fix(a) <= fix(b))
    }
  }

}
