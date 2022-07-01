package absass

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

class LogicSpec extends AnyFreeSpec with ChiselScalatestTester {
  "Logic operators obey their truth table" in {
    test(new LogicUnit(1)) { l =>
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
        l.io.op.poke(logic_op);
        var ix = 0;
        for ((p, q) <- List((1.U, 1.U), (1.U, 0.U), (0.U, 1.U), (0.U, 0.U))) {
          l.io.p.poke(p);
          l.io.q.poke(q);
          val expected = if (result_string(ix) == '0') { 0.U }
          else { 1.U }
          l.io.out.expect(expected)
          ix += 1;
        }
      }
    }
  }
  "Move works" in {
    test(new LogicUnit(16)) { l => 
      l.io.op.poke(0xa)
      l.io.p.poke(0x1010)
      l.io.q.poke(0x0101)
      l.io.out.expect(0x0101)
    }
  }
}
