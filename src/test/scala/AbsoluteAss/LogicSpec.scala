package absass

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

class LogicSpec extends AnyFreeSpec with ChiselScalatestTester {
  "Logic operators obey their truth table" in {
    test(new LogicUnit(1)) { l =>
      val truth_tables = List(
        (l.l_f, "0000"),
        (l.l_nor, "0001"),
        (l.l_nci, "0010"),
        (l.l_np, "0011"),
        (l.l_nmi, "0100"),
        (l.l_nq, "0101"),
        (l.l_xor, "0110"),
        (l.l_nand, "0111"),
        (l.l_and, "1000"),
        (l.l_xnor, "1001"),
        (l.l_q, "1010"),
        (l.l_mi, "1011"),
        (l.l_p, "1100"),
        (l.l_ci, "1101"),
        (l.l_or, "1110"),
        (l.l_t, "1111")
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
}
