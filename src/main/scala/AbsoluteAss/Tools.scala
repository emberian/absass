package absass

object Tools {
  val comps = List(
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
  )

  val maths =
    List("+", "-", "<<", ">>", ">>>", "*", "/", "%")

  val logics = List(
    "F",
    "NOR",
    "NCI",
    "NP",
    "NMI",
    "NQ",
    "XOR",
    "NAND",
    "AND",
    "XNOR",
    "Q",
    "MI",
    "P",
    "CI",
    "OR",
    "T"
  )

  def decode(x: Int): String = {
    (x & 0xf000 >> 12) match {
      case 1 => s"LOGI ${logics(x & 0x0f00 >> 8)} ${x & 0xf0 >> 4} ${x & 0xf}"
      case 2 => s"MATH ${maths(x & 0x0f00 >> 8)} ${x & 0xf0 >> 4} ${x & 0xf}"
      case 8 => s"COND ${comps(x & 0x0f00 >> 8)} ${x & 0xf0 >> 4} ${x & 0xf}"
      case 9 => s"JAL ${x & 0xf0 >> 4} ${x & 0xf}"
      case op =>
        if ((op & 4) == 4) {
          "MOVE " + x.toBinaryString
        } else {
          "UNK " + x.toHexString
        }
    }
  }
}