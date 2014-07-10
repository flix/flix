package msz3

import z3.scala._

object Example01 {

  def main(args: Array[String]) {
    val cfg = new Z3Config("MODEL" -> true) // required if you plan to query models of satisfiable constraints
    val z3 = new Z3Context(cfg)

    // prepares the integer sort and three constants (the "unknowns")
    val i = z3.mkIntSort()
    val h = z3.mkConst(z3.mkStringSymbol("h"), i)
    val m = z3.mkConst(z3.mkStringSymbol("m"), i)
    val s = z3.mkConst(z3.mkStringSymbol("s"), i)
  }

}
