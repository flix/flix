package impl.verifier

object Simplifier {
  /**
   * ...
   */
  def simplify(b: BoolExp): BoolExp = {
      dnf(b, identity)
  }

  /**
   * Returns the disjunctive normal form of the given boolean expression.
   */
  private def dnf(b: BoolExp, k: BoolExp => BoolExp): BoolExp = b match {
    case BoolExp.Not(b1) => dnf(b1, c => k(neg(c)))
    case BoolExp.And(b1, b2) => dnf(b1, c1 => dnf(b2, c2 => k(BoolExp.And(c1, c2))))
    case BoolExp.Or(b1, b2) => BoolExp.Or(dnf(b1, k), dnf(b2, k))
    case _ => k(b)
  }

  def neg(c: BoolExp): BoolExp = c match {
    case BoolExp.Or(e1, e2) => BoolExp.Or(neg(e1), neg(e2))
    case BoolExp.And(e1, e2) => ???
    case _ => BoolExp.Not(c)
  }

  def main(args: Array[String]): Unit = {
    println(simplify(BoolExp.And(BoolExp.Or(BoolExp.True, BoolExp.True), BoolExp.Or(BoolExp.False, BoolExp.False))))
  }


}
