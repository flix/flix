package impl.verifier

object Simplifier {
  /**
   * ...
   */
  def simplify(b: BoolExp): BoolExp = {

    b match {
      case BoolExp.Eq(IntExp.Int(i1), IntExp.Int(i2)) if i1 == i2 => BoolExp.True
    }
  }

  /**
   * Returns the conjunctive normal form of the given boolean expression.
   */
  private def cnf(b: BoolExp): BoolExp = ???
}
