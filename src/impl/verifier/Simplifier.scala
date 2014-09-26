package impl.verifier

object Simplifier {

  /**
   * Returns a simplification of the given boolean expressed
   * based on some very simple heuristic transformations.
   */
  def simplify(b0: BoolExp): BoolExp = {
    def eval(b: BoolExp): BoolExp = b match {
      case BoolExp.Eq(e1, e2) if e1 == e2 => BoolExp.True
      case BoolExp.Not(e1) =>
        val r1 = eval(e1)
        r1 match {
          case BoolExp.True => BoolExp.False
          case BoolExp.False => BoolExp.True
          case _ => BoolExp.Not(r1)
        }

      case BoolExp.Or(e1, e2) =>
        val r1 = eval(e1)
        val r2 = eval(e2)
        if (r1 == BoolExp.True || r2 == BoolExp.True)
          BoolExp.True
        else
          BoolExp.Or(r1, r2)
      case BoolExp.And(e1, e2) =>
        val r1 = eval(e1)
        val r2 = eval(e2)
        if (r1 == BoolExp.True && r2 == BoolExp.True)
          BoolExp.True
        else if (r1 == BoolExp.True)
          r2
        else if (r2 == BoolExp.True)
          r1
        else
          BoolExp.And(r1, r2)

      case _ => b
    }

    eval(b0)
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

  /**
   * Returns the given boolean expression
   * (which must be a pure disjunction) with all literals negated.
   */
  private def neg(c: BoolExp): BoolExp = c match {
    case BoolExp.Or(e1, e2) => BoolExp.Or(neg(e1), neg(e2))
    case _ => BoolExp.Not(c)
  }

}
