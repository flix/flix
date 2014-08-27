package impl.verifier

import impl.logic.Symbol

/**
 * An SMT-LIB formula.
 */
sealed trait SmtExp {
  def fmt(indent: Int): String = ???
}

object SmtExp {

  /**
   * A variable.
   */
  case class Var(v: Symbol.VariableSymbol) extends SmtExp

  /**
   * A literal.
   */
  case class Literal(token: String) extends SmtExp

  /**
   * A literal.
   */
  case class Lst(xs: List[SmtExp]) extends SmtExp


}
