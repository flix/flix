package impl.verifier

/**
 * An SMT-LIB formula.
 */
sealed trait SmtExp {
  def fmt(): String = ???
}

object SmtExp {
  /**
   * An atom.
   */
  case class Literal(a: String) extends SmtExp

  /**
   * A s-expression list.
   */
  case class Lst(xs: List[SmtExp]) extends SmtExp
}
