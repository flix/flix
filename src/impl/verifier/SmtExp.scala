package impl.verifier

/**
 * An SMT-LIB formula.
 */
sealed trait SmtExp {
  def fmt(indent: Int): String = this match {
    case SmtExp.Lit(s) => s
    case SmtExp.Lst(xs) => "\n" + " " * indent + "(" + xs.map(_.fmt(indent + 2)).mkString(" ") + ")"
  }
}

object SmtExp {
  /**
   * An atom.
   */
  case class Lit(a: String) extends SmtExp

  /**
   * A s-expression list.
   */
  case class Lst(xs: List[SmtExp]) extends SmtExp
}
