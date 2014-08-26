package impl.verifier

import impl.logic.Symbol
import syntax.Symbols._

/**
 * An SMT-LIB formula.
 */
sealed trait SmtExp {
  def fmt(indent: Int): String = this match {
    case SmtExp.True => "true"
    case SmtExp.False => "false"
    case SmtExp.Int(i) => i.toString
    case SmtExp.Variable(s) => s.fmt
    case SmtExp.And(formulae) => "(and " + formulae.map(_.fmt(indent)).mkString(" ") + ")"
    case SmtExp.Or(formulae) => "(or \n" + "    " * (indent + 1) + formulae.map(_.fmt(indent + 1)).mkString("\n" + "    " * (indent + 1)) + ")"
    case SmtExp.Implies(antecedent, consequent) => "(=> " + antecedent.fmt(indent) + " " + consequent.fmt(indent) + ")"
    case SmtExp.Eq(lhs, rhs) => "(= " + lhs.fmt(indent) + " " + rhs.fmt(indent) + ")"
  }
}

object SmtExp {

  /**
   * The true literal.
   */
  case object True extends SmtExp

  /**
   * The false literal.
   */
  case object False extends SmtExp

  /**
   * An integer literal.
   */
  case class Int(i: scala.Int) extends SmtExp

  /**
   * A variable.
   */
  case class Variable(v: Symbol.VariableSymbol) extends SmtExp

  /**
   * A equality formula.
   */
  case class Eq(left: SmtExp, right: SmtExp) extends SmtExp

  /**
   * A conjunction of formulae.
   */
  case class And(formulae: List[SmtExp]) extends SmtExp

  /**
   * A disjunction of formulae.
   */
  case class Or(formulae: List[SmtExp]) extends SmtExp

  /**
   * An implication of antecedent => consequent.
   */
  case class Implies(antecedent: SmtExp, consequent: SmtExp) extends SmtExp

}
