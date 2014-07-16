package impl.verifier

import impl.logic.Symbol
import syntax.Symbols._

/**
 * An SMT-LIB formula.
 */
sealed trait SmtFormula {
  def variables: Set[Symbol.VariableSymbol] = this match {
    case SmtFormula.True => Set.empty
    case SmtFormula.False => Set.empty
    case SmtFormula.Variable(s) => Set(s)
    case SmtFormula.Constructor0(s) => Set.empty
    case SmtFormula.Conjunction(formulae) => (Set.empty[Symbol.VariableSymbol] /: formulae)((xs, f) => xs ++ f.variables)
    case SmtFormula.Disjunction(formulae) => (Set.empty[Symbol.VariableSymbol] /: formulae)((xs, f) => xs ++ f.variables)
    case SmtFormula.Eq(lhs, rhs) => lhs.variables ++ rhs.variables
  }

  def fmt(indent: Int): String = this match {
    case SmtFormula.True => "true"
    case SmtFormula.False => "false"
    case SmtFormula.Variable(s) => s.fmt
    case SmtFormula.Constructor0(s) => s.fmt
    case SmtFormula.Conjunction(formulae) => "(and " + formulae.map(_.fmt(indent)).mkString(" ") + ")"
    case SmtFormula.Disjunction(formulae) => "(or \n" + "    " * (indent + 1) + formulae.map(_.fmt(indent + 1)).mkString("\n" + "    " * (indent + 1)) + ")"
    case SmtFormula.Eq(lhs, rhs) => "(= " + lhs.fmt(indent) + " " + rhs.fmt(indent) + ")"
  }
}

object SmtFormula {

  /**
   * The true literal.
   */
  case object True extends SmtFormula

  /**
   * The false literal.
   */
  case object False extends SmtFormula

  /**
   * A variable.
   */
  case class Variable(v: Symbol.VariableSymbol) extends SmtFormula

  /**
   * A equality formula.
   */
  case class Eq(left: SmtFormula, right: SmtFormula) extends SmtFormula

  /**
   * A conjunction of formulae.
   */
  case class Conjunction(formulae: List[SmtFormula]) extends SmtFormula

  /**
   * A disjunction of formulae.
   */
  case class Disjunction(formulae: List[SmtFormula]) extends SmtFormula

  /**
   * An implication of antecedent => consequent.
   */
  case class Implication(antecedent: SmtFormula, consequent: SmtFormula) extends SmtFormula

  /**
   * A null-ary constructor.
   */
  case class Constructor0(s: Symbol.NamedSymbol) extends SmtFormula

  /**
   * A 1-ary constructor.
   */
  case class Constructor1(s: Symbol.NamedSymbol, f1: SmtFormula) extends SmtFormula

  /**
   * A 2-ary constructor.
   */
  case class Constructor2(s: Symbol.NamedSymbol, f1: SmtFormula, f2: SmtFormula) extends SmtFormula

  /**
   * A 3-ary constructor.
   */
  case class Constructor3(s: Symbol.NamedSymbol, f1: SmtFormula, f2: SmtFormula, f3: SmtFormula) extends SmtFormula

  /**
   * A 4-ary constructor.
   */
  case class Constructor4(s: Symbol.NamedSymbol, f1: SmtFormula, f2: SmtFormula, f3: SmtFormula, f4: SmtFormula) extends SmtFormula

  /**
   * A 5-ary constructor.
   */
  case class Constructor5(s: Symbol.NamedSymbol, f1: SmtFormula, f2: SmtFormula, f3: SmtFormula, f4: SmtFormula, f5: SmtFormula) extends SmtFormula

}
