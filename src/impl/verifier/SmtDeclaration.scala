package impl.verifier

import impl.logic.Symbol
import syntax.Symbols._
import syntax._

/**
 * An SMT-LIB declaration.
 */
sealed trait SmtDeclaration {
  def fmt: String = this match {
    case SmtDeclaration.Relation(s, sorts, vars, formula) =>
      val args = (vars zip sorts).map({ case (x, y) => "(" + x.fmt + " " + y + ")"}).mkString(" ")
      smt"(define-fun $s ($args) Bool" + "\n    " + formula.fmt(1) + ")\n"

    case SmtDeclaration.Datatype(s, variants) =>
      smt"(declare-datatypes () (($s " + variants.map(_.s).mkString(" ") + ")))\n"
  }
}

object SmtDeclaration {

  /**
   * A n-ary boolean function declaration.
   */
  case class Relation(name: Symbol.PredicateSymbol, sorts: List[String], vars: List[Symbol.VariableSymbol], formula: SmtFormula) extends SmtDeclaration

  /**
   * A datatype declaration.
   */
  case class Datatype(name: Symbol.LatticeSymbol, variants: List[Symbol.NamedSymbol]) extends SmtDeclaration

}
