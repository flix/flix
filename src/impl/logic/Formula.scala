package impl.logic

sealed trait Formula

object Formula {

  case class Atom(p: Predicate) extends Formula

  case class Negation(formula: Formula) extends Formula

  case class Conjunction(formulae: Set[Formula]) extends Formula

  case class Disjunction(formulae: Set[Formula]) extends Formula

  case class Implication(left: Formula, right: Formula) extends Formula

  case class Exists(s: Symbol.VariableSymbol, f: Formula) extends Formula

  case class Forall(s: Symbol.VariableSymbol, f: Formula) extends Formula

}