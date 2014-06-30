package impl

import impl.logic.Term
import impl.logic.Symbol

trait Error

object Error {

  case class UnknownInterpretation(s: Symbol.PredicateSymbol) extends RuntimeException(s"$s has no interpretation.")

  case class NonRelationalPredicate(p: Predicate) extends RuntimeException

  case class PredicateArityMismatch(p: Predicate, index: Int) extends RuntimeException

  case class UnboundVariable(v: Symbol.VariableSymbol) extends RuntimeException

  case class NonValueTerm(t: Term) extends RuntimeException

}
