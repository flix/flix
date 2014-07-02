package impl.solver

import impl.logic.{Predicate, Symbol, Term, Value}

trait Error

object Error {

  /**
   * An error which represents that the interpretation for predicate symbol `s` is missing.
   */
  case class InterpretationNotFound(s: Symbol.PredicateSymbol) extends RuntimeException(s"The interpretation for $s was not found.")

  case class NonRelationalPredicate(p: Predicate) extends RuntimeException

  case class PredicateArityMismatch(p: Predicate, index: Int) extends RuntimeException

  case class UnboundVariableSymbol(v: Symbol.VariableSymbol, t: Term) extends RuntimeException(s"The variable symbol '$v' is unbound in the term $t")

  case class NonValueTerm(t: Term) extends RuntimeException

  case class UnificationError(t: Term, v: Value) extends RuntimeException

  case class UnexpectedEmptyModel() extends RuntimeException

}
