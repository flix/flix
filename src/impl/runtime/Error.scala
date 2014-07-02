package impl.runtime

import impl.logic._

trait Error

object Error {

  // TODO: Cleanup in errors.
  // TODO: Remove "new" since these are case classes.

  /**
   * An error which represents that the interpretation for predicate symbol `s` is missing.
   */
  case class InterpretationNotFound(s: Symbol.PredicateSymbol) extends RuntimeException(s"The interpretation for $s was not found.")

  case class NonRelationalPredicate(p: Symbol.PredicateSymbol) extends RuntimeException

  case class PredicateArityMismatch(p: Predicate, index: Int) extends RuntimeException

  case class UnboundVariableSymbol(v: Symbol.VariableSymbol, t: Term) extends RuntimeException(s"The variable symbol '$v' is unbound in the term $t")

  case class NonValueTerm(t: Term) extends RuntimeException

  case class UnificationError(t: Term, v: Value) extends RuntimeException

  case class UnsafeGroundFact(p: Predicate) extends RuntimeException
  
  case class UnsafeVariableSymbol(h: HornClause, v: Symbol.VariableSymbol) extends RuntimeException
}
