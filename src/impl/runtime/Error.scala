package impl.runtime

import impl.logic._

trait Error

object Error {

  case class NonRelationalPredicate(p: Symbol.PredicateSymbol) extends RuntimeException

  case class NonValueTerm(t: Term) extends RuntimeException

  case class UnsafeGroundFact(p: Predicate) extends RuntimeException(s"The ground fact $p is unsafe.")

  case class UnsafeVariableSymbol(h: HornClause, v: Symbol.VariableSymbol) extends RuntimeException

  case class UnsupportedInterpretation(s: Symbol.PredicateSymbol, i: Interpretation) extends RuntimeException

  case class InterpretationNotFound(s: Symbol.PredicateSymbol) extends RuntimeException(s"The interpretation for $s was not found.")

  /*
   * An error to indicate that the meaning of the function symbol is unknown.
   */
  case class UnknownFunctionSymbol(s: Symbol.FunctionSymbol) extends RuntimeException(s"The function symbol '$s' has no semantics.")

  /*
   * An error to indicate that a value has the wrong type.
   */
  case class TypeError(expected: Type, actual: Value) extends RuntimeException(s"Expected value of type '$expected'. But got: '$actual'.")

}
