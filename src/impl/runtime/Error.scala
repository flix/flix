package impl.runtime

import impl.logic._

trait Error

object Error {

  case class NonValueTerm(t: Term) extends RuntimeException

  case class UnsupportedInterpretation(s: Symbol.PredicateSymbol, i: Interpretation) extends RuntimeException(s"$s: $i")

  case class InterpretationNotFound(s: Symbol.PredicateSymbol) extends RuntimeException(s"The interpretation for $s was not found.")

  case class NonRelationalPredicateSymbol(p: Symbol.PredicateSymbol) extends RuntimeException

  case class NonUniqueModel(p: Symbol.PredicateSymbol) extends RuntimeException

  /*
   * An error to indicate that the meaning of the function symbol is unknown.
   */
  case class UnknownFunctionSymbol(s: Symbol.FunctionSymbol) extends RuntimeException(s"The function symbol '$s' has no semantics.")

  /*
   * An error to indicate that a value has the wrong type.
   */
  case class TypeError(expected: Type, actual: Value) extends RuntimeException(s"Expected value of type '$expected'. But got: '$actual'.")

}
