package impl.runtime

import impl.logic._

trait Error

object Error {
  // TODO: Cleanup in errors...
  case class NonUniqueModel(p: Symbol.PredicateSymbol) extends RuntimeException
  case class NonGroundPredicate(p: Symbol.PredicateSymbol) extends RuntimeException
  case class TypeError(expected: Type, actual: Value) extends RuntimeException(s"Expected value of type '$expected'. But got: '$actual'.")
  case class TypeError2(expected: Type, actual: Term) extends RuntimeException(s"Expected value of type '$expected'. But got: '$actual'.")
  case class UnboundVariable(s: Symbol.VariableSymbol) extends RuntimeException
  case class TypingError(expected: Type, actual: Type, t: Term) extends RuntimeException
  case class TypingError2(t: Term) extends RuntimeException
}
