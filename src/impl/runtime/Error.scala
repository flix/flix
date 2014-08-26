package impl.runtime

import impl.ast.SExp
import impl.logic._

trait Error

object Error {
  case class UnboundVariable(s: Symbol.VariableSymbol) extends RuntimeException
  // TODO: Cleanup in errors...
  case class TypeError(expected: Type, actual: Value) extends RuntimeException(s"Expected value of type '$expected'. But got: '$actual'.")
  case class TypeError2(expected: Type, actual: Term) extends RuntimeException(s"Expected value of type '$expected'. But got: '$actual'.")
  case class TypingError(expected: Type, actual: Type, t: Term) extends RuntimeException
  case class TypingError2(t: Term) extends RuntimeException

  sealed trait ParseException extends RuntimeException
  case class UnableToParsePredicate(e: SExp) extends ParseException
  case class UnableToParseTerm(e: SExp) extends ParseException
  case class UnableToParsePattern(e: SExp) extends ParseException
  case class UnableToParseType(e: SExp) extends ParseException
}
