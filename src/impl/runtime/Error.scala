package impl.runtime

import impl.logic._

trait Error

object Error {

  // TODO: Cleanup in errors...

  case class NonValueTerm(t: Term) extends RuntimeException

  case class UnsupportedInterpretation(s: Symbol.PredicateSymbol, i: Interpretation) extends RuntimeException(s"$s: $i")

  case class InterpretationNotFound(s: Symbol.PredicateSymbol) extends RuntimeException(s"The interpretation for $s was not found.")

  case class NonUniqueModel(p: Symbol.PredicateSymbol) extends RuntimeException

  case class NonGroundPredicate(p: Symbol.PredicateSymbol) extends RuntimeException


  case class UnknownFunctionSymbol(s: Symbol.FunctionSymbol) extends RuntimeException(s"The function symbol '$s' has no semantics.")

  case class TypeError(expected: Type, actual: Value) extends RuntimeException(s"Expected value of type '$expected'. But got: '$actual'.")

  case class TypeError2(expected: Type, actual: Term) extends RuntimeException(s"Expected value of type '$expected'. But got: '$actual'.")



  case class UnboundVariable(s: Symbol.VariableSymbol) extends RuntimeException

  case class TypingError(expected: Type, actual: Type, t: Term) extends RuntimeException

  case class TypingError2(t: Term) extends RuntimeException

  // TODO: Consider better type error exceptions?

}
