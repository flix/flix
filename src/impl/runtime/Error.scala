package impl.runtime

import impl.ast.SExp
import impl.logic._

object Error {

  /**
   * An exception thrown to indicate that the variable `x` was expected to be bound, but is actually unbound.
   */
  case class UnboundVariableError(x: Symbol.VariableSymbol) extends RuntimeException(s"Expected the variable '$x' to be bound.")

  /**
   * An exception thrown to indicate a run-time type error.
   */
  case class RuntimeTypeError(expected: Type, actual: Value) extends RuntimeException(s"Expected a value of type '$expected' but got '$actual'.")

  /**
   * An exception thrown to indicate a static type error.
   */
  case class StaticTypeError(expected: Type, actual: Type, t: Term) extends RuntimeException(s"Expected the term '$t' to have type '$expected' but it has type '$actual'.")

  /**
   * An exception thrown to indicate that an "unexpected" type appeared during type checking.
   */
  case class UnexpectedTypeError(t: Term, msg: String) extends RuntimeException(msg)
  // TODO: Maybe fix this by introducing variable in type?

  /**
   * An exception thrown to indicate a parsing error when trying to parse a predicate.
   */
  case class PredicateParseError(e: SExp) extends RuntimeException(s"Unable to parse predicate $e")

  /**
   * An exception thrown to indicate a parsing error when trying to parse a term.
   */
  case class TermParseError(e: SExp) extends RuntimeException(s"Unable to parse term $e")

  /**
   * An exception thrown to indicate a parsing error when trying to parse a pattern.
   */
  case class PatternParseError(e: SExp) extends RuntimeException(s"Unable to parse pattern $e")

  /**
   * An exception thrown to indicate a parsing error when trying to parse a type.
   */
  case class TypeParseError(e: SExp) extends RuntimeException(s"Unable to parse type $e")

}
