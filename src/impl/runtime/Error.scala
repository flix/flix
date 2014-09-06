package impl.runtime

import impl.ast.SExp
import impl.logic._
import syntax.Types.RichType

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
   * An exception thrown to indicate that the bottom value for `typ` is missing.
   */
  case class MissingBot(typ: Type) extends RuntimeException(s"Missing bottom value for type ${typ.fmt}")

  /**
   * An exception thrown to indicate that the less-than-equal function for `typ` is missing.
   */
  case class MissingLeq(typ: Type) extends RuntimeException(s"Missing less-than-equal for type ${typ.fmt}")

  /**
   * An exception thrown to indicate that the least-upper-bound function for `typ` is missing.
   */
  case class MissingLub(typ: Type) extends RuntimeException(s"Missing least-upper-bound for type ${typ.fmt}")

  /**
   * An exception thrown to indicate that the height function for `typ` is missing.
   */
  case class MissingHeight(typ: Type) extends RuntimeException(s"Missing height function for type ${typ.fmt}")

  /**
   * An exception thrown to indicate a parsing error.
   */
  case class ParseError(e: SExp) extends RuntimeException(s"Unable to parse predicate $e")

}
