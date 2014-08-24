package impl.verifier

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic.{Term, Type, UnaryOperator}
import impl.runtime.Error

object TypeChecker {

  /**
   * Type-checks and returns the type of the given term `t` under the given type environment `typenv`.
   */
  def typecheck(t: Term, typenv: Map[VSym, Type]): Type = t match {
    case Term.Bool(b) => Type.Bool
    case Term.UnaryOp(op, t1) =>
      val typ1 = typecheck(t1, typenv)
      op match {
        case UnaryOperator.Not => assertType(Type.Bool, typ1, t)
        case UnaryOperator.UnaryPlus => assertType(Type.Int, typ1, t)
        case UnaryOperator.UnaryMinus => assertType(Type.Int, typ1, t)
      }
  }

  /**
   * Asserts that the given `expected` type is equal to the `actual` type for the given term `t`.
   *
   * Throws an exception if not.
   */
  private def assertType(expected: Type, actual: Type, t: Term): Type =
    if (expected == actual)
      actual
    else
      throw Error.TypingError(expected, actual, t)

}
