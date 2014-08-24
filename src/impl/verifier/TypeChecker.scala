package impl.verifier

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic.{BinaryOperator, Term, Type, UnaryOperator}
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
    case Term.BinaryOp(op, t1, t2) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      op match {
        case BinaryOperator.Plus => assertTypes(expected = List(Type.Int, Type.Int), actual = List(typ1, typ2), result = Type.Int, t)
        case BinaryOperator.Minus => assertTypes(expected = List(Type.Int, Type.Int), actual = List(typ1, typ2), result = Type.Int, t)
        case BinaryOperator.Times => assertTypes(expected = List(Type.Int, Type.Int), actual = List(typ1, typ2), result = Type.Int, t)
        case BinaryOperator.Divide => assertTypes(expected = List(Type.Int, Type.Int), actual = List(typ1, typ2), result = Type.Int, t)
        case BinaryOperator.Modulo => assertTypes(expected = List(Type.Int, Type.Int), actual = List(typ1, typ2), result = Type.Int, t)
        case BinaryOperator.Less => assertTypes(expected = List(Type.Int, Type.Int), actual = List(typ1, typ2), result = Type.Bool, t)
        case BinaryOperator.LessEqual => assertTypes(expected = List(Type.Int, Type.Int), actual = List(typ1, typ2), result = Type.Bool, t)
        case BinaryOperator.Greater => assertTypes(expected = List(Type.Int, Type.Int), actual = List(typ1, typ2), result = Type.Bool, t)
        case BinaryOperator.GreaterEqual => assertTypes(expected = List(Type.Int, Type.Int), actual = List(typ1, typ2), result = Type.Bool, t)
        case BinaryOperator.Equal | BinaryOperator.NotEqual => assertType(typ1, typ2, t)
      }
  }

  /**
   * Asserts that the given `expected` type is equal to the `actual` type for the given term `t`.
   * Otherwise, throws an exception.
   */
  private def assertType(expected: Type, actual: Type, t: Term): Type =
    if (expected == actual)
      actual
    else
      throw Error.TypingError(expected, actual, t)

  /**
   * Asserts that the given `expected` types are equal to the `actual` types for the given term `t`.
   * Otherwise, throws an exception.
   */
  private def assertTypes(expected: List[Type], actual: List[Type], result: Type, t: Term): Type = {
    for ((ee, tt) <- expected zip actual) {
      assertType(ee, tt, t)
    }
    result
  }
}
