package impl.verifier

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic.{BinaryOperator, Term, Type, UnaryOperator}
import impl.runtime.Error

import scala.util.Try

object TypeChecker {

  /**
   * Tryes to type check the given term `t`. 
   */
  def typecheck(t: Term): Try[Type] = Try(typecheck(t, Map.empty))

  /**
   * Type-checks and returns the type of the given term `t` under the given type environment `typenv`.
   */
  def typecheck(t: Term, typenv: Map[VSym, Type]): Type = t match {
    case Term.Unit => Type.Unit
    case Term.Bool(b) => Type.Bool
    case Term.Int(i) => Type.Int
    case Term.Str(s) => Type.Str

    case Term.IfThenElse(t1, t2, t3) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      val typ3 = typecheck(t3, typenv)
      assertType(Type.Bool, typ1, t1)
      assertType(typ2, typ3, t)

    case Term.UnaryOp(op, t1) =>
      val typ1 = typecheck(t1, typenv)
      op match {
        case UnaryOperator.Not => assertType(Type.Bool, typ1, t1)
        case UnaryOperator.UnaryPlus => assertType(Type.Int, typ1, t1)
        case UnaryOperator.UnaryMinus => assertType(Type.Int, typ1, t1)
      }

    case Term.BinaryOp(op, t1, t2) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      op match {
        case BinaryOperator.Plus | BinaryOperator.Minus | BinaryOperator.Times | BinaryOperator.Divide | BinaryOperator.Modulo =>
          assertType(Type.Int, typ1, t1)
          assertType(Type.Int, typ2, t2)
          Type.Int

        case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual | BinaryOperator.GreaterEqual =>
          assertType(Type.Int, typ1, t1)
          assertType(Type.Int, typ2, t2)
          Type.Int

        case BinaryOperator.Equal | BinaryOperator.NotEqual =>
          assertType(typ1, typ2, t)
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
}
