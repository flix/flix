package impl.verifier

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic.{BinaryOperator, Term, Type, UnaryOperator}
import impl.runtime.Error

object Typer {

  /**
   * Returns the type of the given term `t` under the empty typing enviroment.
   *
   * Throws an exception if the term cannot be typed.
   */
  def typecheck(t: Term): Type = typecheck(t, Map.empty[VSym, Type])

  /**
   * Returns the type of the given term `t` under the given typing enviroment `typenv`.
   *
   * Throws an exception if the term cannot be typed.
   */
  def typecheck(t: Term, typenv: Map[VSym, Type]): Type = t match {
    case Term.Unit => Type.Unit
    case Term.Bool(b) => Type.Bool
    case Term.Int(i) => Type.Int
    case Term.Str(s) => Type.Str

    case Term.Var(s) => typenv.getOrElse(s, throw Error.UnboundVariableError(s))
    case Term.Abs(s, typ1, t1) =>
      val typ2 = typecheck(t1, typenv + (s -> typ1))
      Type.Function(typ1, typ2)
    case Term.App(t1, t2) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      typ1 match {
        case Type.Function(a, b) if a == typ2 => b
        case Type.Function(a, b) => throw Error.StaticTypeError(a, typ2, t)
        case _ => throw Error.UnexpectedTypeError(t, s"The term '$t1' does not have a function type in the application '$t'.")
      }

    case Term.Let(x, t1, t2) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv + (x -> typ1))
      typ2

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

    case Term.Tagged(s, t1, typ) =>
      val typ1 = typecheck(t1, typenv)
      if (typ.ts contains Type.Tagged(s, typ1))
        typ
      else
        throw Error.StaticTypeError(typ1, typ, t)

    case Term.Case(t1, cases) =>
      val typ1 = typecheck(t1, typenv)
      typ1 match {
        case Type.Sum(types) =>
          for ((typ2, (x, t2)) <- types zip cases.values) {
            val resultType = typecheck(t2, typenv + (x -> typ2))
          }
          ??? // TODO The result type should be the same.
        case _ => ???
      }

    case Term.Tuple2(t1, t2) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      Type.Tuple2(typ1, typ2)

    case Term.Tuple3(t1, t2, t3) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      val typ3 = typecheck(t3, typenv)
      Type.Tuple3(typ1, typ2, typ3)

    case Term.Tuple4(t1, t2, t3, t4) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      val typ3 = typecheck(t3, typenv)
      val typ4 = typecheck(t4, typenv)
      Type.Tuple4(typ1, typ2, typ3, typ4)

    case Term.Tuple5(t1, t2, t3, t4, t5) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      val typ3 = typecheck(t3, typenv)
      val typ4 = typecheck(t4, typenv)
      val typ5 = typecheck(t5, typenv)
      Type.Tuple5(typ1, typ2, typ3, typ4, typ5)
  }

  /**
   * Asserts that the given `expected` type is equal to the `actual` type for the given term `t`.
   * Otherwise, throws an exception.
   */
  private def assertType(expected: Type, actual: Type, t: Term): Type =
    if (expected == actual)
      actual
    else
      throw Error.StaticTypeError(expected, actual, t)
}
