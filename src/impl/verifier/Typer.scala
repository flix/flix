package impl.verifier

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic._
import impl.runtime.Error
import syntax.Patterns.RichPattern
import syntax.Types.RichType

object Typer {

  /**
   * Typechecks the given logic program `p`.
   */
  def typecheck(p: Program): Unit = {
    // finds all types in the program
    val types = p.facts.map(_.head.typ) ++ p.rules.flatMap(p => p.head.typ :: p.body.map(_.typ))

    // verify that all base types have lattice declarations
    for (typ <- types) {
      val baseType = typ.resultType
      p.lookupBot(baseType).getOrElse(throw Error.MissingBot(baseType))
      p.lookupLeq(baseType).getOrElse(throw Error.MissingLeq(baseType))
      p.lookupLub(baseType).getOrElse(throw Error.MissingLub(baseType))
    }
  }

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
    case Term.Set(xs) =>
      if (xs.isEmpty)
        Type.Set(Type.Var(Symbol.freshVariableSymbol("t")))
      else {
        val types = xs.map(x => typecheck(x, typenv))
        if (types.size == 1)
          Type.Set(types.head)
        else
          throw new RuntimeException(s"The subterms have different types: {${types.mkString(", ")}}")
      }

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
        case _ => throw Error.StaticTypeError(Type.Function(Type.Var(Symbol.VariableSymbol("t0")), Type.Var(Symbol.VariableSymbol("t0"))), typ1, t)
      }

    case Term.Match(t1, rules) =>
      val typ1 = typecheck(t1, typenv)

      val types = rules.map {
        case (p, t2) =>
          val typenv2 = typecheck(p, typ1)
          typecheck(t2, typenv2)
      }

      types.reduce[Type] {
        case (typ2, typ3) if typ2 == typ3 => typ2
        case _ => (???): Type // Type error
      }

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
          Type.Bool

        case BinaryOperator.Equal | BinaryOperator.NotEqual =>
          assertType(typ1, typ2, t)
          Type.Bool

        case BinaryOperator.Union =>
          assertType(typ1, typ2, t)

        case BinaryOperator.Subset =>
          assertType(typ1, typ2, t)
          Type.Bool
      }

    case Term.Tag(s, t1, typ) =>
      val typ1 = typecheck(t1, typenv)
      if (typ.ts contains Type.Tag(s, typ1))
        typ
      else
        throw Error.StaticTypeError(typ1, typ, t)

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
   * Unifies the given pattern `p` with the given type `typ`.
   *
   * Returns a type environment where all free variables in the pattern has been bound to their appropriate type.
   *
   * NB: Pattern variables are assumed not to occur more than once.
   */
  private def typecheck(p: Pattern, typ: Type): Map[VSym, Type] = (p, typ) match {
    case (Pattern.Wildcard, _) => Map.empty
    case (Pattern.Var(s), typ1) => Map(s -> typ1)

    case (Pattern.Unit, Type.Unit) => Map.empty
    case (Pattern.Bool(b), Type.Bool) => Map.empty
    case (Pattern.Int(i), Type.Int) => Map.empty
    case (Pattern.Str(s), Type.Str) => Map.empty

    case (Pattern.Tag(s1, p1), Type.Sum(ts)) =>
      ts.collectFirst {
        case Type.Tag(s2, typ2) if s1 == s2 => typecheck(p1, typ2)
      }.get

    case (Pattern.Tuple2(p1, p2), Type.Tuple2(typ1, typ2)) =>
      typecheck(p1, typ1) ++ typecheck(p2, typ2)
    case (Pattern.Tuple3(p1, p2, p3), Type.Tuple3(typ1, typ2, typ3)) =>
      typecheck(p1, typ1) ++ typecheck(p2, typ2) ++ typecheck(p3, typ3)
    case (Pattern.Tuple4(p1, p2, p3, p4), Type.Tuple4(typ1, typ2, typ3, typ4)) =>
      typecheck(p1, typ1) ++ typecheck(p2, typ2) ++ typecheck(p3, typ3) ++ typecheck(p4, typ4)
    case (Pattern.Tuple5(p1, p2, p3, p4, p5), Type.Tuple5(typ1, typ2, typ3, typ4, typ5)) =>
      typecheck(p1, typ1) ++ typecheck(p2, typ2) ++ typecheck(p3, typ3) ++ typecheck(p4, typ4) ++ typecheck(p5, typ5)

    case _ => throw new RuntimeException(s"Unable to typecheck pattern ${p.fmt} against type ${typ.fmt}")
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
