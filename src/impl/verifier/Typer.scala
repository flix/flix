package impl.verifier

import impl.logic.Constraint.Fact
import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic._
import impl.runtime.{Error, Unification}
import syntax.Constraints.RichConstraint
import syntax.Predicates.RichPredicate
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

    // type check all constraints
    for (constraint <- p.constraints) {
      //typecheck(constraint)
    }
  }

  /**
   * Typechecks the given constraint `c`.
   */
  def typecheck(c: Constraint): Unit = {
    val predicates = c.head :: c.body

    predicates.foldLeft(Map.empty[VSym, Type]) {
      case (typenv0, p) =>
        val actual = typecheck(p, typenv0)
        val declared = p.typ

        Unification.unify(actual, declared, typenv0) match {
          case None => throw new RuntimeException(s"Type Error in Constraint: '${c.fmt}'. Unable to unify actual type '${actual.fmt}' with declared type ${declared.fmt}.")
          case Some(typenv1) => typenv1
        }
    }
  }

  /**
   * Returns the type of the given predicate `p` under the given typing environment `typenv`.
   *
   * Throws an exception if the term cannot be typed.
   */
  def typecheck(p: Predicate, typenv: Map[VSym, Type] = Map.empty): Type = {
    def visit(ts: List[Term]): Type = ts match {
      case Nil => throw new RuntimeException(s"Unable to type zero-arity predicate: ${p.fmt}.")
      case x :: Nil => typecheck(x, typenv)
      case x :: xs => Type.Function(typecheck(x, typenv), visit(xs))
    }
    visit(p.terms)
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
        Type.Set(assertEqual(types.toSeq))
      }

    case Term.Var(s) => typenv.getOrElse(s, Type.Var(s)) // TODO: We are not required to check that all types are closed.
    case Term.Abs(s, typ1, t1) =>
      val typ2 = typecheck(t1, typenv + (s -> typ1))
      Type.Function(typ1, typ2)
    case Term.App(t1, t2) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      typ1 match {
        case Type.Function(a, b) if a == typ2 => b
        case Type.Function(a, b) if typ2.isInstanceOf[Type.Var] => b // TODO: Hack until we get unification for terms.
        case Type.Function(a, b) => throw Error.StaticTypeError(a, typ2, t)
        case _ => throw Error.StaticTypeError(Type.Function(Type.Var(Symbol.VariableSymbol("t0")), Type.Var(Symbol.VariableSymbol("t0"))), typ1, t)
      }

    case Term.IfThenElse(t1, t2, t3) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      val typ3 = typecheck(t3, typenv)
      assertType(Type.Bool, typ1, t1)
      assertEqual(List(typ2, typ3))

    case Term.Match(t1, rules) =>
      // type check the match value
      val typ1 = typecheck(t1, typenv)
      // type check the rules
      val types = typecheck(typ1, rules)
      assertEqual(types)

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

        case BinaryOperator.And | BinaryOperator.Or =>
          assertType(Type.Bool, typ1, t1)
          assertType(Type.Bool, typ2, t2)
          Type.Bool

        case BinaryOperator.Minimum | BinaryOperator.Maximum =>
          assertType(Type.Int, typ1, t1)
          assertType(Type.Int, typ2, t2)
          Type.Int

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
   * Typechecks a sequence of `rules` against a match value of the type `typ`.
   *
   * Returns a list of types of each rule.
   */
  private def typecheck(typ: Type, rules: List[(Pattern, Term)]): List[Type] = rules.map {
    case (p, t) => typecheck(t, Unification.unify(p, typ))
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

  /**
   * Asserts that given sequence of types are actually the same types.
   */
  private def assertEqual(types: Seq[Type]): Type = types.reduce[Type] {
    case (typ1, typ2) if typ1 == typ2 => typ1
    case (typ1, typ2) if typ1 != typ2 => throw new RuntimeException(s"Expected equal types, but got: ${typ1.fmt} and ${typ2.fmt}")
  }

}
