package impl.verifier

import ca.uwaterloo.flix.lang.ast.{BinaryOperator, UnaryOperator}
import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic._
import impl.runtime.{Error, Unification}
import syntax.Predicates.RichPredicate
import syntax.Terms.RichTerm
import syntax.Types.RichType

object Typer {

  /**
   * Typechecks the given program `p`.
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
      typecheck(constraint)
    }
  }

  /**
   * Typechecks the given constraint `c`.
   */
  def typecheck(c: Constraint): Unit = {
    val predicates = c.head :: c.body

    val env = predicates.foldLeft(Map.empty[VSym, Type]) {
      case (typenv0, p) =>
        val typ = typecheck(p, c, typenv0)
        Map.empty
    }

    if (c.proposition.nonEmpty) {
      typecheck(c.proposition.get, env)
    }
  }

  /**
   * Returns the type of the given predicate `p` under the given typing environment `typenv`.
   *
   * Throws a static type error if the predicate cannot be typed.
   */
  def typecheck(p: Predicate, c: Constraint, typenv: Map[VSym, Type]): Type = {
    def visit(ts: List[Term]): Type = ts match {
      case Nil => throw new RuntimeException(s"Unable to type zero-arity predicate: ${p.fmt}.")
      case x :: Nil => typecheck(x, typenv)
      case x :: xs => Type.Function(typecheck(x, typenv), visit(xs))
    }
    val actual = visit(p.terms)
    val declared = p.typ

    Unification.unify(actual, declared, typenv) match {
      case None => throw Error.PredicateTypeError(List(actual, declared), p, c)
      case Some(r) => r
    }
  }

  /**
   * Returns the type of the given term `t` under the empty typing environment.
   *
   * Throws a static type error if the term cannot be typed.
   */
  def typecheck(t: Term): Type = typecheck(t, Map.empty[VSym, Type])

  /**
   * Returns the type of the given term `t` under the given typing enviroment `typenv`.
   *
   * Throws a static type error if the term cannot be typed.
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
        Type.Set(unify(types, typenv, t))
      }

    case Term.Var(x) => typenv.getOrElse(x, Type.Var(x))
    case Term.Abs(s, typ1, t1) =>
      val typ2 = typecheck(t1, typenv + (s -> typ1))
      Type.Function(typ1, typ2)
    case Term.App(t1, t2) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      val abs = Type.Function(typ2, Type.Var(Symbol.VariableSymbol("t0")))
      val Type.Function(_, b) = unify(abs, typ1, typenv, t)
      b

    case Term.IfThenElse(t1, t2, t3) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      val typ3 = typecheck(t3, typenv)
      unify(Type.Bool, typ1, typenv, t)
      unify(typ2, typ3, typenv, t)

    case Term.Match(t1, rules) =>
      // type check the match value
      val typ1 = typecheck(t1, typenv)
      // type check the rules
      val types = typecheck(typ1, rules, typenv)
      unify(types, typenv, t)

    case Term.UnaryOp(op, t1) =>
      import UnaryOperator._
      val typ1 = typecheck(t1, typenv)
      op match {
        case Not => unify(Type.Bool, typ1, typenv, t)
        case UnaryPlus => unify(Type.Int, typ1, typenv, t)
        case UnaryMinus => unify(Type.Int, typ1, typenv, t)
      }

    case Term.BinaryOp(op, t1, t2) =>
      import BinaryOperator._
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      op match {
        case Plus | Minus | Times | Divide | Modulo | Minimum | Maximum =>
          unify(Type.Int, typ1, typenv, t)
          unify(Type.Int, typ2, typenv, t)
          Type.Int

        case Less | LessEqual | Greater | GreaterEqual =>
          unify(Type.Int, typ1, typenv, t)
          unify(Type.Int, typ2, typenv, t)
          Type.Bool

        case Equal | NotEqual =>
          unify(typ1, typ2, typenv, t)
          Type.Bool

        case BinaryOperator.And | BinaryOperator.Or =>
          unify(Type.Bool, typ1, typenv, t)
          unify(Type.Bool, typ2, typenv, t)
          Type.Bool

        case BinaryOperator.Union =>
          unify(typ1, typ2, typenv, t)

        case BinaryOperator.Subset =>
          unify(typ1, typ2, typenv, t)
          Type.Bool
      }

    case Term.Tag(n, t1, typ) =>
      val typ1 = typecheck(t1, typenv)

      if (typ.ts.contains(typ1))
        throw new RuntimeException()
      else
        typ

    case Term.Tuple2(t1, t2) => Type.Tuple2(typecheck(t1, typenv), typecheck(t2, typenv))
    case Term.Tuple3(t1, t2, t3) => Type.Tuple3(typecheck(t1, typenv), typecheck(t2, typenv), typecheck(t3, typenv))
    case Term.Tuple4(t1, t2, t3, t4) => Type.Tuple4(typecheck(t1, typenv), typecheck(t2, typenv), typecheck(t3, typenv), typecheck(t4, typenv))
    case Term.Tuple5(t1, t2, t3, t4, t5) => Type.Tuple5(typecheck(t1, typenv), typecheck(t2, typenv), typecheck(t3, typenv), typecheck(t4, typenv), typecheck(t5, typenv))
  }

  /**
   * Typechecks a sequence of `rules` against a match value of the type `typ`.
   *
   * Returns a list of types of each rule.
   */
  private def typecheck(typ: Type, rules: List[(Pattern, Term)], env: Map[VSym, Type]): List[Type] = rules.map {
    case (p, t) => typecheck(t, env ++ Unification.unify(p, typ))
  }

  /**
   * Returns the type of the given proposition under the given typing environment.
   *
   * Throws a static type error if the proposition cannot be typed.
   */
  private def typecheck(p: Proposition, typenv: Map[VSym, Type]): Type = p match {
    case Proposition.True => Type.Bool
    case Proposition.False => Type.Bool
    case Proposition.Not(p1) =>
      val typ1 = typecheck(p1, typenv)
      unify(Type.Bool, typ1, typenv, p)

    case Proposition.Conj(ps) =>
      val types = ps.map(p => typecheck(p, typenv))
      unify(Type.Bool :: types, typenv, p)

    case Proposition.Disj(ps) =>
      val types = ps.map(p => typecheck(p, typenv))
      unify(Type.Bool :: types, typenv, p)

    case Proposition.Eq(t1, t2) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      unify(typ1, typ2, typenv, p)
      Type.Bool

    case Proposition.NotEq(t1, t2) =>
      val typ1 = typecheck(t1, typenv)
      val typ2 = typecheck(t2, typenv)
      unify(typ1, typ2, typenv, p)
      Type.Bool
  }

  /**
   * TODO
   */
  private def environment(p: Predicate): Map[VSym, Type] = {
    p.terms.foldLeft(Map.empty[VSym, Type]) {
      case (env, t) => merge(environment(t), env)
    }
  }

  /**
   * TODO
   */
  private def environment(t: Term): Map[VSym, Type] = t match {
    case Term.Unit => Map.empty
    case Term.Bool(b) => Map.empty
    case Term.Int(i) => Map.empty
    case Term.Str(s) => Map.empty
    case Term.Set(xs) => xs.map(environment).reduce(_ ++ _)

    case Term.App(t1, t2) => ???

    case Term.Tag(n, t1, typ1) => environment(t1)
    case Term.Tuple2(t1, t2) => merge(environment(t1), environment(t2))
    case Term.Tuple3(t1, t2, t3) => merge(environment(t1), environment(t2), environment(t3))
    case Term.Tuple4(t1, t2, t3, t4) => merge(environment(t1), environment(t2), environment(t3), environment(t4))
    case Term.Tuple5(t1, t2, t3, t4, t5) => merge(environment(t1), environment(t2), environment(t3), environment(t4), environment(t5))

    case _ => throw new RuntimeException(s"Unexpected term: ${t.fmt}")
  }

  private def merge(xs: Map[VSym, Type]*): Map[VSym, Type] = {
    ???
  }

  /**
   * Returns the result of unifying the given types `typ1` and `typ2` under the given type environment `typenv` for the given term `t`.
   *
   * Throws a static type error if unification is not possible.
   */
  private def unify(typ1: Type, typ2: Type, typenv: Map[VSym, Type], t: Term): Type = Unification.unify(typ1, typ2, typenv) match {
    case None => throw Error.TermTypeError(List(Type.Int, typ1), t)
    case Some(r) => r
  }

  /**
   * Returns the result of unifying the given types `types` under the given type environment `typenv` for the given term `t`.
   *
   * Throws a static type error if unification is not possible.
   */
  private def unify(types: Traversable[Type], typenv: Map[VSym, Type], t: Term): Type = Unification.unify(types.toList, typenv) match {
    case None => throw Error.TermTypeError(types.toList, t)
    case Some(r) => r
  }

  /**
   * Returns the result of unifying the given types `typ1` and `typ2` under the given type environment `typenv` for the given proposition `p`.
   *
   * Throws a static type error if unification is not possible.
   */
  private def unify(typ1: Type, typ2: Type, typenv: Map[VSym, Type], p: Proposition): Type = Unification.unify(typ1, typ2, typenv) match {
    case None => throw Error.PropositionTypeError(List(Type.Int, typ1), p)
    case Some(r) => r
  }

  /**
   * Returns the result of unifying the given types `types` under the given type environment `typenv` for the given proposition `p`.
   *
   * Throws a static type error if unification is not possible.
   */
  private def unify(types: Traversable[Type], typenv: Map[VSym, Type], p: Proposition): Type = Unification.unify(types.toList, typenv) match {
    case None => throw Error.PropositionTypeError(types.toList, p)
    case Some(r) => r
  }
}
