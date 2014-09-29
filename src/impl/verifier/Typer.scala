package impl.verifier

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
        environment(p, typ)
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
      case x :: Nil => typer(x, typenv)
      case x :: xs => Type.Function(typer(x, typenv), visit(xs))
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
  def typecheck(t: Term): Type = typer(t, Map.empty[VSym, Type])

  /**
   * Returns the type of the given term `t` under the given typing enviroment `typenv`.
   *
   * Throws a static type error if the term cannot be typed.
   */
  def typer(t: Term, typenv: Map[VSym, Type]): Type = t match {
    case Term.Unit => Type.Unit
    case Term.Bool(b) => Type.Bool
    case Term.Int(i) => Type.Int
    case Term.Str(s) => Type.Str
    case Term.Set(xs) =>
      if (xs.isEmpty)
        Type.Set(Type.Var(Symbol.freshVariableSymbol("t")))
      else {
        val types = xs.map(x => typer(x, typenv))
        Type.Set(unify(types, typenv, t))
      }

    case Term.Var(x) => typenv.getOrElse(x, Type.Var(x))
    case Term.Abs(s, typ1, t1) =>
      val typ2 = typer(t1, typenv + (s -> typ1))
      Type.Function(typ1, typ2)
    case Term.App(t1, t2) =>
      val typ1 = typer(t1, typenv)
      val typ2 = typer(t2, typenv)
      val abs = Type.Function(typ2, Type.Var(Symbol.VariableSymbol("t0")))
      val Type.Function(_, b) = unify(abs, typ1, typenv, t)
      b

    case Term.IfThenElse(t1, t2, t3) =>
      val typ1 = typer(t1, typenv)
      val typ2 = typer(t2, typenv)
      val typ3 = typer(t3, typenv)
      unify(Type.Bool, typ1, typenv, t)
      unify(typ2, typ3, typenv, t)

    case Term.Match(t1, rules) =>
      // type check the match value
      val typ1 = typer(t1, typenv)
      // type check the rules
      val types = typecheck(typ1, rules, typenv)
      unify(types, typenv, t)

    case Term.UnaryOp(op, t1) =>
      import UnaryOperator._
      val typ1 = typer(t1, typenv)
      op match {
        case Not => unify(Type.Bool, typ1, typenv, t)
        case UnaryPlus => unify(Type.Int, typ1, typenv, t)
        case UnaryMinus => unify(Type.Int, typ1, typenv, t)
      }

    case Term.BinaryOp(op, t1, t2) =>
      import BinaryOperator._
      val typ1 = typer(t1, typenv)
      val typ2 = typer(t2, typenv)
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
      val typ1 = typer(t1, typenv)

      if (typ.ts.contains(typ1))
        throw new RuntimeException()
      else
        typ

    case Term.Tuple2(t1, t2) => Type.Tuple2(typer(t1, typenv), typer(t2, typenv))
    case Term.Tuple3(t1, t2, t3) => Type.Tuple3(typer(t1, typenv), typer(t2, typenv), typer(t3, typenv))
    case Term.Tuple4(t1, t2, t3, t4) => Type.Tuple4(typer(t1, typenv), typer(t2, typenv), typer(t3, typenv), typer(t4, typenv))
    case Term.Tuple5(t1, t2, t3, t4, t5) => Type.Tuple5(typer(t1, typenv), typer(t2, typenv), typer(t3, typenv), typer(t4, typenv), typer(t5, typenv))
  }

  /**
   * Typechecks a sequence of `rules` against a match value of the type `typ`.
   *
   * Returns a list of types of each rule.
   */
  private def typecheck(typ: Type, rules: List[(Pattern, Term)], env: Map[VSym, Type]): List[Type] = rules.map {
    case (p, t) => typer(t, env ++ Unification.unify(p, typ))
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
      val typ1 = typer(t1, typenv)
      val typ2 = typer(t2, typenv)
      unify(typ1, typ2, typenv, p)
      Type.Bool

    case Proposition.NotEq(t1, t2) =>
      val typ1 = typer(t1, typenv)
      val typ2 = typer(t2, typenv)
      unify(typ1, typ2, typenv, p)
      Type.Bool
  }


  /**
   * Returns a typing environment where every free variable in the given predicate `p`
   * is mapped to its appropriate type according to the given type `typ`.
   *
   * Assumes/Requires that the type of `t` is `typ`.
   */
  private def environment(p: Predicate, typ0: Type): Map[VSym, Type] = {
    (p.terms zip typ0.unfold).foldLeft(Map.empty[VSym, Type]) {
      case (env, (typ, term)) => env ++ environment(typ, term)
    }
  }

  /**
   * Returns a typing environment where every free variable in the given term `t`
   * is mapped to its appropriate type according to the given type `typ`.
   *
   * Assumes/Requires that the type of `t` is `typ`.
   */
  // TODO: Rewrite to work "up to abs"
  private def environment(t: Term, typ: Type): Map[VSym, Type] = (t, typ) match {
    case (Term.Unit, Type.Unit) => Map.empty
    case (Term.Bool(b), Type.Bool) => Map.empty
    case (Term.Int(i), Type.Int) => Map.empty
    case (Term.Str(s), Type.Str) => Map.empty
    case (Term.Set(xs), Type.Set(typ1)) => xs.map(x => environment(x, typ1)).reduce(_ ++ _)

    case (Term.Var(x), _) => Map(x -> typ)
    case (Term.Abs(x, _, t1), Type.Function(a, b)) => Map.empty

    case (Term.App(t1, t2), _) =>
      val typ2 = Type.Var(Symbol.freshVariableSymbol("t"))
      val typ1 = Type.Function(typ2, typ)
      environment(t1, typ1) ++ environment(t2, typ2)

    case (Term.IfThenElse(t1, t2, t3), _) => ???
    case (Term.Match(t1, rules), _) => ???
    case (Term.UnaryOp(op, t1), _) => ???
    case (Term.BinaryOp(op, t1, t2), _) => ???

    case (Term.Tag(n, t1, typ1), _) =>
      val typ2 = typ1.ts.collectFirst {
        case Type.Tag(m, typ3) if n == m => typ3
      }
      environment(t1, typ2.get)
    case (Term.Tuple2(t1, t2), Type.Tuple2(typ1, typ2)) =>
      environment(t1, typ1) ++ environment(t2, typ2)
    case (Term.Tuple3(t1, t2, t3), Type.Tuple3(typ1, typ2, typ3)) =>
      environment(t1, typ1) ++ environment(t2, typ2) ++ environment(t3, typ3)
    case (Term.Tuple4(t1, t2, t3, t4), Type.Tuple4(typ1, typ2, typ3, typ4)) =>
      environment(t1, typ1) ++ environment(t2, typ2) ++ environment(t3, typ3) ++ environment(t4, typ4)
    case (Term.Tuple5(t1, t2, t3, t4, t5), Type.Tuple5(typ1, typ2, typ3, typ4, typ5)) =>
      environment(t1, typ1) ++ environment(t2, typ2) ++ environment(t3, typ3) ++ environment(t4, typ4) ++ environment(t5, typ5)

    case _ => throw new RuntimeException(s"Internal Error: Term: ${t.fmt} should be typable with the given type: ${typ.fmt}")
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
