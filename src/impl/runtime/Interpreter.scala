package impl.runtime

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic._
import syntax.Propositions.RichProposition
import syntax.Values.RichValue

import scala.util.Try

object Interpreter {

  /**
   * Returns the value of evaluating the given term `t` under the given environment `env`.
   *
   * If the environment is not specified, the empty environment is assumed.
   *
   * Throws an exception if the term is not reducible, i.e. it contains free variables under the environment.
   */
  def evaluate(t: Term): Value = t match {
    case Term.Unit => Value.Unit
    case Term.Bool(b) => Value.Bool(b)
    case Term.Int(i) => Value.Int(i)
    case Term.Str(s) => Value.Str(s)
    case Term.Set(xs) => Value.Set(xs.map(x => evaluate(x)))

    case Term.Var(s) => throw Error.UnboundVariableError(s)
    case Term.Abs(s, typ, t1) => Value.Abs(s, typ, t1)
    case Term.App(t1, t2) =>
      val v1 = evaluate(t1).toAbs
      val v2 = evaluate(t2)
      val x = v1.s
      val y = Symbol.freshVariableSymbol(x)
      val r = v1.t.rename(x, y).substitute(y, v2.toTerm)
      evaluate(r)

    case Term.IfThenElse(t1, t2, t3) =>
      val cond = evaluate(t1).toBool
      if (cond)
        evaluate(t2)
      else
        evaluate(t3)

    case Term.Match(t1, rules) =>
      val v1 = evaluate(t1)
      matchRule(rules, v1) match {
        case None => throw new RuntimeException(s"Unmatched value ${v1.fmt}")
        case Some((t2, env)) => evaluate(t2.substitute(env))
      }

    case Term.UnaryOp(op, t1) =>
      val v1 = evaluate(t)
      apply(op, v1)

    case Term.BinaryOp(op, t1, t2) =>
      val v1 = evaluate(t1)
      val v2 = evaluate(t2)
      apply(op, v1, v2)

    case Term.Tag(s, t1, typ) =>
      val v1 = evaluate(t1)
      Value.Tag(s, v1, typ)

    case Term.Tuple2(t1, t2) =>
      val v1 = evaluate(t1)
      val v2 = evaluate(t2)
      Value.Tuple2(v1, v2)

    case Term.Tuple3(t1, t2, t3) =>
      val v1 = evaluate(t1)
      val v2 = evaluate(t2)
      val v3 = evaluate(t3)
      Value.Tuple3(v1, v2, v3)

    case Term.Tuple4(t1, t2, t3, t4) =>
      val v1 = evaluate(t1)
      val v2 = evaluate(t2)
      val v3 = evaluate(t3)
      val v4 = evaluate(t4)
      Value.Tuple4(v1, v2, v3, v4)

    case Term.Tuple5(t1, t2, t3, t4, t5) =>
      val v1 = evaluate(t1)
      val v2 = evaluate(t2)
      val v3 = evaluate(t3)
      val v4 = evaluate(t4)
      val v5 = evaluate(t5)
      Value.Tuple5(v1, v2, v3, v4, v5)
  }

  /**
   * Returns a predicate with ground terms for the given predicate `p` under the environment `env`.
   *
   * If the environment is not specified, the empty environment is assumed.
   */
  def evaluatePredicate(p: Predicate, env: Map[VSym, Value] = Map.empty): Predicate.GroundPredicate =
    Predicate.GroundPredicate(p.name, p.terms map (t => evaluate(t.substitute(env))), p.typ)

  /**
   * Optionally returns a predicate with ground terms for the given predicate `p` under the environment `env`.
   *
   * If the environment is not specified, the empty environment is assumed.
   *
   * Returns `None` if the predicate contains non-ground terms after evaluation.
   */
  def evaluatePredicateOpt(p: Predicate, env: Map[VSym, Value] = Map.empty): Option[Predicate.GroundPredicate] = {
    val terms = p.terms.map(t => evaluateOpt(t, env))
    if (terms.exists(_.isEmpty))
      None
    else
      Some(Predicate.GroundPredicate(p.name, terms.map(_.get), p.typ))
  }

  /**
   * Optionally returns the value of evaluating the given term `t` under the given environment `env`.
   *
   * If the environment is not specified, the empty environment is assumed.
   *
   * Returns `None` iff the term does not reduce to a value under the given environment.
   */
  // TODO: Eliminate the use of Try.
  def evaluateOpt(t: Term, env: Map[VSym, Value] = Map.empty): Option[Value] =
    Try(evaluate(t.substitute(env))).toOption

  /**
   * Optionally returns a pair of a term and environment for which one of the given `rules` match the given value `v`.
   */
  private def matchRule(rules: List[(Pattern, Term)], v: Value): Option[(Term, Map[VSym, Value])] = rules match {
    case Nil => None
    case (p, t) :: rest => Unification.unify(p, v) match {
      case None => matchRule(rest, v)
      case Some(env) => Some((t, env))
    }
  }

  /**
   * Returns the result of applying the unary operator `op` to the given value `v`.
   */
  def apply(op: UnaryOperator, v: Value): Value = op match {
    case UnaryOperator.Not => Value.Bool(!v.toBool)
    case UnaryOperator.UnaryPlus => Value.Int(v.toInt)
    case UnaryOperator.UnaryMinus => Value.Int(-v.toInt)
  }

  /**
   * Returns the result of applying the binary operator `op` to the given values `v1` and `v2`.
   */
  def apply(op: BinaryOperator, v1: Value, v2: Value): Value = op match {
    case BinaryOperator.Plus => Value.Int(v1.toInt + v2.toInt)
    case BinaryOperator.Minus => Value.Int(v1.toInt - v2.toInt)
    case BinaryOperator.Times => Value.Int(v1.toInt * v2.toInt)
    case BinaryOperator.Divide => Value.Int(v1.toInt / v2.toInt)
    case BinaryOperator.Modulo => Value.Int(v1.toInt % v2.toInt)
    case BinaryOperator.Less => Value.Bool(v1.toInt < v2.toInt)
    case BinaryOperator.LessEqual => Value.Bool(v1.toInt <= v2.toInt)
    case BinaryOperator.Greater => Value.Bool(v1.toInt > v2.toInt)
    case BinaryOperator.GreaterEqual => Value.Bool(v1.toInt >= v2.toInt)
    case BinaryOperator.Equal => Value.Bool(v1 == v2)
    case BinaryOperator.NotEqual => Value.Bool(v1 != v2)
    case BinaryOperator.And => Value.Bool(v1.toBool && v2.toBool)
    case BinaryOperator.Or => Value.Bool(v1.toBool || v2.toBool)
    case BinaryOperator.Minimum => Value.Int(math.min(v1.toInt, v2.toInt))
    case BinaryOperator.Maximum => Value.Int(math.max(v1.toInt, v2.toInt))
    case BinaryOperator.Union => Value.Set(v1.toSet ++ v2.toSet)
    case BinaryOperator.Subset => Value.Bool(v1.toSet subsetOf v2.toSet)
  }

  /**
   * Returns `true` iff the given proposition `p` is satisfied under the given environment `env`.
   *
   * Throws an exception if a variable in `p` is not bound by the environment `env`.
   */
  def satisfiable(p: Proposition, env: Map[VSym, Value]): Boolean = {
    if (!p.isClosed(env)) {
      throw new RuntimeException(s"The propositional formula ${p.fmt} is not closed under the environment ${env.keySet.mkString(", ")}")
    }
    p match {
      case Proposition.True => true
      case Proposition.False => false
      case Proposition.Not(p1) => !satisfiable(p1, env)
      case Proposition.Conj(ps) => ps.forall(p1 => satisfiable(p1, env))
      case Proposition.Disj(ps) => ps.exists(p1 => satisfiable(p1, env))
      case Proposition.Eq(x, y) => x.substitute(env) == y.substitute(env) // TODO: Careful w.r.t. monotonicity
      case Proposition.NotEq(x, y) => x.substitute(env) != y.substitute(env) // TODO: Careful w.r.t. monotonicity
    }
  }

}
