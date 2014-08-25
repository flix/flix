package impl.runtime

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic.{Predicate, Term, Value}

object Unification {
  /////////////////////////////////////////////////////////////////////////////
  // Predicate Unification                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Unifies all terms in the given predicate `p1` with the predicate `p2` iff they share the same symbol.
   */
  def unify(p1: Predicate, p2: Predicate, env0: Map[VSym, Term]): Option[Map[VSym, Term]] =
    if (p1.name != p2.name)
      None
    else
      Unification.unifyTerms(p1.terms, p2.terms, env0)


  /////////////////////////////////////////////////////////////////////////////
  // Term-Value Unification                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
   * Unifies the term `t` with the value `v` under the given environment `env0`.
   */
  def unify(t: Term, v: Value, env0: Map[VSym, Value]): Option[Map[VSym, Value]] = (t, v) match {
    case (Term.Bool(b1), Value.Bool(b2)) if b1 == b2 => Some(env0)
    case (Term.Int(i1), Value.Int(i2)) if i1 == i2 => Some(env0)
    case (Term.Str(s1), Value.Str(s2)) if s1 == s2 => Some(env0)
    case (Term.Variable(s), v2) => env0.get(s) match {
      case None => Some(env0 + (s -> v2))
      case Some(v3) if v2 != v3 => None
      case Some(v3) if v2 == v3 => Some(env0)
    }
    case _ => None
  }

  /**
   * Unifies all terms in `tx` with all terms in `ty` under the initial environment `env0`.
   */
  def unifyValues(tx: List[Term], ty: List[Value], env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    (tx zip ty).foldLeft(Option(env0)) {
      case (env, (t1, t2)) => env.flatMap(e => unify(t1, t2, e))
    }

  /////////////////////////////////////////////////////////////////////////////
  // Term-Term Unification                                                   //
  /////////////////////////////////////////////////////////////////////////////
  // TODO: Consider substitions.

  /**
   * Unifies the term `t1` with the term `t2` under the given environment `env0`.
   */
  def unify(t1: Term, t2: Term, env0: Map[VSym, Term]): Option[Map[VSym, Term]] = (t1, t2) match {
    case (Term.Bool(b1), Term.Bool(b2)) if b1 == b2 => Some(env0)
    case (Term.Int(i1), Term.Int(i2)) if i1 == i2 => Some(env0)
    case (Term.Str(s1), Term.Str(s2)) if s1 == s2 => Some(env0)
    case (Term.Variable(x), Term.Variable(y)) => (env0.get(x), env0.get(y)) match {
      case (None, None) => Some(substitute(x, Term.Variable(y), env0) + (y -> Term.Variable(x)))
      case (None, Some(tt2)) => Some(substitute(x, tt2, env0) + (x -> tt2))
      case (Some(tt1), None) => Some(substitute(x, tt1, env0) + (y -> tt1))
      case (Some(tt1), Some(tt2)) => unify(tt1, tt2, env0)
    }
    case (Term.Variable(x), t) => env0.get(x) match {
      case None =>
        if (t.freeVariables contains x)
          None // Ensure that y does not occur free in t.
        else
          Some(substitute(x, t, env0) + (x -> t))
      case Some(tt) => unify(tt, t, env0)
    }
    case (t, Term.Variable(y)) => env0.get(y) match {
      case None =>
        if (t.freeVariables contains y)
          None // Ensure that y does not occur free in t.
        else
          Some(substitute(y, t, env0) + (y -> t))
      case Some(tt) => unify(t, tt, env0)
    }

    // TODO: More

    case (Term.Tuple2(x1, x2), Term.Tuple2(y1, y2)) =>
      for (env1 <- unify(x1, y1, env0);
           env2 <- unify(x2, y2, env1))
      yield env2
    case (Term.Tuple3(x1, x2, x3), Term.Tuple3(y1, y2, y3)) =>
      for (env1 <- unify(x1, y1, env0);
           env2 <- unify(x2, y2, env1);
           env3 <- unify(x3, y3, env2))
      yield env3
    case (Term.Tuple4(x1, x2, x3, x4), Term.Tuple4(y1, y2, y3, y4)) =>
      for (env1 <- unify(x1, y1, env0);
           env2 <- unify(x2, y2, env1);
           env3 <- unify(x3, y3, env2);
           env4 <- unify(x4, y4, env3))
      yield env4
    case (Term.Tuple5(x1, x2, x3, x4, x5), Term.Tuple5(y1, y2, y3, y4, y5)) =>
      for (env1 <- unify(x1, y1, env0);
           env2 <- unify(x2, y2, env1);
           env3 <- unify(x3, y3, env2);
           env4 <- unify(x4, y4, env3);
           env5 <- unify(x5, y5, env4))
      yield env5
    case _ => None
  }

  /**
   * Unifies all terms in `tx` with all terms in `ty` under the initial environment `env0`.
   */
  def unifyTerms(tx: List[Term], ty: List[Term], env0: Map[VSym, Term]): Option[Map[VSym, Term]] =
    (tx zip ty).foldLeft(Option(env0)) {
      case (env, (t1, t2)) => env.flatMap(e => unify(t1, t2, e))
    }

  /**
   * Returns a new environment where every occurence of `x` in the terms of `env` have been replaced by `y`.
   */
  private def substitute(x: VSym, y: Term, env: Map[VSym, Term]): Map[VSym, Term] =
    env mapValues (_.substitute(x, y))

}
