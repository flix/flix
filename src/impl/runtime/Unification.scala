package impl.runtime

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic.{Predicate, Value, Term}

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
    case (Term.Constant(v1), v2) if v1 == v2 => Some(env0)
    case (Term.Variable(s), v2) => env0.get(s) match {
      case None => Some(env0 + (s -> v2))
      case Some(v3) if v2 != v3 => None
      case Some(v3) if v2 == v3 => Some(env0)
    }
    case (Term.Constructor0(s1), Value.Constructor0(s2)) if s1 == s2 => Some(env0)
    case (Term.Constructor1(s1, t1), Value.Constructor1(s2, v1)) if s1 == s2 => unify(t1, v1, env0)
    case (Term.Constructor2(s1, t1, t2), Value.Constructor2(s2, v1, v2)) if s1 == s2 =>
      for (env1 <- unify(t1, v1, env0);
           env2 <- unify(t2, v2, env1))
      yield env2
    case (Term.Constructor3(s1, t1, t2, t3), Value.Constructor3(s2, v1, v2, v3)) if s1 == s2 =>
      for (env1 <- unify(t1, v1, env0);
           env2 <- unify(t2, v2, env1);
           env3 <- unify(t3, v3, env2))
      yield env3
    case (Term.Constructor4(s1, t1, t2, t3, t4), Value.Constructor4(s2, v1, v2, v3, v4)) if s1 == s2 =>
      for (env1 <- unify(t1, v1, env0);
           env2 <- unify(t2, v2, env1);
           env3 <- unify(t3, v3, env2);
           env4 <- unify(t4, v4, env3))
      yield env4
    case (Term.Constructor5(s1, t1, t2, t3, t4, t5), Value.Constructor5(s2, v1, v2, v3, v4, v5)) if s1 == s2 =>
      for (env1 <- unify(t1, v1, env0);
           env2 <- unify(t2, v2, env1);
           env3 <- unify(t3, v3, env2);
           env4 <- unify(t4, v4, env3);
           env5 <- unify(t5, v5, env4))
      yield env5
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

  /**
   * Unifies the term `t1` with the term `t2` under the given environment `env0`.
   */
  def unify(t1: Term, t2: Term, env0: Map[VSym, Term]): Option[Map[VSym, Term]] = (t1, t2) match {
    case (Term.Constant(v1), Term.Constant(v2)) if v1 == v2 => Some(env0)
    case (Term.Variable(x), Term.Variable(y)) => (env0.get(x), env0.get(y)) match {
      case (None, None) => Some(substitute(y, Term.Variable(x), env0))
      case (Some(tt1), None) => Some(substitute(y, tt1, env0))
      case (None, Some(tt2)) => Some(substitute(x, tt2, env0))
      case (Some(tt1), Some(tt2)) => if (tt1 == tt2) Some(env0) else None
    }
    case (Term.Variable(x), t) => env0.get(x) match {
      case None => Some(substitute(x, t, env0) + (x -> t))
      case Some(tt) => if (t == tt) Some(env0) else None
    }
    case (t, Term.Variable(y)) => env0.get(y) match {
      case None => Some(substitute(y, t, env0) + (y -> t))
      case Some(tt) => if (t == tt) Some(env0) else None
    }
    case (Term.Constructor0(s1), Term.Constructor0(s2)) if s1 == s2 => Some(env0)
    case (Term.Constructor1(s1, x1), Term.Constructor1(s2, y1)) if s1 == s2 =>
      unify(x1, y1, env0)
    case (Term.Constructor2(s1, x1, x2), Term.Constructor2(s2, y1, y2)) if s1 == s2 =>
      for (env1 <- unify(x1, y1, env0);
           env2 <- unify(x2, y2, env1))
      yield env2
    case (Term.Constructor3(s1, x1, x2, x3), Term.Constructor3(s2, y1, y2, y3)) if s1 == s2 =>
      for (env1 <- unify(x1, y1, env0);
           env2 <- unify(x2, y2, env1);
           env3 <- unify(x3, y3, env2))
      yield env3
    case (Term.Constructor4(s1, x1, x2, x3, x4), Term.Constructor4(s2, y1, y2, y3, y4)) if s1 == s2 =>
      for (env1 <- unify(x1, y1, env0);
           env2 <- unify(x2, y2, env1);
           env3 <- unify(x3, y3, env2);
           env4 <- unify(x4, y4, env3))
      yield env4
    case (Term.Constructor5(s1, x1, x2, x3, x4, x5), Term.Constructor5(s2, y1, y2, y3, y4, y5)) if s1 == s2 =>
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
