package impl.runtime

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic.{Value, Term}

object Unification {
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

  // TODO: Consider replacing these with a generalized implementation:

  /**
   * Optionally returns an environment where the given terms `t1`, `t2` have been unified with the values `v1`, `v2`.
   */
  @deprecated("", "")
  def unify(t1: Term, t2: Term, v1: Value, v2: Value, env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1))
    yield env2

  /**
   * Optionally returns an environment where the given terms `t1`, `t2`, `t3` have been unified with the values `v1`, `v2`, `v3`.
   */
  @deprecated("", "")
  def unify(t1: Term, t2: Term, t3: Term, v1: Value, v2: Value, v3: Value, env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1);
         env3 <- unify(t3, v3, env2))
    yield env3

  /**
   * Optionally returns an environment where the given terms `t1`, `t2`, `t3`, `t4` have been unified with the values `v1`, `v2`, `v3`, `v4`.
   */
  @deprecated("", "")
  def unify(t1: Term, t2: Term, t3: Term, t4: Term, v1: Value, v2: Value, v3: Value, v4: Value, env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1);
         env3 <- unify(t3, v3, env2);
         env4 <- unify(t4, v4, env3))
    yield env4

  /**
   * Optionally returns an environment where the given terms `t1`, `t2`, `t3`, `t4`, `t5` have been unified with the values `v1`, `v2`, `v3`, `v4`, `v5`.
   */
  @deprecated("", "")
  def unify(t1: Term, t2: Term, t3: Term, t4: Term, t5: Term, v1: Value, v2: Value, v3: Value, v4: Value, v5: Value, env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1);
         env3 <- unify(t3, v3, env2);
         env4 <- unify(t4, v4, env3);
         env5 <- unify(t5, v5, env4))
    yield env5

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


  /////////////////////////////////////////////////////////////////////////////
  // Optional Unification                                                    //
  /////////////////////////////////////////////////////////////////////////////
  // TODO: All this is deprecated and should be removed: ....

  /**
   * Optionally returns the unification of the given term `t` with the optional value `v`.
   *
   * If `v` is `None` then the given environment `env0` is returned unmodified.
   */
  def unify(t: Term, v: Option[Value], env0: Map[VSym, Value]): Option[Map[VSym, Value]] = v match {
    case None => Some(env0)
    case Some(v2) => unify(t, v2, env0)
  }

  /**
   * Optionally returns an environment where the given terms `t1`, `t2` have been unified with the optional values `v1`, `v2`
   */
  def unify(t1: Term, t2: Term, v1: Option[Value], v2: Option[Value], env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1))
    yield env2

  /**
   * Optionally returns an environment where the given terms `t1`, `t2`, `t3` have been unified with the optional values `v1`, `v2`, `v3`.
   */
  def unify(t1: Term, t2: Term, t3: Term, v1: Option[Value], v2: Option[Value], v3: Option[Value], env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1);
         env3 <- unify(t3, v3, env2))
    yield env3

  /**
   * Optionally returns an environment where the given terms `t1`, `t2`, `t3`, `t4` have been unified with the optional values `v1`, `v2`, `v3`, `v4`.
   */
  def unify(t1: Term, t2: Term, t3: Term, t4: Term, v1: Option[Value], v2: Option[Value], v3: Option[Value], v4: Option[Value], env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1);
         env3 <- unify(t3, v3, env2);
         env4 <- unify(t4, v4, env3))
    yield env4

  /**
   * Optionally returns an environment where the given terms `t1`, `t2`, `t3`, `t4`, `t5` have been unified with the optional values `v1`, `v2`, `v3`, `v4`, `v5`.
   */
  def unify(t1: Term, t2: Term, t3: Term, t4: Term, t5: Term, v1: Option[Value], v2: Option[Value], v3: Option[Value], v4: Option[Value], v5: Option[Value], env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1);
         env3 <- unify(t3, v3, env2);
         env4 <- unify(t4, v4, env3);
         env5 <- unify(t5, v5, env4))
    yield env4
}
