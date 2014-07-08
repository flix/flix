package impl.runtime

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic.{Value, Term}

object Unification {
  /////////////////////////////////////////////////////////////////////////////
  // Term-Value Unification                                                  //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Optionally returns an environment such that the given term `t` is equal to the value `v`
   * under the returned environment `env` assuming an initial environment `env0`.
   *
   * That is, t.asValue(unify(t, v)) == v.
   *
   * Returns `None` if unification is impossible.
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

  // TODO: Consider replacing these with a generalized implementation:

  /**
   * Optionally returns an environment where the given terms `t1`, `t2` have been unified with the values `v1`, `v2`.
   */
  def unify(t1: Term, t2: Term, v1: Value, v2: Value, env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1))
    yield env2

  /**
   * Optionally returns an environment where the given terms `t1`, `t2`, `t3` have been unified with the values `v1`, `v2`, `v3`.
   */
  def unify(t1: Term, t2: Term, t3: Term, v1: Value, v2: Value, v3: Value, env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1);
         env3 <- unify(t3, v3, env2))
    yield env3

  /**
   * Optionally returns an environment where the given terms `t1`, `t2`, `t3`, `t4` have been unified with the values `v1`, `v2`, `v3`, `v4`.
   */
  def unify(t1: Term, t2: Term, t3: Term, t4: Term, v1: Value, v2: Value, v3: Value, v4: Value, env0: Map[VSym, Value]): Option[Map[VSym, Value]] =
    for (env1 <- unify(t1, v1, env0);
         env2 <- unify(t2, v2, env1);
         env3 <- unify(t3, v3, env2);
         env4 <- unify(t4, v4, env3))
    yield env4

  /**
   * Optionally returns an environment where the given terms `t1`, `t2`, `t3`, `t4`, `t5` have been unified with the values `v1`, `v2`, `v3`, `v4`, `v5`.
   */
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
   * Unifies the term `t1` with the term `t2`.
   */
  def unify(t1: Term, t2: Term, env0: Map[VSym, Term]): Option[Map[VSym, Term]] = (t1, t2) match {
    case _ => ???
  }

  /**
   * Unifies all terms in `tx` with all terms in `ty` under the initial environment `env0`.
   */
  def unify(tx: List[Term], ty: List[Term], env0: Map[VSym, Term]): Option[Map[VSym, Term]] =
    (tx zip ty).foldLeft(Option(env0)) {
      case (env, (t1, t2)) => env.flatMap(e => unify(t1, t2, e))
    }


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
