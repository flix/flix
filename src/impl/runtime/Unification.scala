package impl.runtime

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic.{Predicate, Term, Value}

object Unification {
  /////////////////////////////////////////////////////////////////////////////
  // Predicate Unification                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Unifies the given ground predicate `p1` with the predicate `p2` under the given environment `env0`
   *
   * If the environment is not specified, the empty environment is used.
   */
  def unifyPredicate(p1: Predicate.GroundPredicate, p2: Predicate, env0: Map[VSym, Value] = Map.empty): List[Map[VSym, Value]] =
    if (p1.name != p2.name)
      List.empty
    else
      Unification.unifyValues(p2.terms, p1.values, env0)


  /////////////////////////////////////////////////////////////////////////////
  // Term-Value Unification                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
   * Unifies the term `t` with the value `v` under the given environment `env0`.
   *
   * If the environment is not specified, the empty environment is used.
   */
  def unify(t: Term, v: Value, env0: Map[VSym, Value] = Map.empty): List[Map[VSym, Value]] = (t, v) match {
    case (Term.Unit, Value.Unit) => List(env0)
    case (Term.Bool(b1), Value.Bool(b2)) if b1 == b2 => List(env0)
    case (Term.Int(i1), Value.Int(i2)) if i1 == i2 => List(env0)
    case (Term.Str(s1), Value.Str(s2)) if s1 == s2 => List(env0)

    case (Term.Set(xs), Value.Set(ys)) =>
      val xss = xs.toList.permutations
      val yss = ys.toList.permutations
      xss.flatMap(xs =>
        yss.flatMap(ys =>
          unifyValues(xs, ys, env0))).toList

    case (Term.Var(s), v2) => env0.get(s) match {
      case None => List(env0 + (s -> v2))
      case Some(v3) if v2 != v3 => List.empty
      case Some(v3) if v2 == v3 => List(env0)
    }

    case (Term.Tagged(s1, t1, _), Value.Tagged(s2, v1, _)) if s1 == s2 => unify(t1, v1, env0)
    case (Term.Tuple2(t1, t2), Value.Tuple2(v1, v2)) =>
      for (env1 <- unify(t1, v1, env0);
           env2 <- unify(t2, v2, env1))
      yield env2
    case (Term.Tuple3(t1, t2, t3), Value.Tuple3(v1, v2, v3)) =>
      for (env1 <- unify(t1, v1, env0);
           env2 <- unify(t2, v2, env1);
           env3 <- unify(t3, v3, env2))
      yield env3
    case (Term.Tuple4(t1, t2, t3, t4), Value.Tuple4(v1, v2, v3, v4)) =>
      for (env1 <- unify(t1, v1, env0);
           env2 <- unify(t2, v2, env1);
           env3 <- unify(t3, v3, env2);
           env4 <- unify(t4, v4, env3))
      yield env4
    case (Term.Tuple5(t1, t2, t3, t4, t5), Value.Tuple5(v1, v2, v3, v4, v5)) =>
      for (env1 <- unify(t1, v1, env0);
           env2 <- unify(t2, v2, env1);
           env3 <- unify(t3, v3, env2);
           env4 <- unify(t4, v4, env3);
           env5 <- unify(t5, v5, env4))
      yield env5

    case _ => List.empty
  }

  /**
   * Unifies all terms in `tx` with all terms in `ty` under the initial environment `env0`.
   */
  def unifyValues(tx: List[Term], ty: List[Value], env0: Map[VSym, Value]): List[Map[VSym, Value]] =
    (tx zip ty).foldLeft(List(env0)) {
      case (env, (t1, t2)) => env.flatMap(e => unify(t1, t2, e))
    }

}
