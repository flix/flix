package impl.runtime

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.logic._

import syntax.Patterns.RichPattern
import syntax.Types.RichType

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

    case (Term.Tag(s1, t1, _), Value.Tag(s2, v1, _)) if s1 == s2 => unify(t1, v1, env0)
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

  /////////////////////////////////////////////////////////////////////////////
  // Pattern-Value Unification                                               //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Optionally returns an environment mapping every free variable
   * in the pattern `p` with the given value `v`.
   */
  def unify(p: Pattern, v: Value): Option[Map[VSym, Value]] = (p, v) match {
    case (Pattern.Wildcard, _) => Some(Map.empty)
    case (Pattern.Var(x), _) => Some(Map(x -> v))

    case (Pattern.Unit, Value.Unit) => Some(Map.empty)
    case (Pattern.Bool(b1), Value.Bool(b2)) if b1 == b2 => Some(Map.empty)
    case (Pattern.Int(i1), Value.Int(i2)) if i1 == i2 => Some(Map.empty)
    case (Pattern.Str(s1), Value.Str(s2)) if s1 == s2 => Some(Map.empty)

    case (Pattern.Tag(s1, p1), Value.Tag(s2, v1, _)) if s1 == s2 => unify(p1, v1)

    case (Pattern.Tuple2(p1, p2), Value.Tuple2(v1, v2)) =>
      for (env1 <- unify(p1, v1);
           env2 <- unify(p2, v2))
      yield env1 ++ env2
    case (Pattern.Tuple3(p1, p2, p3), Value.Tuple3(v1, v2, v3)) =>
      for (env1 <- unify(p1, v1);
           env2 <- unify(p2, v2);
           env3 <- unify(p3, v3))
      yield env1 ++ env2 ++ env3
    case (Pattern.Tuple4(p1, p2, p3, p4), Value.Tuple4(v1, v2, v3, v4)) =>
      for (env1 <- unify(p1, v1);
           env2 <- unify(p2, v2);
           env3 <- unify(p3, v3);
           env4 <- unify(p4, v4))
      yield env1 ++ env2 ++ env3 ++ env4
    case (Pattern.Tuple5(p1, p2, p3, p4, p5), Value.Tuple5(v1, v2, v3, v4, v5)) =>
      for (env1 <- unify(p1, v1);
           env2 <- unify(p2, v2);
           env3 <- unify(p3, v3);
           env4 <- unify(p4, v4);
           env5 <- unify(p5, v5))
      yield env1 ++ env2 ++ env3 ++ env4 ++ env5

    case _ => None
  }

  /////////////////////////////////////////////////////////////////////////////
  // Pattern-Type Unification                                                //
  /////////////////////////////////////////////////////////////////////////////
  /**
   * Unifies the given pattern `p` with the given type `typ`.
   *
   * Returns a type environment where all free variables in the pattern has been bound to their appropriate type.
   *
   * NB: Pattern variables are assumed not to occur more than once.
   */
  def unify(p: Pattern, typ: Type): Map[VSym, Type] = (p, typ) match {
    case (Pattern.Wildcard, _) => Map.empty
    case (Pattern.Var(s), typ1) => Map(s -> typ1)

    case (Pattern.Unit, Type.Unit) => Map.empty
    case (Pattern.Bool(b), Type.Bool) => Map.empty
    case (Pattern.Int(i), Type.Int) => Map.empty
    case (Pattern.Str(s), Type.Str) => Map.empty

    case (Pattern.Tag(s1, p1), Type.Sum(ts)) =>
      ts.collectFirst {
        case Type.Tag(s2, typ2) if s1 == s2 => unify(p1, typ2)
      }.get

    case (Pattern.Tuple2(p1, p2), Type.Tuple2(typ1, typ2)) =>
      unify(p1, typ1) ++ unify(p2, typ2)
    case (Pattern.Tuple3(p1, p2, p3), Type.Tuple3(typ1, typ2, typ3)) =>
      unify(p1, typ1) ++ unify(p2, typ2) ++ unify(p3, typ3)
    case (Pattern.Tuple4(p1, p2, p3, p4), Type.Tuple4(typ1, typ2, typ3, typ4)) =>
      unify(p1, typ1) ++ unify(p2, typ2) ++ unify(p3, typ3) ++ unify(p4, typ4)
    case (Pattern.Tuple5(p1, p2, p3, p4, p5), Type.Tuple5(typ1, typ2, typ3, typ4, typ5)) =>
      unify(p1, typ1) ++ unify(p2, typ2) ++ unify(p3, typ3) ++ unify(p4, typ4) ++ unify(p5, typ5)

    case _ => throw new RuntimeException(s"Unable to typecheck pattern ${p.fmt} against type ${typ.fmt}")
  }

}
