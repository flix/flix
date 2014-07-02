package impl.runtime

import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import util.collection.mutable

/**
 * A semi-naive solver.
 */
class Solver(program: Program) {

  /**
   * The tables for n-ary relations.
   */
  val relation1 = mutable.MultiMap1.empty[PSym, Value]
  val relation2 = mutable.MultiMap1.empty[PSym, (Value, Value)]
  val relation3 = mutable.MultiMap1.empty[PSym, (Value, Value, Value)]
  val relation4 = mutable.MultiMap1.empty[PSym, (Value, Value, Value, Value)]
  val relation5 = mutable.MultiMap1.empty[PSym, (Value, Value, Value, Value, Value)]

  /**
   * A map of dependencies between predicate symbols and horn clauses.
   *
   * If a horn clause `h` contains a predicate `p` then the map contains the element `p -> h`.
   */
  val dependencies = mutable.MultiMap1.empty[PSym, HornClause]

  /**
   * A work list of pending horn clauses (and their associated environments).
   */
  val queue = scala.collection.mutable.Queue.empty[(HornClause, Map[VSym, Value])]

  /**
   * The fixpoint computation.
   */
  def solve(): Unit = {
    // Find dependencies between predicates and horn clauses.
    // A horn clause `h` depends on predicate `p` iff `p` occurs in the body of `h`.
    for (h <- program.clauses; p <- h.body) {
      dependencies.put(p.name, h)
    }

    // Satisfy all facts. Satisfying a fact adds violated horn clauses (and environments) to the work list.
    for (h <- program.facts) {
      satisfy(h.head, interpretationOf(h.head, program.interpretation), Map.empty[VSym, Value])
    }

    // Iteratively try to satisfy pending horn clauses.
    // Satisfying a horn clause may cause additional items to be added to the work list.
    while (queue.nonEmpty) {
      val (h, env) = queue.dequeue()
      satisfy(h, program.interpretation, env)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Evaluation                                                              //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Returns a set of models for the given horn clause `h` with interpretations `inv` under the given environment `env`.
   */
  def evaluate(h: HornClause, inv: Map[PSym, Interpretation], env: Map[VSym, Value]): List[Map[VSym, Value]] = {
    val relationals = h.body filter (p => isRelational(p, inv))
    val functionals = h.body -- relationals

    val init = List(env)
    val predicates = relationals.toList ::: functionals.toList

    (init /: predicates) {
      case (m, p) => evaluate(p, interpretationOf(p, inv), m)
    }
  }

  /**
   *
   */
  def evaluate(p: Predicate, i: Interpretation, m: List[Map[VSym, Value]]): List[Map[VSym, Value]] = {
    m flatMap (evaluate(p, i, _))
  }

  /**
   * Returns a model for the given predicate `p` with interpretation `i` under the given environment `env`.
   */
  def evaluate(p: Predicate, i: Interpretation, env0: Map[VSym, Value]): List[Map[VSym, Value]] = i match {
    case Interpretation.Proposition(Value.Bool(false)) => List.empty

    case Interpretation.Proposition(Value.Bool(true)) => List(env0)

    case Interpretation.Relation.In1(_) =>
      val List(t1) = p.terms
      relation1.get(p.name).toList.flatMap {
        case v1 => unify(t1, v1, env0)
      }

    case Interpretation.Relation.In2(_, _) =>
      val List(t1, t2) = p.terms
      relation2.get(p.name).toList.flatMap {
        case (v1, v2) =>
          for (
            env1 <- unify(t1, v1, env0);
            env2 <- unify(t2, v2, env1)
          ) yield env2
      }

    case Interpretation.Relation.In3(_, _, _) =>
      val List(t1, t2, t3) = p.terms
      relation3.get(p.name).toList.flatMap {
        case (v1, v2, v3) =>
          for (
            env1 <- unify(t1, v1, env0);
            env2 <- unify(t2, v2, env1);
            env3 <- unify(t3, v3, env2)
          ) yield env3
      }

    case Interpretation.Relation.In4(_, _, _, _) =>
      val List(t1, t2, t3, t4) = p.terms
      relation4.get(p.name).toList.flatMap {
        case (v1, v2, v3, v4) =>
          for (
            env1 <- unify(t1, v1, env0);
            env2 <- unify(t2, v2, env1);
            env3 <- unify(t3, v3, env2);
            env4 <- unify(t4, v4, env3)
          ) yield env4
      }

    case Interpretation.Relation.In5(_, _, _, _, _) =>
      val List(t1, t2, t3, t4, t5) = p.terms
      relation5.get(p.name).toList.flatMap {
        case (v1, v2, v3, v4, v5) =>
          for (
            env1 <- unify(t1, v1, env0);
            env2 <- unify(t2, v2, env1);
            env3 <- unify(t3, v3, env2);
            env4 <- unify(t4, v4, env3);
            env5 <- unify(t5, v5, env4)
          ) yield env5
      }

    case _ => throw new Error.NonRelationalPredicate(p)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Satisfaction                                                            //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Satisfies the given horn clause `h` by finding all valid models of the given environment `env`.
   *
   * Adds all facts which satisfies the given horn clause `h`.
   */
  def satisfy(h: HornClause, inv: Map[PSym, Interpretation], env: Map[VSym, Value]): Unit = {
    val models = evaluate(h, inv, env)
    for (model <- models) {
      satisfy(h.head, interpretationOf(h.head, inv), model)
    }
  }

  /**
   * Satisfies the given predicate `p` under the given interpretation `i` and environment `env`.
   */
  def satisfy(p: Predicate, i: Interpretation, env: Map[VSym, Value]): Unit = i match {
    case Interpretation.Relation.In1(t1) =>
      val List(t1) = p.terms
      val v1 = t1.toValue(env)
      val newFact = relation1.put(p.name, v1)
      if (newFact)
        propagate(p, IndexedSeq(v1))

    case Interpretation.Relation.In2(_, _) =>
      val List(t1, t2) = p.terms
      val (v1, v2) = (t1.toValue(env), t2.toValue(env))
      val newFact = relation2.put(p.name, (v1, v2))
      if (newFact)
        propagate(p, IndexedSeq(v1, v2))

    case Interpretation.Relation.In3(t1, t2, t3) =>
      val List(t1, t2, t3) = p.terms
      val (v1, v2, v3) = (t1.toValue(env), t2.toValue(env), t3.toValue(env))
      val newFact = relation3.put(p.name, (v1, v2, v3))
      if (newFact)
        propagate(p, IndexedSeq(v1, v2, v3))

    case Interpretation.Relation.In4(t1, t2, t3, t4) =>
      val List(t1, t2, t3, t4) = p.terms
      val (v1, v2, v3, v4) = (t1.toValue(env), t2.toValue(env), t3.toValue(env), t4.toValue(env))
      val newFact = relation4.put(p.name, (v1, v2, v3, v4))
      if (newFact)
        propagate(p, IndexedSeq(v1, v2, v3, v4))

    case Interpretation.Relation.In5(t1, t2, t3, t4, t5) =>
      val List(t1, t2, t3, t4, t5) = p.terms
      val (v1, v2, v3, v4, v5) = (t1.toValue(env), t2.toValue(env), t3.toValue(env), t4.toValue(env), t5.toValue(env))
      val newFact = relation5.put(p.name, (v1, v2, v3, v4, v5))
      if (newFact)
        propagate(p, IndexedSeq(v1, v2, v3, v4, v5))

    case _ => throw new Error.NonRelationalPredicate(p)
  }


  /**
   * Enqueues all depedencies of the given predicate with the given environment.
   */
  def propagate(p: Predicate, env: IndexedSeq[Value]): Unit = {
    for (h <- dependencies.get(p.name)) {
      bind(h, p, env) match {
        case None => // nop
        case Some(m) => queue.enqueue((h, m))
      }
    }
  }

  /**
   * Returns a new environment where all free variables, for the given predicate `p`,
   * have been mapped to the value in the given environment `env`.
   *
   * That is, if the horn clause is A(x, y, z) :- B(x, y), C(z), the predicate is B
   * and the environment is [0 -> a, 1 -> b] then the return environment is [x -> a, y -> b].
   *
   * TODO: Update doc
   */
  def bind(h: HornClause, p: Predicate, env: IndexedSeq[Value]): Option[Map[VSym, Value]] = {
    var m = Map.empty[VSym, Value]
    for (p2 <- h.body; if p.name == p2.name) {
      for ((t, i) <- p2.terms.zipWithIndex) {
        unify(t, env(i), m) match {
          case None => return None
          case Some(m2) => m = m2
        }
      }
    }
    Some(m)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unification                                                             //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Careful about existing bindings/free variables occuring in multiple places.
  // TODO: DOC
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


  /////////////////////////////////////////////////////////////////////////////
  // Utilities                                                               //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Returns the interpretation of the given predicate `p`.
   */
  def interpretationOf(p: Predicate, inv: Map[PSym, Interpretation]): Interpretation = inv.get(p.name) match {
    case None => throw new Error.InterpretationNotFound(p.name)
    case Some(i) => i
  }

  /**
   * Returns `true` iff the given predicate `p` is relational under the given interpretations `inv`.
   */
  private def isRelational(p: Predicate, inv: Map[PSym, Interpretation]): Boolean = interpretationOf(p, inv) match {
    case _: Interpretation.Relation.In1 => true
    case _: Interpretation.Relation.In2 => true
    case _: Interpretation.Relation.In3 => true
    case _: Interpretation.Relation.In4 => true
    case _: Interpretation.Relation.In5 => true
    case _: Interpretation.Map.Leq1 => true
    case _: Interpretation.Map.Leq2 => true
    case _: Interpretation.Map.Leq3 => true
    case _: Interpretation.Map.Leq4 => true
    case _: Interpretation.Map.Leq5 => true
    case _ => false
  }
}
