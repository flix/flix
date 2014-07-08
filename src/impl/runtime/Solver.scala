package impl.runtime

import Unification._
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
   * Maps for n-ary lattices.
   */
  val map1 = mutable.Map1.empty[PSym, Value]
  val map2 = mutable.Map1.empty[PSym, (Value, Value)]
  val map3 = mutable.Map1.empty[PSym, (Value, Value, Value)]
  val map4 = mutable.Map1.empty[PSym, (Value, Value, Value, Value)]
  val map5 = mutable.Map1.empty[PSym, (Value, Value, Value, Value, Value)]

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
      val interpretation = interpretationOf(h.head, program.interpretation)

      satisfy(h.head, interpretation, Map.empty[VSym, Value])
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
   * Returns a list of models for the given horn clause `h` with interpretations `inv` under the given environment `env`.
   */
  def evaluate(h: HornClause, inv: Map[PSym, Interpretation], env: Map[VSym, Value]): List[Map[VSym, Value]] = {
    // Evaluate relational predicates before functional predicates.
    val relationals = h.body filter (p => isData(p, inv))
    val functionals = h.body -- relationals
    val predicates = relationals.toList ::: functionals.toList

    // Fold each predicate over the intial environment.
    val init = List(env)
    (init /: predicates) {
      case (envs, p) => evaluate(p, interpretationOf(p, inv), envs)
    }
  }

  private def isData(p: Predicate, inv: Map[PSym, Interpretation]): Boolean = interpretationOf(p, inv) match {
    case Interpretation.Relation(Representation.Data) => true
    case _ => false
  }

  /**
   * Returns a list of environments for the given predicate `p` with interpretation `i` under *each* of the given environments `envs`.
   */
  def evaluate(p: Predicate, i: Interpretation, envs: List[Map[VSym, Value]]): List[Map[VSym, Value]] = {
    envs flatMap (evaluate(p, i, _))
  }

  /**
   * Returns a list of environments for the given predicate `p` with interpretation `i` under the given environment `env0`.
   */
  def evaluate(p: Predicate, i: Interpretation, env0: Map[VSym, Value]): List[Map[VSym, Value]] = (i, p.terms) match {
    case (Interpretation.Relation(Representation.Data), List(t1)) =>
      relation1.get(p.name).toList.flatMap {
        case v1 => unify(t1, v1, env0)
      }

    case (Interpretation.Relation(Representation.Data), List(t1, t2)) =>
      val List(t1, t2) = p.terms
      relation2.get(p.name).toList.flatMap {
        case (v1, v2) => unify(t1, t2, v1, v2, env0)
      }

    case (Interpretation.Relation(Representation.Data), List(t1, t2, t3)) =>
      val List(t1, t2, t3) = p.terms
      relation3.get(p.name).toList.flatMap {
        case (v1, v2, v3) => unify(t1, t2, t3, v1, v2, v3, env0)
      }

    case (Interpretation.Relation(Representation.Data), List(t1, t2, t3, t4)) =>
      val List(t1, t2, t3, t4) = p.terms
      relation4.get(p.name).toList.flatMap {
        case (v1, v2, v3, v4) => unify(t1, t2, t3, t4, v1, v2, v3, v4, env0)
      }

    case (Interpretation.Relation(Representation.Data), List(t1, t2, t3, t4, t5)) =>
      val List(t1, t2, t3, t4, t5) = p.terms
      relation5.get(p.name).toList.flatMap {
        case (v1, v2, v3, v4, v5) => unify(t1, t2, t3, t4, t5, v1, v2, v3, v4, v5, env0)
      }

    case _ => throw new RuntimeException() // TODO
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
  def satisfy(p: Predicate, i: Interpretation, env: Map[VSym, Value]): Unit = (i, p.terms) match {
    case (Interpretation.Relation(Representation.Data), List(t1)) =>
      val v1 = t1.toValue(env)
      val newFact = relation1.put(p.name, v1)
      if (newFact)
        propagate(p, IndexedSeq(v1))

    case (Interpretation.Relation(Representation.Data), List(t1, t2)) =>
      val (v1, v2) = (t1.toValue(env), t2.toValue(env))
      val newFact = relation2.put(p.name, (v1, v2))
      if (newFact)
        propagate(p, IndexedSeq(v1, v2))

    case (Interpretation.Relation(Representation.Data), List(t1, t2, t3)) =>
      val List(t1, t2, t3) = p.terms
      val (v1, v2, v3) = (t1.toValue(env), t2.toValue(env), t3.toValue(env))
      val newFact = relation3.put(p.name, (v1, v2, v3))
      if (newFact)
        propagate(p, IndexedSeq(v1, v2, v3))

    case (Interpretation.Relation(Representation.Data), List(t1, t2, t3, t4)) =>
      val (v1, v2, v3, v4) = (t1.toValue(env), t2.toValue(env), t3.toValue(env), t4.toValue(env))
      val newFact = relation4.put(p.name, (v1, v2, v3, v4))
      if (newFact)
        propagate(p, IndexedSeq(v1, v2, v3, v4))

    case (Interpretation.Relation(Representation.Data), List(t1, t2, t3, t4, t5)) =>
      val List(t1, t2, t3, t4, t5) = p.terms
      val (v1, v2, v3, v4, v5) = (t1.toValue(env), t2.toValue(env), t3.toValue(env), t4.toValue(env), t5.toValue(env))
      val newFact = relation5.put(p.name, (v1, v2, v3, v4, v5))
      if (newFact)
        propagate(p, IndexedSeq(v1, v2, v3, v4, v5))

    case (Interpretation.LatticeMap(lattice), List(t1)) =>
      val newValue = t1.toValue(env)
      val oldValue = map1.get(p.name).getOrElse(lattice.bot)
      val joinValue = join(lattice.join, newValue, oldValue)
      val newFact: Boolean = !leq(lattice.leq, joinValue, oldValue)
      if (newFact) {
        map1.put(p.name, joinValue)
        propagate(p, IndexedSeq(joinValue))
      }

    case _ => throw new RuntimeException() // TODO
  }


  /**
   * Enqueues all depedencies of the given predicate with the given environment.
   */
  def propagate(p: Predicate, values: IndexedSeq[Value]): Unit = {
    for (h <- dependencies.get(p.name)) {
      bind(h, p, values) match {
        case None => // nop
        case Some(m) => queue.enqueue((h, m))
      }
    }
  }

  /**
   * Optionally returns a new environment where all free variables, for the given predicate `p`,
   * have been mapped to the value in the given environment `env`.
   *
   * That is, if the horn clause is A(x, y, z) :- B(x, y), C(z), the predicate is B
   * and the environment is [0 -> a, 1 -> b] then the return environment is [x -> a, y -> b].
   *
   * Returns `None` if no satisfying assignment exists.
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
  // Top-down satisfiability                                                 //
  /////////////////////////////////////////////////////////////////////////////

  def getModel(h: HornClause, inv: Map[PSym, Interpretation], env0: Map[VSym, Value]): List[Map[VSym, Value]] = {
    val init = List(env0)

    // Notice that if the body is empty then env0 is returned!
    (init /: h.body.toList) {
      case (envs, p) => envs flatMap {
        case env =>
          val values = p.terms.toIndexedSeq.map(_.asValue(env0))
          getModel(p, values, inv, env)
      }
    }
  }

  def getModel(p: Predicate, vs: IndexedSeq[Option[Value]], inv: Map[PSym, Interpretation], env0: Map[VSym, Value]): List[Map[VSym, Value]] = {
    val clauses = program.clauses.filter(_.head.name == p.name)

    if (clauses.isEmpty) throw new RuntimeException() // TODO: better exception

    clauses.toList.flatMap {
      h => satisfiable(h.head, interpretationOf(h.head, inv), vs, env0) match {
        case None => List.empty
        case Some(env) => getModel(h, inv, env)
      }
    }
  }

  /**
   * Optionally returns an environment satisfying the given predicate `p` under the interpretation `i` with valuations `vs` under environment `env0`.
   *
   * That is, if the list of terms in the predicate can be unified with the given values.
   *
   * As an example:
   *
   * The predicate P(x, y, 42) unified with the values (Some(5), None, Some(42)) yields [x -> 5] and y is free.
   */
  // TODO: Why do we need preicate and interpretation here?
  // TODO: Bad idea with IndexedSeq[Option[Value]]. Instead we need unification of terms... so that we can bind the result, instead of just having "None".
  def satisfiable(p: Predicate, i: Interpretation, vs: IndexedSeq[Option[Value]], env0: Map[VSym, Value]): Option[Map[VSym, Value]] = (i, p.terms) match {
    case (Interpretation.Relation(Representation.Code), List(t1)) =>
      val IndexedSeq(v1) = vs
      unify(t1, v1, env0)

    case (Interpretation.Relation(Representation.Code), List(t1, t2)) =>
      val IndexedSeq(v1, v2) = vs
      unify(t1, t2, v1, v2, env0)

    case (Interpretation.Relation(Representation.Code), List(t1, t2, t3)) =>
      val IndexedSeq(v1, v2, v3) = vs
      unify(t1, t2, t3, v1, v2, v3, env0)

    case (Interpretation.Relation(Representation.Code), List(t1, t2, t3, t4)) =>
      val List(t1, t2, t3, t4) = p.terms
      val IndexedSeq(v1, v2, v3, v4) = vs
      unify(t1, t2, t3, t4, v1, v2, v3, v4, env0)

    case (Interpretation.Relation(Representation.Code), List(t1, t2, t3, t4, t5)) =>
      val List(t1, t2, t3, t4, t5) = p.terms
      val IndexedSeq(v1, v2, v3, v4, v5) = vs
      unify(t1, t2, t3, t4, t5, v1, v2, v3, v4, v5, env0)

    case _ => throw Error.UnsupportedInterpretation(p.name, i)
  }


  /**
   * Returns `true` iff the given predicate symbol `s` is satisfiable with the given valuation `vs`.
   */
  def isPredicateSatisfiable(s: PSym, vs: IndexedSeq[Value], inv: Map[PSym, Interpretation]): Boolean = {
    // The goal is the predicate `s` where all the terms are bound to the values `vs`.
    val goal = Predicate(s, vs.map(_.asTerm).toList)
    // The models are all satisfiable ways to derive the goal under the empty environment.
    val models = getModel(goal, vs.map(v => Some(v)), inv, Map.empty)
    // The predicate is satisfiable iff there is atleast one satisfiable model.
    isSatisfiable(models)
  }

  /**
   * TODO: DOC
   */
  def evaluateFunction(s: PSym, vs: IndexedSeq[Value], inv: Map[PSym, Interpretation]): Value = {
    val goal = Predicate(s, vs.map(_.asTerm).toList ::: Term.Variable(Symbol.VariableSymbol("x")) :: Nil)
    val values: IndexedSeq[Option[Value]] = (vs.toList.map(v => Some(v)) ::: None :: Nil).toIndexedSeq
    val models = getModel(goal, values, inv, Map.empty)

    isUnique(Symbol.VariableSymbol("x"), models) match {
      case None => ???
      case Some(v) => v
    }
  }

  /**
   * Returns `true` iff `v1` is less or equal to `v2`.
   */
  def leq(s: PSym, v1: Value, v2: Value): Boolean = isPredicateSatisfiable(s, IndexedSeq(v1, v2), program.interpretation)

  /**
   * Returns the join of `v1` and `v2`.
   */
  def join(s: PSym, v1: Value, v2: Value): Value = evaluateFunction(s, IndexedSeq(v1, v2), program.interpretation)

  /////////////////////////////////////////////////////////////////////////////
  // Utilities                                                               //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Returns `true` iff the given solution `xs` contains at least one satisfiable model.
   */
  def isSatisfiable(xs: List[Map[VSym, Value]]) = xs.nonEmpty

  /**
   * TODO: Doc: Is unique?
  // TODO: FOld
   TODO: Careful about free??
   */
  def isUnique(x: VSym, xs: List[Map[VSym, Value]]): Option[Value] = {
    if (xs.isEmpty)
      return None

    if (xs.size == 1) {
      return xs.head.get(x)
    }

    println(xs)
    ???
  }


  /**
   * Returns the interpretation of the given predicate `p`.
   */
  def interpretationOf(p: Predicate, inv: Map[PSym, Interpretation]): Interpretation = inv.get(p.name) match {
    case None => throw Error.InterpretationNotFound(p.name)
    case Some(i) => i
  }
}
