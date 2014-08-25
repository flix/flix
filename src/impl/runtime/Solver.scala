package impl.runtime

import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import syntax.Predicates.RichPredicate
import util.collection.mutable

/**
 * A semi-naive solver.
 */
class Solver(val program: Program) {

  /**
   * A set of predicate facts.
   */
  val facts = scala.collection.mutable.Set.empty[Predicate]

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
      newProvenFact(h.head, program.interpretation(h.head.name), Map.empty[VSym, Value])
    }

    // Iteratively try to satisfy pending horn clauses.
    // Satisfying a horn clause may cause additional items to be added to the work list.
    while (queue.nonEmpty) {
      val (h, env) = queue.dequeue()

      val models = evaluate(h, env)
      for (model <- models) {
        newProvenFact(h.head, program.interpretation(h.head.name), model)
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Split into intrisinic and extrinsic

  /**
   * Returns `true` iff the given predicate `p` under the environment `env0` is a ground fact.
   */
  def isProvenFact(p: Predicate, env0: Map[VSym, Value]): Boolean = p.asGround(env0) match {
    case None => false
    case Some(pg) => facts contains pg
  }

  /**
   * Adds the given predicate `p` as a known ground fact under the given interpretation `i` and environment `env`.
   */
  def newProvenFact(p: Predicate, i: Interpretation, env: Map[VSym, Value]): Unit = {
    println("--> " + p.toGround(env).fmt)

    facts += p.toGround(env)
    // TODO: Collapse into one DataStore
    i match {
      case Interpretation.Relation => p.terms match {
        case List(t1) =>
          val v1 = Interpreter.evaluate(t1, env)
          val newFact = relation1.put(p.name, v1)
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm)))

        case List(t1, t2) =>
          val (v1, v2) = (Interpreter.evaluate(t1, env), Interpreter.evaluate(t2, env))
          val newFact = relation2.put(p.name, (v1, v2))
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm, v2.toTerm)))

        case List(t1, t2, t3) =>
          val (v1, v2, v3) = (Interpreter.evaluate(t1, env), Interpreter.evaluate(t2, env), Interpreter.evaluate(t3, env))
          val newFact = relation3.put(p.name, (v1, v2, v3))
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm, v2.toTerm, v3.toTerm)))

        case List(t1, t2, t3, t4) =>
          val (v1, v2, v3, v4) = (Interpreter.evaluate(t1, env), Interpreter.evaluate(t2, env), Interpreter.evaluate(t3, env), Interpreter.evaluate(t4, env))
          val newFact = relation4.put(p.name, (v1, v2, v3, v4))
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm, v2.toTerm, v3.toTerm, v4.toTerm)))

        case List(t1, t2, t3, t4, t5) =>
          val (v1, v2, v3, v4, v5) = (Interpreter.evaluate(t1, env), Interpreter.evaluate(t2, env), Interpreter.evaluate(t3, env), Interpreter.evaluate(t4, env), Interpreter.evaluate(t5, env))
          val newFact = relation5.put(p.name, (v1, v2, v3, v4, v5))
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm, v2.toTerm, v3.toTerm, v4.toTerm, v5.toTerm)))
      }

      case Interpretation.Lattice => p.terms match {
        case List(t1) =>
          val lattice = program.lattices(p.name)
          val newValue = Interpreter.evaluate(t1, env)
          val oldValue = map1.get(p.name).getOrElse(lattice.bot)
          val joinValue = join2(lattice.lub, newValue, oldValue)
          val newFact: Boolean = !leq2(lattice.leq, joinValue, oldValue)
          if (newFact) {
            map1.put(p.name, joinValue)
            propagateFact(Predicate(p.name, List(joinValue.toTerm)))
          }
      }
    }
  }

  /**
   * Enqueues all horn clauses which depend on the given predicate.
   */
  def propagateFact(p: Predicate): Unit = {
    for (h <- dependencies.get(p.name)) {
      for (p2 <- h.body) {
        Unification.unify(p, p2, Map.empty[VSym, Term]) match {
          case None => // nop
          case Some(env0) => queue.enqueue((h, env0.mapValues(t => Interpreter.evaluate(t, Map.empty))))
        }
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Evaluation                                                              //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Harmonize evaluate and getModel

  /**
   * Returns a list of models for the given horn clause `h` under the given environment `env0`.
   */
  def evaluate(h: HornClause, env0: Map[VSym, Value]): List[Map[VSym, Value]] = {
    // Evaluate relational predicates before functional predicates.
    val predicates = h.body // TODO: Decide evaluation order.

    // Fold each predicate over the intial environment.
    val init = List(env0)
    (init /: predicates) {
      case (envs, p) => envs.flatMap(env => evaluate(p, program.interpretation(p.name), env))
    }
  }

  /**
   * Returns a list of environments for the given predicate `p` with interpretation `i` under the given environment `env0`.
   */
  def evaluate(p: Predicate, i: Interpretation, env0: Map[VSym, Value]): List[Map[VSym, Value]] = {
    if (isProvenFact(p, env0)) {
      return List(env0)
    }

    i match {
      case Interpretation.Relation =>
        p.terms match {
          case ts@List(t1) => relation1.get(p.name).toList.flatMap {
            case v1 => Unification.unifyValues(ts, List(v1), env0)
          }
          case ts@List(t1, t2) => relation2.get(p.name).toList.flatMap {
            case (v1, v2) => Unification.unifyValues(ts, List(v1, v2), env0)
          }
          case ts@List(t1, t2, t3) => relation3.get(p.name).toList.flatMap {
            case (v1, v2, v3) => Unification.unifyValues(ts, List(v1, v2, v3), env0)
          }
          case ts@List(t1, t2, t3, t4) => relation4.get(p.name).toList.flatMap {
            case (v1, v2, v3, v4) => Unification.unifyValues(ts, List(v1, v2, v3, v4), env0)
          }
          case ts@List(t1, t2, t3, t4, t5) => relation5.get(p.name).toList.flatMap {
            case (v1, v2, v3, v4, v5) => Unification.unifyValues(ts, List(v1, v2, v3, v4, v5), env0)
          }
        }
      case _ => throw Error.UnsupportedInterpretation(p.name, i)
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Top-down satisfiability                                                 //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Returns all satisfiable models for the given predicate `p` under the given environment `env0`.
   */
  // TODO: Must take cycles into account.
  def getSat(p: Predicate, env0: Map[VSym, Term] = Map.empty): List[Map[VSym, Term]] = {
    // TODO: Rewrite to loop
    program.clauses.flatMap {
      h => Unification.unify(p, h.head, env0) match {
        case None =>
          // Head predicate is not satisfied. No need to look at body.
          List.empty
        case Some(env) =>
          // Head predicate is satisfied. Check whether the body can be made satisfiable.
          h.body.foldLeft(List(env)) {
            case (envs, p2) => envs.flatMap(e => getSat(p2, e))
          }
      }
    }
  }

  /**
   * Optionally returns the unique term of the variable `x` in all the given models `xs`.
   */
  private def asUniqueTerm(x: VSym, xs: List[Map[VSym, Term]]): Option[Term] = {
    val vs = xs.flatMap(_.get(x)).toSet
    if (vs.size == 1)
      Some(vs.head)
    else
      None
  }

  /**
   * Optionally returns the unique value of the variable `x` in all the given models `xs`.
   */
  private def asUniqueValue(x: VSym, xs: List[Map[VSym, Term]]): Option[Value] =
    asUniqueTerm(x, xs).map(t => Interpreter.evaluate(t, Map.empty))

  /**
   * Returns `true` iff `v1` is less or equal to `v2`.
   */
  private def leq2(s: PSym, v1: Value, v2: Value): Boolean = {
    val p = Predicate(s, List(v1.toTerm, v2.toTerm))
    val models = getSat(p)
    models.nonEmpty
  }

  /**
   * Returns the join of `v1` and `v2`.
   */
  private def join2(s: PSym, v1: Value, v2: Value): Value = {
    val p = Predicate(s, List(v1.toTerm, v2.toTerm, Term.Var(Symbol.VariableSymbol("!x"))))
    val models = getSat(p)

    val value = asUniqueValue(Symbol.VariableSymbol("!x"), models)
    value match {
      case None => throw Error.NonUniqueModel(s)
      case Some(v) => v
    }
  }

  /**
   * Returns `true` iff `v1` is less or equal to `v2`.
   */
  private def leq(t: Term.Abs, v1: Value, v2: Value): Boolean = {
    val tt = Term.App(Term.App(t, v2.toTerm), v1.toTerm)
    val Value.Bool(b) = Interpreter.evaluate(tt, Map.empty)
    b
  }

  /**
   * Returns the join of `v1` and `v2`.
   */
  private def join(t: Term.Abs, v1: Value, v2: Value): Value = {
    val tt = Term.App(Term.App(t, v2.toTerm), v1.toTerm)
    Interpreter.evaluate(tt, Map.empty)
  }
}
