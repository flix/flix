package impl.runtime

import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import syntax.Predicates.RichPredicate
import util.collection.mutable

/**
 * A semi-naive solver.
 */
class Solver(program: Program) {

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
  val dependencies = mutable.MultiMap1.empty[PSym, Constraint.Rule]

  /**
   * A work list of pending horn clauses (and their associated environments).
   */
  val queue = scala.collection.mutable.Queue.empty[(Constraint, Map[VSym, Value])]

  /**
   * The fixpoint computation.
   */
  def solve(): Unit = {
    // Find dependencies between predicates and horn clauses.
    // A horn clause `h` depends on predicate `p` iff `p` occurs in the body of `h`.
    for (h <- program.rules; p <- h.body) {
      dependencies.put(p.name, h)
    }

    // Satisfy all facts. Satisfying a fact adds violated horn clauses (and environments) to the work list.
    for (h <- program.facts) {
      newProvenFact(h.head, Map.empty[VSym, Value])
    }

    // Iteratively try to satisfy pending horn clauses.
    // Satisfying a horn clause may cause additional items to be added to the work list.
    while (queue.nonEmpty) {
      val (h, env) = queue.dequeue()

      val models = evaluate(h, env)
      for (model <- models) {
        newProvenFact(h.head, model)
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Returns `true` iff the given predicate `p` under the environment `env0` is a ground fact.
   */
  def isProvenFact(p: Predicate, env0: Map[VSym, Value]): Boolean = Interpreter.evaluatePredicateOpt(p, env0) match {
    case None => false
    case Some(pg) => facts contains pg
  }

  /**
   * Adds the given predicate `p` as a known ground fact under the given interpretation `i` and environment `env`.
   */
  def newProvenFact(p: Predicate, env: Map[VSym, Value]): Unit = {
    val p2 = Interpreter.evaluatePredicate(p, env)
    println("--> " + p2.fmt)

    facts += p2

    // TODO: Need Term.Set to implement this.

    if (p.typ.isSetMap) {
      p.terms match {
        case List(t1) =>
          val v1 = Interpreter.evaluate(t1, env)

          //relation1.get(p.name)

          val newFact = relation1.put(p.name, v1)
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm), p.typ))

        case List(t1, t2) =>
          val (v1, v2) = (Interpreter.evaluate(t1, env), Interpreter.evaluate(t2, env))
          val newFact = relation2.put(p.name, (v1, v2))
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm, v2.toTerm), p.typ))

        case List(t1, t2, t3) =>
          val (v1, v2, v3) = (Interpreter.evaluate(t1, env), Interpreter.evaluate(t2, env), Interpreter.evaluate(t3, env))
          val newFact = relation3.put(p.name, (v1, v2, v3))
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm, v2.toTerm, v3.toTerm), p.typ))

        case List(t1, t2, t3, t4) =>
          val (v1, v2, v3, v4) = (Interpreter.evaluate(t1, env), Interpreter.evaluate(t2, env), Interpreter.evaluate(t3, env), Interpreter.evaluate(t4, env))
          val newFact = relation4.put(p.name, (v1, v2, v3, v4))
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm, v2.toTerm, v3.toTerm, v4.toTerm), p.typ))

        case List(t1, t2, t3, t4, t5) =>
          val (v1, v2, v3, v4, v5) = (Interpreter.evaluate(t1, env), Interpreter.evaluate(t2, env), Interpreter.evaluate(t3, env), Interpreter.evaluate(t4, env), Interpreter.evaluate(t5, env))
          val newFact = relation5.put(p.name, (v1, v2, v3, v4, v5))
          if (newFact)
            propagateFact(Predicate(p.name, List(v1.toTerm, v2.toTerm, v3.toTerm, v4.toTerm, v5.toTerm), p.typ))
      }
    } else if (p.typ.isLatMap) {
      val elmType = p.typ.resultType
      p.terms match {
        case List(t1) =>
          val newValue = Interpreter.evaluate(t1, env)
          val oldValue = map1.get(p.name).getOrElse(bot(elmType))
          val lubValue = lub(elmType, newValue, oldValue)
          val newFact: Boolean = !leq(elmType, lubValue, oldValue)
          if (newFact) {
            map1.put(p.name, lubValue)
            propagateFact(Predicate(p.name, List(lubValue.toTerm), p.typ))
          }
      }
    } else {
      throw new RuntimeException(s"Unworkable type: ${p.typ}")
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
          case Some(env0) => queue.enqueue((h, env0.mapValues(t => Interpreter.evaluate(t))))
        }
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Evaluation                                                              //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Returns a list of models for the given horn clause `h` under the given environment `env0`.
   */
  def evaluate(h: Constraint, env0: Map[VSym, Value]): List[Map[VSym, Value]] = {
    val predicates = h.body

    // Fold each predicate over the intial environment.
    val init = List(env0)
    (init /: predicates) {
      case (envs, p) => envs.flatMap(env => evaluate(p, env))
    }
  }

  /**
   * Returns a list of environments for the given predicate `p` with interpretation `i` under the given environment `env0`.
   */
  def evaluate(p: Predicate, env0: Map[VSym, Value]): List[Map[VSym, Value]] = {
    if (isProvenFact(p, env0)) {
      return List(env0)
    }

    if (p.typ.isSetMap) {
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
    } else {
      throw new RuntimeException()
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Lattice Operations                                                      //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Returns the bottom element for the given type `targetType`.
   */
  private def bot(targetType: Type): Value = {
    // find declaration
    program.declarations.collectFirst {
      case Declaration.DeclareBot(bot, actualType) if actualType == targetType => bot
    }.get
  }

  /**
   * Returns `true` iff `v1` is less or equal to `v2`.
   */
  private def leq(typ: Type, v1: Value, v2: Value): Boolean = {
    // construct the specific function type we are looking for
    val targetType = Type.Function(typ, Type.Function(typ, Type.Bool))
    // find the declaration
    val abs = program.declarations.collectFirst {
      case Declaration.DeclareLeq(leq, actualType) if actualType == targetType => leq
    }.get

    val app = Term.App(Term.App(abs, v2.toTerm), v1.toTerm)
    Interpreter.evaluate(app).toBool
  }

  /**
   * Returns the join of `v1` and `v2`.
   */
  private def lub(typ: Type, v1: Value, v2: Value): Value = {
    // construct the specific function type we are looking for
    val targetType = Type.Function(typ, Type.Function(typ, typ))
    // find the declaration
    val abs = program.declarations.collectFirst {
      case Declaration.DeclareLub(lub, actualType) if actualType == targetType => lub
    }.get

    val app = Term.App(Term.App(abs, v2.toTerm), v1.toTerm)
    Interpreter.evaluate(app)
  }

}
