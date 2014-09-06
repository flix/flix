package impl.runtime

import impl.datastore.{IndexedStore, SimpleStore, DataStore}
import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import util.collection.mutable

/**
 * A semi-naive solver.
 */
class Solver(program: Program) {

  /**
   * A datastore for facts.
   */
  val datastore: DataStore = new IndexedStore

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
      val fact = Interpreter.evaluatePredicate(h.head)
      newProvenFact(fact)
    }

    // Iteratively try to satisfy pending horn clauses.
    // Satisfying a horn clause may cause additional items to be added to the work list.
    while (queue.nonEmpty) {
      val (h, env) = queue.dequeue()

      val models = evaluate(h, env)
      for (model <- models) {
        val fact = Interpreter.evaluatePredicate(h.head, model)
        newProvenFact(fact)
      }
    }
  }

  /**
   * Prints the solution.
   */
  def print(): Unit = {
    datastore.output()
  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
   * Adds the given predicate `p` as a known ground fact.
   */
  def newProvenFact(p: Predicate.GroundPredicate): Unit = {
    val newValue = p.values.last
    val oldValue = datastore.lookup(p).getOrElse(bot(p.typ))
    val lubValue = lub(newValue, oldValue, p.typ)
    val newFact: Boolean = !leq(lubValue, oldValue, p.typ)
    if (newFact) {
      val np = Predicate.GroundPredicate(p.name, p.values.init ::: lubValue :: Nil, p.typ)
      datastore.store(np)
      propagateFact(np)
    }
  }

  /**
   * Enqueues all horn clauses which depend on the given predicate.
   */
  def propagateFact(p: Predicate.GroundPredicate): Unit = {
    for (h <- dependencies.get(p.name)) {
      for (p2 <- h.body) {
        for (env0 <- Unification.unifyPredicate(p, p2)) {
          queue.enqueue((h, env0))
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
   * Returns a list of environments for the given predicate `p` with interpretation `i` under the given environment `env`.
   */
  def evaluate(p: Predicate, env: Map[VSym, Value]): List[Map[VSym, Value]] = {
    datastore.query(p) flatMap {
      case values => Unification.unifyValues(p.terms, values, env)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Lattice Operations                                                      //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Returns the bottom element for the given type `typ`.
   */
  private def bot(typ: Type): Value = {
    program.lookupBot(typ.resultType).get
  }

  /**
   * Returns `true` iff `v1` is less or equal to `v2`.
   */
  private def leq(v1: Value, v2: Value, typ: Type): Boolean = {
    val abs = program.lookupLeq(typ.resultType).get
    val app = Term.App(Term.App(abs, v1.toTerm), v2.toTerm)
    Interpreter.evaluate(app).toBool
  }

  /**
   * Returns the join of `v1` and `v2`.
   */
  private def lub(v1: Value, v2: Value, typ: Type): Value = {
    val abs = program.lookupLub(typ.resultType).get
    val app = Term.App(Term.App(abs, v1.toTerm), v2.toTerm)
    Interpreter.evaluate(app)
  }

}
