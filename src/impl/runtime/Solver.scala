package impl.runtime

import impl.datastore.{DataStore, IndexedStore}
import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import util.collection.mutable

/**
 * A semi-naive solver.
 */
class Solver(program: Program, options: Options) {

  // TODO: Need to implement filter. E.g. Foo(x, y), Bar(x, z), x != y.
//  One thing that I definitely do need, however, and soon, is predicates
//    that are functions evaluated top-down. For example:
//    (rule (KillEmpty l) ((Store <l p q>) (Pt p {a b}) (!= a b)))
//  (rule (KillNot l {a}) ((Store <l p q>) (Pt p {b}) (AllObjects {a}) (!= a b)))
//  The != at the end should be implemented as a filter on the environment
//    resulting from the (usual bottom-up) evaluation of the other predicates.
//  For now, I think I only need !=, but in general, one would want to
//    allow arbitrary filter functions that take a number of parameters and
//  return a boolean. And even more generally, functions that compute a
//    (non-boolean) result and bind it to some variable to be used in
//    subsequent predicates.


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
   * A run-time performance monitor.
   */
  val monitor = new Monitor()

  /**
   * The fixpoint computation.
   */
  def solve(): Unit = {
    // Find dependencies between predicates and horn clauses.
    // A horn clause `h` depends on predicate `p` iff `p` occurs in the body of `h`.
    for (h <- program.rules; p <- h.body) {
      dependencies.put(p.name, h)
    }

    monitor.init {
      // Satisfy all facts. Satisfying a fact adds violated horn clauses (and environments) to the work list.
      for (h <- program.facts) {
        val fact = Interpreter.evaluatePredicate(h.head)
        newProvenFact(fact)
      }
    }

    monitor.fixpoint {
      // Iteratively try to satisfy pending horn clauses.
      // Satisfying a horn clause may cause additional items to be added to the work list.
      while (queue.nonEmpty) {
        val (h, env) = queue.dequeue()

        val models = satisfy(h, env)
        for (model <- models) {
          val fact = Interpreter.evaluatePredicate(h.head, model)
          newProvenFact(fact)
        }
      }
    }
  }

  /**
   * Prints the solution.
   */
  def print(): Unit = {
    datastore.output()
    monitor.output()
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
      options.propagation match {
        case Propagation.Diff => propagateFact(p)
        case Propagation.Full => propagateFact(np)
      }
    }
  }

  /**
   * Enqueues all horn clauses which depend on the given predicate.
   */
  def propagateFact(p: Predicate.GroundPredicate): Unit = {
    for (h <- dependencies.get(p.name)) {
      for (p2 <- h.body) {
        for (env0 <- Unification.unifyPredicate(p, p2)) {
          options.simplify match {
            case Simplify.Enable => queue.enqueue((h, env0))
            case Simplify.Disable => queue.enqueue((h.simplify(p2), env0))
          }
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
  def satisfy(h: Constraint, env0: Map[VSym, Value]): List[Map[VSym, Value]] = monitor.constraint(h) {
    val predicates = h.body

    // Fold each predicate over the initial environment.
    val init = List(env0)
    val result = (init /: predicates) {
      case (envs, p) => envs.flatMap(env => satisfy(p, env))
    }

    filterAll(h, result)
  }

  /**
   * Returns a list of environments for the given predicate `p` with interpretation `i` under the given environment `env`.
   */
  def satisfy(p: Predicate, env: Map[VSym, Value]): List[Map[VSym, Value]] = monitor.predicate[List[Map[VSym, Value]]](p) {
    datastore.query(p) flatMap {
      case values => Unification.unifyValues(p.terms, values, env)
    }
  }

  /**
   * Returns the subset of given environments `envs` which satisfy the propositional formula of the given constraint `c`.
   */
  def filterAll(h: Constraint, envs: List[Map[VSym, Value]]): List[Map[VSym, Value]] = h.proposition match {
    case None => envs
    case Some(f) => envs.flatMap {
      case env => filter(f, env)
    }
  }

  // TODO> Use filter on list.

  /**
   * Optionally returns the given environment `env` if it satisfies the given propositional formula.
   */
  def filter(f: Proposition, env: Map[VSym, Value]): Option[Map[VSym, Value]] =
    if (Interpreter.satisfiable(f, env))
      Some(env)
    else
      None

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
  private def leq(v1: Value, v2: Value, typ: Type): Boolean = monitor.leq(typ) {
    val abs = program.lookupLeq(typ.resultType).get
    val app = Term.App(Term.App(abs, v1.toTerm), v2.toTerm)
    Interpreter.evaluate(app).toBool
  }

  /**
   * Returns the join of `v1` and `v2`.
   */
  private def lub(v1: Value, v2: Value, typ: Type): Value = monitor.lub(typ) {
    val abs = program.lookupLub(typ.resultType).get
    val app = Term.App(Term.App(abs, v1.toTerm), v2.toTerm)
    Interpreter.evaluate(app)
  }

}
