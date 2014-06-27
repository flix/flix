package impl.runtime

import impl._
import impl.util.collection.mutable

/**
 * A semi-naive solver.
 */
class Solver(p: Program) {

  /**
   * Relations.
   */
  val relation1 = mutable.MultiMap1.empty[Symbol, Value]
  val relation2 = mutable.MultiMap2.empty[Symbol, Value, Value]
  val relation3 = mutable.MultiMap3.empty[Symbol, Value, Value, Value]
  val relation4 = mutable.MultiMap4.empty[Symbol, Value, Value, Value, Value]
  val relation5 = mutable.MultiMap5.empty[Symbol, Value, Value, Value, Value, Value]

  /**
   * Lattice Maps.
   */
  val map1 = mutable.Map1.empty[Symbol, Value]
  val map2 = mutable.Map2.empty[Symbol, Value, Value]
  val map3 = mutable.Map3.empty[Symbol, Value, Value, Value]
  val map4 = mutable.Map4.empty[Symbol, Value, Value, Value, Value]
  val map5 = mutable.Map5.empty[Symbol, Value, Value, Value, Value, Value]

  /**
   * Dependencies.
   */
  val dependencies = mutable.MultiMap1.empty[Symbol, HornClause]

  /**
   * Depedency computation.
   */
  def deps(): Unit = {
    for (h <- p.clauses; p <- h.body) {
      dependencies.put(p.name, h)
    }
  }

  /**
   * Fixpoint computation.
   */
  def solve(): Unit = {
    val Q = scala.collection.mutable.Queue.empty[HornClause]

    for (f <- p.facts) {
      val name = f.head.name
      val terms = f.head.terms
    }

    while (Q.nonEmpty) {

    }

    ???
  }


  /**
   * Evaluates the given horn clause `h` under the given interpretations `inv` and environment `env`.
   *
   * Returns `true` iff the body is satisfied.
   */
  def evaluate(h: HornClause, inv: Map[Symbol, Interpretation], env: Map[Symbol, Value]): Boolean = inv.get(h.head.name) match {
    case None => throw new Error.UnknownInterpretation(h.head.name)
    case Some(i) => evaluate(h, i, env)
  }


  /**
   * Evaluates the given horn clause `h` under the given interpretation `i` and environment `env`.
   *
   * Returns `true` iff the body is satisfied.
   */
  def evaluate(h: HornClause, i: Interpretation, env: Map[Symbol, Value]): Boolean =
    ???


  def evaluate(p: Predicate, i: Interpretation, env: Map[Symbol, Value]): Boolean = i match {
    case Interpretation.Proposition(Value.Bool(true)) => true;
    case Interpretation.Proposition(Value.Bool(false)) => false;
    case Interpretation.Relation.In1(t1) => ???
  }

  /**
   * Satisfies the given predicate `p` under the given interpretation `i` and environment `env`.
   */
  def satisfy(p: Predicate, i: Interpretation, env: Map[Symbol, Value]): Unit = i match {
    case Interpretation.Relation.In1(t1) =>
      val v = lookupValue(0, p, env)
      val vs = relation1.get(p.name)
      if (!(vs contains v)) {
        relation1.put(p.name, v)
        propagate(p.name, v)
      }

    case Interpretation.Relation.In2(t1, t2) => ???
    case Interpretation.Relation.In3(t1, t2, t3) => ???
    case Interpretation.Relation.In4(t1, t2, t3, t4) => ???
    case Interpretation.Relation.In5(t1, t2, t3, t4, t5) => ???

    case Interpretation.Map.Leq1(t1) => ???
    case Interpretation.Map.Leq2(t1, t2) => ???
    case Interpretation.Map.Leq3(t1, t2, t3) => ???
    case Interpretation.Map.Leq4(t1, t2, t3, t4) => ???
    case Interpretation.Map.Leq5(t1, t2, t3, t4, t5) => ???

    case _ => throw new Error.UnableToSatisfyPredicate(p)
  }


  // TODO:Carefull about free variables.
  def lookupValue(index: Int, p: Predicate, env: Map[Symbol, Value]): Value = ???


  // TODO: Binding variables to values... for solving

  // TODO: Type binding for verification.

  def propagate(name: Symbol, v: Value): Unit = {

  }

}
