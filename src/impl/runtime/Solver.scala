package impl.runtime

import impl._

import scala.collection.mutable
import scala.util.Try

class Solver(p: Program) {

  /**
   * Relations.
   */
  val relation1 = mutable.Map.empty[Symbol, Set[Value]]
  val relation2 = mutable.Map.empty[Symbol, Map[Value, Map[Value, Set[Value]]]]
  val relation3 = mutable.Map.empty[Symbol, Map[Value, Map[Value, Map[Value, Set[Value]]]]]
  val relation4 = mutable.Map.empty[Symbol, Map[Value, Map[Value, Map[Value, Map[Value, Set[Value]]]]]]
  val relation5 = mutable.Map.empty[Symbol, Map[Value, Map[Value, Map[Value, Map[Value, Map[Value, Set[Value]]]]]]]

  /**
   * Lattice Maps.
   */
  val map1 = mutable.Map.empty[Symbol, Value]
  val map2 = mutable.Map.empty[Symbol, Map[Value, Map[Value, Value]]]
  val map3 = mutable.Map.empty[Symbol, Map[Value, Map[Value, Map[Value, Value]]]]
  val map4 = mutable.Map.empty[Symbol, Map[Value, Map[Value, Map[Value, Map[Value, Value]]]]]
  val map5 = mutable.Map.empty[Symbol, Map[Value, Map[Value, Map[Value, Map[Value, Map[Value, Value]]]]]]

  /**
   * Depedency computation.
   */
  def deps(): Unit = {

  }

  /**
   * Fixpoint computation.
   */
  def solve(): Unit = {
    val Q = mutable.Queue.empty[HornClause]

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
  // TODO: Add new items to worklist
  def satisfy(p: Predicate, i: Interpretation, env: Map[Symbol, Value]): Unit = i match {
    case Interpretation.Relation.In1(t) => relation1.put(p.name, Set.empty) // TODO
    case Interpretation.Map.Leq1(t) => ??? // TODO
    case _ => // TODO: Cannot satisfy
  }



  // TODO:Carefull about free variables.
  def lookupValue(index: Int, p: Predicate, env: Map[Symbol, Value]): Value = ???

}
