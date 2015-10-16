package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.runtime.{Solver, Value}

import scala.collection.mutable

class IndexedLattice(lattice: TypedAst.Collection.Lattice, indexes: Set[Seq[Int]])(implicit sCtx: Solver.SolverContext) {

  private val store = mutable.Map.empty[(Seq[Int], Seq[Value]), mutable.Map[Array[Value], Array[Value]]]

  // TODO: What if the lattice has only one lattice column????
  private val defaultIndex: Seq[Int] = Seq(0)
  assert(lattice.keys.nonEmpty)

  private val split = lattice.keys.length

  private val latticeOps: Array[TypedAst.Definition.BoundedLattice] = Array.empty

  private val bottom = latticeOps.map(_.bot)

  def table: Iterator[Array[Value]] = scan

  /**
   * Processes a new inferred fact `f`.
   *
   * Joins the fact into the lattice and returns `true` iff the fact was not subsumed by a previous fact.
   */
  def inferredFact(f: Array[Value]): Boolean = {
    val key = (defaultIndex, defaultIndex map f)
    val (keys1, elms1) = f.splitAt(split)

    store.get(key) match {
      case None =>
      case Some(m) =>
        val elms2 = m(keys1)
        if (leq(elms1, elms2))
          return false
    }

    newFact(f)
  }

  /**
   * Updates all indexes and tables with a new fact `f`.
   */
  private def newFact(f: Array[Value]): Boolean = {
    val (keys, elms) = f.splitAt(split)

    for ((idx, _) <- store.keys) {
      val key = (idx, idx map f)
      store.get(key) match {
        case None =>
          val m = mutable.Map.empty[Array[Value], Array[Value]]
          m(keys) = elms
          store(key) = m
        case Some(m) =>
          val elms2 = m(keys)
          m(keys) = lub(elms, elms2)
      }
    }
    true
  }

  def lookup(keys: Array[Value], values: Array[Value]): Iterator[Array[Value]] = {
    ???
  }

  /**
   * Returns all rows in the relation using a table scan.
   */
  // TODO: Improve performance ...
  private def scan: Iterator[Array[Value]] = ???


  private def leq(a: Array[Value], b: Array[Value]): Boolean = ???

  private def lub(a: Array[Value], b: Array[Value]): Array[Value] = ???

}
