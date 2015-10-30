package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.runtime.{Interpreter, Solver, Value}

import scala.collection.mutable

class IndexedLattice(lattice: TypedAst.Collection.Lattice, indexes: Set[Seq[Int]])(implicit sCtx: Solver.SolverContext) extends IndexedCollection {
  // TODO: Initialize store for all indexes?
  // TODO: Careful with equality semantics for keys
  private val store = mutable.Map.empty[(Seq[Int], Seq[Value]), mutable.Map[Seq[Value], Array[Value]]]

  // TODO: What if the lattice has only one lattice column????
  private val defaultIndex: Seq[Int] = Seq(0)
  assert(lattice.keys.nonEmpty)


  private val split = lattice.keys.length

  private val latticeOps: Array[TypedAst.Definition.BoundedLattice] = lattice.values.map {
    case TypedAst.Attribute(_, tpe) => sCtx.root.lattices(tpe)
  }.toArray

  def table: Iterator[Array[Value]] = scan.map {
    case (keys, elms) => (keys.toSeq ++ elms.toSeq).toArray
  }

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
        m.get(keys1) match {
          case None =>
          case Some(elms2) => if (leq(elms1, elms2)) {
            return false
          }
        }

    }

    newFact(f)
  }

  /**
   * Updates all indexes and tables with a new fact `f`.
   */
  private def newFact(f: Array[Value]): Boolean = {
    val (keys1, elms1) = f.splitAt(split)

    for (idx <- indexes + defaultIndex) {
      val key = (idx, idx map f)
      store.get(key) match {
        case None =>
          val m = mutable.Map.empty[Seq[Value], Array[Value]]
          m(keys1) = elms1
          store(key) = m
        case Some(m) =>
          m.get(keys1) match {
            case None => m(keys1) = elms1
            case Some(elms2) => m(keys1) = lub(elms1, elms2)
          }
      }
    }
    true
  }


  def lookup(row: Array[Value]): Iterator[Array[Value]] = {
    val (keys, elms) = row.splitAt(split)

    val idx = keys.toSeq.zipWithIndex.collect {
      case (v, i) if v != null => i
    }
    val key = (idx, idx map row)

    val resultSet = if (indexes contains idx) {
      store.getOrElse(key, mutable.Map.empty).iterator
    } else {
      scan
    }

    resultSet filter {
      case (keys2, elms2) => keyMatches(keys, keys2.toArray) && elmsMatches(elms, elms2)
    } map {
      case (keys, elms) => keys.toArray ++ elms
    }
  }

  /**
   * Returns all rows in the relation using a table scan.
   */
  // TODO: Improve performance ...
  // TODO: Careful with duplicates...
  def scan: Iterator[(Seq[Value], Array[Value])] = (store map {
    case (keys, m) => m.toList
  }).flatten[(Seq[Value], Array[Value])].iterator


  def keyMatches(pat: Array[Value], row: Array[Value]): Boolean = {
    for (i <- row.indices) {
      val p = pat(i)
      if (p != null && p != row(i)) {
        return false
      }
    }
    true
  }

  // TODO: Careful with lattice values, should not return boolean
  def elmsMatches(pat: Array[Value], row: Array[Value]): Boolean = {
    // TODO: so this is incorrect. Need to use glb.
    for (i <- row.indices) {
      val p = pat(i)
      if (p != null) {
        if (p != row(i)) {
          throw new RuntimeException("Need to compute GLB")
        }
      }
    }
    true
  }

  private def leq(a: Array[Value], b: Array[Value]): Boolean = {
    for (i <- a.indices) {
      val leq = latticeOps(i).leq
      val value = Interpreter.eval2(leq, a(i), b(i), sCtx.root)
      if (!value.toBool)
        return false
    }
    true
  }

  private def lub(a: Array[Value], b: Array[Value]): Array[Value] = {
    val result = Array.ofDim[Value](a.length)
    for (i <- result.indices) {
      val lub = latticeOps(i).lub
      val value = Interpreter.eval2(lub, a(i), b(i), sCtx.root)
      result(i) = value
    }
    result
  }

}
