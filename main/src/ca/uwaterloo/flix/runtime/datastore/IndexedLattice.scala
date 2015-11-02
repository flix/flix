package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.runtime.{Interpreter, Solver, Value}
import ca.uwaterloo.flix.util.BitOps

import scala.collection.mutable

/**
 * Companion object for the [[IndexedLattice]] class.
 */
object IndexedLattice {

  /**
   * Constructs a new indexed relation for the given `relation` and `indexes`.
   */
  def mk(relation: TypedAst.Collection.Lattice, indexes: Set[Seq[Int]])(implicit sCtx: Solver.SolverContext) = {
    // translate indexes into their binary representation.
    val idx = indexes map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    // assume a default index on the first column.
    val defaultIndex = BitOps.setBit(vec = 0, bit = 0)

    new IndexedLattice(relation, idx + defaultIndex, defaultIndex)
  }

}

class IndexedLattice(lattice: TypedAst.Collection.Lattice, indexes: Set[Int], default: Int)(implicit sCtx: Solver.SolverContext) extends IndexedCollection {
  /**
   * A map from indexes to a map from keys to rows (represented as map from keys to elements).
   */
  private val store = mutable.Map.empty[Int, mutable.Map[Key, mutable.Map[Seq[Value], Array[Value]]]]

  private val split = lattice.keys.length

  private val numberOfKeys = lattice.keys.length

  private val numberOfElms = lattice.values.length

  private val latticeOps: Array[TypedAst.Definition.BoundedLattice] = lattice.values.map {
    case TypedAst.Attribute(_, tpe) => sCtx.root.lattices(tpe)
  }.toArray

  /**
   * Initialize the store for all indexes.
   */
  for (idx <- indexes) {
    store(idx) = mutable.Map.empty
  }

  /**
   * Processes a new inferred `fact`.
   *
   * Adds the fact to the relation. All entries in the fact must be non-null.
   *
   * Returns `true` iff the fact did not already exist in the relation.
   */
  def inferredFact(fact: Array[Value]): Boolean = {
    if (lookup(fact).isEmpty) {
      newFact(fact)
      return true
    }
    false
  }

  /**
   * Updates all indexes and tables with a new `fact`.
   */
  private def newFact(fact: Array[Value]): Unit = {
    // loop through all the indexes and update the tables.
    for (idx <- indexes) {
      val ikey = keyOf(idx, fact)
      val table = store(idx).getOrElseUpdate(ikey, mutable.Map.empty)

      val elms1 = elmPart(fact)
      val elms2 = table.getOrElseUpdate(keyPart(fact), elms1)

      table += (keyPart(fact) -> lub(elms1, elms2))
    }
  }

  /**
   * Performs a lookup of the given pattern `pat`. 
   *
   * If the pattern contains `null` entries these are interpreted as free variables.
   * 
   * If the pattern contains lattice elements then greatest-lower-bound is computed for these.
   *
   * Returns an iterator over the matching rows.
   */
  def lookup(pat: Array[Value]): Iterator[Array[Value]] = {
    // check if there is an exact index.
    var idx = getExactIndex(indexes, pat)
    val table = if (idx != 0) {
      // use exact index.
      val ikey = keyOf(idx, pat)
      store(idx).getOrElseUpdate(ikey, mutable.Map.empty).iterator
    } else {
      // check if there is an approximate index.
      idx = getApproximateIndex(indexes, pat)
      val ikey = keyOf(idx, pat)
      if (idx != 0) {
        // use approximate index.
        store(idx).getOrElseUpdate(ikey, mutable.Map.empty).iterator
      } else {
        // perform full table scan.
        scan
      }
    }

    table filter {
      // match the keys
      case (keys, _) => matchKey(keyPart(pat).toArray, keys.toArray)
    } map {
      case (keys, elms) =>
        val elms2 = elms.clone()
        // match the elements possibly changing elms2 to their greatest lower bound.
        if (matchElms(elmPart(pat), elms2)) (keys, elms2) else null
    } filter {
      case e => e != null
    } map {
      case (keys, elms) => keys.toArray ++ elms
    }
  }

  /**
   * Returns all rows in the relation using a table scan.
   */
  def scan: Iterator[(Seq[Value], Array[Value])] = store(default).iterator.flatMap {
    case (key, m) => m.iterator
  }

  /**
   * Returns the key part of the given array `a`.
   */
  def keyPart(a: Array[Value]): Seq[Value] = {
    a.take(numberOfKeys).toSeq
  }

  /**
   * Returns the element part of the given array `a`.
   */
  def elmPart(a: Array[Value]): Array[Value] = {
    val result = new Array[Value](numberOfElms)
    var i = 0
    while (i < numberOfElms) {
      result(i) = a(i + numberOfKeys)
      i = i + 1
    }
    return result
  }

  /**
   * Returns `true` iff all non-null entries in the given pattern `pat`
   * are equal to their corresponding entry in the given `row`.
   */
  def matchKey(pat: Array[Value], row: Array[Value]): Boolean = {
    var i = 0
    while (i < pat.length) {
      val pv = pat(i)
      if (pv != null)
        if (pv != row(i))
          return false
      i = i + 1
    }
    return true
  }

  // TODO: Careful with lattice values, should not return boolean
  def matchElms(pat: Array[Value], row: Array[Value]): Boolean = {
    // TODO: so this is incorrect. Need to use glb.
    for (i <- row.indices) {
      val pv = pat(i)
      if (pv != null) {
        val rv = row(i)
        if (pv != rv) {
          throw new RuntimeException("Need to compute GLB")
        }
      }
    }
    true
  }

  /**
   * Returns `true` iff `a` is pairwise less than or equal to `b`.
   */
  private def leq(a: Array[Value], b: Array[Value]): Boolean = {
    var i = 0
    while (i < a.length) {
      val leq = latticeOps(i).leq
      val value = Interpreter.eval2(leq, a(i), b(i), sCtx.root)
      if (!value.toBool) {
        return false
      }
      i = i + 1
    }
    return true
  }

  /**
   * Returns the pairwise least upper bound of `a` and `b`.
   */
  private def lub(a: Array[Value], b: Array[Value]): Array[Value] = {
    val result = new Array[Value](a.length)
    var i = 0
    while (i < result.length) {
      val lub = latticeOps(i).lub
      val value = Interpreter.eval2(lub, a(i), b(i), sCtx.root)
      result(i) = value
      i = i + 1
    }
    return result
  }

}
