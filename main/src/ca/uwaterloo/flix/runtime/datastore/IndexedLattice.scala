package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.runtime.{Interpreter, Solver, Value}

import scala.annotation.switch
import scala.collection.mutable

import java.util

class IndexedLattice(lattice: TypedAst.Collection.Lattice, indexes: Set[Int], default: Int)(implicit sCtx: Solver.SolverContext) extends IndexedCollection {
  /**
    * A map from indexes to a map from keys to rows (represented as map from keys to elements).
    */
  private val store = mutable.Map.empty[Int, mutable.Map[Key, mutable.Map[Key, Array[Value]]]]

  /**
    * The number of key columns in the lattice.
    */
  private val numberOfKeys = lattice.keys.length

  /**
    * The number of element columns in the lattice.
    */
  private val numberOfElms = lattice.values.length

  /**
    * The lattice operations associated with each lattice.
    */
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
    val matches = lookup(fact)
    if (matches.isEmpty) {
      newFact(fact)
      return true
    } else {
      val oldFact = matches.next()
      if (!leq(elmPart(fact), elmPart(oldFact))) {
        newFact(fact)
        return true
      }
    }
    return false
  }

  /**
    * Updates all indexes and tables with a new `fact`.
    */
  private def newFact(fact: Array[Value]): Unit = {
    // loop through all the indexes and update the tables.
    for (idx <- indexes) {
      val ikey = keyOf(idx, fact)
      val table = store(idx).getOrElseUpdate(ikey, mutable.Map.empty)

      val newElms = elmPart(fact)
      val oldElms = table.getOrElseUpdate(keyPart(fact), newElms)

      // if newElms and oldElms are different we must compute their lub.
      if (newElms ne oldElms) {
        // compute the lub and update oldElms directly.
        val result = lub(newElms, oldElms)
        System.arraycopy(result, 0, oldElms, 0, oldElms.length)
      }
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
      store(idx).getOrElse(ikey, mutable.Map.empty).iterator
    } else {
      // check if there is an approximate index.
      idx = getApproximateIndex(indexes, pat)
      if (idx != 0) {
        // use approximate index.
        val ikey = keyOf(idx, pat)
        store(idx).getOrElse(ikey, mutable.Map.empty).iterator
      } else {
        // perform full table scan.
        scan
      }
    }

    table filter {
      // match the keys
      case (keys, _) => matchKey(util.Arrays.copyOf(pat, numberOfKeys), keys.toArray)
    } map {
      case (keys, elms) =>
        val elmsCopy = elms.clone()
        // match the elements possibly changing elms2 to their greatest lower bound.
        if (matchElms(elmPart(pat), elmsCopy)) (keys, elmsCopy) else null
    } filter {
      case e => e != null
    } map {
      case (keys, elms) =>
        val result = new Array[Value](numberOfKeys + numberOfElms)
        System.arraycopy(keys.toArray, 0, result, 0, numberOfKeys)
        System.arraycopy(elms, 0, result, numberOfKeys, numberOfElms)
        result
    }
  }

  /**
    * Returns all rows in the relation using a table scan.
    */
  def scan: Iterator[(Key, Array[Value])] = store(default).iterator.flatMap {
    case (key, m) => m.iterator
  }

  /**
    * Returns the key part of the given array `a`.
    */
  def keyPart(a: Array[Value]): Key = {
    (numberOfKeys: @switch) match {
      case 1 => new Key1(a(0))
      case 2 => new Key2(a(0), a(1))
      case 3 => new Key3(a(0), a(1), a(2))
      case 4 => new Key4(a(0), a(1), a(2), a(3))
      case 5 => new Key5(a(0), a(1), a(2), a(3), a(4))
      case _ => throw new RuntimeException("Internal Error. Keys longer than 5 not supported.");
    }
  }

  /**
    * Returns the element part of the given array `a`.
    */
  def elmPart(a: Array[Value]): Array[Value] = {
    return util.Arrays.copyOfRange(a, numberOfKeys, a.length)
  }

  /**
    * Returns `true` iff all non-null entries in the given pattern `pat`
    * are equal to their corresponding entry in the given `row`.
    */
  // TODO: Optimize by changing signature
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

  /**
    * Returns `true` iff the pairwise greatest lower bound of
    * the given pattern `pat` and the given `row` is non-bottom.
    *
    * Modifies the given `row` in the process.
    */
  def matchElms(pat: Array[Value], row: Array[Value]): Boolean = {
    var i = 0
    while (i < pat.length) {
      val pv = pat(i)
      if (pv != null) {
        val rv = row(i)

        if (rv != pv) {
          val lattice = latticeOps(i)
          val bot = Interpreter.eval(lattice.bot, sCtx.root)
          val glb = Interpreter.eval2(lattice.glb, pv, rv, sCtx.root)

          if (bot == glb)
            return false

          row(i) = glb
        }
      }
      i = i + 1
    }
    return true
  }

  /**
    * Returns `true` iff `a` is pairwise less than or equal to `b`.
    */
  private def leq(a: Array[Value], b: Array[Value]): Boolean = {
    var i = 0
    while (i < a.length) {
      val leq = latticeOps(i).leq
      val v1: Value = a(i)
      val v2: Value = b(i)

      // if v1 and v2 are equal we do not need to compute leq.
      if (v1 ne v2) {
        // v1 and v2 are different, must compute leq.
        val value = Interpreter.eval2(leq, v1, v2, sCtx.root)
        if (!value.toBool) {
          return false
        }
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
