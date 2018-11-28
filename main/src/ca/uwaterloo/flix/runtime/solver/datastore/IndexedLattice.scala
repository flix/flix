/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.runtime.solver.datastore

import java.util

import flix.runtime.ProxyObject
import flix.runtime.fixpoint.{Attribute, LatticeOps}

import scala.annotation.switch
import scala.collection.mutable

class IndexedLattice(val name: String, val keys: Array[Attribute], val value: Attribute, indexes: Set[Int], ops: LatticeOps) extends IndexedCollection {

  /**
    * A map from indexes to a map from keys to rows (represented as map from keys to an element):
    *
    * Index -> IndexKey -> Keys -> Elm.
    */
  private val store = mutable.Map.empty[Int, mutable.Map[Key, mutable.Map[Key, ProxyObject]]]

  /**
    * The number of key columns in the lattice.
    */
  private val numberOfKeys = keys.length

  /**
    * The lattice operations.
    */
  val latticeOps = new LatticeImpl(ops)

  /**
    * Initialize the store for all indexes.
    */
  for (idx <- indexes) {
    store(idx) = mutable.Map.empty
  }

  /**
    * Returns the size of the relation.
    */
  def getSize: Int = scan.size

  override def toString: String = "IndexedLattice(" + getSize + ")"

  /**
    * Processes a new inferred `fact`.
    *
    * Adds the fact to the relation. All entries in the fact must be non-null.
    *
    * Returns `true` iff the fact did not already exist in the relation.
    */
  def inferredFact(fact: Array[ProxyObject]): Boolean = {
    // Get the index on every column.
    val pat = util.Arrays.copyOfRange(fact, 0, numberOfKeys)
    val idx = getExactIndex(indexes, pat)
    assert(idx != 0)

    // Lookup the lattice map (create it, if it doesn't exist).
    val ikey = keyOf(idx, fact)
    val map = store(idx).getOrElseUpdate(ikey, mutable.Map.empty)
    val key = keysOf(fact)

    // Lookup the old element (create it, if it doesn't exist).
    val newElm = elmOf(fact)
    val oldElm = map.getOrElseUpdate(key, latticeOps.bot)

    // Compute the lub and check if it is subsumed by the old element.
    val result = latticeOps.lub(newElm, oldElm)
    if (!latticeOps.leq(result, oldElm)) {
      // Update all indexes.
      for (idx <- indexes) {
        val ikey = keyOf(idx, fact)
        val map = store(idx).getOrElseUpdate(ikey, mutable.Map.empty)
        map(key) = result
      }
      return true
    }

    return false
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
  def lookup(pat: Array[ProxyObject]): Iterator[Array[ProxyObject]] = {
    // check if there is an exact index.
    var idx = getExactIndex(indexes, pat)
    val table = if (idx != 0) {
      // use exact index.
      val ikey = keyOf(idx, pat)
      getOrEmptyIterator(store(idx).get(ikey))
    } else {
      // check if there is an approximate index.
      idx = getApproximateIndex(indexes, pat)
      if (idx != 0) {
        // use approximate index.
        val ikey = keyOf(idx, pat)
        getOrEmptyIterator(store(idx).get(ikey))
      } else {
        // perform full table scan.
        scan
      }
    }

    table filter {
      // match the keys
      case (keys, _) => matchKeys(pat, keys.toArray)
    } map {
      case (keys, elm) =>
        // compute greatest lower bounds.
        if (elmOf(pat) == null)
          (keys, elm)
        else
          (keys, latticeOps.glb(elmOf(pat), elm))
    } filter {
      // remove null elements introduced above.
      case e => !latticeOps.equal(latticeOps.bot, e._2)
    } map {
      case (keys, elms) =>
        // construct the result.
        val result = new Array[ProxyObject](numberOfKeys + 1)
        System.arraycopy(keys.toArray, 0, result, 0, numberOfKeys)
        result(result.length - 1) = elms
        result.asInstanceOf[Array[ProxyObject]]
    }
  }

  /**
    * Returns all rows in the relation using a table scan.
    */
  def scan: Iterator[(Key, ProxyObject)] = store(indexes.head).iterator.flatMap {
    case (key, m) => m.iterator
  }

  /**
    * Returns the key part of the given array `a`.
    */
  private def keysOf(a: Array[ProxyObject]): Key = {
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
  @inline
  private def elmOf(a: Array[ProxyObject]): ProxyObject = {
    return a(a.length - 1)
  }

  /**
    * Returns `true` iff all non-null keys in the given pattern `pat` are
    * equal to their corresponding entry in the given `row`.
    */
  private def matchKeys(pat: Array[ProxyObject], row: Array[ProxyObject]): Boolean = {
    var i = 0
    while (i < numberOfKeys) {
      val pv = pat(i)
      if (pv != null)
        if (pv != row(i))
          return false
      i = i + 1
    }
    return true
  }

  /**
    * Returns an iterator over the given mutable map.
    *
    * Returns the empty iterator if the option is [[None]].
    */
  @inline
  private def getOrEmptyIterator(opt: Option[mutable.Map[Key, ProxyObject]]) = opt match {
    case None => Iterator.empty
    case Some(xs) => xs.iterator
  }

}
