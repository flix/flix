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

package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.ExecutableAst
import ca.uwaterloo.flix.util.BitOps

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A class that stores a relation in an indexed database. An index is a subset of the columns encoded in binary.
  *
  * An index on the first column corresponds to 0b0000...0001.
  * An index on the first and third columns corresponds to 0b0000...0101.
  *
  * @param relation the relation.
  * @param indexes  the indexes.
  * @param default  the default index.
  */
final class IndexedRelation[ValueType](val relation: ExecutableAst.Table.Relation, indexes: Set[Int], default: Int) extends IndexedCollection[ValueType] {

  /**
    * A map from indexes to keys to rows of values.
    */
  private val store = mutable.Map.empty[Int, mutable.Map[Key[ValueType], mutable.ArrayBuffer[Array[ValueType]]]]

  /**
    * A map from indexes to number of index hits.
    */
  private val indexHits = mutable.Map.empty[Int, Int]

  /**
    * A map from indexes to number of index misses.
    */
  private val indexMisses = mutable.Map.empty[Int, Int]

  /**
    * Records the number of indexed lookups, i.e. exact lookups.
    */
  private var indexedLookups = 0

  /**
    * Records the number of indexed scans, i.e. table scans which can use an index.
    */
  private var indexedScans = 0

  /**
    * Records the number of full scans, i.e. table scans which cannot use an index.
    */
  private var fullScans = 0

  /**
    * Initialize the store for all indexes.
    */
  for (idx <- indexes) {
    store(idx) = mutable.Map.empty
    indexHits(idx) = 0
  }

  /**
    * Returns the size of the relation.
    */
  // TODO: Optimize
  def getSize: Int = scan.size

  /**
    * Returns the number of indexed lookups.
    */
  def getIndexHits: Map[Seq[String], Int] = indexHits.toMap.map {
    case (idx, count) =>
      val columns = (0 until 31).filter(n => BitOps.getBit(vec = idx, bit = n))
      val names = columns map (column => relation.attributes(column).name)
      names -> count
  }

  /**
    * Returns the number of indexed misses.
    */
  def getIndexMisses: Map[Seq[String], Int] = indexMisses.toMap.map {
    case (idx, count) =>
      val columns = (0 until 31).filter(n => BitOps.getBit(vec = idx, bit = n))
      val names = columns map (column => relation.attributes(column).name)
      names -> count
  }

  /**
    * Returns the number of indexed lookups.
    */
  def getNumberOfIndexedLookups: Int = indexedLookups

  /**
    * Returns the number of indexed scans.
    */
  def getNumberOfIndexedScans: Int = indexedScans

  /**
    * Returns the number of full scans.
    */
  def getNumberOfFullScans: Int = fullScans

  /**
    * Processes a new inferred `fact`.
    *
    * Adds the fact to the relation. All entries in the fact must be non-null.
    *
    * Returns `true` iff the fact did not already exist in the relation.
    */
  def inferredFact(fact: Array[ValueType]): Boolean = {
    if (lookup(fact).isEmpty) {
      newFact(fact)
      return true
    }
    false
  }

  /**
    * Updates all indexes and tables with a new `fact`.
    */
  private def newFact(fact: Array[ValueType]): Unit = {
    // loop through all the indexes and update the tables.
    for (idx <- indexes) {
      val key = keyOf(idx, fact)
      val table = store(idx).getOrElseUpdate(key, mutable.ArrayBuffer.empty[Array[ValueType]])
      table += fact
    }
  }

  /**
    * Performs a lookup of the given pattern `pat`.
    *
    * If the pattern contains `null` entries these are interpreted as free variables.
    *
    * Returns an iterator over the matching rows.
    */
  def lookup(pat: Array[ValueType]): Iterator[Array[ValueType]] = {
    // case 1: Check if there is an exact index.
    var idx = getExactIndex(indexes, pat)
    if (idx != 0) {
      // an exact index exists. Use it.
      indexedLookups += 1
      // NB: It is too expensive to count indexed lookups.
      // indexHits.update(idx, indexHits(idx) + 1)
      val key = keyOf(idx, pat)
      getOrEmptyIterator(store(idx).get(key))
    } else {
      // case 2: No exact index available. Check if there is an approximate index.
      val indexMiss = getIndex(pat) // NB: Only used for the next line
      indexMisses.update(indexMiss, indexMisses.getOrElse(indexMiss, 0) + 1)

      idx = getApproximateIndex(indexes, pat)
      val table = if (idx != 0) {
        // case 2.1: An approximate index exists. Use it.
        indexHits.update(idx, indexHits(idx) + 1)
        indexedScans += 1
        val key = keyOf(idx, pat)
        getOrEmptyIterator(store(idx).get(key))
      } else {
        // case 2.2: No usable index. Perform a full table scan.
        fullScans += 1
        scan
      }

      // filter rows returned by a partial index or table scan.
      table filter {
        case row => matchRow(pat, row)
      }
    }
  }

  /**
    * Returns all rows in the relation using a table scan.
    */
  def scan: Iterator[Array[ValueType]] = store(default).iterator.flatMap {
    case (key, value) => value
  }

  /**
    * Returns `true` if the given pattern `pat` matches the given `row`.
    *
    * A pattern matches if all is non-null entries are equal to the row.
    */
  private def matchRow(pat: Array[ValueType], row: Array[ValueType]): Boolean = {
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
    * Returns an iterator over the given array buffer.
    *
    * Returns the empty iterator if the option is [[None]].
    */
  @inline
  private def getOrEmptyIterator(opt: Option[ArrayBuffer[Array[ValueType]]]) = opt match {
    case None => Iterator.empty
    case Some(xs) => xs.iterator
  }

}
