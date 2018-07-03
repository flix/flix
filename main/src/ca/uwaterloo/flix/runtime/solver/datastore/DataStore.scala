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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.runtime.solver.api.symbol.TableSym
import ca.uwaterloo.flix.runtime.solver.api.ConstraintSet

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * A class implementing a data store for indexed relations and lattices.
  */
class DataStore[ValueType <: AnyRef](root: ConstraintSet)(implicit m: ClassTag[ValueType], flix: Flix) {

  /**
    * A map from names to indexed relations.
    */
  private val relations = mutable.Map.empty[TableSym, IndexedRelation]

  /**
    * A map from names to indexed lattices.
    */
  private val lattices = mutable.Map.empty[TableSym, IndexedLattice]

  for (sym <- root.getRelSyms()) {
    relations += (sym -> sym.getIndexedRelation())
  }

  for (sym <- root.getLatSyms()) {
    lattices += (sym -> sym.getIndexedLattice())
  }

  /**
    * Returns the total number of facts in the datastore.
    */
  def numberOfFacts: Int = {
    var result: Int = 0
    for ((name, relation) <- relations) {
      result += relation.getSize
    }
    for ((name, lattices) <- lattices) {
      result += lattices.getSize
    }
    return result
  }

  def indexHits: List[(String, String, Int)] = relations.flatMap {
    case (name, relation) => relation.getIndexHits.map {
      case (index, count) => (name.toString, "{" + index.mkString(", ") + "}", count)
    }
  }.toSeq.sortBy(_._3).reverse.toList

  def indexMisses: List[(String, String, Int)] = relations.flatMap {
    case (name, relation) => relation.getIndexMisses.map {
      case (index, count) => (name.toString, "{" + index.mkString(", ") + "}", count)
    }
  }.toSeq.sortBy(_._3).reverse.toList

  def predicateStats: List[(String, Int, Int, Int, Int)] = relations.map {
    case (name, relation) => (
      name.toString,
      relation.getSize,
      relation.getNumberOfIndexedLookups,
      relation.getNumberOfIndexedScans,
      relation.getNumberOfFullScans)
  }.toSeq.sortBy(_._3).reverse.toList

}
