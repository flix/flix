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

import ca.uwaterloo.flix.runtime.solver.api.{ConstraintSet, Table}

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * A class implementing a data store for indexed relations and lattices.
  */
class DataStore[ValueType <: AnyRef](constraintSet: ConstraintSet)(implicit m: ClassTag[ValueType]) {

  /**
    * A map from names to indexed relations.
    */
  private val relations = mutable.Map.empty[Table, IndexedRelation]

  /**
    * A map from names to indexed lattices.
    */
  private val lattices = mutable.Map.empty[Table, IndexedLattice]

  for (sym <- constraintSet.getRelations()) {
    relations += (sym -> sym.getIndexedRelation())
  }

  for (sym <- constraintSet.getLattices()) {
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
    case (pred, relation) => relation.getIndexHits.map {
      case (index, count) => (pred.getName(), "{" + index.mkString(", ") + "}", count)
    }
  }.toSeq.sortBy(_._3).reverse.toList

  def indexMisses: List[(String, String, Int)] = relations.flatMap {
    case (pred, relation) => relation.getIndexMisses.map {
      case (index, count) => (pred.getName(), "{" + index.mkString(", ") + "}", count)
    }
  }.toSeq.sortBy(_._3).reverse.toList

  def predicateStats: List[(String, Int, Int, Int, Int)] = relations.map {
    case (pred, relation) => (
      pred.getName(),
      relation.getSize,
      relation.getNumberOfIndexedLookups,
      relation.getNumberOfIndexedScans,
      relation.getNumberOfFullScans)
  }.toSeq.sortBy(_._3).reverse.toList

}
