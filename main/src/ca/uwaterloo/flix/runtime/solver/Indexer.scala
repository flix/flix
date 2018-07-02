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

package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.runtime.solver.api._
import ca.uwaterloo.flix.runtime.solver.api.symbol.TableSym

import scala.collection.mutable

object Indexer {

  // TODO: This class is most likely currently broken. Rewrite from scratch!

  /**
    * Returns an index selection strategy based on left-to-right evaluation of constraint rules.
    */
  def index(root: ConstraintSet): Map[TableSym, Set[Seq[Int]]] = {
    val indexes = mutable.Map.empty[TableSym, Set[Seq[Int]]]

    // ensure every table has at least one index.
    for ((name, table) <- root.getTables()) {
      table match {
        case r: Table.Relation =>
          val idxs = indexes.getOrElse(name, Set.empty)
          indexes(name) = Set(Seq(0))
        case l: Table.Lattice =>
          val idxs = indexes.getOrElse(name, Set.empty)
          val everyKey = l.keys.indices
          indexes(name) = Set(Seq(0), everyKey)
      }
    }

    // return the result as an immutable map.
    indexes.toMap
  }

}
