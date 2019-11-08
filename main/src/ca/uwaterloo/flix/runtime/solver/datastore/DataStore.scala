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

import ca.uwaterloo.flix.util.BitOps
import flix.runtime.fixpoint.ConstraintSystem
import flix.runtime.fixpoint.symbol.{LatSym, PredSym, RelSym}

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * A class implementing a data store for indexed relations and lattices.
  */
class DataStore[ValueType <: AnyRef](constraintSet: ConstraintSystem)(implicit m: ClassTag[ValueType]) {

  /**
    * A map from names to indexed relations.
    */
  val relations = mutable.Map.empty[PredSym, IndexedRelation]

  /**
    * A map from names to indexed lattices.
    */
  val lattices = mutable.Map.empty[PredSym, IndexedLattice]

  def getRelations(): Iterable[IndexedRelation] = relations.values

  def getRelation(r: PredSym): IndexedRelation = relations.getOrElseUpdate(r, initRelation(r.asInstanceOf[RelSym]))

  def getLattices(): Iterable[IndexedLattice] = lattices.values

  def getLattice(l: PredSym): IndexedLattice = lattices.getOrElseUpdate(l, initLattice(l.asInstanceOf[LatSym]))

  private def initRelation(relSym: RelSym): IndexedRelation = {
    val name = relSym.getName()
    val attributes = relSym.getAttributes()

    // NB: Just an index on the first attribute.
    val idx = Set(Seq(0))
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedRelation(name, relSym.getArity, indexes, indexes.head)
  }

  private def initLattice(latSym: LatSym): IndexedLattice = {
    val name = latSym.getName()
    val keys = latSym.getKeys()
    val ops = latSym.getOps()

    // NB: Just an index on the first attribute and on all the keys.
    val idx = Set(Seq(0), keys.indices)
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }

    new IndexedLattice(name, latSym.getArity, indexes, ops)
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

  def indexHits: List[(String, String, Int)] = Nil // TODO: Currently broken

  def indexMisses: List[(String, String, Int)] = Nil // TODO: Currently broken

  def predicateStats: List[(String, Int, Int, Int, Int)] = Nil // TODO: Currently broken

}
