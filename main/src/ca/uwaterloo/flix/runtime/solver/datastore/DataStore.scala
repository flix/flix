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
class DataStore[ValueType <: AnyRef](implicit m: ClassTag[ValueType]) {

  /**
    * A map from names to indexed relations.
    */
  val relations = mutable.Map.empty[PredSym, IndexedRelation]

  /**
    * A map from names to indexed lattices.
    */
  val lattices = mutable.Map.empty[PredSym, IndexedLattice]

  def getRelations(): Iterable[IndexedRelation] = relations.values

  def getRelation(r: PredSym, cs: ConstraintSystem): IndexedRelation = relations.getOrElseUpdate(r, initRelation(r.asInstanceOf[RelSym], cs.getArity(r)))

  def getLattices(): Iterable[IndexedLattice] = lattices.values

  def getLattice(l: PredSym, cs: ConstraintSystem): IndexedLattice = lattices.getOrElseUpdate(l, initLattice(l.asInstanceOf[LatSym], cs.getArity(l)))

  private def initRelation(relSym: RelSym, arity: Int): IndexedRelation = {
    val name = relSym.getName()
    val attributes = relSym.getAttributes()

    // NB: Just an index on the first attribute.
    val idx = Set(Seq(0))
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedRelation(name, arity, indexes, indexes.head)
  }

  private def initLattice(latSym: LatSym, arity: Int): IndexedLattice = {
    val name = latSym.getName()
    val ops = latSym.getOps()

    // NB: Just an index on the first attribute and on all the keys.
    val idx = Set(Seq(0), Range(0, arity - 1))
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }

    new IndexedLattice(name, arity, indexes, ops)
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
