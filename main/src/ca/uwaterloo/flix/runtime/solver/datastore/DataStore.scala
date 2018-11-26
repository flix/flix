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

  // Initialize relations for every relation symbol in the constraint system.
  for (relSym <- constraintSet.getRelationSymbols()) {
    relations += relSym -> initRelation(relSym)
  }

  // Initialize lattices for every lattice symbol in the constraint system.
  for (latSym <- constraintSet.getLatticeSymbols()) {
    lattices += (latSym -> initLattice(latSym))
  }

  def getRelations(): Traversable[IndexedRelation] = relations.values

  def getRelation(r: PredSym): IndexedRelation = relations(r)

  def getLattices(): Traversable[IndexedLattice] = lattices.values

  def getLattice(l: PredSym): IndexedLattice = lattices(l)

  def initRelation(relSym: RelSym): IndexedRelation = {
    val name = relSym.getName()
    val attributes = relSym.getAttributes()

    // NB: Just an index on the first attribute.
    val idx = Set(Seq(0))
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedRelation(name, attributes, indexes, indexes.head)
  }

  def initLattice(latSym: LatSym): IndexedLattice = {
    val name = latSym.getName()
    val keys = latSym.getKeys()
    val value = latSym.getValue()
    val ops = latSym.getOps()

    // NB: Just an index on the first attribute and on all the keys.
    val idx = Set(Seq(0), keys.indices)
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }

    new IndexedLattice(name, keys, value, indexes, ops)
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


  //  /**
  //    * Returns a human readable string representation of the relation.
  //    */
  //  private def asString(r: Relation): String = {
  //    val sb = new StringBuilder
  //
  //    // Construct an ASCII table with a column for each attribute.
  //    val columns = attributes.map(_.getName())
  //    val table = new AsciiTable().withTitle(name).withCols(columns: _*)
  //
  //    // Add each row to the ASCII table.
  //    for (row <- indexedRelation.scan) {
  //      table.mkRow(row.toList)
  //    }
  //
  //    // Write the ASCII table to the string buffer.
  //    val sw = new StringWriter()
  //    table.write(new PrintWriter(sw))
  //    sb.append(sw.toString)
  //
  //    sb.toString()
  //  }
  //
  //  /**
  //    * Returns a human readable string representation of the lattice.
  //    */
  //  override def toString: String = {
  //    val sb = new StringBuilder
  //
  //    // Construct an ASCII table with a column for each attribute.
  //    val attributes = keys.toList ::: value :: Nil
  //    val columns = attributes.map(_.getName())
  //    val table = new AsciiTable().withTitle(name).withCols(columns: _*)
  //
  //    // Add each row to the ASCII table.
  //    for ((key, value) <- indexedLattice.scan) {
  //      val row = key.toArray.toList ::: value :: Nil
  //      table.mkRow(row)
  //    }
  //
  //    // Write the ASCII table to the string buffer.
  //    val sw = new StringWriter()
  //    table.write(new PrintWriter(sw))
  //    sb.append(sw.toString)
  //
  //    sb.toString()
  //  }


}
