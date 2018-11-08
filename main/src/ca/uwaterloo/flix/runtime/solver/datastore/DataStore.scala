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

import java.io.{PrintWriter, StringWriter}

import ca.uwaterloo.flix.runtime.solver.api.{ConstraintSet, Relation, Table}
import ca.uwaterloo.flix.util.{AsciiTable, BitOps, InternalRuntimeException}

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * A class implementing a data store for indexed relations and lattices.
  */
class DataStore[ValueType <: AnyRef](constraintSet: ConstraintSet)(implicit m: ClassTag[ValueType]) {

  /**
    * A map from names to indexed relations.
    */
  val relations = mutable.Map.empty[Table, IndexedRelation]

  /**
    * A map from names to indexed lattices.
    */
  val lattices = mutable.Map.empty[Table, IndexedLattice]

  for (relation <- constraintSet.getRelations()) {
    val name = relation.getName()
    val attributes = relation.getAttributes()

    // NB: Just an index on the first attribute.
    val idx = Set(Seq(0))
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    relations += relation -> new IndexedRelation(name, attributes, indexes, indexes.head)
  }

  for (lattice <- constraintSet.getLattices()) {
    val name = lattice.getName()
    val keys = lattice.getKeys()
    val value = lattice.getValue()
    val ops = lattice.getOps()

    // NB: Just an index on the first attribute and on all the keys.
    val idx = Set(Seq(0), keys.indices)
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    lattices += (lattice -> new IndexedLattice(name, keys, value, indexes, ops))
  }

  def getRelations(): Traversable[IndexedRelation] = relations.values

  def getRelation(r: Relation): IndexedRelation = relations(r)

  def getLattices(): Traversable[IndexedLattice] = lattices.values

  def getLattice(l: ca.uwaterloo.flix.runtime.solver.api.Lattice): IndexedLattice = lattices(l)

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
