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
import ca.uwaterloo.flix.language.ast.ExecutableAst.Table
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}
import ca.uwaterloo.flix.language.phase.Indexer
import ca.uwaterloo.flix.runtime.{InvocationTarget, Linker}
import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.util.BitOps

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * A class implementing a data store for indexed relations and lattices.
  */
class DataStore[ValueType <: AnyRef](root: ExecutableAst.Root)(implicit m: ClassTag[ValueType], flix: Flix) {

  /**
    * A map from names to indexed relations.
    */
  val relations = mutable.Map.empty[Symbol.TableSym, IndexedRelation]

  /**
    * A map from names to indexed lattices.
    */
  val lattices = mutable.Map.empty[Symbol.TableSym, IndexedLattice]

  /**
    * Initializes the relations and lattices.
    */
  // compute indexes based on the program constraint rules.
  val indexes = Indexer.index(root)

  // initialize all indexed relations and lattices.
  for ((sym, table) <- root.tables) {
    // translate indexes into their binary representation.
    val idx = indexes(sym) map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }

    table match {
      case r: Table.Relation => relations(sym) = new IndexedRelation(r, idx, idx.head)
      case l: Table.Lattice =>
        val ops = getOps(l)
        lattices(sym) = new IndexedLattice(l, idx, ops)
    }
  }

  def getOps(l: Table.Lattice): LatticeOps = {
    val latticeOps = root.lattices(l.value.tpe)

    new LatticeOps {
      override def bot: ProxyObject = Linker.link(latticeOps.bot, root).invoke(Array.empty)

      override def equ: InvocationTarget = Linker.link(latticeOps.equ, root)

      override def leq: InvocationTarget = Linker.link(latticeOps.leq, root)

      override def lub: InvocationTarget = Linker.link(latticeOps.lub, root)

      override def glb: InvocationTarget = Linker.link(latticeOps.glb, root)
    }
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
