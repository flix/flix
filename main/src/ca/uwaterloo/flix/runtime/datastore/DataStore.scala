package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.ExecutableAst.Collection
import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.phase.Indexer
import ca.uwaterloo.flix.runtime.Solver
import ca.uwaterloo.flix.util.BitOps

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * A class implementing a data store for indexed relations and lattices.
  */
class DataStore[ValueType <: AnyRef](implicit sCtx: Solver.SolverContext, m: ClassTag[ValueType]) {

  /**
    * A map from names to indexed relations.
    */
  val relations = mutable.Map.empty[Name.Resolved, IndexedRelation[ValueType]]

  /**
    * A map from names to indexed lattices.
    */
  val lattices = mutable.Map.empty[Name.Resolved, IndexedLattice[ValueType]]

  /**
    * Initializes the relations and lattices.
    */
  // compute indexes based on the program constraint rules.
  val indexes = Indexer.index(sCtx.root)

  // initialize all indexed relations and lattices.
  for ((name, collection) <- sCtx.root.collections) {
    // translate indexes into their binary representation.
    val idx = indexes(name) map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }

    collection match {
      case r: Collection.Relation =>
        relations(name) = new IndexedRelation[ValueType](r, idx, idx.head)

      case l: Collection.Lattice =>
        lattices(name) = new IndexedLattice[ValueType](l, idx)
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
