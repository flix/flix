package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Collection
import ca.uwaterloo.flix.language.backend.phase.Indexer
import ca.uwaterloo.flix.runtime.Solver

import scala.collection.mutable

class DataStore(implicit sCtx: Solver.SolverContext) {

  /**
   * A map from names to indexed relations.
   */
  val relations = mutable.Map.empty[Name.Resolved, IndexedRelation]

  /**
   * A map from names to indexed lattices.
   */
  val lattices = mutable.Map.empty[Name.Resolved, IndexedLattice]

  /**
   * Initializes the collections.
   */
  def init(): Unit = {
    // compute indexes based on the program constraint rules.
    val indexes = Indexer.index(sCtx.root)

    // initialize all indexed relations and lattices.
    for ((name, collection) <- sCtx.root.collections) {
      collection match {
        case r: Collection.Relation => relations(name) = new IndexedRelation(r, indexes.getOrElse(name, Set.empty))
        case l: Collection.Lattice => lattices(name) = new IndexedLattice(l, indexes.getOrElse(name, Set.empty))
      }
    }
  }

}
