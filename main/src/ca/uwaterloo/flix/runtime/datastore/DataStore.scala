package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Collection
import ca.uwaterloo.flix.language.backend.phase.Indexer
import ca.uwaterloo.flix.runtime.Solver
import ca.uwaterloo.flix.util.{BitOps, AsciiTable}

import scala.collection.mutable

/**
  * A class implementing a data store for indexed relations and lattices.
  */
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
        relations(name) = new IndexedRelation(r, idx, idx.head)

      case l: Collection.Lattice =>
        lattices(name) = new IndexedLattice(l, idx, idx.head)
    }
  }

  def stats(): Unit = {
    val t = new AsciiTable().withCols("Name", "Size", "Indexed Lookups", "Indexed Scans", "Full Scans")
    for ((name, relation) <- relations) {
      t.mkRow(List(
        name,
        relation.getSize,
        relation.getNumberOfIndexedLookups,
        relation.getNumberOfIndexedScans,
        relation.getNumberOfFullScans
      ))
    }
    t.write(Console.out)
  }

}
