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
        lattices(name) = new IndexedLattice(l, idx)
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
    Console.out.println()

    val t2 = new AsciiTable().withCols("Name", "Size")
    for ((name, lattice) <- lattices) {
      t2.mkRow(List(
        name,
        lattice.scan.length
      ))
    }
    t2.write(Console.out)
    Console.out.println()

    Console.out.println(">> Index Hits")
    val t3 = new AsciiTable().withCols("Relation", "Index", "Hits")
    val table = relations flatMap {
      case (name, relation) => relation.getIndexHitCounts.map {
        case (index, count) => (name, "{" + index.mkString(", ") + "}", count)
      }
    }
    for ((name, index, count) <- table.toList.sortBy(_._3).reverse) {
      t3.mkRow(List(name, index, count))
    }
    t3.write(Console.out)
    Console.out.println()
  }

  def indexStats: List[(String, String, Int)] = relations.flatMap {
    case (name, relation) => relation.getIndexHitCounts.map {
      case (index, count) => (name.toString, "{" + index.mkString(", ") + "}", count)
    }
  }.toSeq.sortBy(_._3).reverse.toList

}
