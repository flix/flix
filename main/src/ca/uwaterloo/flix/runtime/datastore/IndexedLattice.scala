package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.runtime.Value

import scala.collection.mutable

class IndexedLattice(lattice: TypedAst.Collection.Lattice, indexes: Set[Seq[Int]], root: TypedAst.Root) {

  private val store = mutable.Map.empty[(Seq[Int], Seq[Value]), mutable.Set[(Array[Value], Array[Value])]]

  private val defaultIndex: Seq[Int] = Seq(0)

  private val ops: Array[TypedAst.Definition.BoundedLattice] = Array.empty

  def table: Iterator[Array[Value]] = scan

  def inferredFact(f: Array[Value]): Boolean = {
    // TODO: Need to lookup that thing
    ???
  }

  private def newFact(f: Array[Value]): Boolean = {
    // todo

    ???
  }

  def lookup(keys: Array[Value], values: Array[Value]): Iterator[Array[Value]] = {

    ???
  }

  /**
   * Returns all rows in the relation using a table scan.
   */
  // TODO: Improve performance ...
  private def scan: Iterator[Array[Value]] = ???

}
