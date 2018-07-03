package ca.uwaterloo.flix.runtime.solver.api.symbol

import ca.uwaterloo.flix.runtime.solver.api.Attribute
import ca.uwaterloo.flix.runtime.solver.datastore.IndexedRelation
import ca.uwaterloo.flix.util.BitOps

class RelSym(val name: String, val attributes: Array[Attribute]) extends TableSym {

  private val idx = Set(Seq(0), attributes.indices)
  private val indexes = idx map {
    case columns => BitOps.setBits(vec = 0, bits = columns)
  }
  private val indexedRelation = new IndexedRelation(attributes, indexes, indexes.head)

  def getIndexedRelation(): IndexedRelation = indexedRelation

  def canEqual(other: Any): Boolean = other.isInstanceOf[RelSym]

  override def equals(other: Any): Boolean = other match {
    case that: RelSym =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

}
