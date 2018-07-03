package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.datastore.IndexedRelation
import ca.uwaterloo.flix.util.BitOps

class Relation(val name: String, val attributes: Array[Attribute]) extends Table {

  private val indexedRelation = {
    val idx = Set(Seq(0))
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedRelation(attributes, indexes, indexes.head)
  }

  def getIndexedRelation(): IndexedRelation = indexedRelation

  def canEqual(other: Any): Boolean = other.isInstanceOf[Relation]

  override def equals(other: Any): Boolean = other match {
    case that: Relation =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

}
