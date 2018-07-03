package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.datastore.IndexedRelation
import ca.uwaterloo.flix.util.BitOps

/**
  * Represents a relation value.
  */
class Relation(val name: String, val attributes: Array[Attribute]) extends Table {

  private val indexedRelation = {
    // NB: Just an index on the first attribute.
    val idx = Set(Seq(0))
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedRelation(attributes, indexes, indexes.head)
  }

  def getIndexedRelation(): IndexedRelation = indexedRelation

  override def toString: String = s"$name(${indexedRelation.getSize})"

}
