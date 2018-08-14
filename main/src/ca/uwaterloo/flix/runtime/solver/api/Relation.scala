package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.datastore.IndexedRelation
import ca.uwaterloo.flix.util.BitOps

/**
  * Represents a relation value.
  */
class Relation(name: String, attributes: Array[Attribute]) extends Table {

  /**
    * Returns the name of the relation.
    */
  def getName(): String = name

  /**
    * Returns the attributes of the relation.
    */
  def getAttributes(): Array[Attribute] = attributes

  private val indexedRelation = {
    // NB: Just an index on the first attribute.
    val idx = Set(Seq(0))
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedRelation(attributes, indexes, indexes.head)
  }

  def getIndexedRelation(): IndexedRelation = indexedRelation

  /**
    * Returns a string representation of the relation.
    */
  override def toString: String = s"$name(${indexedRelation.toString})"

}
