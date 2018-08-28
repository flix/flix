package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents an uninitialized relation.
  *
  * @param name the name of the relation.
  * @param attr the attributes of the relation.
  */
class RelationVar(name: String, attr: Array[Attribute]) extends Table {
  override def getName(): String = name

  def getAttributes(): Array[Attribute] = attr
}
