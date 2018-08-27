package ca.uwaterloo.flix.runtime.solver.api

class RelationPlaceholder(name: String, val attr: Array[Attribute]) extends Table {
  override def getName(): String = name
}
