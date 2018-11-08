package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a relation value.
  */
class Relation(val name: String, attributes: Array[Attribute]) extends Table {

  /**
    * Returns the name of the relation.
    */
  def getName(): String = name

  /**
    * Returns the attributes of the relation.
    */
  def getAttributes(): Array[Attribute] = attributes

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
