package ca.uwaterloo.flix.api

/**
  * Public interface for Flix models.
  */
trait IModel {

  /**
    * Returns the constant with the given fully qualified `name`.
    *
    * @throws IllegalArgumentException if no such constant exists.
    */
  def getConstant(name: String): IValue

  /**
    * Returns an iterable over the relation with the given fully qualified `name`.
    *
    * @throws IllegalArgumentException if no such relation exists.
    */
  def getRelation(name: String): java.lang.Iterable[Array[IValue]]

  /**
    * Returns an iterable over the lattice with the given fully qualified `name`.
    *
    * @throws IllegalArgumentException if no such lattice exists.
    */
  def getLattice(name: String): java.lang.Iterable[Array[IValue]]

}
