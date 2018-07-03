package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a collection of constraints.
  */
class ConstraintSet(relations: Array[Relation], lattices: Array[Lattice], strata: Array[Stratum]) {

  /**
    * Returns all the relation values in the constraint set.
    */
  def getRelations(): Array[Relation] = relations

  /**
    * Returns all the lattice values in the constraint set.
    */
  def getLattices(): Array[Lattice] = lattices

  /**
    * Returns the strata in the constraint set.
    */
  def getStrata(): Array[Stratum] = strata

  /**
    * Returns the union of `this` constraint set with `that` constraint set.
    */
  def union(that: ConstraintSet): Constraint = {
    // TODO: How to deal with the strata?

    ???
  }

}
