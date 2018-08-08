package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a collection of constraints.
  */
class ConstraintSet(relations: Array[Relation], lattices: Array[Lattice], strata: Array[Stratum]) {

  /**
    * Returns all the relation values in the constraint set.
    */
  // TODO: Maybe it would be easier if these are computed based on the actual constraints in the set?
  def getRelations(): Array[Relation] = relations

  /**
    * Returns all the lattice values in the constraint set.
    */
  // TODO: Maybe it would be easier if these are computed based on the actual constraints in the set?
  def getLattices(): Array[Lattice] = lattices

  /**
    * Returns the strata in the constraint set.
    */
  def getStrata(): Array[Stratum] = strata

  /**
    * Returns the union of `this` constraint set with `that` constraint set.
    */
  def union(that: ConstraintSet): ConstraintSet = {
    // TODO: Correctness. This is just a hack for now.

    // TODO: What about duplicates?
    val newRelations = this.getRelations() ++ that.getRelations()
    val newLattices = this.getLattices() ++ that.getLattices()
    val newStrata = (this.getStrata() zip that.getStrata()) map {
      case (stratum1, stratum2) => new Stratum(stratum1.getConstraints() ++ stratum2.getConstraints())
    }

    new ConstraintSet(newRelations, newLattices, newStrata)
  }

}
