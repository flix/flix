package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.predicate.AtomPredicate

import scala.collection.mutable

/**
  * Represents a collection of constraints.
  */
class ConstraintSet(relations: Array[Relation], lattices: Array[Lattice], strata: Array[Stratum]) {

  /**
    * Returns all the relation values in the constraint set.
    */
  def getRelations(): Array[Relation] = getAllRelations()

  /**
    * Returns all the lattice values in the constraint set.
    */
  def getLattices(): Array[Lattice] = getAllLattices()

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

  override def toString: String = strata.mkString(", ")

  /**
    * Computes all relations in the constraint set.
    */
  private def getAllRelations(): Array[Relation] = {
    val relations = mutable.Set.empty[Relation]
    for (stratum <- strata) {
      for (constraint <- stratum.getConstraints()) {
        constraint.getHeadPredicate() match {
          case p: AtomPredicate => p.getSym() match {
            case r: Relation => relations += r
            case _ =>
          }
          case _ => // nop
        }
        for (predicate <- constraint.getAtoms()) {
          predicate match {
            case p: AtomPredicate => p.getSym() match {
              case r: Relation => relations += r
              case _ =>
            }
            case _ => // nop
          }
        }
      }
    }
    relations.toArray
  }

  /**
    * Computes all lattices in the constraint set.
    */
  private def getAllLattices(): Array[Lattice] = {
    val lattices = mutable.Set.empty[Lattice]
    for (stratum <- strata) {
      for (constraint <- stratum.getConstraints()) {
        constraint.getHeadPredicate() match {
          case p: AtomPredicate => p.getSym() match {
            case l: Lattice => lattices += l
            case _ =>
          }
          case _ => // nop
        }
        for (predicate <- constraint.getAtoms()) {
          predicate match {
            case p: AtomPredicate => p.getSym() match {
              case l: Lattice => lattices += l
              case _ =>
            }
            case _ => // nop
          }
        }
      }
    }
    lattices.toArray
  }

}
