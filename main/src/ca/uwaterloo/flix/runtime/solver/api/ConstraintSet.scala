package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.predicate._

import scala.collection.mutable

/**
  * Represents a collection of constraints.
  */
class ConstraintSet(constraints: Array[Constraint]) {

  /**
    * Returns all the constraints in the constraint set.
    */
  def getConstraints(): Array[Constraint] = constraints

  /**
    * Returns all the constraints in the constraint set by stratum.
    */
  def getConstraintsByStrata: Array[Array[Constraint]] = {
    val groupedByStratum = constraints.groupBy(_.getStratum()).toList
    groupedByStratum.sortBy(_._1).map(_._2).toArray
  }

  /**
    * Returns all relations in the constraint set.
    */
  def getRelations(): Array[Relation] = getTables() collect {
    case r: Relation => r
  }

  /**
    * Returns all the lattices in the constraint set.
    */
  def getLattices(): Array[Lattice] = getTables() collect {
    case l: Lattice => l
  }

  /**
    * Returns the union of `this` constraint set with `that` constraint set.
    */
  def union(that: ConstraintSet): ConstraintSet = {
    new ConstraintSet(this.getConstraints() ++ that.getConstraints())
  }

  /**
    * Returns a new constraint set with all relations/lattices instantiated.
    */
  def complete(): ConstraintSet = {
    // TODO: Cleanup.
    val relationPlaceholders = getRelationVars().groupBy(_.getName())
    val latticePlaceholders = getLatticeVars().groupBy(_.getName())

    // Introduce a proper relation for each relation placeholder.
    val newRelations = relationPlaceholders map {
      case (name, placeholders) => {
        val placeholder = placeholders(0) // guaranteed to be non-empty.
        val attr = placeholder.getAttributes()
        name -> new Relation(name, attr)
      }
    }

    val newLattices = latticePlaceholders map {
      case (name, placeholders) => {
        val placeholder = placeholders(0)
        name -> new Lattice(name, placeholder.getKeys(), placeholder.getValue(), placeholder.getOps())
      }
    }

    def replace(c: Constraint): Constraint = {
      val head = replacePredicate(c.getHeadPredicate())
      val body = c.getBodyPredicates().map(replacePredicate)
      new Constraint(c.getParams(), head, body)
    }

    def replacePredicate(p0: Predicate): Predicate = p0 match {
      case p: AtomPredicate =>
        val sym = p.getSym() match {
          case r: RelationVar => newRelations(r.getName())
          case l: LatticeVar => newLattices(l.getName())
          case _ => p.getSym()
        }
        new AtomPredicate(sym, p.isPositive(), p.getTerms(), p.getIndex2SymTEMPORARY)
      case _ => p0
    }

    val newConstraints = constraints.map(replace)

    new ConstraintSet(newConstraints)
  }

  /**
    * Returns all relation variables in the constraint set.
    */
  private def getRelationVars(): Array[RelationVar] = getTables() collect {
    case r: RelationVar => r
  }

  /**
    * Returns all lattice variables in the constraint set.
    */
  private def getLatticeVars(): Array[LatticeVar] = getTables() collect {
    case l: LatticeVar => l
  }

  /**
    * Returns all tables in the constraint set.
    */
  private def getTables(): Array[Table] =
    getAtomPredicates().map(_.getSym()).distinct

  /**
    * Returns all predicates in the constraint set.
    */
  private def getAtomPredicates(): Array[AtomPredicate] = constraints.flatMap(_.getAllAtoms())

  /**
    * Returns a human readable representation the constraint set.
    */
  override def toString: String = constraints.mkString(", ")

}
