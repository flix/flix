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
    // Find all relation/lattice variables.
    val relationVarsByName = getRelationVars().groupBy(_.getName())
    val latticeVarsByName = getLatticeVars().groupBy(_.getName())

    // Introduce a fresh relation for each name.
    val instantiatedRelations = relationVarsByName map {
      case (name, relationVars) => {
        val relationVar = relationVars(0) // guaranteed to be non-empty.
        val attr = relationVar.getAttributes()
        name -> new Relation(name, attr)
      }
    }

    // Introduce a fresh lattice for each name.
    val instantiatedLattices = latticeVarsByName map {
      case (name, latticeVars) => {
        val latticeVar = latticeVars(0)
        name -> new Lattice(name, latticeVar.getKeys(), latticeVar.getValue(), latticeVar.getOps())
      }
    }

    /**
      * Replaces every occurrence of a relation/lattice variable with its instantiated value.
      */
    def replaceConstraint(c: Constraint): Constraint = {
      val head = replacePredicate(c.getHeadPredicate())
      val body = c.getBodyPredicates().map(replacePredicate)
      new Constraint(c.getParams(), head, body)
    }

    /**
      * Replaces every occurrence of a relation/lattice variable with its instantiated value.
      */
    def replacePredicate(p0: Predicate): Predicate = p0 match {
      case p: AtomPredicate =>
        val sym = p.getSym() match {
          case r: RelationVar => instantiatedRelations(r.getName())
          case l: LatticeVar => instantiatedLattices(l.getName())
          case _ => p.getSym()
        }
        new AtomPredicate(sym, p.isPositive(), p.getTerms(), p.getIndex2SymTEMPORARY)
      case _ => p0
    }

    // Replace every relation/lattice variable with its instantiated value.
    val cs = constraints map replaceConstraint

    new ConstraintSet(cs)
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
