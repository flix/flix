package ca.uwaterloo.flix.runtime.solver.api

import java.io.{PrintWriter, StringWriter}

import ca.uwaterloo.flix.runtime.solver.api.predicate._
import ca.uwaterloo.flix.runtime.solver.api.symbol.{RelSym, AnonRelSym}
import ca.uwaterloo.flix.util.AsciiTable

// TODO: Need to standardize on a functional interface for all functions... Perhaps Function[AnyRef, AnyRef]?

/**
  * Represents a collection of constraints.
  */
// TODO: Rename
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
    val strata = groupedByStratum.sortBy(_._1).map(_._2).toArray

    // Ensure that there is always at least one empty stratum.
    if (strata.nonEmpty) {
      strata
    } else {
      val a = Array.ofDim[Constraint](1, 1)
      a(0) = Array.empty[Constraint]
      a
    }
  }

  /**
    * Returns all facts in the constraint set as a new constraint set.
    */
  // TODO: In the future it would be better if constraint sets where immutable.
  def getFacts(): ConstraintSet = {
    val facts = constraints collect {
      case constraint if constraint.isFact() => constraint
    }
    new ConstraintSet(facts)
  }

  /**
    * Returns all relations in the constraint set.
    */
  def getRelations(): Array[RelSym] = getTables() collect {
    case r: RelSym => r
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
        name -> RelSym.getInstance(name, attr)
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
          case r: AnonRelSym => instantiatedRelations(r.getName())
          case l: LatticeVar => instantiatedLattices(l.getName())
          case _ => p.getSym()
        }
        new AtomPredicate(sym, p.isPositive(), p.getTerms(), p.index2sym)
      case _ => p0
    }

    // Replace every relation/lattice variable with its instantiated value.
    val cs = constraints map replaceConstraint

    new ConstraintSet(cs)
  }

  /**
    * Returns all relation variables in the constraint set.
    */
  private def getRelationVars(): Array[AnonRelSym] = getTables() collect {
    case r: AnonRelSym => r
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
    * Returns `true` if all constraints are facts.
    */
  private def isOnlyFacts(): Boolean = constraints.forall(_.isFact())

  /**
    * Returns a human readable representation the constraint set.
    */
  override def toString: String = {
    if (!isOnlyFacts()) {
      return constraints.mkString(", ")
    }

    // TODO: Cleanup.

    val sb = new StringBuilder

    // Group all facts by predicate symbol.
    val constraintsByHead = constraints.groupBy(_.getHeadPredicate())
    for ((predicate, constraints) <- constraintsByHead) {
      predicate match {
        case head: AtomPredicate =>
          // Retrieve the name and attributes of the predicate.
          val name = head.getSym().getName()
          val attributes = head.getTerms().map(_ => "attr")

          // Construct an ascii table with a column for each attribute.
          val columns = attributes
          val table = new AsciiTable().withTitle(name).withCols(columns: _*)

          // Add each row to the ASCII table.
          for (row <- constraints) {
            row.getHeadPredicate() match {
              case atom: AtomPredicate =>
                table.mkRow(atom.getTerms().toList)
              case head: TruePredicate => //  nop
              case head: FalsePredicate => //  nop
              case _ => throw new RuntimeException(s"Unexpected head predicate: '$predicate'.")
            }
          }

          // Write the ASCII table to the string buffer.
          val sw = new StringWriter()
          table.write(new PrintWriter(sw))
          sb.append(sw.toString)

        case head: TruePredicate => //  nop
        case head: FalsePredicate => //  nop
        case _ => throw new RuntimeException(s"Unexpected head predicate: '$predicate'.")
      }
    }

    sb.toString()
  }

}
