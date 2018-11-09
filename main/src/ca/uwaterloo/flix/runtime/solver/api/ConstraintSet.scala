package ca.uwaterloo.flix.runtime.solver.api

import java.io.{PrintWriter, StringWriter}

import ca.uwaterloo.flix.runtime.solver.api.predicate._
import ca.uwaterloo.flix.runtime.solver.api.symbol._
import ca.uwaterloo.flix.util.AsciiTable

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
  def getLattices(): Array[LatSym] = getTables() collect {
    case l: LatSym => l
  }

  /**
    * Returns the union of `this` constraint set with `that` constraint set.
    */
  def union(that: ConstraintSet): ConstraintSet = {
    new ConstraintSet(this.getConstraints() ++ that.getConstraints())
  }

  /**
    * Returns all tables in the constraint set.
    */
  private def getTables(): Array[PredSym] =
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
