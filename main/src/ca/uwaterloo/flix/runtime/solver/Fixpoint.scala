package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.runtime.solver.api.{Relation, Lattice}

/**
  * Represents the fixpoint solution to a constraint set.
  *
  * @param relations the relation values in the solution.
  * @param lattices  the lattice values in the solution.
  */
class Fixpoint(relations: Array[Relation], lattices: Array[Lattice]) {

  /**
    * Returns a human readable string representation of the relations and lattices in the solution.
    */
  override def toString: String = {
    val sb = new StringBuilder

    // Construct an array with both relations and lattices.
    val tables = (relations ++ lattices) map {
      case r: Relation => r.getName() -> r
      case l: Lattice => l.getName() -> l
    }

    // Append the tables in alphabetic order.
    for ((name, table) <- tables.sortBy(_._1)) {
      sb.append(table.toString)
      sb.append("\n")
    }

    sb.toString()
  }

}
