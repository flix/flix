package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.runtime.solver.api.{Relation, Lattice}

class Fixpoint(relations: Array[Relation], lattices: Array[Lattice]) {

  override def toString: String = {
    // TODO
    val r = relations.mkString(", ")
    val l = lattices.mkString(", ")

    "Relations: " + r + "\n\nLattices: " + l
  }

}
