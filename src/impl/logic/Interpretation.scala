package impl.logic

sealed trait Interpretation

object Interpretation {
  case object Relation extends Interpretation
  case class LatticeMap(lattice: Lattice) extends Interpretation

// TODO: Why are these not used by the solver?
  case object Leq extends Interpretation
  case object Join extends Interpretation

  // TODO: Atleast and Atmost
}
