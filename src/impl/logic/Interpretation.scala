package impl.logic

sealed trait Interpretation

object Interpretation {
  case object Relation extends Interpretation
  case class LatticeMap(lattice: Lattice) extends Interpretation

  // TODO: Atleast and Atmost
  case object Leq extends Interpretation
  case object Join extends Interpretation
}
