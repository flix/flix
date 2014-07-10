package impl.logic

sealed trait Interpretation

object Interpretation {
  case class Relation(repr: Representation) extends Interpretation
  case class Function(repr: Representation) extends Interpretation
  case class LatticeMap(lattice: Lattice) extends Interpretation

  case object Leq extends Interpretation
  case object Join extends Interpretation

  // TODO: Atleast and Atmost
}

sealed trait Representation

object Representation {
  case object Code extends Representation
  case object Data extends Representation
}
