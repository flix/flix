package impl.logic

sealed trait Interpretation {
  /**
   * Returns `true` iff the interpretation is relational.
   */
  def isRelational: Boolean = this match {
    case Interpretation.Relation.In1 => true
    case Interpretation.Relation.In2 => true
    case Interpretation.Relation.In3 => true
    case Interpretation.Relation.In4 => true
    case Interpretation.Relation.In5 => true
    case Interpretation.Map.Leq1(_) => true
    case Interpretation.Map.Leq2 => true
    case Interpretation.Map.Leq3 => true
    case Interpretation.Map.Leq4 => true
    case Interpretation.Map.Leq5 => true
    case _ => false
  }
}

object Interpretation {

  object Relation {
    case object In1 extends Interpretation
    case object In2 extends Interpretation
    case object In3 extends Interpretation
    case object In4 extends Interpretation
    case object In5 extends Interpretation
  }

  object Map {
    case class Leq1(lattice: Lattice) extends Interpretation
    case object Leq2 extends Interpretation
    case object Leq3 extends Interpretation
    case object Leq4 extends Interpretation
    case object Leq5 extends Interpretation
  }

  object Functional {
    case object Functional1 extends Interpretation
    case object Functional2 extends Interpretation
    case object Functional3 extends Interpretation
    case object Functional4 extends Interpretation
    case object Functional5 extends Interpretation
  }

//  object Boolean {
//    case object Equal extends Interpretation
//    case object NotEqual extends Interpretation
//  }
//
//  object Integer {
//    case object Plus extends Interpretation
//    case object Minus extends Interpretation
//    case object Times extends Interpretation
//    case object Divide extends Interpretation
//    case object Modulo extends Interpretation
//
//    case object Less extends Interpretation
//    case object LessEqual extends Interpretation
//    case object Greater extends Interpretation
//    case object GreaterEqual extends Interpretation
//    case object Equal extends Interpretation
//    case object NotEqual extends Interpretation
//  }
//
//  object String {
//    case object Equal extends Interpretation
//    case object NotEqual extends Interpretation
//  }

}