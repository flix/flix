package impl.logic

sealed trait Interpretation

object Interpretation {

  case class Proposition(v: Value.Bool) extends Interpretation

  // TODO: Probably move types...

  object Relation {
    case class In1(t1: Type) extends Interpretation
    case class In2(t1: Type, t2: Type) extends Interpretation
    case class In3(t1: Type, t2: Type, t3: Type) extends Interpretation
    case class In4(t1: Type, t2: Type, t3: Type, t4: Type) extends Interpretation
    case class In5(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type) extends Interpretation

    case class Atleast1(size: Int, t1: Type) extends Interpretation
    case class NonSingleton2(t1: Type, t2: Type) extends Interpretation
    // ...
  }

  object Map {
    case class Leq1(t1: Type) extends Interpretation
    case class Leq2(t1: Type, t2: Type) extends Interpretation
    case class Leq3(t1: Type, t2: Type, t3: Type) extends Interpretation
    case class Leq4(t1: Type, t2: Type, t3: Type, t4: Type) extends Interpretation
    case class Leq5(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type) extends Interpretation
  }

  object Functional {
    case class Functional1(t1: Type, clauses: Set[HornClause]) extends Interpretation
    case class Functional2(t1: Type, t2: Type, clauses: Set[HornClause]) extends Interpretation
    case class Functional3(t1: Type, t2: Type, t3: Type, clauses: Set[HornClause]) extends Interpretation
    case class Functional4(t1: Type, t2: Type, t3: Type, t4: Type, clauses: Set[HornClause]) extends Interpretation
    case class Functional5(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, clauses: Set[HornClause]) extends Interpretation
  }

  object Boolean {
    case object Equal extends Interpretation
    case object NotEqual extends Interpretation
  }

  object Integer {
    case object Plus extends Interpretation
    case object Minus extends Interpretation
    case object Times extends Interpretation
    case object Divide extends Interpretation
    case object Modulo extends Interpretation

    case object Less extends Interpretation
    case object LessEqual extends Interpretation
    case object Greater extends Interpretation
    case object GreaterEqual extends Interpretation
    case object Equal extends Interpretation
    case object NotEqual extends Interpretation
  }

  object String {
    case object Equal extends Interpretation
    case object NotEqual extends Interpretation
  }

}