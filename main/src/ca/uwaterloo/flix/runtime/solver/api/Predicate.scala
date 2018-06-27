package ca.uwaterloo.flix.runtime.solver.api

object Predicate {

  sealed trait Body

  object Body {

    case class Atom(sym: TableSym, polarity: Polarity, terms: Array[Term.Body], index2sym: Array[VarSym]) extends Predicate.Body {
      val arity: Int = terms.length
    }

    case class Filter(f: Array[AnyRef] => Boolean, terms: Array[Term.Body]) extends Predicate.Body

  }

}
