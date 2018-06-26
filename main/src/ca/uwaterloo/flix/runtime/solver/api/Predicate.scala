package ca.uwaterloo.flix.runtime.solver.api

object Predicate {

  sealed trait Head

  object Head {

    case class True() extends Head

    case class False() extends Head

    case class Atom(sym: TableSym, terms: List[Term.Head]) extends Head {
      val arity: Int = terms.length
      val termsAsArray: Array[Term.Head] = terms.toArray
    }

  }

  sealed trait Body

  object Body {

    case class Atom(sym: TableSym, polarity: Polarity, terms: Array[Term.Body], index2sym: Array[VarSym]) extends Predicate.Body {
      val arity: Int = terms.length
    }

    case class Filter(f: Array[AnyRef] => Boolean, terms: Array[Term.Body]) extends Predicate.Body

  }

}
