package ca.uwaterloo.flix.runtime.solver.data

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}

object Predicate {

  sealed trait Head

  object Head {

    case class True(loc: SourceLocation) extends Head

    case class False(loc: SourceLocation) extends Head

    case class Atom(sym: Symbol.TableSym, terms: List[Term.Head], loc: SourceLocation) extends Head {
      val arity: Int = terms.length
      val termsAsArray: Array[Term.Head] = terms.toArray
    }

  }

}
