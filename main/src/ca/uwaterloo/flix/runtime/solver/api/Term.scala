package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

object Term {

  sealed trait Body

  object Body {

    case class Wild() extends Term.Body

    case class Var(sym: VarSym) extends Term.Body

    case class Lit(lit: () => ProxyObject) extends Term.Body

  }

}