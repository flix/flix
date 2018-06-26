package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

object Term {

  sealed trait Head

  object Head {

    case class Var(sym: VarSym) extends Term.Head

    case class Lit(f: () => ProxyObject) extends Term.Head

    case class Cst(f: () => ProxyObject) extends Term.Head

    case class App(f: Array[AnyRef] => ProxyObject, args: Array[VarSym]) extends Term.Head

  }

  sealed trait Body

  object Body {

    case class Wild() extends Term.Body

    case class Var(sym: VarSym) extends Term.Body

    case class Lit(lit: () => ProxyObject) extends Term.Body

    case class Cst(f: () => ProxyObject) extends Term.Body

  }

}