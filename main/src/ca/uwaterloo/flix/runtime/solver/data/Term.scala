package ca.uwaterloo.flix.runtime.solver.data

import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

object Term {

  sealed trait Head

  object Head {

    case class Var(sym: VarSym) extends Term.Head

    case class Lit(f: () => ProxyObject) extends Term.Head

    case class Cst(f: () => AnyRef) extends Term.Head

    case class App(f: AnyRef => AnyRef, args: Array[VarSym]) extends Term.Head

  }

  sealed trait Body

  object Body {

    case class Wild() extends Term.Body

    case class Var(sym: VarSym) extends Term.Body

    case class Lit(lit: () => ProxyObject) extends Term.Body

    case class Cst(f: () => AnyRef) extends Term.Body

  }

}