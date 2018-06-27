package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

case class AppHeadTerm(f: Array[AnyRef] => ProxyObject, args: Array[VarSym]) extends HeadTerm
