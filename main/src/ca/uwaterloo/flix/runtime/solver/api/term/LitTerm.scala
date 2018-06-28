package ca.uwaterloo.flix.runtime.solver.api.term

import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

case class LitTerm(f: () => ProxyObject) extends Term
