package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

case class LitHeadTerm(f: () => ProxyObject) extends HeadTerm
