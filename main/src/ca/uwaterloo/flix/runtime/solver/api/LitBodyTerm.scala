package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

case class LitBodyTerm(f: () => ProxyObject) extends BodyTerm
