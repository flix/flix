package ca.uwaterloo.flix.runtime.solver.api.term

import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

class LitTerm(f: () => ProxyObject) extends Term {

  /**
    * Returns a unit function that returns the literal value.
    */
  def getFunction(): () => ProxyObject = f

}
