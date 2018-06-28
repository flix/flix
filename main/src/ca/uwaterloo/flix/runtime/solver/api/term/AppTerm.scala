package ca.uwaterloo.flix.runtime.solver.api.term

import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym
import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

class AppTerm(f: Array[AnyRef] => ProxyObject, args: Array[VarSym]) extends Term {

  /**
    * Returns the function object.
    */
  def getFunction(): Array[AnyRef] => ProxyObject = f

  /**
    * Returns the function arguments.
    */
  def getArguments(): Array[VarSym] = args

}
