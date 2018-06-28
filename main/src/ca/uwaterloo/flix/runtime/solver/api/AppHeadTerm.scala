package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

class AppHeadTerm(f: Array[AnyRef] => ProxyObject, args: Array[VarSym]) extends Term {

  /**
    * Returns the function.
    */
  def getFunction(): Array[AnyRef] => ProxyObject = f

  /**
    * Returns the function arguments.
    */
  def getArguments(): Array[VarSym] = args

}
