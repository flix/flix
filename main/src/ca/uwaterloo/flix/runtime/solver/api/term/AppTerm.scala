package ca.uwaterloo.flix.runtime.solver.api.term

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

/**
  * Represents a function application term with function `f` and arguments `args`.
  */
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
