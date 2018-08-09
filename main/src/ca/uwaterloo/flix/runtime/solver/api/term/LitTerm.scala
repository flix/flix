package ca.uwaterloo.flix.runtime.solver.api.term

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject

/**
  * Represents a literal returned by the function `f`.
  */
class LitTerm(f: () => ProxyObject) extends Term {

  // TODO: should not take a function?

  /**
    * Returns a unit function that returns the literal value.
    */
  def getFunction(): () => ProxyObject = f

  /**
    * Returns a string representation of `this` term.
    */
  override def toString: String = f().toString

}
