package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

/**
  * Represents a functional predicate with variable `sym`, function `f`, and arguments `terms`.
  */
class FunctionalPredicate(sym: VarSym, f: Array[AnyRef] => Array[ProxyObject], args: Array[VarSym]) extends Predicate {

  /**
    * Returns a copy of this predicate.
    */
  override def copy(): Predicate = new FunctionalPredicate(sym, f, args)

  /**
    * Returns the variable sym.
    */
  def getVarSym(): VarSym = sym

  /**
    * Returns the function.
    */
  def getFunction(): Array[AnyRef] => Array[ProxyObject] = f

  /**
    * Returns the arguments.
    */
  def getArguments(): Array[VarSym] = args

  /**
    * Returns a string representation of `this` predicate.
    */
  override def toString: String = sym.toString + " <- <f>(" + args.mkString(", ") + ")"

}
