package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.InvocationTarget

// TODO: Java interface

// TODO: Need to standardize on a functional interface for all functions... Perhaps Function[AnyRef, AnyRef]?

trait LatticeOps {

  def bot: ProxyObject

  def equ: InvocationTarget

  def leq: InvocationTarget

  def lub: InvocationTarget

  def glb: InvocationTarget

}
