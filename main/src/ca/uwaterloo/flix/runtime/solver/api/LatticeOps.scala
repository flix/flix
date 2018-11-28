package ca.uwaterloo.flix.runtime.solver.api

import flix.runtime.ProxyObject

// TODO: Java interface

// TODO: Need to standardize on a functional interface for all functions... Perhaps Function[AnyRef, AnyRef]?

trait LatticeOps {

  def bot: ProxyObject

  def equ: java.util.function.Function[Array[AnyRef], ProxyObject]

  def leq: java.util.function.Function[Array[AnyRef], ProxyObject]

  def lub: java.util.function.Function[Array[AnyRef], ProxyObject]

  def glb: java.util.function.Function[Array[AnyRef], ProxyObject]

}
