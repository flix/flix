package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.runtime.InvocationTarget
import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

trait LatticeOps {

  def bot: ProxyObject

  def equ: InvocationTarget

  def leq: InvocationTarget

  def lub: InvocationTarget

  def glb: InvocationTarget

}
