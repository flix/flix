/*
 *  Copyright 2017 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.runtime.solver.datastore

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst
import ca.uwaterloo.flix.runtime.{InvocationTarget, Linker}

class LatticeImpl(lattice: ExecutableAst.Table.Lattice, root: ExecutableAst.Root)(implicit flix: Flix) extends Lattice[ProxyObject] {

  /**
    * The lattice operations associated with each lattice.
    */
  private val latticeOps: ExecutableAst.Lattice = root.lattices(lattice.value.tpe)

  /**
    * The bottom element.
    */
  private val Bot: ProxyObject = Linker.link(latticeOps.bot, root).invoke(Array.empty)

  /**
    * The equality operator.
    */
  private val Equ: InvocationTarget = Linker.link(latticeOps.equ, root)

  /**
    * The partial order operator.
    */
  private val Leq: InvocationTarget = Linker.link(latticeOps.leq, root)

  /**
    * The least upper bound operator.
    */
  private val Lub: InvocationTarget = Linker.link(latticeOps.lub, root)

  /**
    * The greatest lower bound operator.
    */
  private val Glb: InvocationTarget = Linker.link(latticeOps.glb, root)

  /**
    * Returns the bottom element of the lattice.
    */
  def bot: ProxyObject = Bot

  /**
    * Returns `true` if `x` is equal to `y` according to the partial order of the lattice.
    */
  def equal(e1: ProxyObject, e2: ProxyObject): Boolean = {
    // Unwrap the proxy objects.
    val x = e1.getValue
    val y = e2.getValue

    // if `x` and `y` are the same object then they must be equal.
    if (x eq y) return true

    // evaluate the equality function passing the arguments `x` and `y`.
    val args = Array(x, y)
    val result = Equ.invoke(args).getValue
    return result.asInstanceOf[java.lang.Boolean].booleanValue()
  }

  /**
    * Returns `true` if `x` is less than or equal to `y` according to the partial order of the lattice.
    */
  def leq(e1: ProxyObject, e2: ProxyObject): Boolean = {
    // Unwrap the proxy objects.
    val x = e1.getValue
    val y = e2.getValue

    // if `x` and `y` are the same object then they must be equal.
    if (x eq y) return true

    // evaluate the partial order function passing the arguments `x` and `y`.
    val args = Array(x, y)
    val result = Leq.invoke(args).getValue
    return result.asInstanceOf[java.lang.Boolean].booleanValue()
  }

  /**
    * Returns the least upper bound of `x` and `y` according to the partial order of the lattice.
    */
  def lub(e1: ProxyObject, e2: ProxyObject): ProxyObject = {
    // Unwrap the proxy objects.
    val x = e1.getValue
    val y = e2.getValue

    // if `x` and `y` are the same object then there is no need to compute the lub.
    if (x eq y) return e1

    // evaluate the least upper bound function passing the arguments `x` and `y`.
    val args = Array(x, y)
    Lub.invoke(args)
  }

  /**
    * Returns the greatest lower bound of `x` and `y` according to the partial order of the lattice.
    */
  def glb(e1: ProxyObject, e2: ProxyObject): ProxyObject = {
    // Unwrap the proxy objects.
    val x = e1.getValue
    val y = e2.getValue

    // if `x` and `y` are the same object then there is no need to compute the glb.
    if (x eq y) return e1

    // evaluate the greatest lower bound function passing the arguments `x` and `y`.
    val args = Array(x, y)
    Glb.invoke(args)
  }

}
