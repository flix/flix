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

import ca.uwaterloo.flix.runtime.solver.api.{LatticeOps, ProxyObject}

class LatticeImpl(ops: LatticeOps) extends Lattice[ProxyObject] {

  /**
    * Returns the bottom element of the lattice.
    */
  def bot: ProxyObject = ops.bot

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
    val result = ops.equ.invoke(args).getValue
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
    val result = ops.leq.invoke(args).getValue
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
    ops.lub.invoke(args)
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
    ops.glb.invoke(args)
  }

}
