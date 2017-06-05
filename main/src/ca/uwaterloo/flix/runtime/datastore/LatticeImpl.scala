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

package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.ExecutableAst
import ca.uwaterloo.flix.runtime.{InvocationTarget, Linker, Value}

import scala.reflect.ClassTag

class LatticeImpl[ValueType <: AnyRef](lattice: ExecutableAst.Table.Lattice, root: ExecutableAst.Root)(implicit m: ClassTag[ValueType]) extends Lattice[ValueType] {

  /**
    * The lattice operations associated with each lattice.
    */
  private val latticeOps: ExecutableAst.Definition.Lattice = root.lattices(lattice.value.tpe)

  /**
    * The bottom element.
    */
  private val Bot: ValueType = Linker.link(latticeOps.bot, root).invoke(Array.empty).asInstanceOf[ValueType]

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
  def bot: ValueType = Bot

  /**
    * Returns `true` if `x` is equal to `y` according to the partial order of the lattice.
    */
  def equal(x: ValueType, y: ValueType): Boolean = x.equals(y)

  /**
    * Returns `true` if `x` is less than or equal to `y` according to the partial order of the lattice.
    */
  def leq(x: ValueType, y: ValueType): Boolean = {
    // if `x` and `y` are the same object then they must be equal.
    if (x eq y) return true

    // evaluate the partial order function passing the arguments `x` and `y`.
    val args = Array(x, y).asInstanceOf[Array[AnyRef]]
    val result = Leq.invoke(args)
    return result.asInstanceOf[java.lang.Boolean].booleanValue()
  }

  /**
    * Returns the least upper bound of `x` and `y` according to the partial order of the lattice.
    */
  def lub(x: ValueType, y: ValueType): ValueType = {
    // if `x` and `y` are the same object then there is no need to compute the lub.
    if (x eq y) return x

    // evaluate the least upper bound function passing the arguments `x` and `y`.
    val args = Array(x, y).asInstanceOf[Array[AnyRef]]
    Lub.invoke(args).asInstanceOf[ValueType]
  }

  /**
    * Returns the greatest lower bound of `x` and `y` according to the partial order of the lattice.
    */
  def glb(x: ValueType, y: ValueType): ValueType = {
    // if `x` and `y` are the same object then there is no need to compute the glb.
    if (x eq y) return x

    // evaluate the greatest lower bound function passing the arguments `x` and `y`.
    val args = Array(x, y).asInstanceOf[Array[AnyRef]]
    Glb.invoke(args).asInstanceOf[ValueType]
  }

}
