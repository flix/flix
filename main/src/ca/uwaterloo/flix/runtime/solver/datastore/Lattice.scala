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

/**
  * Type class for types equipped with lattice operations.
  */
trait Lattice[E] {

  /**
    * Returns the bottom element of the lattice.
    */
  def bot: E

  /**
    * Returns `true` if `x` is equal to `y` according to the partial order of the lattice.
    */
  def equal(x: E, y: E): Boolean

  /**
    * Returns `true` if `x` is less than or equal to `y` according to the partial order of the lattice.
    */
  def leq(x: E, y: E): Boolean

  /**
    * Returns the least upper bound of `x` and `y` according to the partial order of the lattice.
    */
  def lub(x: E, y: E): E

  /**
    * Returns the greatest lower bound of `x` and `y` according to the partial order of the lattice.
    */
  def glb(x: E, y: E): E

}
