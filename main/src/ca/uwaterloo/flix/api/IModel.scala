/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.api

/**
  * Public interface for Flix models.
  */
trait IModel {

  /**
    * Returns the constant with the given fully qualified `name`.
    *
    * @throws IllegalArgumentException if no such constant exists.
    */
  def getConstant(name: String): IValue

  /**
    * Returns an iterable over the relation with the given fully qualified `name`.
    *
    * @throws IllegalArgumentException if no such relation exists.
    */
  def getRelation(name: String): java.lang.Iterable[Array[IValue]]

  /**
    * Returns an iterable over the lattice with the given fully qualified `name`.
    *
    * @throws IllegalArgumentException if no such lattice exists.
    */
  def getLattice(name: String): java.lang.Iterable[Array[IValue]]

}
