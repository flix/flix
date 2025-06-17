/*
 * Copyright 2025 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification.shared

/**
  * A type class for Boolean lattices.
  */
trait BoolLattice[T] {

  /** The least element. */
  def Bot: T

  /** The greatest element. */
  def Top: T

  /** Returns `true` if the given element `t` is the least element. */
  def isBot(t: T): Boolean

  /** Returns `true` if the given element `t` is the greatest element. */
  def isTop(t: T): Boolean

  /** Returns the complement of the given element `t`. */
  def comp(t: T): T

  /** Returns the least upper bound of the two given elements `t1` and `t2`. */
  def join(t1: T, t2: T): T

  /** Returns the greatest lower bound of the two given elements `t1` and `t2`. */
  def meet(t1: T, t2: T): T

}
