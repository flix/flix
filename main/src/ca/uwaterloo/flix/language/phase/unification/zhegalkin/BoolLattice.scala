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
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

/**
  * A type class for Boolean lattices.
  */
trait BoolLattice[T] {

  /** The least element. */
  def Empty: T

  /** The greatest element. */
  def Universe: T

  /** Returns `true` if the given element `t` is the least element. */
  def isEmpty(t: T): Boolean

  /** Returns `true` if the given element `t` is the greatest element. */
  def isUniverse(t: T): Boolean

  /** Returns the complement of the given element `t`. */
  def complement(t: T): T

  /** Returns the union of the two given elements `t1` and `t2`. */
  def union(t1: T, t2: T): T

  /** Returns the intersection of the two given elements `t1` and `t2`. */
  def intersection(t1: T, t2: T): T

}

object BoolLattice {

  /** An instance for [[CofiniteIntSet]]. */
  object CofiniteIntSetWitnesss extends BoolLattice[CofiniteIntSet] {
    override def Empty: CofiniteIntSet = CofiniteIntSet.empty

    override def Universe: CofiniteIntSet = CofiniteIntSet.universe

    override def isEmpty(t: CofiniteIntSet): Boolean = t.isEmpty

    override def isUniverse(t: CofiniteIntSet): Boolean = t.isUniverse

    override def complement(t: CofiniteIntSet): CofiniteIntSet = CofiniteIntSet.complement(t)

    override def union(t1: CofiniteIntSet, t2: CofiniteIntSet): CofiniteIntSet = CofiniteIntSet.union(t1, t2)

    override def intersection(t1: CofiniteIntSet, t2: CofiniteIntSet): CofiniteIntSet = CofiniteIntSet.intersection(t1, t2)
  }

}
