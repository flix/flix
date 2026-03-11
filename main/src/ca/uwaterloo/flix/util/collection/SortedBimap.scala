/*
 * Copyright 2022 Magnus Madsen
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

package ca.uwaterloo.flix.util.collection

import scala.collection.immutable.SortedMap

/**
  * Companion object of the [[SortedBimap]] class.
  */
object SortedBimap {
  /**
    * Returns the empty SortedBimap.
    */
  def empty[A: Ordering, B: Ordering]: SortedBimap[A, B] = SortedBimap(SortedMap.empty, SortedMap.empty)

  /**
    * Constructs a SortedBimap from the given iterable of key-value pairs.
    *
    * OBS: Keys and values must be unique.
    */
  def from[A: Ordering, B: Ordering](it: Iterable[(A, B)]): SortedBimap[A, B] = {
    val forward = SortedMap.from(it)
    val backward = SortedMap.from(it.map { case (k, v) => (v, k) })
    SortedBimap(forward, backward)
  }
}

/**
  * A bi-directional map (i.e. a one-to-one map) from A to B and B to A.
  */
case class SortedBimap[A, B](m1: SortedMap[A, B], m2: SortedMap[B, A]) {

  /**
    * Alias for adds the pair `p` to the map.
    */
  def +(p: (A, B)): SortedBimap[A, B] = SortedBimap(m1 + (p._1 -> p._2), m2 + (p._2 -> p._1))

  /**
    * Optionally returns the value `a` is mapped to.
    */
  def getForward(a: A): Option[B] = m1.get(a)

  /**
    * Optionally returns the value `b` is mapped to.
    */
  def getBackward(b: B): Option[A] = m2.get(b)

  /**
    * Returns the same map but swapped.
    */
  def swap: SortedBimap[B, A] = SortedBimap(m2, m1)

}
