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

/**
  * Companion object of the [[Bimap]] class.
  */
object Bimap {
  /**
    * Returns the empty Bimap.
    */
  def empty[A, B]: Bimap[A, B] = Bimap(Map.empty, Map.empty)

  /**
    * Constructs a Bimap from the given iterable of key-value pairs.
    *
    * Keys and values must be unique.
    */
  def from[A, B](it: Iterable[(A, B)]): Bimap[A, B] = {
    val forward = it.toMap
    val backward = it.map { case (k, v) => (v, k) }.toMap
    Bimap(forward, backward)
  }
}

/**
  * A bi-directional map (i.e. a one-to-one map) from A to B and B to A.
  */
case class Bimap[A, B](m1: Map[A, B], m2: Map[B, A]) {

  /**
    * Alias for adds the pair `p` to the map.
    */
  def +(p: (A, B)): Bimap[A, B] = Bimap(m1 + (p._1 -> p._2), m2 + (p._2 -> p._1))

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
  def swap: Bimap[B, A] = Bimap(m2, m1)

}
