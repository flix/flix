/*
 * Copyright 2022 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
    * OBS: Keys and values must be unique.
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
