/*
 * Copyright 2025 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.shared

/**
  * A trait for Boolean lattices.
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
