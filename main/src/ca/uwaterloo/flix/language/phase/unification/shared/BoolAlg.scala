/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.shared

import scala.collection.immutable.SortedSet

/**
 * A trait for Free Boolean Algebras.
 */
trait BoolAlg[F] {

  /**
   * Returns `true` if the given formula `f` is equivalent to the bottom element.
   */
  def isEquivBot(f: F): Boolean

  /**
    * Returns a representation of bottom (i.e., false, the empty set, ...)
    */
  def mkBot: F

  /**
   * Returns a representation of top (i.e., true, the universe, ...)
   */
  def mkTop: F

  /**
    * Returns a representation of the rigid variable with the given `id`.
    */
  def mkCst(id: Int): F

  /**
   * Returns a representation of the flexible variable with the given `id`.
   */
  def mkVar(id: Int): F

  /**
   * Returns a representation of the complement of `f`.
   */
  def mkNot(f: F): F

  /**
   * Returns a representation of the disjunction of `f1` and `f2`.
   */
  def mkOr(f1: F, f2: F): F

  /**
   * Returns a representation of the conjunction of `f1` and `f2`.
   */
  def mkAnd(f1: F, f2: F): F

  /**
   * Returns a representation of the formula `f1 xor f2`.
   */
  def mkXor(f1: F, f2: F): F = mkOr(mkAnd(f1, mkNot(f2)), mkAnd(mkNot(f1), f2))

  /**
   * Returns the set of free variables in `f`.
   */
  def freeVars(f: F): SortedSet[Int]

  /**
   * Applies the function `fn` to every variable in `f`.
   */
  def map(f: F)(fn: Int => F): F

  /**
   * Returns the result of running the given `sve` algorithm on the given Zhegalkin expression `q`.
   *
   * Performs a lookup in the cache or computes the result.
   */
  def lookupOrComputeSVE(q: F, sve: F => BoolSubstitution[F]): BoolSubstitution[F]

}
