/*
 * Copyright 2025 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.shared

/**
  * Represents a Boolean algebra over a finite set.
  */
object FiniteSet {

  case class FiniteSet[T](s: Set[T])

  class LatticeOps[T](univ: Set[T]) extends BoolLattice[FiniteSet[T]] {
    override def Bot: FiniteSet[T] = FiniteSet(Set.empty)

    override def Top: FiniteSet[T] = FiniteSet(univ)

    override def isBot(t: FiniteSet[T]): Boolean = t.s.isEmpty

    override def isTop(t: FiniteSet[T]): Boolean = t.s == univ

    override def comp(t: FiniteSet[T]): FiniteSet[T] = FiniteSet(univ -- t.s)

    override def join(t1: FiniteSet[T], t2: FiniteSet[T]): FiniteSet[T] = FiniteSet(t1.s.union(t2.s))

    override def meet(t1: FiniteSet[T], t2: FiniteSet[T]): FiniteSet[T] = FiniteSet(t1.s.intersect(t2.s))
  }

}
