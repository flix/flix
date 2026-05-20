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
