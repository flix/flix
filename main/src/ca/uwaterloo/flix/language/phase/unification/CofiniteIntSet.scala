/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification

import scala.collection.immutable.SortedSet

/**
  * Represents a finite or co-finite set with an infinite universe of integers.
  *
  * All sets are either a [[SortedSet]] or a complement of it.
  *
  * No finite set is ever equivalent to universe.
  */
sealed trait CofiniteIntSet {

  import CofiniteIntSet.{Compl, Set}

  /** Returns `true` if `this` is [[CofiniteIntSet.universe]]. */
  def isUniverse: Boolean = this match {
    case Compl(s) if s.isEmpty => true
    case Set(_) => false
    case Compl(_) => false
  }

  /** Returns `true` if `this` is [[CofiniteIntSet.empty]]. */
  def isEmpty: Boolean = this match {
    case Set(s) if s.isEmpty => true
    case Set(_) => false
    case Compl(_) => false
  }

}

object CofiniteIntSet {

  /** Represents a finite set of integers. */
  private case class Set(s: SortedSet[Int]) extends CofiniteIntSet

  /** Represents a co-finite set of integers. */
  private case class Compl(s: SortedSet[Int]) extends CofiniteIntSet

  /** The empty set. */
  val empty: CofiniteIntSet = Set(SortedSet.empty)

  /** The universe set. */
  val universe: CofiniteIntSet = Compl(SortedSet.empty)

  /** Returns the wrapped set of `s`. */
  def mkSet(s: SortedSet[Int]): CofiniteIntSet = Set(s)

  /** Returns the singleton set of `i`. */
  def mkSet(i: Int): CofiniteIntSet = Set(SortedSet(i))

  /** Returns the union of `s1` and `s2` (`s1 ∪ s2`). */
  def union(s1: CofiniteIntSet, s2: CofiniteIntSet): CofiniteIntSet = (s1, s2) match {
    case (Set(x), Set(y)) =>
      // x ∪ y
      Set(x.union(y))
    case (Set(x), Compl(y)) =>
      // x ∪ !y
      // = !!(x ∪ !y)    (double complement)
      // = !(!x ∩ y)     (complement distribution)
      // = !(y ∩ !x)     (intersection symmetry)
      // = !(y - x)      (difference definition)
      Compl(y.diff(x))
    case (Compl(x), Set(y)) =>
      // !x ∪ y
      // = !!(!x ∪ y)    (double complement)
      // = !(x ∩ !y)     (complement distribution)
      // = !(x - y)      (difference definition)
      Compl(x.diff(y))
    case (Compl(x), Compl(y)) =>
      // !x ∪ !y
      // = !!(!x ∪ !y)   (double complement)
      // = !(!!x ∩ !!y)  (complement distribution)
      // = !(x ∩ y)      (double complement)
      Compl(x.intersect(y))
  }

  /** Returns the intersection of `s1` and `s2` (`s1 ∩ s2`). */
  def intersection(s1: CofiniteIntSet, s2: CofiniteIntSet): CofiniteIntSet = (s1, s2) match {
    case (Set(x), Set(y)) =>
      // x ∩ y
      Set(x.intersect(y))
    case (Set(x), Compl(y)) =>
      // x ∩ !y
      // = x - y         (difference definition)
      Set(x.diff(y))
    case (Compl(x), Set(y)) =>
      // !x ∩ y
      // = y ∩ !x        (intersection symmetry)
      // = y - x         (difference definition)
      Set(y.diff(x))
    case (Compl(x), Compl(y)) =>
      // !x ∩ !y
      // = !!(!x ∩ !y)   (double complement)
      // = !(x ∪ y)      (complement distribution)
      Compl(x.union(y))
  }

  /** Returns the complement of `s` (`!s`). */
  def complement(s: CofiniteIntSet): CofiniteIntSet = s match {
    case Set(s) =>
      // !s
      Compl(s)
    case Compl(s) =>
      // !!s
      // = s             (double negation)
      Set(s)
  }

  /**
    * Instantiates the elements of `s` according to `m`.
    *
    * If an element is not mapped in `m`, then it will remain in the set.
    */
  def mapElements(s: CofiniteIntSet, m: Map[Int, CofiniteIntSet]): CofiniteIntSet = s match {
    case Set(s) =>
      s.foldLeft(empty) { case (acc, elm) => union(acc, m.getOrElse(elm, mkSet(elm))) }
    case Compl(s) =>
      complement(mapElements(Set(s), m))
  }
}
