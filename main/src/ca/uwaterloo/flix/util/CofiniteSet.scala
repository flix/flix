/*
 * Copyright 2024 Jonathan Lindegaard Starup
 * Copyright 2025 Matthew Lutze
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

package ca.uwaterloo.flix.util

import scala.collection.immutable.SortedSet

/**
  * Represents a finite or co-finite set with an infinite universe.
  *
  * All sets are either a [[SortedSet]] or a complement of it.
  *
  * No finite set is ever equivalent to universe.
  */
sealed trait CofiniteSet[T] {

  import CofiniteSet.{Compl, Set}

  /** Returns `true` if `this` is [[CofiniteSet.empty]]. */
  def isEmpty: Boolean = this match {
    case Set(s) if s.isEmpty => true
    case Set(_) => false
    case Compl(_) => false
  }

  /**
    * Returns `true` if `this` is [[CofiniteSet.universe]].
    *
    * Remember that the universe is infinite, so no finite set is ever equivalent
    * to universe.
    */
  def isUniverse: Boolean = this match {
    case Set(_) => false
    case Compl(s) => s.isEmpty
  }

}

object CofiniteSet {

  /**
    * A trait that indicates the empty and universal values of this type have been cached.
    *
    * We use this for performance:
    * There is one value for `empty` and `universe` per instance of the trait.
    * As long as the same trait is used as evidence to the [[CofiniteSet.empty]] and [[CofiniteSet.universe]] functions,
    * the same object will be returned every time [[CofiniteSet.empty]] or [[CofiniteSet.universe]] is called.
    *
    * This avoids allocation of new objects every time an empty or universal set is created.
    */
  trait SingletonValues[T] {
    val empty: CofiniteSet[T]
    val universe: CofiniteSet[T]
  }

  implicit object IntSingletonValues extends SingletonValues[Int] {
    override val empty: CofiniteSet[Int] = Set(SortedSet.empty)
    override val universe: CofiniteSet[Int] = Compl(SortedSet.empty)
  }

  /** Represents a finite set. */
  case class Set[T](s: SortedSet[T]) extends CofiniteSet[T]

  /** Represents a co-finite set. */
  case class Compl[T](s: SortedSet[T]) extends CofiniteSet[T]

  /** The empty set. */
  def empty[T](implicit ev: SingletonValues[T]): CofiniteSet[T] = ev.empty

  /** The universe set. */
  def universe[T](implicit ev: SingletonValues[T]): CofiniteSet[T] = ev.universe

  /** Returns the wrapped set of `s`. */
  def mkSet[T](s: SortedSet[T]): CofiniteSet[T] = Set(s)

  /** Returns the singleton set of `i`. */
  def mkSet[T](i: T)(implicit ev: Ordering[T]): CofiniteSet[T] = Set(SortedSet(i))

  /** Returns the complement of `s` (`!s`). */
  def complement[T](s: CofiniteSet[T]): CofiniteSet[T] = s match {
    case Set(s) =>
      // !s
      Compl(s)
    case Compl(s) =>
      // !!s
      // = s             (double negation)
      Set(s)
  }

  /** Returns the union of `s1` and `s2` (`s1 ∪ s2`). */
  def union[T](s1: CofiniteSet[T], s2: CofiniteSet[T]): CofiniteSet[T] = (s1, s2) match {
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

  /** Returns the union of `s1` and `s2` (`s1 ∪ s2`). */
  def union[T](s1: CofiniteSet[T], s2: SortedSet[T]): CofiniteSet[T] =
    union(s1, mkSet(s2))

  /** Returns the intersection of `s1` and `s2` (`s1 ∩ s2`). */
  def intersection[T](s1: CofiniteSet[T], s2: CofiniteSet[T]): CofiniteSet[T] = (s1, s2) match {
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

  /** Returns the intersection of `s1` and `s2` (`s1 ∩ s2`). */
  def intersection[T](s1: CofiniteSet[T], s2: SortedSet[T]): CofiniteSet[T] =
    intersection(s1, mkSet(s2))

  /** Returns the difference of `s1` and `s2` (`s1 - s2`). */
  def difference[T](s1: CofiniteSet[T], s2: CofiniteSet[T]): CofiniteSet[T] =
    intersection(s1, complement(s2))

  /** Returns the difference of `s1` and `s2` (`s1 - s2`). */
  def difference[T](s1: CofiniteSet[T], s2: SortedSet[T]): CofiniteSet[T] =
    difference(s1, mkSet(s2))

  /** Returns the symmetric difference of `s1` and `s2`. */
  def xor[T](s1: CofiniteSet[T], s2: CofiniteSet[T]): CofiniteSet[T] =
    union(difference(s1, s2), difference(s2, s1))
}
