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

package ca.uwaterloo.flix.language.ast

import scala.collection.immutable

/**
  * An infinite set is either a finite set of the complement of a finite set.
  */
sealed trait InfSet[T] {

  def isEmpty: Boolean = this match {
    case InfSet.Set(set) => set.isEmpty
    case InfSet.Compl(_) => false
  }

  def hasSize(size: Int): Boolean = this match {
    case InfSet.Set(set) => set.sizeIs == size
    case InfSet.Compl(_) => false
  }

  def contains(x: T): Boolean = this match {
    case InfSet.Set(set) => set.contains(x)
    case InfSet.Compl(set) => !set.contains(x)
  }

}

object InfSet {

  private case class Set[T](set: immutable.Set[T]) extends InfSet[T]

  private case class Compl[T](set: immutable.Set[T]) extends InfSet[T]


  def apply[T](elems: T*): InfSet[T] = Set(elems.toSet)

  def empty[T]: InfSet[T] = Set(immutable.Set.empty)

  def top[T]: InfSet[T] = Compl(immutable.Set.empty)

  def union[T](x: InfSet[T], y: InfSet[T]): InfSet[T] = (x, y) match {
    case (Set(s1), Set(s2)) =>
      Set(s1.union(s2))
    case (Set(s1), Compl(s2)) =>
      // s1 + !s2 == !!(s1 + !s2) == !(!s1 & s2) == !(s2 & !s1) == !(s2 - s1)
      Compl(s2.diff(s1))
    case (Compl(s1), Set(s2)) =>
      // !s1 + s2 == !!(!s1 + s2) == !(s1 & !s2) == !(s1 - s2)
      Compl(s1.diff(s2))
    case (Compl(s1), Compl(s2)) =>
      // !s1 + !s2 == !!(!s1 + !s2) == !(s1 & s2)
      Compl(s1.intersect(s2))
  }

  def intersect[T](x: InfSet[T], y: InfSet[T]): InfSet[T] = (x, y) match {
    case (Set(s1), Set(s2)) =>
      Set(s1.intersect(s2))
    case (Set(s1), Compl(s2)) =>
      // s1 & !s2 == s1 - s2
      Set(s1.diff(s2))
    case (Compl(s1), Set(s2)) =>
      // !s1 & s2 == s2 & !s1 == s2 - s1
      Set(s2.diff(s1))
    case (Compl(s1), Compl(s2)) =>
      // !s1 & !s2 == !!(!s1 & !s2) == !(s1 + s2)
      Compl(s1.union(s2))
  }

  def compl[T](x: InfSet[T]): InfSet[T] = x match {
    case Set(set) => Compl(set)
    case Compl(set) => Set(set)
  }

}
