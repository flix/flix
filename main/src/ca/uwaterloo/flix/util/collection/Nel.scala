/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * A non-empty list (Nel) - always has at least one element.
  *
  * @param x the first element
  * @param xs the remaining elements
  * @tparam T the type of the elements
  */
case class Nel[T](x: T, xs: List[T]) extends Iterable[T] {

  /** Returns the number of elements in `this` (always at least 1). */
  def length: Int = 1 + xs.length

  /** Returns the first element of `this`. */
  override def head: T = x

  /** Returns all elements of `this` except the first. */
  override def tail: List[T] = xs

  /** Returns all elements of `this` except the last. */
  override def init: List[T] = if (xs.isEmpty) Nil else x :: xs.init

  /** Returns the last element of `this`. */
  override def last: T = if (xs.isEmpty) x else xs.last

  /** Builds a new [[Nel]] by applying `f` to all elements of `this`. */
  override def map[S](f: T => S): Nel[S] = Nel(f(x), xs.map(f))

  /** Builds a new [[List]] by applying `f` to all elements of `this` and concatenating the results. */
  override def flatMap[S](f: T => IterableOnce[S]): List[S] = toList.flatMap(f)

  /** Builds a new [[List]] by applying `pf` to all elements of `this` on which it is defined. */
  override def collect[S](pf: PartialFunction[T, S]): List[S] = toList.collect(pf)

  /** Returns a [[Nel]] of pairs of the elements of `this` and their indices. */
  override def zipWithIndex: Nel[(T, Int)] = Nel((x, 0), xs.zipWithIndex.map { case (y, i) => (y, i + 1) })

  /**
    * Returns a [[Nel]] of pairs of corresponding elements of `this` and `that`.
    *
    * Throws an [[InternalCompilerException]] if `this` and `that` have different lengths.
    */
  def zip[S](that: Nel[S]): Nel[(T, S)] = Nel((x, that.x), ListOps.zip(xs, that.xs))

  /** Returns two lists from a list of tuples. */
  override def unzip[A1, A2](implicit asPair: T => (A1, A2)): (Nel[A1], Nel[A2]) = {
    val (a, b) = asPair(x)
    val (as, bs) = xs.unzip
    (Nel(a, as), Nel(b, bs))
  }

  /** Returns a string representation of `this`. */
  override def toString: String = s"Nel(${this.toList.mkString(", ")})"

  /** Returns an iterator of the elements of `this`. */
  override def iterator: Iterator[T] = Iterator(x) ++ xs.iterator

  /** Returns `this` as a [[List]]. */
  override def toList: List[T] = x :: xs

}

object Nel {

  /**
    * Returns `l` as a [[Nel]].
    *
    * Throws an [[InternalCompilerException]] if `l` is empty.
    */
  def unsafeFrom[T](l: List[T]): Nel[T] = l match {
    case Nil => throw InternalCompilerException("Unexpected empty list", SourceLocation.Unknown)
    case x :: xs => Nel(x, xs)
  }

  /**
    * Returns a [[Nel]] containing the given elements.
    */
  def of[T](x: T, xs: T*): Nel[T] = Nel(x, xs.toList)

}
