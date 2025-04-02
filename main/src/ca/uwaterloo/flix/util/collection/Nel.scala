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

/**
  * A non-empty list - always has at least one element.
  *
  * @param x the first element
  * @param xs the remaining elements
  * @tparam T the type of the elements
  */
case class Nel[T](x: T, xs: List[T]) extends Iterable[T] {

  /** Returns the number of elements in `this` (always at least 1). */
  def length: Int = 1 + xs.length

  /** Builds a new [[Nel]] by applying `f` to all elements of `this`. */
  override def map[S](f: T => S): Nel[S] = Nel(f(x), xs.map(f))

  /** Returns a string representation of `this`. */
  override def toString: String = s"Nel(${this.toList.mkString(", ")})"

  /** Returns an iterator of the elements of `this`. */
  override def iterator: Iterator[T] = Iterator(x) ++ xs.iterator

  /** Returns `this` as a [[List]]. */
  override def toList: List[T] = x :: xs

}
