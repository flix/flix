/*
 * Copyright 2024 Magnus Madsen
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

/**
  * Represents an immutable list of at least size two.
  */
case class TwoList[T](x: T, y: T, rest: List[T]) {

  /**
    * Applies the given function `f` to every element of `this` list.
    */
  def map[S](f: T => S): TwoList[S] = TwoList(f(x), f(y), rest.map(f))

  /**
    * Returns `true` if an element in `this` list satisfies the given predicate `f`.
    */
  def exists(f: T => Boolean): Boolean = f(x) || f(y) || rest.exists(f)

  /**
    * Returns `true` if all elements in `this` list satisfies the given predicate `f`.
    */
  def forall(f: T => Boolean): Boolean = f(x) && f(y) && rest.forall(f)

  /**
   * Folds `f` over the elements of `this` list.
   */
  @inline
  def foldRight[S](z: S)(f: (T, S) => S): S = {
    f(x, f(y, rest.foldRight(z)(f)))
  }

  /**
    * Returns an iterator of the elements of `this` list.
    */
  def iterator: Iterator[T] = Iterator(x) ++ Iterator(y) ++ rest.iterator

  /**
    * Returns the length of `this` list.
    */
  def length: Int = 2 + rest.length

  /**
    * Returns `this` list as an ordinary list.
    */
  def toList: List[T] = x :: y :: rest

}
