/*
 *  Copyright 2017 Ramin Zarifi
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.runtime.Benchmark

// Implementation of Flix List in Scala
object FlixListInScala {
  // This trait represents `List` enum in Flix.
  sealed trait FlixList[+T]

  // This object represents `Nil` case in `List`
  case object FlixNil extends FlixList[Nothing]

  // This case class represents `Cons` case in `List`
  case class FlixCons[+T](field: (T, FlixList[T])) extends FlixList[T]

  ///
  /// Returns the length of `xs`.
  ///
  def length[T](xs: FlixList[T]): Int = xs match {
    case FlixNil => 0
    case FlixCons((x , rs)) => 1 + length(rs)
  }

  ///
  /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
  /// Returns `Nil` if `b >= e`.
  ///
  def range(b: Int, e: Int): FlixList[Int] = if (b >= e) FlixNil else FlixCons((b , range(b + 1, e)))

  ///
  /// Returns the first `n` elements of `xs`.
  /// Returns `xs` if `n > length(xs)`.
  /// Returns `Nil` if `n < 0`.
  ///
  def take[a](n: Int, xs: FlixList[a]): FlixList[a] = if (n < 0) FlixNil else (n, xs) match {
    case (0, _) => FlixNil
    case (i, FlixCons((x, rs))) => FlixCons(x, take(i-1, rs))
  }

  ///
  /// Returns `xs` without the first `n` elements.
  /// Returns `Nil` if `n > length(xs)`.
  /// Returns `xs` if `n < 0`.
  ///
  def drop[a](n: Int, xs: FlixList[a]): FlixList[a] = if (n < 0) xs else (n, xs) match {
    case (_, FlixNil) => FlixNil
    case (0, _) => xs
    case (i, FlixCons((x, rs))) => drop(i-1, rs)
  }

  ///
  /// Helper function for `reverse`.
  ///
  private def reverseHelper[a](xs: FlixList[a], acc: FlixList[a]): FlixList[a] = xs match {
    case FlixNil => acc
    case FlixCons((x, rs)) => reverseHelper(rs, FlixCons(x, acc))
  }

  ///
  /// Returns the reverse of `xs`.
  ///
  def reverse[a](xs: FlixList[a]): FlixList[a] = reverseHelper(xs, FlixNil)

  ///
  /// Returns a list of every element in `xs` that satisfies the predicate `f`.
  ///
  def filter[a](f: a => Boolean, xs: FlixList[a]): FlixList[a] = xs match {
    case FlixNil => FlixNil
    case FlixCons((x, rs)) =>
      val r = filter(f, rs)
      if (f(x)) FlixCons(x, r) else r
  }

  ///
  /// Returns `ys` appended to `xs`.
  /// The infix operator `:::` is an alias for `append` (`xs ::: ys = append(xs, ys)`).
  ///
  def append[a](xs: FlixList[a], ys: FlixList[a]): FlixList[a] = xs match {
    case FlixNil => ys
    case FlixCons((x, rs)) => FlixCons(x, append(rs, ys))
  }

  ///
  /// Returns the result of applying `f` to every element in `xs`.
  /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
  ///
  def map[a,b](f: a => b, xs: FlixList[a]): FlixList[b] = xs match {
    case FlixNil => FlixNil
    case FlixCons((x, rs)) => FlixCons(f(x), map(f, rs))
  }

  ///
  /// Returns the result of applying `f` to every element in `xs` and concatenating the results.
  ///
  def flatMap[a,b](f: a => FlixList[b], xs: FlixList[a]): FlixList[b] = xs match {
    case FlixNil => FlixNil
    case FlixCons((x, rs)) => append(f(x), flatMap(f, rs))
  }

  ///
  /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
  /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
  ///
  def foldLeft[a,b](f: (b, a) => b, s: b, xs: FlixList[a]): b = xs match {
    case FlixNil => s
    case FlixCons((x, rs)) => foldLeft(f, f(s, x), rs)
  }

  ///
  /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
  /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
  ///
  def foldRight[a,b](f: (a, b) => b, s: b, xs: FlixList[a]): b = xs match {
    case FlixNil => s
    case FlixCons((x, rs)) => f(x, foldRight(f, s, rs))
  }

  ///
  /// Returns a list where the element at index `i` is `(a, b)` where
  /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
  /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
  ///
  def zip[a,b](xs: FlixList[a], ys: FlixList[b]): FlixList[(a,b)] = (xs, ys) match {
    case (FlixCons((x, rs)), FlixCons((y, qs))) => FlixCons((x, y), zip(rs, qs))
    case _ => FlixNil
  }

  ///
  /// Returns a pair of lists, the first containing all first components in `xs`
  /// and the second containing all second components in `xs`.
  ///
  def unzip[a,b](xs: FlixList[(a,b)]): (FlixList[a], FlixList[b]) = xs match {
    case FlixNil => (FlixNil, FlixNil)
    case FlixCons(((x1, x2), rs)) =>
      val (r1, r2) = unzip(rs)
      (FlixCons[a](x1, r1), FlixCons[b](x2, r2))
  }

  ///
  /// Returns true if and only if `xs` contains the element `a`.
  ///
  def memberOf[a](a: a, xs: FlixList[a]): Boolean = xs match {
    case FlixNil => false
    case FlixCons((x, rs)) => if (x == a) true else memberOf(a, rs)
  }

  ///
  /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
  /// Returns `false` if `xs` is empty.
  ///
  def exists[a](f: a => Boolean, xs: FlixList[a]): Boolean = xs match {
    case FlixNil => false
    case FlixCons((x, rs)) => if (f(x)) true else exists(f, rs)
  }

  ///
  /// Returns `xs` with `a` inserted between every two adjacent elements.
  ///
  def intersperse[a](a: a, xs: FlixList[a]): FlixList[a] = xs match {
    case FlixCons((x1,  FlixCons((x2, rs)))) => FlixCons(x1, FlixCons(a, intersperse(a, FlixCons(x2, rs))))
    case _ => xs
  }

  ///
  /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
  ///
  def findLeft[a](f: a => Boolean, xs: FlixList[a]): Option[a] = xs match {
    case FlixNil => None
    case FlixCons((x, rs)) => if (f(x)) Some(x) else findLeft(f, rs)
  }

  ///
  /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
  ///
  def findRight[a](f: a => Boolean, xs: FlixList[a]): Option[a] = xs match {
    case FlixNil => None
    case FlixCons((x, rs)) => findRight(f, rs).orElse{
      if(f(x)) Some(x) else None
    }
  }

  ///
  /// Returns a list with the element `a` repeated `n` times.
  /// Returns `Nil` if `n < 0`.
  ///
  def repeat[a](a: a, n: Int): FlixList[a] = if (n <= 0) FlixNil else FlixCons(a, repeat(a, n - 1))
}