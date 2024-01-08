/*
 *  Copyright 2019 Magnus Madsen
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

package ca.uwaterloo.flix.util

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
  * A result either holds a value ([[Result.Ok]]) or holds an error ([[Result.Err]]).
  *
  * @tparam T the type of the value.
  * @tparam E the type of the error.
  */
sealed trait Result[+T, +E] {

  /**
    * Retrieves the value from `this` result.
    *
    * @throws IllegalStateException if `this` result does not hold a value.
    */
  final def get: T = this match {
    case Result.Ok(t) => t
    case Result.Err(e) => throw new IllegalStateException(s"Result is Err($e).")
  }

  /**
    * Applies the given function `f` to value of `this` wrapping it in [[Result.Ok]].
    */
  final def map[U](f: T => U): Result[U, E] = this match {
    case Result.Ok(t) => Result.Ok(f(t))
    case Result.Err(e) => Result.Err(e)
  }

  /**
    * If `this` is a [[Result.Err]] the given function `f` is applied to the contained error.
    */
  final def mapErr[F](f: E => F): Result[T, F] = this match {
    case Result.Ok(t) => Result.Ok(t)
    case Result.Err(e) => Result.Err(f(e))
  }

  /**
    * Applies the given function `f` to the value of `this`.
    */
  final def flatMap[R >: E, B](f: T => Result[B, R]): Result[B, R] = this match {
    case Result.Ok(t) => f(t)
    case Result.Err(e) => Result.Err(e)
  }

  /**
    * Returns `this` result as a [[Validation]].
    */
  final def toValidation: Validation[T, E] = this match {
    case Result.Ok(t) => Validation.Success(t)
    case Result.Err(e) => Validation.HardFailure(LazyList(e))
  }

  /**
    * Returns `this` result as an [[Option]].
    */
  final def toOption: Option[T] = this match {
    case Result.Ok(t) => Some(t)
    case Result.Err(_) => None
  }

  /**
    * Required for pattern-matching in for-patterns.
    * Doesn't actually filter anything.
    */
  final def withFilter(f: T => Boolean): Result[T, E] = this
}

object Result {

  /**
    * A result that holds a value.
    */
  case class Ok[T, E](t: T) extends Result[T, E]

  /**
    * A result that holds an error.
    */
  case class Err[T, E](e: E) extends Result[T, E]

  /**
    * Evaluates the given results from left to right collecting the values into a list.
    *
    * Returns the first error value encountered, if any.
    */
  def sequence[T, E](xs: List[Result[T, E]]): Result[List[T], E] = {
    /**
      * Local tail recursive visitor. Uses an accumulator to avoid stack overflow.
      */
    @tailrec
    def visit(l: List[Result[T, E]], acc: List[T]): Result[List[T], E] = l match {
      case Nil => Ok(acc.reverse)
      case y :: ys => y match {
        case Ok(r) => visit(ys, r :: acc)
        case Err(e) => Err(e)
      }
    }

    visit(xs, Nil)
  }

  /**
    * Applies f to each element in the list.
    *
    * Fails at the first error found, or returns the new list.
    */
  def traverse[T, S, E](xs: Iterable[T])(f: T => Result[S, E]): Result[List[S], E] = {
    val res = ArrayBuffer.empty[S]

    for (x <- xs) {
      f(x) match {
        // Case 1: Ok. Add to the list.
        case Ok(ok) => res.append(ok)
        // Case 2: Error. Short-circuit.
        case Err(err) => return Err(err)
      }
    }

    Ok(res.toList)
  }

  /**
    * Traverses `o` applying the function `f` to the value, if it exists.
    */
  def traverseOpt[T, S, E](o: Option[T])(f: T => Result[S, E]): Result[Option[S], E] = o match {
    case None => Ok(None)
    case Some(x) => f(x) match {
      case Ok(t) => Ok(Some(t))
      case Err(e) => Err(e)
    }
  }

  /**
    * Applies f to each element in the list and flattens the results.
    *
    * Fails at the first error found, or returns the new list.
    */
  def flatTraverse[T, S, E](xs: Iterable[T])(f: T => Result[List[S], E]): Result[List[S], E] = {
    val res = ArrayBuffer.empty[S]

    for (x <- xs) {
      f(x) match {
        // Case 1: Ok. Add to the list.
        case Ok(ok) => res.addAll(ok)
        // Case 2: Error. Short-circuit.
        case Err(err) => return Err(err)
      }
    }

    Ok(res.toList)
  }

  /**
    * Adds an implicit `toOk` method.
    */
  implicit class ToOk[+T](val t: T) {
    def toOk[U >: T, E]: Result[U, E] = Ok(t)
  }

  /**
    * Adds an implicit `toErr` method.
    */
  implicit class ToErr[+E](val e: E) {
    def toErr[T, F >: E]: Result[T, F] = Err(e)
  }

}
