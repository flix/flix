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

/**
  * A result either holds a value ([[Result.Ok]]) or holds an error ([[Result.Err]]).
  *
  * @tparam T the type of the value.
  * @tparam E the type of the error.
  */
sealed trait Result[T, E] {

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
    * Applies the given function `f` to the value of `this`.
    */
  final def flatMap[B](f: T => Result[B, E]): Result[B, E] = this match {
    case Result.Ok(t) => f(t)
    case Result.Err(e) => Result.Err(e)
  }

  /**
    * Returns `this` result as a [[Validation]].
    */
  final def toValidation: Validation[T, E] = this match {
    case Result.Ok(t) => Validation.Success(t)
    case Result.Err(e) => Validation.Failure(LazyList(e))
  }
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
