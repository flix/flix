/*
 *  Copyright 2016 Magnus Madsen
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

/**
  * A result either holds a value ([[Result.Ok]]) or holds an error ([[Result.Err]]).
  *
  * @tparam T the type of  he value.
  * @tparam E the type of the error.
  */
sealed trait Result[T, E] {

  /**
    * Returns `true` iff `this` result holds a value.
    */
  def isOk: Boolean = this match {
    case x: Result.Ok[T, E] => true
    case x: Result.Err[T, E] => false
  }

  /**
    * Returns `true` iff `this` result holds an error.
    */
  def isErr: Boolean = !isOk

  /**
    * Retrieves the value from `this` result.
    *
    * @throws IllegalStateException if `this` result does not hold a value.
    */
  def get: T = this match {
    case Result.Ok(t) => t
    case Result.Err(e) => throw new IllegalStateException(s"Result is Err($e).")
  }

  /**
    * Applies the given function `f` to value of `this` wrapping it in [[Result.Ok]].
    */
  def map[B](f: T => B): Result[B, E] = this match {
    case Result.Ok(t) => Result.Ok(f(t))
    case Result.Err(e) => Result.Err(e)
  }

  /**
    * Applies the given function `f` to the value of `this`.
    */
  def flatMap[B](f: T => Result[B, E]): Result[B, E] = this match {
    case Result.Ok(t) => f(t)
    case Result.Err(e) => Result.Err(e)
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
    * Applies
    */
  //def seqM[T, E](xs: List[Result[T, E]]): Result[List[T], E]

}
