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

// TODO: Implement and document this class better.

sealed trait Result[T, E] {

  def isOk: Boolean = this match {
    case x: Result.Ok[T, E] => true
    case x: Result.Err[T, E] => false
  }

  def get: T = this match {
    case Result.Ok(subst) => subst
    case Result.Err(e) => ???
  }

}

object Result {

  case class Ok[T, E](t: T) extends Result[T, E]

  case class Err[T, E](e: E) extends Result[T, E]

}
