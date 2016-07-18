/*
 * Copyright 2016 Magnus Madsen
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

/**
  * A kind represents the "type" of a type expression.
  */
trait Kind {

  override def toString: String = this match {
    case Kind.Star => "*"
    case Kind.Arrow(Kind.Star, Kind.Star) => "* -> *"
    case Kind.Arrow(Kind.Star, k2) => "* -> (" + k2.toString + ")"
    case Kind.Arrow(k1, Kind.Star) => "(" + k1.toString + ") -> *"
    case Kind.Arrow(k1, k2) => "(" + k1.toString + ") -> (" + k2.toString + ")"
  }

}

object Kind {

  /**
    * The kind of all nullary type expressions.
    */
  object Star extends Kind

  /**
    * The kind of type expressions that take a kind `k1` to kind `k2`.
    */
  case class Arrow(k1: Kind, k2: Kind) extends Kind

}
