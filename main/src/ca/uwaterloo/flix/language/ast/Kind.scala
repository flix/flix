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

import ca.uwaterloo.flix.util.tc.Show

/**
  * A kind represents the "type" of a type expression.
  */
sealed trait Kind {

  /**
    * Constructs an arrow kind.
    *
    * This is a right-associative operator, i.e., the following two kinds are equivalent:
    *   - `Kind.Star ->: Kind.Star ->: Kind.Star`
    *   - `Kind.Star ->: (Kind.Star ->: Kind.Star)`
    */
  def ->:(left: Kind): Kind = Kind.Arrow(left, this)

  /**
    * Returns a human readable representation of `this` kind.
    */
  override def toString: String = Kind.ShowInstance.show(this)

}

object Kind {

  /**
    * The kind of all nullary type expressions.
    */
  case object Star extends Kind

  /**
    * The kind of records.
    */
  case object Record extends Kind

  /**
    * The kind of schemas.
    */
  case object Schema extends Kind

  /**
    * The kind of effects.
    */
  case object Effect extends Kind

  /**
    * The kind of type expressions k1 -> k2.
    */
  case class Arrow(k1: Kind, k2: Kind) extends Kind

  /**
    * Returns the kind: * -> (* ... -> *)
    */
  def mkArrow(length: Int): Kind = {
    if (length == 0) {
      return Kind.Star
    }

    (0 until length + 1).foldRight(Kind.Star: Kind) {
      case (_, acc) => Arrow(Kind.Star, acc)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Type Class Instances                                                    //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Show instance for Type.
    */
  implicit object ShowInstance extends Show[Kind] {
    def show(a: Kind): String = a match {
      case Kind.Star => "*"
      case Kind.Record => "Record"
      case Kind.Schema => "Schema"
      case Kind.Effect => "Effect"
      case Kind.Arrow(k1, Kind.Star) => s"$k1 -> *"
      case Kind.Arrow(k1, k2) => s"$k1 -> ($k2)"
    }
  }

}
