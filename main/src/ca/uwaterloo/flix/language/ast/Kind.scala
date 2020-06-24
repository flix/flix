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

import ca.uwaterloo.flix.api.Flix
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
  def ->:(left: Kind): Kind = Kind.Arrow(List(left), this)

  /**
    * Returns a human readable representation of `this` kind.
    */
  override def toString: String = Kind.ShowInstance.show(this)

}

object Kind {

  /**
    * The kind of kind variables.
    */
  case class Var(id: Int) extends Kind

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
    * The kind of type expressions that take a sequence of kinds `kparams` to a kind `kr`.
    */
  case class Arrow(kparams: List[Kind], kr: Kind) extends Kind {
    assert(kparams.nonEmpty)
  }

  def freshKindVar()(implicit flix: Flix): Kind.Var = {
    Kind.Var(flix.genSym.freshId())
  }

  /////////////////////////////////////////////////////////////////////////////
  // Type Class Instances                                                    //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Show instance for Type.
    */
  implicit object ShowInstance extends Show[Kind] {
    def show(a: Kind): String = a match {
      case Kind.Var(id) => s"k$id"
      case Kind.Star => "*"
      case Kind.Record => "Record"
      case Kind.Schema => "Schema"
      case Kind.Effect => "Effect"
      case Kind.Arrow(List(Kind.Star), Kind.Star) => "* -> *"
      case Kind.Arrow(List(Kind.Star), kr) => s"* -> ($kr)"
      case Kind.Arrow(kparams, Kind.Star) => s"(${kparams.mkString(", ")}) -> *"
      case Kind.Arrow(kparams, kr) => s"(${kparams.mkString(", ")}) -> ($kr)"
    }
  }

}
