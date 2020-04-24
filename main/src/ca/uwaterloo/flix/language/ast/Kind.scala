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
    */
  def ->:(left: Kind): Kind = Kind.Arrow(List(left), this)

  /**
    * Returns a human readable representation of `this` kind.
    */
  override def toString: String = Kind.ShowInstance.show(this)

}

object Kind {

  // MATT docs
  // MATT find better name
  case object Unbound extends Kind

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

//  // MATT docs
//  case object Relation extends Kind
//
//  // MATT docs
//  case object Lattice extends Kind

  // MATT docs
  case object Predicate extends Kind

  /**
    * The kind of natural number expressions.
    */
  case object Nat extends Kind

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

  /////////////////////////////////////////////////////////////////////////////
  // Type Class Instances                                                    //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Show instance for Type.
    */
  implicit object ShowInstance extends Show[Kind] {
    def show(a: Kind): String = a match {
      case Kind.Unbound => "?"
      case Kind.Star => "*"
      case Kind.Record => "Record"
      case Kind.Schema => "Schema"
//      case Kind.Relation => "Relation"
//      case Kind.Lattice => "Lattice"
      case Kind.Predicate => "Predicate"
      case Kind.Nat => "Nat"
      case Kind.Effect => "Effect"
      case Kind.Arrow(List(kparam@ Arrow(_, _)), kr) => s"($kparam) -> $kr"
      case Kind.Arrow(List(kparam), kr) => s"$kparam -> $kr"
      case Kind.Arrow(kparams, kr) => s"(${kparams.mkString(", ")}) -> $kr"
    }
  }

}
