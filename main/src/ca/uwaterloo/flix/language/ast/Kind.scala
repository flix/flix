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

import ca.uwaterloo.flix.language.debug.FormatKind

import scala.annotation.tailrec

/**
  * A kind represents the "type" of a type expression.
  */
sealed trait Kind {

  /**
    * Constructs an arrow kind.
    *
    * This is a right-associative operator, i.e., the following two kinds are equivalent:
    *
    *   - `Kind.Star ->: Kind.Star ->: Kind.Star`
    *   - `Kind.Star ->: (Kind.Star ->: Kind.Star)`
    */
  def ->:(left: Kind): Kind = Kind.Arrow(left, this)

  /**
    * Returns a human readable representation of `this` kind.
    */
  override def toString: String = FormatKind.formatKind(this)

  /**
    * Returns true if `left` is a subkind of `this`.
    * Right-associative.
    */
  def <::(left: Kind): Boolean = (left, this) match {
    // NB: identities first for performance
    // identities
    case (Kind.Star, Kind.Star) => true
    case (Kind.Bool, Kind.Bool) => true
    case (Kind.Record, Kind.Record) => true
    case (Kind.Schema, Kind.Schema) => true

    case (_, Kind.Wild) => true
    case (Kind.Wild, _) => true // MATT is this right?

    // subkinds
    case (Kind.Record, Kind.Star) => true
    case (Kind.Schema, Kind.Star) => true

    // arrow kinds: the left side is contravariant
    case (Kind.Arrow(k11, k12), Kind.Arrow(k21, k22)) => (k21 <:: k11) && (k12 <:: k22)
    case _ => false
  }

}

object Kind {

  /**
    * Represents a wild kind.
    */
  case object Wild extends Kind

  /**
    * Represents the kind of types.
    */
  case object Star extends Kind

  /**
    * Represents the kind of boolean formulas.
    */
  case object Bool extends Kind

  /**
    * Represents the kind of records.
    */
  case object Record extends Kind

  /**
    * Represents the kind of schemas.
    */
  case object Schema extends Kind

  /**
    * Represents the kind of type expressions `k1 -> k2`.
    */
  case class Arrow(k1: Kind, k2: Kind) extends Kind

  /**
    * Returns the kind: * -> (* ... -> *)
    */
  def mkArrow(length: Int): Kind = {
    if (length == 0) {
      return Star
    }

    (0 until length).foldRight(Star: Kind) {
      case (_, acc) => Arrow(Star, acc)
    }
  }

  /**
    * Returns the kind: k1 -> (k2 ... -> kn) for the given list of kinds `ks`.
    */
  def mkArrow(ks: List[Kind]): Kind = ks match {
    case Nil => Star
    case x :: xs => Arrow(x, mkArrow(xs))
  }

  // MATT docs
  @tailrec
  def base(k: Kind): Kind = k match {
    case Arrow(k1, _) => base(k1)
    case _ => k
  }

  // MATT docs
  def args(k: Kind): List[Kind] = k match {
    case Arrow(k1, k2) => k1 :: args(k2)
    case _ => Nil
  }

  // MATT docs
  def min(k1: Kind, k2: Kind): Option[Kind] = {
    if (k1 <:: k2)
      Some(k1)
    else if (k2 <:: k1)
      Some(k2)
    else
      None
  }

}
