/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Type

import scala.collection.immutable

object FChar {

  /**
    * A common super-type for all char operations.
    */
  sealed trait CharOperator extends LibraryOperator

  /**
    * All char operations.
    */
  val Ops: immutable.Map[Symbol.Resolved, CharOperator] = List(
    // Char Predicates.
    "Char/isAscii" -> isAscii,
    "Char/isLetter" -> isLetter,
    "Char/isDigit" -> isDigit,
    "Char/isOctDigit" -> isOctDigit,
    "Char/isHexDigit" -> isHexDigit,
    "Char/isLower" -> isLower,
    "Char/isUpper" -> isUpper,
    "Char/isWhiteSpace" -> isWhiteSpace,

    // Char Conversions.
    "Char/toLower" -> toLower,
    "Char/toUpper" -> toUpper,
    "Char/toInt" -> toInt
  ).map {
    case (name, op) => Symbol.Resolved.mk(name) -> op
  }.toMap

  /////////////////////////////////////////////////////////////////////////////
  // Char Predicates                                                         //
  /////////////////////////////////////////////////////////////////////////////
  object isAscii extends CharOperator {
    val tpe = Type.Char ~> Type.Bool
  }

  object isLetter extends CharOperator {
    val tpe = Type.Char ~> Type.Bool
  }

  object isDigit extends CharOperator {
    val tpe = Type.Char ~> Type.Bool
  }

  object isOctDigit extends CharOperator {
    val tpe = Type.Char ~> Type.Bool
  }

  object isHexDigit extends CharOperator {
    val tpe = Type.Char ~> Type.Bool
  }

  object isLower extends CharOperator {
    val tpe = Type.Char ~> Type.Bool
  }

  object isUpper extends CharOperator {
    val tpe = Type.Char ~> Type.Bool
  }

  object isWhiteSpace extends CharOperator {
    val tpe = Type.Char ~> Type.Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Char Conversions                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object toInt extends CharOperator {
    val tpe = Type.Char ~> Type.Int32
  }

  object toLower extends CharOperator {
    val tpe = Type.Char ~> Type.Char
  }

  object toUpper extends CharOperator {
    val tpe = Type.Char ~> Type.Char
  }

}
