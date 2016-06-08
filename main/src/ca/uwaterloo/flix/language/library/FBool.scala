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

object FBool {

  /**
    * A common super-type for all boolean operations.
    */
  sealed trait BoolOperator extends LibraryOperator

  /**
    * All bool operations.
    */
  val Ops: immutable.Map[Symbol.Resolved, BoolOperator] = List(
    "Bool/&&" -> and,
    "Bool/||" -> or
  ).map {
    case (name, op) => Symbol.Resolved.mk(name) -> op
  }.toMap

  /////////////////////////////////////////////////////////////////////////////
  // Logical Operations                                                      //
  /////////////////////////////////////////////////////////////////////////////
  object and extends BoolOperator {
    val tpe = (Type.Bool, Type.Bool) ~> Type.Bool
  }

  object or extends BoolOperator {
    val tpe = (Type.Bool, Type.Bool) ~> Type.Bool
  }

}
