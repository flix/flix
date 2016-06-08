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
import ca.uwaterloo.flix.language.ast.Type._

import scala.collection.immutable

object FOpt {

  /**
    * A common super-type for all option operations.
    */
  sealed trait OptOperator extends LibraryOperator

  /**
    * All opt operations.
    */
  val Ops: immutable.Map[Symbol.Resolved, OptOperator] = List(
    "Opt/null" -> nul,
    "Opt/get" -> get,
    "Opt/getWithDefault" -> getWithDefault,
    "Opt/exists" -> exists,
    "Opt/forall" -> forall,
    "Opt/filter" -> filter,
    "Opt/map" -> map,
    "Opt/map2" -> map,
    "Opt/flatMap" -> flatMap,
    "Opt/toList" -> toList,
    "Opt/toSet" -> toSet,
    "Opt/withDefault" -> withDefault
  ).map {
    case (name, op) => Symbol.Resolved.mk(name) -> op
  }.toMap

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")
  val B = Type.Var("B")
  val C = Type.Var("C")

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object nul extends OptOperator {
    val tpe = Type.Abs(A, Type.FOpt(A) ~> Bool)
  }

  object get extends OptOperator {
    val tpe = Type.FOpt(A) ~> A
  }

  object getWithDefault extends OptOperator {
    val tpe = (Type.FOpt(A), A) ~> A
  }

  object exists extends OptOperator {
    val tpe = (A ~> Bool, Type.FOpt(A)) ~> Bool
  }

  object forall extends OptOperator {
    val tpe = (A ~> Bool, Type.FOpt(A)) ~> Bool
  }

  object filter extends OptOperator {
    val tpe = (A ~> Bool, Type.FOpt(A)) ~> Type.FOpt(A)
  }

  object map extends OptOperator {
    val tpe = (A ~> B, Type.FOpt(A)) ~> Type.FOpt(B)
  }

  object map2 extends OptOperator {
    val tpe = ((A, B) ~> C, Type.FOpt(A), Type.FOpt(B)) ~> Type.FOpt(C)
  }

  object flatMap extends OptOperator {
    val tpe = (A ~> Type.FOpt(B), Type.FOpt(A)) ~> Type.FOpt(B)
  }

  object flatMap2 extends OptOperator {
    val tpe = ((A, B) ~> Type.FOpt(C), Type.FOpt(A), Type.FOpt(B)) ~> Type.FOpt(C)
  }

  object withDefault extends OptOperator {
    val tpe = (Type.FOpt(A), Type.FOpt(A)) ~> Type.FOpt(A)
  }

  object toList extends OptOperator {
    val tpe = Type.FOpt(A) ~> Type.FList(A)
  }

  object toSet extends OptOperator {
    val tpe = Type.FOpt(A) ~> Type.FSet(A)
  }

}
