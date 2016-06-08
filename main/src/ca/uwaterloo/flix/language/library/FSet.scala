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

object FSet {

  /**
    * A common super-type for all set operations.
    */
  sealed trait SetOperator extends LibraryOperator

  /**
    * All set operations.
    */
  val Ops: immutable.Map[Symbol.Resolved, SetOperator] = List(
    // Set Construction.
    "Set/empty" -> empty,
    "Set/singleton" -> singleton,
    "Set/insert" -> insert,
    "Set/delete" -> delete,

    // Set Predicates.
    "Set/null" -> nul,
    "Set/memberOf" -> memberOf,
    "Set/isSubsetOf" -> isSubsetOf,
    "Set/isProperSubsetOf" -> isProperSubsetOf,

    // Elementary Set Operations.
    "Set/union" -> union,
    "Set/intersection" -> intersection,
    "Set/difference" -> difference,
    "Set/subsets" -> subsets,

    // Set Transformation.
    "Set/filter" -> filter,
    "Set/map" -> map,
    "Set/flatMap" -> flatMap,

    // Set Conversions.
    "Set/toList" -> toAscList,
    "Set/toAscList" -> toAscList,
    "Set/toDescList" -> toDescList,
    "Set/toMap" -> toMap,

    // Order and Lattice Operations.
    "Set/isAntiChain" -> isAntiChain,
    "Set/join" -> join,
    "Set/meet" -> meet,
    "Set/widen" -> widen,
    "Set/narrow" -> narrow
  ).map {
    case (name, op) => Symbol.Resolved.mk(name) -> op
  }.toMap

  /**
    * Generic type variables.
    */
  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Set Construction                                                        //
  /////////////////////////////////////////////////////////////////////////////
  object empty extends SetOperator {
    val tpe = () ~> Bool
  }

  object singleton extends SetOperator {
    val tpe = A ~> Bool
  }

  object insert extends SetOperator {
    val tpe = (A, Type.FSet(A)) ~> Type.FSet(A)
  }

  object delete extends SetOperator {
    val tpe = (A, Type.FSet(A)) ~> Type.FSet(A)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Set Predicates                                                          //
  /////////////////////////////////////////////////////////////////////////////
  object nul extends SetOperator {
    val tpe = Type.FSet(A) ~> Bool
  }

  object memberOf extends SetOperator {
    val tpe = (A, Type.FSet(A)) ~> Bool
  }

  object isSubsetOf extends SetOperator {
    val tpe = (Type.FSet(A), Type.FSet(A)) ~> Bool
  }

  object isProperSubsetOf extends SetOperator {
    val tpe = (Type.FSet(A), Type.FSet(A)) ~> Bool
  }

  /////////////////////////////////////////////////////////////////////////////
  // Two Set Operations                                                      //
  /////////////////////////////////////////////////////////////////////////////
  object union extends SetOperator {
    val tpe = (Type.FSet(A), Type.FSet(A)) ~> Type.FSet(A)
  }

  object intersection extends SetOperator {
    val tpe = (Type.FSet(A), Type.FSet(A)) ~> Type.FSet(A)
  }

  object difference extends SetOperator {
    val tpe = (Type.FSet(A), Type.FSet(A)) ~> Type.FSet(A)
  }

  object subsets extends SetOperator {
    val tpe = Type.FSet(A) ~> Type.FSet(Type.FSet(A))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Set Transformation                                                      //
  /////////////////////////////////////////////////////////////////////////////
  object filter extends SetOperator {
    val tpe = (A ~> Bool, Type.FSet(A)) ~> Type.FSet(A)
  }

  object map extends SetOperator {
    val tpe = (A ~> B, Type.FSet(A)) ~> Type.FSet(B)
  }

  object flatMap extends SetOperator {
    val tpe = (A ~> Type.FSet(B), Type.FSet(A)) ~> Type.FSet(B)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Set Conversions                                                         //
  /////////////////////////////////////////////////////////////////////////////
  object toAscList extends SetOperator {
    val tpe = Type.FSet(A) ~> Type.FList(A)
  }

  object toDescList extends SetOperator {
    val tpe = Type.FSet(A) ~> Type.FList(A)
  }

  object toMap extends SetOperator {
    val tpe = Type.FSet((A, B)) ~> Type.FMap(A, B)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Order and Lattice Operations                                            //
  /////////////////////////////////////////////////////////////////////////////
  object isAntiChain extends SetOperator {
    val tpe = Type.FList(A) ~> Bool
  }

  object join extends SetOperator {
    val tpe = Type.FList(A) ~> A
  }

  object meet extends SetOperator {
    val tpe = Type.FList(A) ~> A
  }

  object widen extends SetOperator {
    val tpe = Type.FList(A) ~> A
  }

  object narrow extends SetOperator {
    val tpe = Type.FList(A) ~> A
  }

}
