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

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Type

import scala.collection.immutable

package object library {

  /**
    * A common super-type for all library operators.
    */
  trait LibraryOperator

  /**
    * A map of all library operators.
    */
  val Library: immutable.Map[Symbol.Resolved, LibraryOperator] =
    FBool.Ops ++ FChar.Ops ++ FDebug.Ops ++ FInt.Ops ++ FList.Ops ++ FMap.Ops ++ FOpt.Ops ++ FSet.Ops

  /////////////////////////////////////////////////////////////////////////////
  // Mini Type DSL                                                           //
  /////////////////////////////////////////////////////////////////////////////
  implicit class RichType(thiz: Type) {
    def ~>(that: Type): Type = Type.Lambda(List(thiz), that)
  }

  implicit class RichTuple(thiz: Unit) {
    def ~>(that: Type): Type = Type.Lambda(List.empty[Type], that)
  }

  implicit class RichTuple2(thiz: (Type, Type)) {
    def ~>(that: Type): Type = Type.Lambda(List(thiz._1, thiz._2), that)
  }

  implicit class RichTuple3(thiz: (Type, Type, Type)) {
    def ~>(that: Type): Type = Type.Lambda(List(thiz._1, thiz._2, thiz._3), that)
  }

  implicit class RichTuple4(thiz: (Type, Type, Type, Type)) {
    def ~>(that: Type): Type = Type.Lambda(List(thiz._1, thiz._2, thiz._3, thiz._4), that)
  }

  implicit def tuple2type(tuple: (Type, Type)): Type = Type.Tuple(List(tuple._1, tuple._2))

}
