/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._

// actual flix types
sealed trait EType[T <: PType]

object EType {

  case class Bool() extends EType[PInt32]

  case class Int8() extends EType[PInt8]

  case class Int16() extends EType[PInt16]

  case class Int32() extends EType[PInt32]

  case class Int64() extends EType[PInt64]

  case class Char() extends EType[PChar]

  case class Float32() extends EType[PFloat32]

  case class Float64() extends EType[PFloat64]

  case class Reference[T <: PRefType](referenceType: ERefType[T]) extends EType[PReference[T]]

}


sealed trait ERefType[T <: PRefType]

object ERefType {

  case class BoxedBool() extends ERefType[PBoxedBool]

  case class BoxedInt8() extends ERefType[PBoxedInt8]

  case class BoxedInt16() extends ERefType[PBoxedInt16]

  case class BoxedInt32() extends ERefType[PBoxedInt32]

  case class BoxedInt64() extends ERefType[PBoxedInt64]

  case class BoxedChar() extends ERefType[PBoxedChar]

  case class BoxedFloat32() extends ERefType[PBoxedFloat32]

  case class BoxedFloat64() extends ERefType[PBoxedFloat64]

  case class Unit() extends ERefType[PUnit]

  case class Array[T <: PType](tpe: EType[T]) extends ERefType[PArray[T]]

  case class Channel[T <: PType](tpe: EType[T]) extends ERefType[PChan[T]]

  case class Lazy[T <: PType](tpe: EType[T]) extends ERefType[PLazy[T]]

  case class Ref[T <: PType](tpe: EType[T]) extends ERefType[PRef[T]]

  // TODO: Should be removed.
  case class Var(id: Int) extends ERefType[PAnyObject]

  case class Tuple(elms: List[EType[PType]]) extends ERefType[PAnyObject]

  case class Enum(sym: Symbol.EnumSym, args: List[EType[PType]]) extends ERefType[PAnyObject]

  case class BigInt() extends ERefType[PBigInt]

  case class Str() extends ERefType[PStr]

  case class Arrow(args: List[EType[PType]], result: EType[PType]) extends ERefType[PAnyObject]

  case class RecordEmpty() extends ERefType[PAnyObject]

  case class RecordExtend(field: String, value: EType[PType], rest: EType[PReference[PAnyObject]]) extends ERefType[PAnyObject]

  case class SchemaEmpty() extends ERefType[PAnyObject]

  case class SchemaExtend(name: String, tpe: EType[PType], rest: EType[PReference[PAnyObject]]) extends ERefType[PAnyObject]

  case class Relation(tpes: List[EType[PType]]) extends ERefType[PAnyObject]

  case class Lattice(tpes: List[EType[PType]]) extends ERefType[PAnyObject]

  case class Native(clazz: Class[_]) extends ERefType[PAnyObject]

}