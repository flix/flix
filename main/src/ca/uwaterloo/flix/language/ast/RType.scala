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
sealed trait RType[T <: PType] {
  def toInternalName: String = RType.toInternalName(this)
  def erasedType: String = RType.erasedString(this)
}

object RType {

  def toInternalName[T <: PType](e: RType[T]): String = e match {
    case RBool() => "Z"
    case RInt8() => "B"
    case RInt16() => "S"
    case RInt32() => "I"
    case RInt64() => "J"
    case RChar() => "C"
    case RFloat32() => "F"
    case RFloat64() => "D"
    case RReference(referenceType) => referenceType.toInternalName
  }

  def erasedString[T <: PType](e: RType[T]): String = e match {
    case RBool() => "Bool"
    case RInt8() => "Int8"
    case RInt16() => "Int16"
    case RInt32() => "Int32"
    case RInt64() => "Int64"
    case RChar() => "Char"
    case RFloat32() => "Float32"
    case RFloat64() => "Float64"
    case RReference(_) => "Obj"
  }

  case class RBool() extends RType[PInt32]

  case class RInt8() extends RType[PInt8]

  case class RInt16() extends RType[PInt16]

  case class RInt32() extends RType[PInt32]

  case class RInt64() extends RType[PInt64]

  case class RChar() extends RType[PChar]

  case class RFloat32() extends RType[PFloat32]

  case class RFloat64() extends RType[PFloat64]

  case class RReference[T <: PRefType](referenceType: RRefType[T]) extends RType[PReference[T]]

}


sealed trait RRefType[T <: PRefType] {
  def toInternalName: String = RRefType.toInternalName(this)
}

object RRefType {

  // todo
  def toInternalName[T <: PRefType](e: RRefType[T]): String = e match {
    case RBoxedBool() => "???"
    case RBoxedInt8() => "???"
    case RBoxedInt16() => "???"
    case RBoxedInt32() => "???"
    case RBoxedInt64() => "???"
    case RBoxedChar() => "???"
    case RBoxedFloat32() => "???"
    case RBoxedFloat64() => "???"
    case RUnit() => "flix/runtime/value/Unit"
    case RArray(tpe) => "???"
    case RChannel(tpe) => "???"
    case RLazy(tpe) => "Lazy$" + tpe.erasedType
    case RRef(tpe) => "Ref$" + tpe.erasedType
    case RVar(id) => "???"
    case RTuple(elms) => "???"
    case REnum(sym, args) => "???"
    case RBigInt() => "???"
    case RStr() => "???"
    case RArrow(args, result) => "???"
    case RRecordEmpty() => "???"
    case RRecordExtend(field, value, rest) => "???"
    case RSchemaEmpty() => "???"
    case RSchemaExtend(name, tpe, rest) => "???"
    case RRelation(tpes) => "???"
    case RLattice(tpes) => "???"
    case RNative(clazz) => "???"
  }

  case class RBoxedBool() extends RRefType[PBoxedBool]

  case class RBoxedInt8() extends RRefType[PBoxedInt8]

  case class RBoxedInt16() extends RRefType[PBoxedInt16]

  case class RBoxedInt32() extends RRefType[PBoxedInt32]

  case class RBoxedInt64() extends RRefType[PBoxedInt64]

  case class RBoxedChar() extends RRefType[PBoxedChar]

  case class RBoxedFloat32() extends RRefType[PBoxedFloat32]

  case class RBoxedFloat64() extends RRefType[PBoxedFloat64]

  case class RUnit() extends RRefType[PUnit]

  case class RArray[T <: PType](tpe: RType[T]) extends RRefType[PArray[T]]

  case class RChannel[T <: PType](tpe: RType[T]) extends RRefType[PChan[T]]

  case class RLazy[T <: PType](tpe: RType[T]) extends RRefType[PLazy[T]]

  case class RRef[T <: PType](tpe: RType[T]) extends RRefType[PRef[T]]

  // TODO: Should be removed.
  case class RVar(id: Int) extends RRefType[PAnyObject]

  case class RTuple(elms: List[RType[PType]]) extends RRefType[PAnyObject]

  case class REnum(sym: Symbol.EnumSym, args: List[RType[PType]]) extends RRefType[PAnyObject]

  case class RBigInt() extends RRefType[PBigInt]

  case class RStr() extends RRefType[PStr]

  case class RArrow(args: List[RType[PType]], result: RType[PType]) extends RRefType[PAnyObject]

  case class RRecordEmpty() extends RRefType[PAnyObject]

  case class RRecordExtend(field: String, value: RType[PType], rest: RType[PReference[PAnyObject]]) extends RRefType[PAnyObject]

  case class RSchemaEmpty() extends RRefType[PAnyObject]

  case class RSchemaExtend(name: String, tpe: RType[PType], rest: RType[PReference[PAnyObject]]) extends RRefType[PAnyObject]

  case class RRelation(tpes: List[RType[PType]]) extends RRefType[PAnyObject]

  case class RLattice(tpes: List[RType[PType]]) extends RRefType[PAnyObject]

  case class RNative(clazz: Class[_]) extends RRefType[PAnyObject]

}