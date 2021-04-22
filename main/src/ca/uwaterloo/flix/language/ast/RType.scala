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
import ca.uwaterloo.flix.language.phase.sjvm.JvmName

// actual flix types
sealed trait RType[T <: PType] {
  def toInternalName: String = RType.toInternalName(this)
  def toDescriptor: String = RType.toDescriptor(this)
  def toErasedType: String = RType.toErasedString(this)
}

object RType {

  def toDescriptor[T <: PType](e: RType[T]): String = e match {
    case RBool() => "Z"
    case RInt8() => "B"
    case RInt16() => "S"
    case RInt32() => "I"
    case RInt64() => "J"
    case RChar() => "C"
    case RFloat32() => "F"
    case RFloat64() => "D"
    case RReference(referenceType) => referenceType.toDescriptor
  }

  // TODO: maybe it shouldn't be defined on RType
  def toInternalName[T <: PType](e: RType[T]): String = e match {
    case RReference(referenceType) => referenceType.toInternalName
    case _ => throw new IllegalArgumentException("Primitive types do not have internal names")
  }

  def toErasedString[T <: PType](e: RType[T]): String = e match {
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

  case class RBool() extends RType[PInt32 with Cat1]

  case class RInt8() extends RType[PInt8 with Cat1]

  case class RInt16() extends RType[PInt16 with Cat1]

  case class RInt32() extends RType[PInt32 with Cat1]

  case class RInt64() extends RType[PInt64 with Cat2]

  case class RChar() extends RType[PChar with Cat1]

  case class RFloat32() extends RType[PFloat32 with Cat1]

  case class RFloat64() extends RType[PFloat64 with Cat2]

  case class RReference[T <: PRefType](referenceType: RRefType[T]) extends RType[PReference[T] with Cat1]

}


sealed trait RRefType[T <: PRefType] {
  val jvmName: JvmName
  def toInternalName: String = RRefType.toInternalName(this)
  def toDescriptor: String = RRefType.toDescriptor(this)
}

object RRefType {

  def toDescriptor[T <: PRefType](e: RRefType[T]): String = e.jvmName.toDescriptor

  def toInternalName[T <: PRefType](e: RRefType[T]): String = e.jvmName.toInternalName

  // TODO: These should be object for the sake of jvm strings
  case class RBoxedBool() extends RRefType[PBoxedBool] {
    override val jvmName: JvmName = JvmName.Java.Lang.Boolean
  }

  case class RBoxedInt8() extends RRefType[PBoxedInt8] {
    override val jvmName: JvmName = JvmName.Java.Lang.Byte
  }

  case class RBoxedInt16() extends RRefType[PBoxedInt16] {
    override val jvmName: JvmName = JvmName.Java.Lang.Short
  }

  case class RBoxedInt32() extends RRefType[PBoxedInt32] {
    override val jvmName: JvmName = JvmName.Java.Lang.Integer
  }

  case class RBoxedInt64() extends RRefType[PBoxedInt64] {
    override val jvmName: JvmName = JvmName.Java.Lang.Long
  }

  case class RBoxedChar() extends RRefType[PBoxedChar] {
    override val jvmName: JvmName = JvmName.Java.Lang.Character
  }

  case class RBoxedFloat32() extends RRefType[PBoxedFloat32] {
    override val jvmName: JvmName = JvmName.Java.Lang.Float
  }

  case class RBoxedFloat64() extends RRefType[PBoxedFloat64] {
    override val jvmName: JvmName = JvmName.Java.Lang.Double
  }

  case class RUnit() extends RRefType[PUnit] {
    override val jvmName: JvmName = JvmName.Flix.Runtime.Value.Unit
  }

  case class RArray[T <: PType](tpe: RType[T]) extends RRefType[PArray[T]] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RChannel[T <: PType](tpe: RType[T]) extends RRefType[PChan[T]] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RLazy[T <: PType](tpe: RType[T]) extends RRefType[PLazy[T]] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RRef[T <: PType](tpe: RType[T]) extends RRefType[PRef[T]] {
    private val className = s"Ref${JvmName.reservedDelimiter}${tpe.toErasedType}"
    override val jvmName: JvmName = JvmName(Nil, className)
  }

  // TODO: Should be removed.
  case class RVar(id: Int) extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RTuple(elms: List[RType[PType]]) extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class REnum(sym: Symbol.EnumSym, args: List[RType[PType]]) extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RBigInt() extends RRefType[PBigInt] {
    override val jvmName: JvmName = JvmName.Java.Math.BigInteger
  }

  case class RStr() extends RRefType[PStr] {
    override val jvmName: JvmName = JvmName.Java.Lang.String
  }

  case class RArrow(args: List[RType[PType]], result: RType[PType]) extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RRecordEmpty() extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RRecordExtend(field: String, value: RType[PType], rest: RType[PReference[PAnyObject]]) extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RSchemaEmpty() extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RSchemaExtend(name: String, tpe: RType[PType], rest: RType[PReference[PAnyObject]]) extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RRelation(tpes: List[RType[PType]]) extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RLattice(tpes: List[RType[PType]]) extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RNative(clazz: Class[_]) extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

  case class RObject() extends RRefType[PAnyObject] {
    override val jvmName: JvmName = JvmName.Java.Lang.Object
  }

}