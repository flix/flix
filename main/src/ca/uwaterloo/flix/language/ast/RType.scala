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
import ca.uwaterloo.flix.language.ast.RRefType.{RArray, RArrow, RLazy, RObject}
import ca.uwaterloo.flix.language.phase.sjvm.{GenLazyClasses, JvmName}
import org.objectweb.asm.{MethodVisitor, Opcodes}

trait Describable {
  def toDescriptor: String
}

// actual flix types
sealed trait RType[T <: PType] extends Describable {
  private lazy val descriptor: String = RType.toDescriptor(this)

  def toDescriptor: String = descriptor

  def isCat1: Boolean = RType.isCat1(this)

  lazy val toErasedString: String = RType.toErasedString(this)
  lazy val erasedType: RType[_ <: PType] = RType.erasedType(this)
  lazy val erasedDescriptor: String = RType.erasedDescriptor(this)
  // TODO(JLS): add cont and Fn in RRefType maybe?
  lazy val contName: JvmName = JvmName(Nil, s"Cont${JvmName.reservedDelimiter}${this.toErasedString}")
  lazy val nothingToContMethodDescriptor: String = JvmName.getMethodDescriptor(Nil, this.contName)
  lazy val erasedNothingToThisMethodDescriptor: String = JvmName.getMethodDescriptor(Nil, this.erasedType)
  lazy val nothingToThisMethodDescriptor: String = JvmName.getMethodDescriptor(Nil, this)
  lazy val thisToNothingMethodDescriptor: String = JvmName.getMethodDescriptor(this, None)
}

object RType {

  val baseTypes = List(RBool, RInt8, RInt16, RInt16, RInt32, RInt64, RChar, RFloat32, RFloat64, RReference(RObject))

  // TODO(JLS): can these be added implicitly?
  def squeezeReference[T <: PRefType](x: RType[PReference[T]]): RReference[T] = x match {
    case res@RReference(_) => res
  }

  def squeezeArray[T <: PType](x: RReference[PArray[T]]): RArray[T] = x.referenceType match {
    case res@RArray(_) => res
  }

  def squeezeLazy[T <: PType](x: RReference[PLazy[T]]): RLazy[T] = x.referenceType match {
    case res@RLazy(_) => res
  }

  def squeezeFunction[T <: PType](x: RReference[PFunction[T]]): RArrow[T] = x.referenceType match {
    case res@RArrow(_, _) => res
  }

  def internalNameOfReference[T <: PRefType](e: RType[PReference[T]]): String = e match {
    case RReference(referenceType) => referenceType.toInternalName
  }

  def isCat1(rType: RType[_ <: PType]): Boolean = rType match {
    case RBool | RInt8 | RInt16 | RInt32 | RChar | RFloat32 | RReference(_) => true
    case RInt64 | RFloat64 => false
  }

  def toDescriptor[T <: PType](e: RType[T]): String = e match {
    case RBool => "Z"
    case RInt8 => "B"
    case RInt16 => "S"
    case RInt32 => "I"
    case RInt64 => "J"
    case RChar => "C"
    case RFloat32 => "F"
    case RFloat64 => "D"
    case RReference(referenceType) => referenceType.toDescriptor
  }

  def erasedType[T <: PType](e: RType[T]): RType[_ <: PType] = {
    pureErasedType(e) //TODO(JLS): should arrays be erased?
    //    e match {
    //      case RBool | RInt8 | RInt16 | RInt32 | RInt64 | RChar | RFloat32 | RFloat64 => e
    //      case RReference(rref) => getErasedInnerType(rref)
    //    }
  }

  def erasedDescriptor[T <: PType](e: RType[T]): String = e.erasedType.toDescriptor

  private def getErasedInnerType[T <: PRefType](r: RRefType[T]): RReference[_ <: PRefType] = r match {
    case RArray(tpe) => RReference(RArray(pureErasedType(tpe)))
    case _ => RReference(RObject)
  }

  private def pureErasedType[T <: PType](e: RType[T]): RType[_ <: PType] = e match {
    case RBool | RInt8 | RInt16 | RInt32 | RInt64 | RChar | RFloat32 | RFloat64 => e
    case RReference(_) => RReference(RObject)
  }

  def undoErasure(rType: RType[_ <: PType], methodVisitor: MethodVisitor): Unit =
    rType match {
      case RReference(referenceType) => methodVisitor.visitTypeInsn(Opcodes.CHECKCAST, referenceType.toInternalName)
      case RBool | RInt8 | RInt16 | RInt32 | RInt64 | RChar | RFloat32 | RFloat64 => ()
    }

  def toErasedString[T <: PType](e: RType[T]): String = e match {
    case RBool => "Bool"
    case RInt8 => "Int8"
    case RInt16 => "Int16"
    case RInt32 => "Int32"
    case RInt64 => "Int64"
    case RChar => "Char"
    case RFloat32 => "Float32"
    case RFloat64 => "Float64"
    case RReference(_) => "Obj"
  }

  object RBool extends RType[PInt32]

  object RInt8 extends RType[PInt8]

  object RInt16 extends RType[PInt16]

  object RInt32 extends RType[PInt32]

  object RInt64 extends RType[PInt64]

  object RChar extends RType[PChar]

  object RFloat32 extends RType[PFloat32]

  object RFloat64 extends RType[PFloat64]

  case class RReference[T <: PRefType](referenceType: RRefType[T]) extends RType[PReference[T]] {
    def toInternalName: String = referenceType.toInternalName

    def jvmName: JvmName = referenceType.jvmName
  }

}

sealed trait RRefType[T <: PRefType] extends Describable {
  val jvmName: JvmName

  def toInternalName: String = jvmName.toInternalName

  def toDescriptor: String = jvmName.toDescriptor

  lazy val erasedNothingToThisMethodDescriptor: String = JvmName.getMethodDescriptor(Nil, RObject) //TODO(JLS): Implicit erased type
  lazy val nothingToThisMethodDescriptor: String = JvmName.getMethodDescriptor(Nil, this)
}

object RRefType {

  def toDescriptor[T <: PRefType](e: RRefType[T]): String = e.toDescriptor

  def toInternalName[T <: PRefType](e: RRefType[T]): String = e.toInternalName

  private def mkName(prefix: String, types: List[String]): JvmName = JvmName(Nil, prefix + JvmName.reservedDelimiter + types.mkString(JvmName.reservedDelimiter))

  private def mkName(prefix: String, tpe: String): JvmName = mkName(prefix, List(tpe))


  object RBoxedBool extends RRefType[PBoxedBool] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.Boolean
  }

  object RBoxedInt8 extends RRefType[PBoxedInt8] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.Byte
  }

  object RBoxedInt16 extends RRefType[PBoxedInt16] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.Short
  }

  object RBoxedInt32 extends RRefType[PBoxedInt32] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.Integer
  }

  object RBoxedInt64 extends RRefType[PBoxedInt64] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.Long
  }

  object RBoxedChar extends RRefType[PBoxedChar] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.Character
  }

  object RBoxedFloat32 extends RRefType[PBoxedFloat32] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.Float
  }

  object RBoxedFloat64 extends RRefType[PBoxedFloat64] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.Double
  }

  object RUnit extends RRefType[PUnit] {
    override lazy val jvmName: JvmName = JvmName.Flix.Runtime.Value.Unit
  }

  case class RArray[T <: PType](tpe: RType[T]) extends RRefType[PArray[T]] {
    private lazy val className: String = s"[${tpe.erasedDescriptor}"
    override lazy val jvmName: JvmName = new JvmName(Nil, className) {
      override lazy val toDescriptor: String = this.toInternalName
    }
  }

  case class RChannel[T <: PType](tpe: RType[T]) extends RRefType[PChan[T]] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RTYPE)")
  }

  case class RLazy[T <: PType](tpe: RType[T]) extends RRefType[PLazy[T]] {
    override lazy val jvmName: JvmName = mkName("Lazy", List(tpe.toErasedString))
  }

  case class RRef[T <: PType](tpe: RType[T]) extends RRefType[PRef[T]] {
    override lazy val jvmName: JvmName = mkName("Ref", tpe.toErasedString)
  }

  // TODO: Should be removed.
  case class RVar(id: Int) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RTYPE)")
  }

  case class RTuple(elms: List[RType[_ <: PType]]) extends RRefType[PTuple] {
    override lazy val jvmName: JvmName = mkName("Tuple", elms.map(_.toErasedString))
  }

  case class REnum(sym: Symbol.EnumSym, args: List[RType[_ <: PType]]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RTYPE)")
  }

  object RBigInt extends RRefType[PBigInt] {
    override lazy val jvmName: JvmName = JvmName.Java.Math.BigInteger
  }

  object RStr extends RRefType[PStr] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.String
  }

  case class RArrow[T <: PType](args: List[RType[_ <: PType]], result: RType[T]) extends RRefType[PFunction[T]] {
    override lazy val jvmName: JvmName = mkName("Fn", args.appended(result).map(_.toErasedString))
  }

  object RRecordEmpty extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RTYPE)")
  }

  case class RRecordExtend(field: String, value: RType[_ <: PType], rest: RType[_ <: PReference[_ <: PRefType]]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RTYPE)")
  }

  object RSchemaEmpty extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RTYPE)")
  }

  case class RSchemaExtend(name: String, tpe: RType[_ <: PType], rest: RType[_ <: PReference[_ <: PRefType]]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RTYPE)")
  }

  case class RRelation(tpes: List[RType[_ <: PType]]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RTYPE)")
  }

  case class RLattice(tpes: List[RType[_ <: PType]]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RTYPE)")
  }

  case class RNative(clazz: Class[_]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName.fromClass(clazz)
  }

  object RObject extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName.Java.Lang.Object
  }

}