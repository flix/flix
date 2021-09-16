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
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType.RReference
import ca.uwaterloo.flix.language.phase.sjvm.{Describable, Descriptor, InternalName, JvmName}
import org.objectweb.asm.{MethodVisitor, Opcodes}

sealed trait RType[T <: PType] extends Describable {
  private lazy val lazyDescriptor: Descriptor = RType.descriptorOf(this)

  def isCat1: Boolean = RType.isCat1(this)

  def descriptor: Descriptor = lazyDescriptor

  lazy val erasedString: String = RType.erasedStringOf(this)
  lazy val erasedType: RType[_ <: PType] = RType.erasedType(this)
  lazy val erasedDescriptor: Descriptor = RType.erasedDescriptor(this)
  lazy val nothingToThisDescriptor: Descriptor = JvmName.getMethodDescriptor(Nil, this)
  lazy val thisToNothingDescriptor: Descriptor = JvmName.getMethodDescriptor(this, None)

  // TODO(JLS): move to arrow type
  lazy val contName: JvmName = JvmName(Nil, s"Cont${JvmName.reservedDelimiter}${this.erasedString}")
  lazy val nothingToContDescriptor: Descriptor = JvmName.getMethodDescriptor(Nil, this.contName)
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

  def squeezeChannel[T <: PRefType](x: RReference[PChan[T]]): RChannel[T] = x.referenceType match {
    case res@RChannel(_) => res
  }

  def squeezeLazy[T <: PType](x: RReference[PLazy[T]]): RLazy[T] = x.referenceType match {
    case res@RLazy(_) => res
  }

  def squeezeTuple[T <: PType](x: RReference[PTuple]): RTuple = x.referenceType match {
    case res@RTuple(_) => res
  }

  def squeezeFunction[T <: PType](x: RReference[PFunction[T]]): RArrow[T] = x.referenceType match {
    case res@RArrow(_, _) => res
  }

  def isCat1(rType: RType[_ <: PType]): Boolean = rType match {
    case RBool | RInt8 | RInt16 | RInt32 | RChar | RFloat32 | RReference(_) => true
    case RInt64 | RFloat64 => false
  }

  def descriptorOf[T <: PType](e: RType[T]): Descriptor = e match {
    case RBool => Descriptor.of("Z")
    case RInt8 => Descriptor.of("B")
    case RInt16 => Descriptor.of("S")
    case RInt32 => Descriptor.of("I")
    case RInt64 => Descriptor.of("J")
    case RChar => Descriptor.of("C")
    case RFloat32 => Descriptor.of("F")
    case RFloat64 => Descriptor.of("D")
    case RReference(referenceType) => referenceType.descriptor
  }

  def erasedType[T <: PType](e: RType[T]): RType[_ <: PType] = {
    pureErasedType(e) //TODO(JLS): should arrays be erased?
    //    e match {
    //      case RBool | RInt8 | RInt16 | RInt32 | RInt64 | RChar | RFloat32 | RFloat64 => e
    //      case RReference(rref) => getErasedInnerType(rref)
    //    }
  }

  def erasedDescriptor[T <: PType](e: RType[T]): Descriptor = e.erasedType.descriptor

  private def getErasedInnerType[T <: PRefType](r: RRefType[T]): RReference[_ <: PRefType] = r match {
    case RArray(tpe) => RReference(RArray(pureErasedType(tpe)))
    case _ => RReference(RObject)
  }

  private def pureErasedType[T <: PType](e: RType[T]): RType[_ <: PType] = e match {
    case RBool | RInt8 | RInt16 | RInt32 | RInt64 | RChar | RFloat32 | RFloat64 => e
    case RReference(_) => RReference(RObject)
  }

  // TODO(JLS): should probably be in Instructions
  def undoErasure(rType: RType[_ <: PType], methodVisitor: MethodVisitor): Unit = rType match {
    // TODO(JLS): Should not output ins if referenceType is RObject
    case RReference(referenceType) => undoErasure(referenceType.jvmName, methodVisitor)
    case RBool | RInt8 | RInt16 | RInt32 | RInt64 | RChar | RFloat32 | RFloat64 => ()
  }

  def undoErasure(name: JvmName, methodVisitor: MethodVisitor): Unit =
    methodVisitor.visitTypeInsn(Opcodes.CHECKCAST, name.internalName.toString)

  def erasedStringOf[T <: PType](e: RType[T]): String = e match {
    case RBool => "Bool"
    case RInt8 => "Int8"
    case RInt16 => "Int16"
    case RInt32 => "Int32"
    case RInt64 => "Int64"
    case RChar => "Char"
    case RFloat32 => "Float32"
    case RFloat64 => "Float64"
    case RReference(referenceType) => referenceType.erasedString
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
    def internalName: InternalName = referenceType.internalName

    def jvmName: JvmName = referenceType.jvmName
  }

}

sealed trait RRefType[T <: PRefType] extends Describable {
  val jvmName: JvmName

  def internalName: InternalName = jvmName.internalName

  def descriptor: Descriptor = jvmName.descriptor

  def rType: RReference[T] = RReference(this)

  override lazy val nothingToThisDescriptor: Descriptor = jvmName.nothingToThisDescriptor
  override lazy val thisToNothingDescriptor: Descriptor = jvmName.thisToNothingDescriptor
  override val erasedDescriptor: Descriptor = jvmName.erasedDescriptor
  override val erasedString: String = jvmName.erasedString
}

object RRefType {

  def descriptorOf[T <: PRefType](e: RRefType[T]): Descriptor = e.descriptor

  def internalNameOf[T <: PRefType](e: RRefType[T]): InternalName = e.internalName

  // TODO(JLS): skip delimiter if list is empty
  private def mkName(prefix: String, types: List[String]): JvmName = JvmName(Nil, prefix + JvmName.reservedDelimiter + types.mkString(JvmName.reservedDelimiter))

  private def mkName(prefix: String, tpe: String): JvmName = mkName(prefix, List(tpe))


  object RBoxedBool extends RRefType[PBoxedBool] {
    override lazy val jvmName: JvmName = JvmName.Java.Boolean
  }

  object RBoxedInt8 extends RRefType[PBoxedInt8] {
    override lazy val jvmName: JvmName = JvmName.Java.Byte
  }

  object RBoxedInt16 extends RRefType[PBoxedInt16] {
    override lazy val jvmName: JvmName = JvmName.Java.Short
  }

  object RBoxedInt32 extends RRefType[PBoxedInt32] {
    override lazy val jvmName: JvmName = JvmName.Java.Integer
  }

  object RBoxedInt64 extends RRefType[PBoxedInt64] {
    override lazy val jvmName: JvmName = JvmName.Java.Long
  }

  object RBoxedChar extends RRefType[PBoxedChar] {
    override lazy val jvmName: JvmName = JvmName.Java.Character
  }

  object RBoxedFloat32 extends RRefType[PBoxedFloat32] {
    override lazy val jvmName: JvmName = JvmName.Java.Float
  }

  object RBoxedFloat64 extends RRefType[PBoxedFloat64] {
    override lazy val jvmName: JvmName = JvmName.Java.Double
  }

  object RUnit extends RRefType[PUnit] {
    override lazy val jvmName: JvmName = JvmName.Flix.Unit
  }

  case class RArray[T <: PType](tpe: RType[T]) extends RRefType[PArray[T]] {
    private lazy val className: String = s"[${tpe.descriptor}"
    override lazy val jvmName: JvmName = new JvmName(Nil, className) {
      override lazy val descriptor: Descriptor = Descriptor.of(this.internalName.toString)
    }
  }

  case class RChannel[T <: PRefType](tpe: RType[PReference[T]]) extends RRefType[PChan[T]] {
    override lazy val jvmName: JvmName = JvmName.Flix.Channel
  }

  case class RLazy[T <: PType](tpe: RType[T]) extends RRefType[PLazy[T]] {
    override lazy val jvmName: JvmName = mkName("Lazy", List(tpe.erasedString))
  }

  case class RRef[T <: PType](tpe: RType[T]) extends RRefType[PRef[T]] {
    override lazy val jvmName: JvmName = mkName("Ref", tpe.erasedString)
  }

  case class RTuple(elms: List[RType[_ <: PType]]) extends RRefType[PTuple] {
    override lazy val jvmName: JvmName = mkName("Tuple", elms.map(_.erasedString))
  }

  case class REnum(sym: Symbol.EnumSym, args: List[RType[_ <: PType]]) extends RRefType[PEnum] {
    override lazy val jvmName: JvmName = mkName(s"I${sym.name}", args.map(_.erasedString))
  }

  object RBigInt extends RRefType[PBigInt] {
    override lazy val jvmName: JvmName = JvmName.Java.BigInteger
  }

  object RStr extends RRefType[PStr] {
    override lazy val jvmName: JvmName = JvmName.Java.String
  }

  case class RArrow[T <: PType](args: List[RType[_ <: PType]], result: RType[T]) extends RRefType[PFunction[T]] {
    override lazy val jvmName: JvmName = mkName("Fn", args.appended(result).map(_.erasedString))
  }

  object RRecordEmpty extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RRecordEmpty)")
  }

  case class RRecordExtend(field: String, value: RType[_ <: PType], rest: RType[_ <: PReference[_ <: PRefType]]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RRecordExtend)")
  }

  object RSchemaEmpty extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RSchemaEmpty)")
  }

  case class RSchemaExtend(name: String, tpe: RType[_ <: PType], rest: RType[_ <: PReference[_ <: PRefType]]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RSchemaExtend)")
  }

  case class RRelation(tpes: List[RType[_ <: PType]]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RRelation)")
  }

  case class RLattice(tpes: List[RType[_ <: PType]]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName(Nil, "NOT_IMPLEMENTED(RLattice)")
  }

  case class RNative(clazz: Class[_]) extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName.fromClass(clazz)
  }

  object RObject extends RRefType[PAnyObject] {
    override lazy val jvmName: JvmName = JvmName.Java.Object
  }

}