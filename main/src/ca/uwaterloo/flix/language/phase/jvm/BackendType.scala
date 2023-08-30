/*
 * Copyright 2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.Opcodes

import scala.annotation.tailrec

/**
  * Represents all Flix types that are not objects on the JVM (array is an exception).
  */
sealed trait BackendType extends VoidableType {

  def toDescriptor: String = {
    @tailrec
    def visit(t: BackendType, acc: String): String = t match {
      case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 |
           BackendType.Int32 | BackendType.Int64 | BackendType.Float32 | BackendType.Float64 |
           BackendType.Reference(_) => acc + t.toDescriptor
      case BackendType.Array(tt) => visit(tt, acc + "[")
    }

    this match {
      case BackendType.Bool => "Z"
      case BackendType.Char => "C"
      case BackendType.Int8 => "B"
      case BackendType.Int16 => "S"
      case BackendType.Int32 => "I"
      case BackendType.Int64 => "J"
      case BackendType.Float32 => "F"
      case BackendType.Float64 => "D"
      case BackendType.Array(tpe) => visit(tpe, "[")
      case BackendType.Reference(ref) => ref.toDescriptor
    }
  }

  /**
    * Returns the erased type, either itself if `this` is primitive or `java.lang.Object`
    * if `this` is an array or a reference.
    *
    * @return
    */
  def toErased: BackendType = this match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 |
         BackendType.Int32 | BackendType.Int64 | BackendType.Float32 | BackendType.Float64 => this
    case BackendType.Array(_) | BackendType.Reference(_) => BackendObjType.JavaObject.toTpe
  }

  /**
    * A string representing the erased type. This is used for parametrized class names.
    */
  val toErasedString: String = this match {
    case BackendType.Bool => "Bool"
    case BackendType.Char => "Char"
    case BackendType.Int8 => "Int8"
    case BackendType.Int16 => "Int16"
    case BackendType.Int32 => "Int32"
    case BackendType.Int64 => "Int64"
    case BackendType.Float32 => "Float32"
    case BackendType.Float64 => "Float64"
    case BackendType.Array(_) | BackendType.Reference(_) => "Obj"
  }

  def is64BitWidth: Boolean = this match {
    case BackendType.Int64 | BackendType.Float64 => true
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 |
         BackendType.Int32 | BackendType.Float32 | BackendType.Array(_) | BackendType.Reference(_) => false
  }

  /**
    * Returns the Array fill type for the value of the type specified by `tpe`.
    */
  def toArrayFillType: String = s"([${this.toErased.toDescriptor}${this.toErased.toDescriptor})V"

  /**
    * Returns the array store instruction for arrays of the given tpe.
    */
  def getArrayStoreInstruction: Int = this match {
    case BackendType.Bool => Opcodes.BASTORE
    case BackendType.Char => Opcodes.CASTORE
    case BackendType.Int8 => Opcodes.BASTORE
    case BackendType.Int16 => Opcodes.SASTORE
    case BackendType.Int32 => Opcodes.IASTORE
    case BackendType.Int64 => Opcodes.LASTORE
    case BackendType.Float32 => Opcodes.FASTORE
    case BackendType.Float64 => Opcodes.DASTORE
    case BackendType.Reference(_) | BackendType.Array(_) => Opcodes.AASTORE
  }

}

object BackendType {

  case object Bool extends PrimitiveType

  case object Char extends PrimitiveType

  case object Int8 extends PrimitiveType

  case object Int16 extends PrimitiveType

  case object Int32 extends PrimitiveType

  case object Int64 extends PrimitiveType

  case object Float32 extends PrimitiveType

  case object Float64 extends PrimitiveType

  case class Array(tpe: BackendType) extends BackendType

  /**
    * Holds a reference to some object type.
    */
  case class Reference(ref: BackendObjType) extends BackendType {
    def name: JvmName = ref.jvmName
  }

  /**
    * Contains all the primitive types and `Reference(Native(JvmName.Object))`.
    */
  def erasedTypes: List[BackendType] =
    Bool :: Char :: Float32 :: Float64 :: Int8 :: Int16 :: Int32 :: Int64 :: BackendObjType.JavaObject.toTpe :: Nil

  /**
    * Computes the erased `BackendType` based on the given `MonoType`.
    */
  def toErasedBackendType(tpe: MonoType): BackendType = tpe match {
    case MonoType.Bool => Bool
    case MonoType.Char => Char
    case MonoType.Int8 => Int8
    case MonoType.Int16 => Int16
    case MonoType.Int32 => Int32
    case MonoType.Int64 => Int64
    case MonoType.Float32 => Float32
    case MonoType.Float64 => Float64
    case MonoType.Unit | MonoType.BigDecimal | MonoType.BigInt | MonoType.String | MonoType.Regex |
         MonoType.Array(_) | MonoType.Lazy(_) | MonoType.Ref(_) | MonoType.Tuple(_) |
         MonoType.Enum(_) | MonoType.Arrow(_, _) | MonoType.RecordEmpty | MonoType.RecordExtend(_, _, _) |
         MonoType.SchemaEmpty | MonoType.SchemaExtend(_, _, _) | MonoType.Native(_) |
         MonoType.Region => BackendObjType.JavaObject.toTpe
  }

  /**
    * Computes the `BackendType` based on the given `MonoType`.
    */
  def toFlixErasedBackendType(tpe: MonoType): BackendType = tpe match {
    case MonoType.Bool => Bool
    case MonoType.Char => Char
    case MonoType.Int8 => Int8
    case MonoType.Int16 => Int16
    case MonoType.Int32 => Int32
    case MonoType.Int64 => Int64
    case MonoType.Float32 => Float32
    case MonoType.Float64 => Float64
    case MonoType.Array(t) => Array(toFlixErasedBackendType(t))
    case MonoType.BigDecimal => BackendObjType.BigDecimal.toTpe
    case MonoType.BigInt => BackendObjType.BigInt.toTpe
    case MonoType.String => BackendObjType.String.toTpe
    case MonoType.Regex => BackendObjType.Regex.toTpe
    case MonoType.Native(clazz) =>
      // Maybe use clazz.getPackage and clazz.getSimpleName
      // TODO: Ugly hack.
      val fqn = clazz.getName.replace('.', '/')
      BackendObjType.Native(JvmName.mk(fqn)).toTpe
    case MonoType.Unit | MonoType.Lazy(_) | MonoType.Ref(_) |
         MonoType.Tuple(_) | MonoType.Arrow(_, _) | MonoType.RecordEmpty |
         MonoType.RecordExtend(_, _, _) | MonoType.Region | MonoType.Enum(_) |
         MonoType.SchemaEmpty | MonoType.SchemaExtend(_, _, _) => BackendObjType.JavaObject.toTpe
  }

  sealed trait PrimitiveType extends BackendType {

    def toTpe: BackendType = this match {
      case Bool => Bool
      case Char => Char
      case Int8 => Int8
      case Int16 => Int16
      case Int32 => Int32
      case Int64 => Int64
      case Float32 => Float32
      case Float64 => Float64
    }

    def toArrayTypeCode: Int = this match {
      case Bool => Opcodes.T_BOOLEAN
      case Char => Opcodes.T_CHAR
      case Int8 => Opcodes.T_BYTE
      case Int16 => Opcodes.T_SHORT
      case Int32 => Opcodes.T_INT
      case Int64 => Opcodes.T_LONG
      case Float32 => Opcodes.T_FLOAT
      case Float64 => Opcodes.T_DOUBLE
    }
  }
}
