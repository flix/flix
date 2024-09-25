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

import ca.uwaterloo.flix.language.ast.ReducedAst.Root
import ca.uwaterloo.flix.language.ast.{MonoType, ReducedAst, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.Opcodes

import scala.annotation.tailrec

/**
  * Represents all Flix types that are not objects on the JVM (array is an exception).
  */
sealed trait BackendType extends VoidableType {

  def toDescriptor: String = {
    /** Returns the nesting degree of the array type together with the element type. */
    @tailrec
    def findArrayNesting(t: BackendType, arrayNesting: Int): (Int, BackendType) = t match {
      case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 |
           BackendType.Int32 | BackendType.Int64 | BackendType.Float32 | BackendType.Float64 |
           BackendType.Reference(_) => (arrayNesting, t)
      case BackendType.Array(tt) => findArrayNesting(tt, arrayNesting + 1)
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
      case BackendType.Array(tpe) =>
        val (nesting, base) = findArrayNesting(tpe, 1)
        s"${"[" * nesting}${base.toDescriptor}"
      case BackendType.Reference(ref) => ref.toDescriptor
    }
  }

  /**
    * Returns the erased type, either itself if `this` is primitive or `java.lang.Object`
    * if `this` is an array or a reference.
    */
  def toErased: BackendType = this match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 |
         BackendType.Int32 | BackendType.Int64 | BackendType.Float32 | BackendType.Float64 => this
    case BackendType.Array(_) | BackendType.Reference(_) => BackendObjType.JavaObject.toTpe
  }

  /**
    * Returns the erased type represented as [[JvmType]]. Arrays are erased.
    */
  def toErasedJvmType: JvmType = this match {
    case BackendType.Array(_) => JvmType.Object
    case BackendType.Reference(_) => JvmType.Object
    case BackendType.Bool => JvmType.PrimBool
    case BackendType.Char => JvmType.PrimChar
    case BackendType.Int8 => JvmType.PrimByte
    case BackendType.Int16 => JvmType.PrimShort
    case BackendType.Int32 => JvmType.PrimInt
    case BackendType.Int64 => JvmType.PrimLong
    case BackendType.Float32 => JvmType.PrimFloat
    case BackendType.Float64 => JvmType.PrimDouble
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

  /**
    * Denotes whether the type requires 64 bits on the jvm rather than 32 bit.
    * This is important when it takes up two elements on the stack rather than one.
    */
  def is64BitWidth: Boolean = this match {
    case BackendType.Int64 | BackendType.Float64 => true
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 |
         BackendType.Int32 | BackendType.Float32 | BackendType.Array(_) | BackendType.Reference(_) => false
  }

  /**
    * Returns the Array fill type for the value of the type specified by `tpe`.
    */
  def toArrayFillType: String = mkDescriptor(BackendType.Array(this.toErased), this.toErased)(VoidableType.Void).toDescriptor

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
    * Converts the given [[MonoType]] into its [[BackendType]] representation.
    *
    * Note: Instead of using [[toBackendType]] and then [[BackendType.toErased]]
    * use [[toErasedBackendType]].
    */
  def toBackendType(tpe0: MonoType)(implicit root: ReducedAst.Root): BackendType = {
    tpe0 match {
      case MonoType.Void => BackendObjType.JavaObject.toTpe
      case MonoType.AnyType => BackendObjType.JavaObject.toTpe
      case MonoType.Unit => BackendObjType.Unit.toTpe
      case MonoType.Bool => BackendType.Bool
      case MonoType.Char => BackendType.Char
      case MonoType.Float32 => BackendType.Float32
      case MonoType.Float64 => BackendType.Float64
      case MonoType.BigDecimal => BackendObjType.BigDecimal.toTpe
      case MonoType.Int8 => BackendType.Int8
      case MonoType.Int16 => BackendType.Int16
      case MonoType.Int32 => BackendType.Int32
      case MonoType.Int64 => BackendType.Int64
      case MonoType.BigInt => BackendObjType.BigInt.toTpe
      case MonoType.String => BackendObjType.String.toTpe
      case MonoType.Regex => BackendObjType.Regex.toTpe
      case MonoType.Region => BackendObjType.Region.toTpe
      case MonoType.Null => BackendObjType.Native(JvmName.ofClass(classOf[Object])).toTpe
      case MonoType.Array(tpe) => Array(toBackendType(tpe))
      case MonoType.Lazy(tpe) => BackendObjType.Lazy(toBackendType(tpe)).toTpe
      case MonoType.Tuple(elms) => BackendObjType.Tuple(elms.map(toBackendType)).toTpe
      case MonoType.Enum(_, _) => BackendObjType.Tagged.toTpe
      case MonoType.Struct(sym, targs) => BackendObjType.Struct(JvmOps.instantiateStruct(root.structs(sym), targs)).toTpe
      case MonoType.Arrow(args, result) => BackendObjType.Arrow(args.map(toBackendType), toBackendType(result)).toTpe
      case MonoType.RecordEmpty => BackendObjType.RecordEmpty.toTpe
      case MonoType.RecordExtend(_, value, _) => BackendObjType.RecordExtend(toBackendType(value)).toTpe
      case MonoType.Native(clazz) => BackendObjType.Native(JvmName.ofClass(clazz)).toTpe
    }
  }

  /**
    * Contains all the primitive types and `Reference(Native(JvmName.Object))`.
    */
  def erasedTypes: List[BackendType] = List(
    BackendType.Bool,
    BackendType.Char,
    BackendType.Float32,
    BackendType.Float64,
    BackendType.Int8,
    BackendType.Int16,
    BackendType.Int32,
    BackendType.Int64,
    BackendObjType.JavaObject.toTpe,
  )

  /**
    * Computes the erased `BackendType` based on the given `MonoType`.
    */
  def toErasedBackendType(tpe: MonoType): BackendType = tpe match {
    case MonoType.Bool => BackendType.Bool
    case MonoType.Char => BackendType.Char
    case MonoType.Int8 => BackendType.Int8
    case MonoType.Int16 => BackendType.Int16
    case MonoType.Int32 => BackendType.Int32
    case MonoType.Int64 => BackendType.Int64
    case MonoType.Float32 => BackendType.Float32
    case MonoType.Float64 => BackendType.Float64
    case MonoType.Void | MonoType.AnyType | MonoType.Unit | MonoType.BigDecimal | MonoType.BigInt |
         MonoType.String | MonoType.Regex | MonoType.Array(_) | MonoType.Lazy(_) |
         MonoType.Tuple(_) | MonoType.Enum(_, _) | MonoType.Struct(_, _) | MonoType.Arrow(_, _) |
         MonoType.RecordEmpty | MonoType.RecordExtend(_, _, _) | MonoType.Native(_) |
         MonoType.Region | MonoType.Null =>
      BackendObjType.JavaObject.toTpe
  }

  def asErasedBackendType(tpe: MonoType): BackendType = tpe match {
    case MonoType.Bool => BackendType.Bool
    case MonoType.Char => BackendType.Char
    case MonoType.Int8 => BackendType.Int8
    case MonoType.Int16 => BackendType.Int16
    case MonoType.Int32 => BackendType.Int32
    case MonoType.Int64 => BackendType.Int64
    case MonoType.Float32 => BackendType.Float32
    case MonoType.Float64 => BackendType.Float64
    case MonoType.Native(clazz) if clazz == classOf[Object] => BackendObjType.JavaObject.toTpe
    case MonoType.Void | MonoType.AnyType | MonoType.Unit | MonoType.BigDecimal | MonoType.BigInt |
         MonoType.String | MonoType.Regex | MonoType.Array(_) | MonoType.Lazy(_) |
         MonoType.Tuple(_) | MonoType.Enum(_, _) | MonoType.Struct(_, _) | MonoType.Arrow(_, _) |
         MonoType.RecordEmpty | MonoType.RecordExtend(_, _, _) | MonoType.Native(_) |
         MonoType.Region | MonoType.Null =>
      throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
  }

  /**
    * Computes the `BackendType` based on the given `MonoType`.
    * Types are erased except for the types that have built-in support in
    * the Java standard library.
    * Additionally, [[MonoType.Native]] is <b>not</b> erased.
    */
  def toFlixErasedBackendType(tpe: MonoType): BackendType = tpe match {
    case MonoType.Bool => BackendType.Bool
    case MonoType.Char => BackendType.Char
    case MonoType.Int8 => BackendType.Int8
    case MonoType.Int16 => BackendType.Int16
    case MonoType.Int32 => BackendType.Int32
    case MonoType.Int64 => BackendType.Int64
    case MonoType.Float32 => BackendType.Float32
    case MonoType.Float64 => BackendType.Float64
    case MonoType.Array(t) => BackendType.Array(toFlixErasedBackendType(t))
    case MonoType.BigDecimal => BackendObjType.BigDecimal.toTpe
    case MonoType.BigInt => BackendObjType.BigInt.toTpe
    case MonoType.String => BackendObjType.String.toTpe
    case MonoType.Regex => BackendObjType.Regex.toTpe
    case MonoType.Native(clazz) => JvmName.ofClass(clazz).toTpe
    case MonoType.Void | MonoType.AnyType | MonoType.Unit | MonoType.Lazy(_) | MonoType.Tuple(_) |
         MonoType.Arrow(_, _) | MonoType.RecordEmpty | MonoType.RecordExtend(_, _, _) |
         MonoType.Region | MonoType.Enum(_, _) | MonoType.Struct(_, _) | MonoType.Null =>
      BackendObjType.JavaObject.toTpe
  }

  sealed trait PrimitiveType extends BackendType {
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
