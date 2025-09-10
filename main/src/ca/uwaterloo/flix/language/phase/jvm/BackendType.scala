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

import ca.uwaterloo.flix.language.ast.{ReducedAst, SimpleType}

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
    case BackendType.Array(_) | BackendType.Reference(_) => BackendType.Object
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
    * Returns `2` if `this` is a 64 bit value, or `1` if it is a 32 bit value.
    */
  def stackSlots: Int = if (is64BitWidth) 2 else 1

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

  val Object: BackendType = Reference(BackendObjType.Native(JvmName.Object))
  val String: BackendType = Reference(BackendObjType.Native(JvmName.String))

  /**
    * Converts the given [[SimpleType]] into its [[BackendType]] representation.
    *
    * Note: Instead of using [[toBackendType]] and then [[BackendType.toErased]]
    * use [[toErasedBackendType]].
    */
  def toBackendType(tpe0: SimpleType)(implicit root: ReducedAst.Root): BackendType = {
    tpe0 match {
      case SimpleType.Void => BackendType.Object
      case SimpleType.AnyType => BackendType.Object
      case SimpleType.Unit => BackendObjType.Unit.toTpe
      case SimpleType.Bool => BackendType.Bool
      case SimpleType.Char => BackendType.Char
      case SimpleType.Float32 => BackendType.Float32
      case SimpleType.Float64 => BackendType.Float64
      case SimpleType.BigDecimal => JvmName.BigDecimal.toTpe
      case SimpleType.Int8 => BackendType.Int8
      case SimpleType.Int16 => BackendType.Int16
      case SimpleType.Int32 => BackendType.Int32
      case SimpleType.Int64 => BackendType.Int64
      case SimpleType.BigInt => JvmName.BigInteger.toTpe
      case SimpleType.String => BackendType.String
      case SimpleType.Regex => JvmName.Regex.toTpe
      case SimpleType.Region => BackendObjType.Region.toTpe
      case SimpleType.Null => BackendType.Object
      case SimpleType.Array(tpe) => Array(toBackendType(tpe))
      case SimpleType.Lazy(tpe) => BackendObjType.Lazy(toBackendType(tpe)).toTpe
      case SimpleType.Tuple(elms) => BackendObjType.Tuple(elms.map(toBackendType)).toTpe
      case SimpleType.Enum(_, _) => BackendObjType.Tagged.toTpe
      case SimpleType.Struct(sym, targs) => BackendObjType.Struct(JvmOps.instantiateStruct(sym, targs)).toTpe
      case SimpleType.Arrow(args, result) => BackendObjType.Arrow(args.map(toBackendType), toBackendType(result)).toTpe
      case SimpleType.RecordEmpty => BackendObjType.Record.toTpe
      case SimpleType.RecordExtend(_, _, _) => BackendObjType.Record.toTpe
      case SimpleType.ExtensibleEmpty => BackendObjType.Tagged.toTpe
      case SimpleType.ExtensibleExtend(_, _, _) => BackendObjType.Tagged.toTpe
      case SimpleType.Native(clazz) => BackendObjType.Native(JvmName.ofClass(clazz)).toTpe
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
    BackendType.Object,
  )

  /**
    * Computes the erased [[BackendType]] based on the given [[SimpleType]].
    */
  def toErasedBackendType(tpe: SimpleType): BackendType = tpe match {
    case SimpleType.Bool => BackendType.Bool
    case SimpleType.Char => BackendType.Char
    case SimpleType.Int8 => BackendType.Int8
    case SimpleType.Int16 => BackendType.Int16
    case SimpleType.Int32 => BackendType.Int32
    case SimpleType.Int64 => BackendType.Int64
    case SimpleType.Float32 => BackendType.Float32
    case SimpleType.Float64 => BackendType.Float64
    case SimpleType.Void | SimpleType.AnyType | SimpleType.Unit | SimpleType.BigDecimal | SimpleType.BigInt |
         SimpleType.String | SimpleType.Regex | SimpleType.Array(_) | SimpleType.Lazy(_) |
         SimpleType.Tuple(_) | SimpleType.Enum(_, _) | SimpleType.Struct(_, _) | SimpleType.Arrow(_, _) |
         SimpleType.RecordEmpty | SimpleType.RecordExtend(_, _, _) |
         SimpleType.ExtensibleExtend(_, _, _) | SimpleType.ExtensibleEmpty | SimpleType.Native(_) |
         SimpleType.Region | SimpleType.Null =>
      BackendType.Object
  }

  sealed trait PrimitiveType extends BackendType
}
