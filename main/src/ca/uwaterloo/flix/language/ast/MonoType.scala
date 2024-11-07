/*
 * Copyright 2019 Magnus Madsen
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

/**
  * Representation of monomorphed types.
  *
  * A mono type is a simplified representation of a type without any type variables.
  */
sealed trait MonoType

object MonoType {

  //
  // Primitive Types.
  //

  /**
    * Represents an uninhabited type, not an absent value like in Java.
    */
  case object Void extends MonoType

  case object AnyType extends MonoType

  case object Unit extends MonoType

  case object Bool extends MonoType

  case object Char extends MonoType

  case object Float32 extends MonoType

  case object Float64 extends MonoType

  case object BigDecimal extends MonoType

  case object Int8 extends MonoType

  case object Int16 extends MonoType

  case object Int32 extends MonoType

  case object Int64 extends MonoType

  case object BigInt extends MonoType

  case object String extends MonoType

  case object Regex extends MonoType

  case object Region extends MonoType

  case object Null extends MonoType

  //
  // Compound Types.
  //

  case class Array(tpe: MonoType) extends MonoType

  case class Lazy(tpe: MonoType) extends MonoType

  case class Tuple(tpes: List[MonoType]) extends MonoType

  case class Enum(sym: Symbol.EnumSym, targs: List[MonoType]) extends MonoType

  case class Struct(sym: Symbol.StructSym, targs: List[MonoType]) extends MonoType

  case class Arrow(args: List[MonoType], result: MonoType) extends MonoType

  case object RecordEmpty extends MonoType

  case class RecordExtend(label: String, value: MonoType, rest: MonoType) extends MonoType

  case class Native(clazz: Class[?]) extends MonoType

  val Object: MonoType = Native(classOf[java.lang.Object])

  /** Returns `tpe` if it's a primitive type and returns [[MonoType.Object]] otherwise. */
  def erase(tpe: MonoType): MonoType = {
    tpe match {
      case Bool => Bool
      case Char => Char
      case Float32 => Float32
      case Float64 => Float64
      case Int8 => Int8
      case Int16 => Int16
      case Int32 => Int32
      case Int64 => Int64
      case Void | AnyType | Unit | BigDecimal | BigInt | String | Regex | Region | Array(_) |
           Lazy(_) | Tuple(_) | Enum(_, _) | Struct(_, _) | Arrow(_, _) | RecordEmpty |
           RecordExtend(_, _, _) | Native(_) | Null =>
        MonoType.Object
    }
  }

  /** All the erased JVM types. */
  val ErasedTypes: Set[MonoType] = Set(
    Bool, Char, Float32, Float64, Int8, Int16, Int32, Int64, MonoType.Object
  )

}
