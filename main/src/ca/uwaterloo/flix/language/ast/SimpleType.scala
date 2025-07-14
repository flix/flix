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

import ca.uwaterloo.flix.util.InternalCompilerException

import scala.annotation.tailrec

/**
  * Representation of simplified types.
  *
  * A simple type has no variables or effects.
  */
sealed trait SimpleType

object SimpleType {

  //
  // Primitive Types.
  //

  /**
    * Represents an uninhabited type, not an absent value like in Java.
    */
  case object Void extends SimpleType

  case object AnyType extends SimpleType

  case object Unit extends SimpleType

  case object Bool extends SimpleType

  case object Char extends SimpleType

  case object Float32 extends SimpleType

  case object Float64 extends SimpleType

  case object BigDecimal extends SimpleType

  case object Int8 extends SimpleType

  case object Int16 extends SimpleType

  case object Int32 extends SimpleType

  case object Int64 extends SimpleType

  case object BigInt extends SimpleType

  case object String extends SimpleType

  case object Regex extends SimpleType

  case object Region extends SimpleType

  case object Null extends SimpleType

  //
  // Compound Types.
  //

  case class Array(tpe: SimpleType) extends SimpleType

  case class Lazy(tpe: SimpleType) extends SimpleType

  case class Tuple(tpes: List[SimpleType]) extends SimpleType

  case class Enum(sym: Symbol.EnumSym, targs: List[SimpleType]) extends SimpleType

  case class Struct(sym: Symbol.StructSym, targs: List[SimpleType]) extends SimpleType

  case class Arrow(args: List[SimpleType], result: SimpleType) extends SimpleType

  case object RecordEmpty extends SimpleType

  case class RecordExtend(label: String, value: SimpleType, rest: SimpleType) extends SimpleType

  case object ExtensibleEmpty extends SimpleType

  case class ExtensibleExtend(cons: Name.Pred, tpes: List[SimpleType], rest: SimpleType) extends SimpleType

  case class Native(clazz: Class[?]) extends SimpleType

  val Object: SimpleType = Native(classOf[java.lang.Object])

  /** Returns `tpe` if it's a primitive type and returns [[SimpleType.Object]] otherwise. */
  def erase(tpe: SimpleType): SimpleType = {
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
           RecordExtend(_, _, _) | ExtensibleEmpty | ExtensibleExtend(_, _, _) | Native(_) | Null =>
        SimpleType.Object
    }
  }

  /** All the erased JVM types. */
  val ErasedTypes: Set[SimpleType] = Set(
    Bool, Char, Float32, Float64, Int8, Int16, Int32, Int64, SimpleType.Object
  )

  /**
    * Returns the term types of extensible constructor `cons` in `tpe`.
    *
    * N.B.: `tpe` must be a chain of [[SimpleType.ExtensibleExtend]] that contains `cons`.
    */
  @tailrec
  def findExtensibleTermTypes(cons: Name.Label, tpe: SimpleType): List[SimpleType] = tpe match {
    case SimpleType.ExtensibleExtend(pred, tpes, rest) =>
      if (pred.name == cons.name) tpes
      else findExtensibleTermTypes(cons, rest)
    case other =>
      throw InternalCompilerException(s"Unexpected type: '$other'", cons.loc)
  }

}
