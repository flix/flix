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

import ca.uwaterloo.flix.util.{ConcurrentCache, InternalCompilerException}

import scala.annotation.tailrec

/**
  * Representation of simplified types.
  *
  * A simple type has no variables or effects.
  */
sealed trait SimpleType

object SimpleType {

  /**
    * A private concurrent cache.
    *
    * Note: We do not cache all simple types because it is not worth it.
    */
  private val Cache: ConcurrentCache[SimpleType] = new ConcurrentCache()

  //
  // Primitive Types.
  //



  //
  // Compound Types.
  //

  val Object: SimpleType = Native(classOf[java.lang.Object])



  /**
    * Smart constructor for [[SimpleType.Array]].
    */
  def mkArray(tpe: SimpleType): SimpleType.Array = {
    val t = Array(tpe)
    Cache.getCanonicalValue(t)
  }

  /**
    * Smart constructor for [[SimpleType.Tuple]].
    */
  def mkTuple(tpes: List[SimpleType]): SimpleType.Tuple = {
    val t = Tuple(tpes)
    Cache.getCanonicalValue(t)
  }

  /**
    * Smart constructor for [[SimpleType.Arrow]].
    */
  def mkArrow(targs: List[SimpleType], result: SimpleType): SimpleType.Arrow = {
    val t = Arrow(targs, result)
    Cache.getCanonicalValue(t)
  }

  /**
    * Smart constructor for [[SimpleType.Enum]].
    */
  def mkEnum(sym: Symbol.EnumSym, targs: List[SimpleType]): SimpleType.Enum = {
    val t = Enum(sym, targs)
    Cache.getCanonicalValue(t)
  }

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
