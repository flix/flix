/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.interpreter

import ca.uwaterloo.flix.api
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.util.InternalRuntimeException

sealed trait Value

object Value {

  /**
    * The `Unit` value.
    */
  object Unit extends Value

  /**
    * The `True` value.
    */
  object True extends Value

  /**
    * The `False` value.
    */
  object False extends Value

  /**
    * A character value.
    */
  case class Char(lit: scala.Char) extends Value

  /**
    * A Float32 value.
    */
  case class Float32(lit: scala.Float) extends Value

  /**
    * A Float64 value.
    */
  case class Float64(lit: scala.Double) extends Value

  /**
    * An Int8 value.
    */
  case class Int8(lit: scala.Byte) extends Value

  /**
    * An Int16 value.
    */
  case class Int16(lit: scala.Short) extends Value

  /**
    * An Int32 value.
    */
  case class Int32(lit: scala.Int) extends Value

  /**
    * An Int64 value.
    */
  case class Int64(lit: scala.Long) extends Value

  /**
    * A BigInt value.
    */
  case class BigInt(lit: java.math.BigInteger) extends Value

  /**
    * A String value.
    */
  case class Str(lit: java.lang.String) extends Value

  /**
    * A Boxed value.
    */
  class Box extends Value {
    /**
      * The internal value of the box.
      */
    private var value: AnyRef = _

    /**
      * Returns the value inside the box.
      */
    def getValue: AnyRef = value

    /**
      * Mutates the value inside the box.
      */
    def setValue(x: AnyRef): Unit = {
      value = x
    }

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Box does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Box does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Box does not support `toString`.")
  }

  /**
    * A Closure value.
    */
  case class Closure(sym: Symbol.DefnSym, bindings: Array[AnyRef]) extends Value {
    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Closure does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Closure does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Closure does not support `toString`.")
  }

  /**
    * Flix internal representation of tags.
    */
  case class Tag(enum: Symbol.EnumSym, tag: String, value: AnyRef) extends Value with api.Tag {
    def getTag: String = tag

    def getBoxedTagValue: AnyRef = value

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Tag does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Tag does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Tag does not support `toString`.")
  }

  /**
    * A Tuple value.
    */
  case class Tuple(elms: List[AnyRef]) extends Value with api.Tuple {
    def getBoxedValue: Array[AnyRef] = elms.toArray

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Tuple does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Tuple does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Tuple does not support `toString`.")
  }

  case class Arr(elms: Array[AnyRef], tpe: Type){
    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Arr does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Arr does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Arr does not support `toString`.")
  }

}
