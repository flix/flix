/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
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

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api
import ca.uwaterloo.flix.language.ast.Symbol
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
  case class Tag(enum: Symbol.EnumSym, tag: String, value: AnyRef) extends Value with api.Enum {

    def getTag: String = tag

    def getBoxedEnumField: AnyRef = value

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

  /////////////////////////////////////////////////////////////////////////////
  // Equality                                                                //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns `true` if the values of the two given references `ref1` and `ref2` are equal.
    *
    * NB: The type system ensures that only values of the same type can be compared.
    * Hence it is sufficient to only inspect the type of the first argument.
    */
  // TODO: Replace by built in native operator.
  def equal(ref1: AnyRef, ref2: AnyRef): Boolean = (ref1, ref2) match {
    case (Value.Unit, Value.Unit) => true
    case (Value.True, Value.True) => true
    case (Value.False, Value.False) => true
    case (Value.True, Value.False) => false
    case (Value.False, Value.True) => false
    case (Value.Char(lit1), Value.Char(lit2)) => lit1 == lit2
    case (Value.Float32(lit1), Value.Float32(lit2)) => lit1 == lit2
    case (Value.Float64(lit1), Value.Float64(lit2)) => lit1 == lit2
    case (Value.Int8(lit1), Value.Int8(lit2)) => lit1 == lit2
    case (Value.Int16(lit1), Value.Int16(lit2)) => lit1 == lit2
    case (Value.Int32(lit1), Value.Int32(lit2)) => lit1 == lit2
    case (Value.Int64(lit1), Value.Int64(lit2)) => lit1 == lit2
    case (Value.BigInt(lit1), Value.BigInt(lit2)) => lit1 == lit2
    case (Value.Str(lit1), Value.Str(lit2)) => lit1 == lit2
    case (Value.Tag(_, tag1, v1), Value.Tag(_, tag2, v2)) => tag1 == tag2 && equal(v1, v2)
    case (Value.Tuple(elms1), Value.Tuple(elms2)) => elms1.zip(elms2).forall {
      case ((e1, e2)) => equal(e1, e2)
    }
    case (b1: Value.Box, b2: Value.Box) =>
      throw InternalRuntimeException(s"Unable to compare Boxes.")
    case (c1: Value.Closure, c2: Value.Closure) =>
      throw InternalRuntimeException(s"Unable to compare Closures.")
    case _ =>
      val tpe1 = ref1.getClass.getCanonicalName
      val tpe2 = ref2.getClass.getCanonicalName
      throw InternalRuntimeException(s"Unable to compare '$tpe1' and '$tpe2'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Pretty Printing                                                         //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns a pretty printed formatting of the given Flix `ref`.
    */
  // TODO: Replace by built in native operator.
  def pretty(ref: AnyRef): String = ref match {
    case Value.Unit => "()"
    case Value.True => "true"
    case Value.False => "false"
    case Value.Char(lit) => lit.toString
    case Value.Int8(lit) => lit.toString
    case Value.Int16(lit) => lit.toString
    case Value.Int32(lit) => lit.toString
    case Value.Int64(lit) => lit.toString
    case Value.BigInt(lit) => lit.toString
    case Value.Str(lit) => lit
    case Value.Tag(enum, "Cons", Value.Tuple(elms)) => pretty(elms(0)) + " :: " + pretty(elms(1))
    case Value.Tag(enum, tag, Value.Unit) => tag
    case Value.Tag(enum, tag, value: Value.Tuple) => tag + pretty(value)
    case Value.Tag(enum, tag, value) => tag + "(" + pretty(value) + ")"
    case Value.Tuple(elms) => "(" + elms.mkString(", ") + ")"
    case o: java.lang.Boolean => o.booleanValue().toString
    case o: java.lang.Character => o.charValue().toString
    case o: java.lang.Byte => o.byteValue().toString
    case o: java.lang.Short => o.shortValue().toString
    case o: java.lang.Integer => o.intValue().toString
    case o: java.lang.Long => o.longValue().toString
    case o: java.lang.String => "\"" + o + "\""
    case _ => ref.toString
  }

}