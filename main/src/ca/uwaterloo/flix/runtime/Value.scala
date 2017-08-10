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

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.util.InternalRuntimeException

sealed trait Value

//  TODO: Replace AnyRef by value.

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
  case class Tag(enum: Symbol.EnumSym, tag: String, value: AnyRef) extends Value {
    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Tag does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Tag does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Tag does not support `toString`.")
  }

  /**
    * A Tuple value.
    */
  case class Tuple(elms: List[AnyRef]) extends Value {
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
  def equal(ref1: AnyRef, ref2: AnyRef): Boolean = ref1 match {
    case _: Unit.type => ref1 eq ref2
    case _: java.lang.Boolean => ref1.equals(ref2)
    case _: java.lang.Character => ref1.equals(ref2)
    case _: java.lang.Float => ref1.equals(ref2)
    case _: java.lang.Double => ref1.equals(ref2)
    case _: java.lang.Byte => ref1.equals(ref2)
    case _: java.lang.Short => ref1.equals(ref2)
    case _: java.lang.Integer => ref1.equals(ref2)
    case _: java.lang.Long => ref1.equals(ref2)
    case _: java.math.BigInteger => ref1.equals(ref2)
    case _: java.lang.String => ref1.equals(ref2)
    case _: Tag =>
      val v1 = ref1.asInstanceOf[Tag]
      val v2 = ref2.asInstanceOf[Tag]
      v1.tag == v2.tag && equal(v1.value, v2.value)
    case _: Array[AnyRef] =>
      val a1 = ref1.asInstanceOf[Array[AnyRef]]
      val a2 = ref2.asInstanceOf[Array[AnyRef]]
      assert(a1.length == a2.length)
      var i = 0
      while (i < a1.length) {
        if (!equal(a1(i), a2(i))) {
          return false
        }
        i = i + 1
      }
      return true
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
    case o: java.lang.Boolean => o.booleanValue().toString
    case o: java.lang.Character => o.charValue().toString
    case o: java.lang.Byte => o.byteValue().toString
    case o: java.lang.Short => o.shortValue().toString
    case o: java.lang.Integer => o.intValue().toString
    case o: java.lang.Long => o.longValue().toString
    case o: java.lang.String => "\"" + o + "\""
    case o: Value.Tag =>
      if (o.tag == "Cons") {
        val e1 = o.value.asInstanceOf[Array[AnyRef]](0)
        val e2 = o.value.asInstanceOf[Array[AnyRef]](1)
        s"${pretty(e1)} :: ${pretty(e2)}"
      }
      else {
        if (o.value.isInstanceOf[Value.Unit.type]) {
          s"${o.tag}"
        } else if (o.value.isInstanceOf[Array[AnyRef]]) {
          s"${o.tag}(${o.value.asInstanceOf[Array[AnyRef]].map(pretty).mkString(", ")})"
        } else {
          s"${o.tag}(${pretty(o.value)})"
        }
      }
    case o: Array[AnyRef] =>
      "(" + o.toList.map(pretty).mkString(", ") + ")"
    case _ => ref.toString
  }

}
