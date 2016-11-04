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

import java.util

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.util.InternalRuntimeException

import scala.collection.{immutable, mutable}

object Value {

  /////////////////////////////////////////////////////////////////////////////
  // Unit                                                                    //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * The Unit value.
    */
  object Unit

  /////////////////////////////////////////////////////////////////////////////
  // Bools                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * The true value.
    */
  @inline
  val True: AnyRef = java.lang.Boolean.TRUE

  /**
    * The false value.
    */
  @inline
  val False: AnyRef = java.lang.Boolean.FALSE

  /**
    * Constructs a bool from the given boolean `b`.
    */
  @inline
  def mkBool(b: Boolean): AnyRef = if (b) True else False

  /**
    * Casts the given reference `ref` to a primitive boolean.
    */
  @inline
  def cast2bool(ref: Any): Boolean = ref match {
    case o: java.lang.Boolean => o.booleanValue()
    case _ => throw new InternalRuntimeException(s"Unexpected non-bool value: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Chars                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Constructs a char value from the given char `c`.
    */
  @inline
  def mkChar(c: Char): AnyRef = new java.lang.Character(c)

  /**
    * Casts the given reference `ref` to a primitive char.
    */
  @inline
  def cast2char(ref: AnyRef): Char = ref match {
    case o: java.lang.Character => o.charValue()
    case _ => throw new InternalRuntimeException(s"Unexpected non-char value: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Floats                                                                  //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Constructs a float32 value from the given float `f`.
    */
  @inline
  def mkFloat32(f: Float): AnyRef = new java.lang.Float(f)

  /**
    * Constructs a float32 value from the given double `d`.
    */
  @inline
  def mkFloat32(d: Double): AnyRef = new java.lang.Float(d.asInstanceOf[Float])

  /**
    * Constructs a float64 value from the given double `d`.
    */
  @inline
  def mkFloat64(d: Double): AnyRef = new java.lang.Double(d)

  /**
    * Casts the given reference `ref` to a Float32.
    */
  @inline
  def cast2float32(ref: AnyRef): Float = ref match {
    case o: java.lang.Float => o.floatValue()
    case _ => throw new InternalRuntimeException(s"Unexpected non-float32 value: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to a Float64.
    */
  @inline
  def cast2float64(ref: AnyRef): Double = ref match {
    case o: java.lang.Double => o.doubleValue()
    case _ => throw new InternalRuntimeException(s"Unexpected non-float64 value: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Ints                                                                    //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Constructs an int8 value from the given byte `b`.
    */
  @inline
  def mkInt8(b: Byte): AnyRef = new java.lang.Byte(b)

  /**
    * Constructs an int8 from the given int `i`.
    */
  @inline
  def mkInt8(i: Int): AnyRef = new java.lang.Byte(i.asInstanceOf[Byte])

  /**
    * Constructs an int16 from the given short `s`.
    */
  @inline
  def mkInt16(s: Short): AnyRef = new java.lang.Short(s)

  /**
    * Constructs an int16 form the given int `i`.
    */
  @inline
  def mkInt16(i: Int): AnyRef = new java.lang.Short(i.asInstanceOf[Short])

  /**
    * Constructs an int32 from the given int `i`.
    */
  @inline
  def mkInt32(i: Int): AnyRef = new java.lang.Integer(i)

  /**
    * Constructs an int64 from the given int `i`.
    */
  @inline
  def mkInt64(i: Int): AnyRef = new java.lang.Long(i)

  /**
    * Constructs an int64 from the given long `l`.
    */
  @inline
  def mkInt64(l: Long): AnyRef = new java.lang.Long(l)

  /**
    * Constructs a java.math.BigInteger from the given int `i`.
    */
  @inline
  def mkBigInt(i: Int): AnyRef = java.math.BigInteger.valueOf(i)

  /**
    * Constructs a java.math.BigInteger from the given long `l`.
    */
  @inline
  def mkBigInt(l: Long): AnyRef = java.math.BigInteger.valueOf(l)

  /**
    * Constructs a java.math.BigInteger from the given string `s`.
    */
  @inline
  def mkBigInt(s: String): AnyRef = new java.math.BigInteger(s)

  /**
    * Constructs the Flix representation of a java.math.BigInteger for the given `ref`.
    */
  @inline
  def mkBigInt(ref: AnyRef): AnyRef = ref match {
    case o: java.math.BigInteger => o
    case _ => throw new InternalRuntimeException(s"Unexpected non-bigint value: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int8.
    */
  @inline
  def cast2int8(ref: AnyRef): Byte = ref match {
    case o: java.lang.Byte => o.byteValue()
    case _ => throw new InternalRuntimeException(s"Unexpected non-int8 value: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int16.
    */
  @inline
  def cast2int16(ref: AnyRef): Short = ref match {
    case o: java.lang.Short => o.shortValue()
    case _ => throw new InternalRuntimeException(s"Unexpected non-int16 value: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int32.
    */
  @inline
  def cast2int32(ref: AnyRef): Int = ref match {
    case o: java.lang.Integer => o.intValue()
    case _ => throw new InternalRuntimeException(s"Unexpected non-int32 value: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int64.
    */
  @inline
  def cast2int64(ref: AnyRef): Long = ref match {
    case o: java.lang.Long => o.longValue()
    case _ => throw new InternalRuntimeException(s"Unexpected non-int64 value: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to a java.math.BigInteger.
    */
  @inline
  def cast2bigInt(ref: AnyRef): java.math.BigInteger = ref match {
    case o: java.math.BigInteger => o
    case _ => throw new InternalRuntimeException(s"Unexpected non-bigint value: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Closures                                                                //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Flix internal representation of closures.
    */
  final case class Closure(name: Symbol.DefnSym, bindings: Array[AnyRef])

  // TODO: Introduce make function and make Closure constructor private.

  /**
    * Casts the given reference `ref` to a closure.
    */
  @inline
  def cast2closure(ref: AnyRef): Closure = ref match {
    case o: Closure => o
    case _ => throw new InternalRuntimeException(s"Unexpected non-closure value: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Strings                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Constructs a str from the given string `s`.
    */
  @inline
  def mkStr(s: String): AnyRef = s.intern()

  /**
    * Casts the given reference `ref` to a string.
    */
  @inline
  def cast2str(ref: AnyRef): String = ref match {
    case o: java.lang.String => o
    case _ => throw new InternalRuntimeException(s"Unexpected non-string value: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Tags                                                                    //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Flix internal representation of tags.
    */
  final class Tag private[Value](val tag: java.lang.String, val value: AnyRef) {
    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Tag => this.tag == that.tag && this.value == that.value
      case _ => false
    }

    override def hashCode: Int = 7 * tag.hashCode + 11 * value.hashCode

    override def toString: java.lang.String = s"Value.Tag($tag, $value)"
  }

  /**
    * Constructs the tag for the given tag `t` and value `v`.
    */
  def mkTag(t: java.lang.String, v: AnyRef): Value.Tag = new Value.Tag(t, v)

  /**
    * Constructs a tag for the List type.
    */
  def mkNil(): Value.Tag = mkTag("Nil", Unit)

  def mkCons(v: AnyRef, vs: AnyRef): Value.Tag = mkTag("Cons", Tuple(Array(v,vs)))

  def mkList(as: List[AnyRef]): Value.Tag = as.foldRight(mkNil()) {
    case (a, l) => mkCons(a, l)
  }

  /**
    * Constructs a tag for the Option type.
    */
  def mkNone(): Value.Tag = mkTag("None", Unit)

  def mkSome(v: AnyRef): Value.Tag = mkTag("Some", v)

  /**
    * Constructs a tag for the Result type.
    */
  def mkOk(v: AnyRef): Value.Tag = mkTag("Ok", v)

  def mkErr(v: AnyRef): Value.Tag = mkTag("Err", v)

  /**
    * Casts the given reference `ref` to a tag.
    */
  @inline
  def cast2tag(ref: AnyRef): Value.Tag = ref match {
    case v: Value.Tag => v
    case _ => throw new InternalRuntimeException(s"Unexpected non-tag value: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Tuples                                                                  //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Remove
  final case class Tuple(elms: Array[AnyRef]) {
    override def toString: java.lang.String = s"Value.Tuple(Array(${elms.mkString(",")}))"

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Value.Tuple =>
        util.Arrays.equals(this.elms.asInstanceOf[Array[AnyRef]], that.elms)
      case _ => false
    }

    override def hashCode: Int = util.Arrays.hashCode(elms.asInstanceOf[Array[AnyRef]])
  }

  /**
    * Casts the given reference `ref` to a tuple.
    */
  @inline
  def cast2tuple(ref: AnyRef): Array[AnyRef] = ref match {
    case o: Array[AnyRef] => o
    case o: Tuple => o.elms // TODO: remove
    case o: Product => o.productIterator.toArray.asInstanceOf[Array[AnyRef]] // TODO: See if this works, and think about better solution.
    case _ => throw new InternalRuntimeException(s"Unexpected non-tuple value: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Opt, List, Set, Map                                                     //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Constructs the Flix representation of a set for the given `ref`.
    */
  @inline
  def mkSet(ref: AnyRef): AnyRef = ref match {
    case o: immutable.Set[_] => o
    case _ => throw new InternalRuntimeException(s"Unexpected non-set value: '$ref'.")
  }

  /**
    * Constructs the Flix representation of a map for the given `ref`.
    */
  @inline
  def mkMap(ref: AnyRef): AnyRef = ref match {
    case o: immutable.Map[_, _] => o
    case _ => throw new InternalRuntimeException(s"Unexpected non-map value: '$ref'.")
  }

  /**
    * Casts the given `ref` to a Flix set value.
    */
  @inline
  def cast2set(ref: AnyRef): immutable.Set[AnyRef] = ref match {
    case o: immutable.Set[AnyRef]@unchecked => o
    case _ => throw new InternalRuntimeException(s"Unexpected non-set value: '$ref'.")
  }

  /**
    * Casts the given `ref` to a Flix map value.
    */
  @inline
  def cast2map(ref: AnyRef): immutable.Map[AnyRef, AnyRef] = ref match {
    case o: immutable.Map[AnyRef, AnyRef]@unchecked => o
    case _ => throw new InternalRuntimeException(s"Unexpected non-map value: '$ref'.")
  }

  /**
    * Casts the given `ref` to a Flix value.
    */
  // TODO: This is temporary fix until we figure out a proper way to deal with non-flix values.
  @inline
  def cast2flix(ref: AnyRef): AnyRef = ref match {
    case Tuple1(a: AnyRef) => Value.Tuple(Array(a))
    case Tuple2(a: AnyRef, b: AnyRef) => Value.Tuple(Array(a, b))
    case Tuple3(a: AnyRef, b: AnyRef, c: AnyRef) => Value.Tuple(Array(a, b, c))
    case Tuple4(a: AnyRef, b: AnyRef, c: AnyRef, d: AnyRef) => Value.Tuple(Array(a, b, c, d))
    case _ => ref
  }

  /////////////////////////////////////////////////////////////////////////////
  // Pretty Printing                                                         //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns a pretty printed formatting of the given Flix `ref`.
    */
  def pretty(ref: AnyRef): String = ref match {
    case Value.Unit => "()"
    case o: java.lang.Boolean => o.booleanValue().toString
    case o: java.lang.Character => o.charValue().toString
    case o: java.lang.Byte => o.byteValue().toString
    case o: java.lang.Short => o.shortValue().toString
    case o: java.lang.Integer => o.intValue().toString
    case o: java.lang.Long => o.longValue().toString
    case o: java.lang.String => o
    case o: Value.Tag => s"${o.tag}(${pretty(o.value)})"
    case Value.Tuple(elms) => "(" + elms.map(pretty).mkString(",") + ")"
    case _ => ref.toString
  }

}
