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

import ca.uwaterloo.flix.api.TagInterface
import ca.uwaterloo.flix.language.ast.ExecutableAst.Pattern
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.util.InternalRuntimeException

import scala.collection.immutable

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
  def mkStr(s: String): AnyRef = s

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
  final class Tag(val tag: java.lang.String, val value: AnyRef) {
    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Tag => this.tag == that.tag && equal(this.value, that.value)
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
    * Returns the Nil element of a list.
    */
  def mkNil: Value.Tag = mkTag("Nil", Unit)

  /**
    * Returns the list `vs` with the element `v` prepended.
    */
  def mkCons(v: AnyRef, vs: AnyRef): Value.Tag = mkTag("Cons", Array(v, vs))

  /**
    * Returns the given Scala list `as` as a Flix list.
    */
  def mkList(as: List[AnyRef]): Value.Tag = as.foldRight(mkNil) {
    case (a, l) => mkCons(a, l)
  }

  /**
    * Returns the None element of a Flix Option.
    */
  def mkNone(): Value.Tag = mkTag("None", Unit)

  /**
    * Returns `Some(v)` of the given value `v`.
    */
  def mkSome(v: AnyRef): Value.Tag = mkTag("Some", v)

  /**
    * Returns `Ok(v)` of the given value `v`.
    */
  def mkOk(v: AnyRef): Value.Tag = mkTag("Ok", v)

  /**
    * Returns `Err(v)` of the given value `v`.
    */
  def mkErr(v: AnyRef): Value.Tag = mkTag("Err", v)

  /**
    * Returns the given Scala list `as` as a Flix set.
    */
  def mkFlixSet(as: List[AnyRef]): Value.Tag = mkTag("Set", mkList(as))

  /**
    * Casts the given reference `ref` to a tag.
    */
  @inline
  def cast2tag(ref: AnyRef): Value.Tag = ref match {
    case v: Value.Tag => v
    case _ => throw new InternalRuntimeException(s"Unexpected non-tag value: '$ref'.")
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

  /////////////////////////////////////////////////////////////////////////////
  // Equality                                                                //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns `true` if the values of the two given references `ref1` and `ref2` are equal.
    *
    * NB: The type system ensures that only values of the same type can be compared.
    * Hence it is sufficient to only inspect the type of the first argument.
    */
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
      val tpe2 = ref2.getClass.getCanonicalName/*
      //TODO: WE SHOULD GET RID OF THIS WHEN WE REMOVE DEPENDENCY OF CODE GEN ON INTERPRETER
      //TODO: For now it just decreases number of false tests
      if(tpe1.split('.').take(3).toList == List("ca","waterloo", "flix") &&
        tpe2.split('.').take(3).toList == List("ca","waterloo", "flix")) {
        return ref1.equals(ref2)
      } else*/
        throw InternalRuntimeException(s"Unable to compare '$tpe1' and '$tpe2'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Iterators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Return an iterator over the given Set.
    */
  def iteratorOf(value: AnyRef): Iterator[AnyRef] = {
    def visit(o: AnyRef): List[AnyRef] = {
      val taggedValue = o.asInstanceOf[Value.Tag]
      if (taggedValue.tag == "Nil") {
        Nil
      } else {
        val hd = taggedValue.value.asInstanceOf[Array[AnyRef]](0)
        val tl = taggedValue.value.asInstanceOf[Array[AnyRef]](1)
        hd :: visit(tl)
      }
    }

    val list = value.asInstanceOf[Value.Tag].value
    visit(list).iterator
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unification                                                             //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Tries to unify the given pattern `p0` with the given value `v0` under the environment `env0`.
    *
    * Mutates the given map. Returns `true` if unification was successful.
    */
  def unify(p0: Pattern, v0: AnyRef, env0: Array[AnyRef]): Boolean = (p0, v0) match {
    case (Pattern.Wild(_, _), _) => true
    case (Pattern.Var(sym, _, _), _) =>
      val v2 = env0(sym.getStackOffset)
      if (v2 == null) {
        env0(sym.getStackOffset) = v0
        true
      } else {
        Value.equal(v0, v2)
      }
    case (Pattern.Unit(_), Value.Unit) => true
    case (Pattern.True(_), java.lang.Boolean.TRUE) => true
    case (Pattern.False(_), java.lang.Boolean.FALSE) => true
    case (Pattern.Char(lit, _), o: java.lang.Character) => lit == o.charValue()
    case (Pattern.Float32(lit, _), o: java.lang.Float) => lit == o.floatValue()
    case (Pattern.Float64(lit, _), o: java.lang.Double) => lit == o.doubleValue()
    case (Pattern.Int8(lit, _), o: java.lang.Byte) => lit == o.byteValue()
    case (Pattern.Int16(lit, _), o: java.lang.Short) => lit == o.shortValue()
    case (Pattern.Int32(lit, _), o: java.lang.Integer) => lit == o.intValue()
    case (Pattern.Int64(lit, _), o: java.lang.Long) => lit == o.longValue()
    case (Pattern.BigInt(lit, _), o: java.math.BigInteger) => lit.equals(o)
    case (Pattern.Str(lit, _), o: java.lang.String) => lit.equals(o)
    case (Pattern.Tag(enum, tag, p, _, _), o: Value.Tag) => if (tag.equals(o.tag)) unify(p, o.value, env0) else false
    case (Pattern.Tag(enum, tag, p, _, _), o: TagInterface) => if(tag == o.getTag) unify(p, o.getBoxedValue, env0) else false
    case (Pattern.Tuple(elms, _, _), o: Array[AnyRef]) =>
      if (elms.length != o.length)
        return false
      var i: Int = 0
      while (i < o.length) {
        val pi = elms(i)
        val vi = o(i)
        val success = unify(pi, vi, env0)
        if (!success)
          return false
        i = i + 1
      }
      true
    case _ =>
      // Unification failed. Return `null`.
      false
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
