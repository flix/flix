/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.util.BitOps

import scala.annotation.switch

package object datastore {

  /**
    * A common super-type for keys.
    */
  sealed trait Key[ValueType] {

    def toArray: Array[ValueType] = this match {
      case k: Key1[_] =>
        val a = new Array[Any](1)
        a(0) = k.v0
        a.asInstanceOf[Array[ValueType]]
      case k: Key2[_] =>
        val a = new Array[Any](2)
        a(0) = k.v0
        a(1) = k.v1
        a.asInstanceOf[Array[ValueType]]
      case k: Key3[_] =>
        val a = new Array[Any](3)
        a(0) = k.v0
        a(1) = k.v1
        a(2) = k.v2
        a.asInstanceOf[Array[ValueType]]
      case k: Key4[_] =>
        val a = new Array[Any](4)
        a(0) = k.v0
        a(1) = k.v1
        a(2) = k.v2
        a(3) = k.v3
        a.asInstanceOf[Array[ValueType]]
      case k: Key5[_] =>
        val a = new Array[Any](5)
        a(0) = k.v0
        a(1) = k.v1
        a(2) = k.v2
        a(3) = k.v3
        a(4) = k.v4
        a.asInstanceOf[Array[ValueType]]
    }
  }

  /**
    * A key with one value.
    */
  final class Key1[ValueType](val v0: ValueType, eq: Array[(AnyRef, AnyRef) => Boolean]) extends Key[ValueType] {
    assert(eq.length == 1)

    override def equals(o: scala.Any): Boolean = o match {
      case that: Key1[_] =>
        eq(0)(this.v0.asInstanceOf[AnyRef], that.v0.asInstanceOf[AnyRef])
      case _ => false
    }

    override val hashCode: Int = 3 * v0.hashCode()
  }

  /**
    * A key with two values.
    */
  final class Key2[ValueType](val v0: ValueType, val v1: ValueType, eq: Array[(AnyRef, AnyRef) => Boolean]) extends Key[ValueType] {
    assert(eq.length == 2)

    override def equals(o: scala.Any): Boolean = o match {
      case that: Key2[_] =>
        eq(0)(this.v0.asInstanceOf[AnyRef], that.v0.asInstanceOf[AnyRef]) &&
          eq(1)(this.v1.asInstanceOf[AnyRef], that.v1.asInstanceOf[AnyRef])
      case _ => false
    }

    override val hashCode: Int = 3 * v0.hashCode() + 5 * v1.hashCode()
  }

  /**
    * A key with three values.
    */
  final class Key3[ValueType](val v0: ValueType, val v1: ValueType, val v2: ValueType, eq: Array[(AnyRef, AnyRef) => Boolean]) extends Key[ValueType] {
    assert(eq.length == 3)

    override def equals(o: scala.Any): Boolean = o match {
      case that: Key3[_] =>
        eq(0)(this.v0.asInstanceOf[AnyRef], that.v0.asInstanceOf[AnyRef]) &&
          eq(1)(this.v1.asInstanceOf[AnyRef], that.v1.asInstanceOf[AnyRef]) &&
          eq(2)(this.v2.asInstanceOf[AnyRef], that.v2.asInstanceOf[AnyRef])
      case _ => false
    }

    override val hashCode: Int = 3 * v0.hashCode() + 5 * v1.hashCode() + 7 * v2.hashCode()
  }

  /**
    * A key with four values.
    */
  final class Key4[ValueType](val v0: ValueType, val v1: ValueType, val v2: ValueType, val v3: ValueType, eq: Array[(AnyRef, AnyRef) => Boolean]) extends Key[ValueType] {
    assert(eq.length == 4)

    override def equals(o: scala.Any): Boolean = o match {
      case that: Key4[_] =>
        eq(0)(this.v0.asInstanceOf[AnyRef], that.v0.asInstanceOf[AnyRef]) &&
          eq(1)(this.v1.asInstanceOf[AnyRef], that.v1.asInstanceOf[AnyRef]) &&
          eq(2)(this.v2.asInstanceOf[AnyRef], that.v2.asInstanceOf[AnyRef]) &&
          eq(3)(this.v3.asInstanceOf[AnyRef], that.v3.asInstanceOf[AnyRef])
      case _ => false
    }

    override val hashCode: Int = 3 * v0.hashCode() + 5 * v1.hashCode() + 7 * v2.hashCode() + 11 * v3.hashCode()
  }

  /**
    * A key with five values.
    */
  final class Key5[ValueType](val v0: ValueType, val v1: ValueType, val v2: ValueType, val v3: ValueType, val v4: ValueType, eq: Array[(AnyRef, AnyRef) => Boolean]) extends Key[ValueType] {
    assert(eq.length == 5)

    override def equals(o: scala.Any): Boolean = o match {
      case that: Key5[_] =>
        eq(0)(this.v0.asInstanceOf[AnyRef], that.v0.asInstanceOf[AnyRef]) &&
          eq(1)(this.v1.asInstanceOf[AnyRef], that.v1.asInstanceOf[AnyRef]) &&
          eq(2)(this.v2.asInstanceOf[AnyRef], that.v2.asInstanceOf[AnyRef]) &&
          eq(3)(this.v3.asInstanceOf[AnyRef], that.v3.asInstanceOf[AnyRef]) &&
          eq(4)(this.v4.asInstanceOf[AnyRef], that.v4.asInstanceOf[AnyRef])
      case _ => false
    }

    override val hashCode: Int = 3 * v0.hashCode() + 5 * v1.hashCode() + 7 * v2.hashCode() + 11 * v3.hashCode() + 13 * v4.hashCode()
  }

  /**
    * Returns an index matching all the non-null columns in the given pattern `pat`.
    */
  @inline
  def getIndex[ValueType](pat: Array[ValueType]): Int = {
    var index = 0
    var i = 0
    while (i < pat.length) {
      if (pat(i) != null) {
        // the i'th column in the pattern is non-null
        // so it should be in the index.
        index = BitOps.setBit(vec = index, bit = i)
      }
      i = i + 1
    }
    index
  }

  /**
    * Returns an index matching all the non-null columns in the given pattern `pat`.
    *
    * An exact index only returns rows that match the pattern.
    *
    * Returns zero if no such index exists.
    */
  def getExactIndex[ValueType](indexes: Set[Int], pat: Array[ValueType]): Int = {
    val index = getIndex(pat)
    if (indexes contains index) index else 0
  }

  /**
    * Returns an approximate index matching all the non-null columns in the given pattern `pat`.
    *
    * An approximate index may returns rows not matching the pattern.
    *
    * Returns zero if no such index exists.
    */
  def getApproximateIndex[ValueType](indexes: Set[Int], pat: Array[ValueType]): Int = {
    // the result index. Defaults to zero representing that no usable index exists.
    var result: Int = 0
    // loop through all available indexes looking for the first partially matching index.
    val iterator = indexes.iterator
    while (iterator.hasNext) {
      val index = iterator.next()
      var i = 0
      var usable = true
      while (i < pat.length) {
        if (BitOps.getBit(vec = index, bit = i) && pat(i) == null) {
          // the index requires the i'th column to be non-null, but it is null in the pattern.
          // thus this specific index is not usable.
          usable = false
          i = pat.length
        }
        i = i + 1
      }

      // heuristic: If multiple indexes are usable, choose the one with the most columns.
      if (usable) {
        if (Integer.bitCount(result) < Integer.bitCount(index)) {
          result = index
        }
      }
    }

    // return result
    return result
  }

  /**
    * Returns an array of equality operations corresponding to the given index.
    */
  // TODO: This is a gruesome hack which is unfortunately necessary at the moment.
  def equalityOf(idx: Int, eq: Array[(AnyRef, AnyRef) => Boolean]): Array[(AnyRef, AnyRef) => Boolean] = {
    val size = Integer.bitCount(idx)
    val result = Array.ofDim[(AnyRef, AnyRef) => Boolean](size)
    var i: Int = 0
    var j: Int = 0
    while (i < eq.length) {
      if (BitOps.getBit(vec = idx, bit = i)) {
        result(j) = eq(i)
        j = j + 1
      }
      i = i + 1
    }
    result
  }

  /**
    * Returns the key for the given index `idx` and pattern `pat`.
    *
    * The pattern must be non-null for all columns in the index.
    *
    * @param idx the index (in binary).
    * @param pat the pattern.
    */
  def keyOf[ValueType](idx: Int, pat: Array[ValueType], eq: Array[(AnyRef, AnyRef) => Boolean]): Key[ValueType] = {
    val columns = Integer.bitCount(idx)
    val i1 = idx
    (columns: @switch) match {
      case 1 =>
        val c1 = BitOps.positionOfLeastSignificantBit(i1)
        new Key1(pat(c1), equalityOf(idx, eq))

      case 2 =>
        val c1 = BitOps.positionOfLeastSignificantBit(i1)
        val i2 = BitOps.clearBit(vec = i1, bit = c1)
        val c2 = BitOps.positionOfLeastSignificantBit(i2)
        new Key2(pat(c1), pat(c2), equalityOf(idx, eq))

      case 3 =>
        val c1 = BitOps.positionOfLeastSignificantBit(i1)
        val i2 = BitOps.clearBit(vec = i1, bit = c1)
        val c2 = BitOps.positionOfLeastSignificantBit(i2)
        val i3 = BitOps.clearBit(vec = i2, bit = c2)
        val c3 = BitOps.positionOfLeastSignificantBit(i3)
        new Key3(pat(c1), pat(c2), pat(c3), equalityOf(idx, eq))

      case 4 =>
        val c1 = BitOps.positionOfLeastSignificantBit(i1)
        val i2 = BitOps.clearBit(vec = i1, bit = c1)
        val c2 = BitOps.positionOfLeastSignificantBit(i2)
        val i3 = BitOps.clearBit(vec = i2, bit = c2)
        val c3 = BitOps.positionOfLeastSignificantBit(i3)
        val i4 = BitOps.clearBit(vec = i3, bit = c3)
        val c4 = BitOps.positionOfLeastSignificantBit(i4)
        new Key4(pat(c1), pat(c2), pat(c3), pat(c4), equalityOf(idx, eq))

      case 5 =>
        val c1 = BitOps.positionOfLeastSignificantBit(i1)
        val i2 = BitOps.clearBit(vec = i1, bit = c1)
        val c2 = BitOps.positionOfLeastSignificantBit(i2)
        val i3 = BitOps.clearBit(vec = i2, bit = c2)
        val c3 = BitOps.positionOfLeastSignificantBit(i3)
        val i4 = BitOps.clearBit(vec = i3, bit = c3)
        val c4 = BitOps.positionOfLeastSignificantBit(i4)
        val i5 = BitOps.clearBit(vec = i4, bit = c4)
        val c5 = BitOps.positionOfLeastSignificantBit(i5)
        new Key5(pat(c1), pat(c2), pat(c3), pat(c4), pat(c5), equalityOf(idx, eq))

      case _ => throw new RuntimeException("Indexes on more than five keys are currently not supported.")
    }
  }

}
