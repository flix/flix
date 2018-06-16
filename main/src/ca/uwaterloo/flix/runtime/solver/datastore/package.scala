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

package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.util.BitOps

import scala.annotation.switch

package object datastore {

  /**
    * A common super-type for keys.
    */
  sealed trait Key {

    def toArray: Array[ProxyObject] = this match {
      case k: Key1 =>
        val a = new Array[ProxyObject](1)
        a(0) = k.v0
        a
      case k: Key2 =>
        val a = new Array[ProxyObject](2)
        a(0) = k.v0
        a(1) = k.v1
        a
      case k: Key3 =>
        val a = new Array[ProxyObject](3)
        a(0) = k.v0
        a(1) = k.v1
        a(2) = k.v2
        a
      case k: Key4 =>
        val a = new Array[ProxyObject](4)
        a(0) = k.v0
        a(1) = k.v1
        a(2) = k.v2
        a(3) = k.v3
        a
      case k: Key5 =>
        val a = new Array[ProxyObject](5)
        a(0) = k.v0
        a(1) = k.v1
        a(2) = k.v2
        a(3) = k.v3
        a(4) = k.v4
        a
    }
  }

  /**
    * A key with one value.
    */
  final class Key1(val v0: ProxyObject) extends Key {

    override def equals(o: scala.Any): Boolean = o match {
      case that: Key1 =>
        this.v0 == that.v0
      case _ => false
    }

    override val hashCode: Int = 3 * v0.hashCode()
  }

  /**
    * A key with two values.
    */
  final class Key2(val v0: ProxyObject, val v1: ProxyObject) extends Key {
    override def equals(o: scala.Any): Boolean = o match {
      case that: Key5 =>
        this.v0 == that.v0 &&
          this.v1 == that.v1
      case _ => false
    }

    override val hashCode: Int = 3 * v0.hashCode() + 5 * v1.hashCode()
  }

  /**
    * A key with three values.
    */
  final class Key3(val v0: ProxyObject, val v1: ProxyObject, val v2: ProxyObject) extends Key {
    override def equals(o: scala.Any): Boolean = o match {
      case that: Key5 =>
        this.v0 == that.v0 &&
          this.v1 == that.v1 &&
          this.v2 == that.v2
      case _ => false
    }

    override val hashCode: Int = 3 * v0.hashCode() + 5 * v1.hashCode() + 7 * v2.hashCode()
  }

  /**
    * A key with four values.
    */
  final class Key4(val v0: ProxyObject, val v1: ProxyObject, val v2: ProxyObject, val v3: ProxyObject) extends Key {
    override def equals(o: scala.Any): Boolean = o match {
      case that: Key5 =>
        this.v0 == that.v0 &&
          this.v1 == that.v1 &&
          this.v2 == that.v2 &&
          this.v3 == that.v3
      case _ => false
    }

    override val hashCode: Int = 3 * v0.hashCode() + 5 * v1.hashCode() + 7 * v2.hashCode() + 11 * v3.hashCode()
  }

  /**
    * A key with five values.
    */
  final class Key5(val v0: ProxyObject, val v1: ProxyObject, val v2: ProxyObject, val v3: ProxyObject, val v4: ProxyObject) extends Key {
    override def equals(o: scala.Any): Boolean = o match {
      case that: Key5 =>
        this.v0 == that.v0 &&
          this.v1 == that.v1 &&
          this.v2 == that.v2 &&
          this.v3 == that.v3 &&
          this.v4 == that.v4
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
    * Returns the key for the given index `idx` and pattern `pat`.
    *
    * The pattern must be non-null for all columns in the index.
    *
    * @param idx the index (in binary).
    * @param pat the pattern.
    */
  def keyOf(idx: Int, pat: Array[ProxyObject]): Key = {
    val columns = Integer.bitCount(idx)
    val i1 = idx
    (columns: @switch) match {
      case 1 =>
        val c1 = BitOps.positionOfLeastSignificantBit(i1)
        new Key1(pat(c1))

      case 2 =>
        val c1 = BitOps.positionOfLeastSignificantBit(i1)
        val i2 = BitOps.clearBit(vec = i1, bit = c1)
        val c2 = BitOps.positionOfLeastSignificantBit(i2)
        new Key2(pat(c1), pat(c2))

      case 3 =>
        val c1 = BitOps.positionOfLeastSignificantBit(i1)
        val i2 = BitOps.clearBit(vec = i1, bit = c1)
        val c2 = BitOps.positionOfLeastSignificantBit(i2)
        val i3 = BitOps.clearBit(vec = i2, bit = c2)
        val c3 = BitOps.positionOfLeastSignificantBit(i3)
        new Key3(pat(c1), pat(c2), pat(c3))

      case 4 =>
        val c1 = BitOps.positionOfLeastSignificantBit(i1)
        val i2 = BitOps.clearBit(vec = i1, bit = c1)
        val c2 = BitOps.positionOfLeastSignificantBit(i2)
        val i3 = BitOps.clearBit(vec = i2, bit = c2)
        val c3 = BitOps.positionOfLeastSignificantBit(i3)
        val i4 = BitOps.clearBit(vec = i3, bit = c3)
        val c4 = BitOps.positionOfLeastSignificantBit(i4)
        new Key4(pat(c1), pat(c2), pat(c3), pat(c4))

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
        new Key5(pat(c1), pat(c2), pat(c3), pat(c4), pat(c5))

      case _ => throw new RuntimeException("Indexes on more than five keys are currently not supported.")
    }
  }

}
