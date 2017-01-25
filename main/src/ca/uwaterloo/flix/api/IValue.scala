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

package ca.uwaterloo.flix.api

import scala.collection.immutable

/**
  * Public interface for Flix values.
  */
trait IValue {

  /////////////////////////////////////////////////////////////////////////////
  // Meta                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the type of `this` value.
    */
  def getType: IType

  /////////////////////////////////////////////////////////////////////////////
  // Unit                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns `true` if `this` value is the unit value.
    */
  def isUnit: Boolean

  /////////////////////////////////////////////////////////////////////////////
  // Booleans                                                                //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the boolean represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of boolean type.
    */
  def getBool: Boolean

  /**
    * Returns `true` if `this` value is the boolean `true` value.
    */
  def isTrue: Boolean

  /**
    * Returns `true` if `this` value is the boolean `false` value.
    */
  def isFalse: Boolean

  /////////////////////////////////////////////////////////////////////////////
  // Chars                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the char represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of char type.
    */
  def getChar: Char

  /////////////////////////////////////////////////////////////////////////////
  // Floats                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the float32 represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of float32 type.
    */
  def getFloat32: Float

  /**
    * Returns the float32 represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of float64 type.
    */
  def getFloat64: Double

  /////////////////////////////////////////////////////////////////////////////
  // Ints                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the int8 represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of int8 type.
    */
  def getInt8: Byte

  /**
    * Returns the int16 represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of int16 type.
    */
  def getInt16: Short

  /**
    * Returns the int32 represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of int32 type.
    */
  def getInt32: Int

  /**
    * Returns the int64 represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of int64 type.
    */
  def getInt64: Long

  /**
    * Returns the BigInt represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of BigInt type.
    */
  def getBigInt: java.math.BigInteger

  /////////////////////////////////////////////////////////////////////////////
  // Strings                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the string represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of string type.
    */
  def getStr: String

  /////////////////////////////////////////////////////////////////////////////
  // Enums                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the name of the tag represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of enum type.
    */
  def getTagName: String

  /**
    * Returns the value of the tag represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of enum type.
    */
  def getTagValue: IValue

  /////////////////////////////////////////////////////////////////////////////
  // Native                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the native object represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of native type.
    */
  def getNativeRef: AnyRef

  /////////////////////////////////////////////////////////////////////////////
  // Option                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the option represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of option type.
    */
  def getJavaOpt: java.util.Optional[IValue]

  /**
    * Returns the option represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of option type.
    */
  def getScalaOpt: scala.Option[IValue]

  /////////////////////////////////////////////////////////////////////////////
  // Result                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the result represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of result type.
    */
  def getScalaEither: Either[IValue, IValue]

  /////////////////////////////////////////////////////////////////////////////
  // Sets                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the set represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of set type.
    */
  def getJavaSet: java.util.Set[IValue]

  /**
    * Returns the set represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of set type.
    */
  def getScalaSet: immutable.Set[IValue]

  /////////////////////////////////////////////////////////////////////////////
  // Maps                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the set represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of map type.
    */
  def getJavaMap: java.util.Map[IValue, IValue]

  /**
    * Returns the set represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of map type.
    */
  def getScalaMap: immutable.Map[IValue, IValue]

  /////////////////////////////////////////////////////////////////////////////
  // Unsafe                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Unsafely returns the Flix representation of `this` value.
    *
    * Warning: This function should not be used.
    */
  def getUnsafeRef: AnyRef

}
