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

/**
  * Public interface for Flix types.
  */
trait IType {

  /////////////////////////////////////////////////////////////////////////////
  // Queries                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns `true` if `this` type is the unit type.
    */
  def isUnit: Boolean

  /**
    * Returns `true` if `this` type is the bool type.
    */
  def isBool: Boolean

  /**
    * Returns `true` if `this` type is the char type.
    */
  def isChar: Boolean

  /**
    * Returns `true` if `this` type is the float32 type.
    */
  def isFloat32: Boolean

  /**
    * Returns `true` if `this` type is the float64 type.
    */
  def isFloat64: Boolean

  /**
    * Returns `true` if `this` type is the int8 type.
    */
  def isInt8: Boolean

  /**
    * Returns `true` if `this` type is the int16 type.
    */
  def isInt16: Boolean

  /**
    * Returns `true` if `this` type is the int32 type.
    */
  def isInt32: Boolean

  /**
    * Returns `true` if `this` type is the int64 type.
    */
  def isInt64: Boolean

  /**
    * Returns `true` if `this` type is the BigInt type.
    */
  def isBigInt: Boolean

  /**
    * Returns `true` if `this` type is the str type.
    */
  def isStr: Boolean

  /**
    * Returns `true` if `this` type is the enum type.
    */
  def isEnum: Boolean

  /**
    * Returns `true` if `this` type is the function type.
    */
  def isFunction: Boolean

  /**
    * Returns `true` if `this` type is the tuple type.
    */
  def isTuple: Boolean

  /**
    * Returns `true` if `this` type is the native type.
    */
  def isNative: Boolean

  /////////////////////////////////////////////////////////////////////////////
  // Extractors                                                              //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns the parametric types of `this` tuple type.
    *
    * @throws UnsupportedOperationException if `this` type is not a tuple.
    */
  def getTupleParams: Array[IType]

}
