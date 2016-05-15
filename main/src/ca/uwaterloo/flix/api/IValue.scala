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
  // Tuples                                                                  //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the tuple represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of tuple type.
    */
  def getTuple: Array[IValue]

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
  // Opts                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the option represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of opt type.
    */
  def getJavaOpt: java.util.Optional[IValue]

  /**
    * Returns the option represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of opt type.
    */
  def getScalaOpt: scala.Option[IValue]

  /////////////////////////////////////////////////////////////////////////////
  // Lists                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the list represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of list type.
    */
  def getJavaList: java.util.List[IValue]

  /**
    * Returns the list represented by `this` value.
    *
    * @throws UnsupportedOperationException if `this` value is not of list type.
    */
  def getScalaList: immutable.List[IValue]

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
