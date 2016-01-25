package ca.uwaterloo.flix.api

/**
  * Public interface for Flix values.
  */
trait IValue {

  /**
    * Returns the type of `this` value.
    */
  def getType: IType

  /////////////////////////////////////////////////////////////////////////////
  // Boolean Extractors                                                      //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns the boolean represented by `this` value.
    *
    * @throws FlixError if `this` value is not of boolean type.
    */
  def getBool: Boolean

  /**
    * Returns `true` if `this` value is equal to the given boolean `b`.
    *
    * @throws FlixError if `this` value is not of boolean type.
    */
  def isBool(b: Boolean): Boolean

  /**
    * Returns `true` if `this` value is the boolean `true` value.
    *
    * @throws FlixError if `this` value is not of boolean type.
    */
  def isTrue: Boolean

  /**
    * Returns `true` if `this` value is the boolean `false` value.
    *
    * @throws FlixError if `this` value is not of boolean type.
    */
  def isFalse: Boolean

  /////////////////////////////////////////////////////////////////////////////
  // Char Extractors                                                         //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns the char represented by `this` value.
    *
    * @throws FlixError if `this` value is not of char type.
    */
  def getChar: Char

  /**
    * Returns `true` if `this` value is equal to the given char `c`.
    *
    * @throws FlixError if `this` value is not of char type.
    */
  def isChar(c: Char): Boolean

  /////////////////////////////////////////////////////////////////////////////
  // Int Extractors                                                          //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns the int8 represented by `this` value.
    *
    * @throws FlixError if `this` value is not of int8 type.
    */
  def getInt8: Byte

  /**
    * Returns the int16 represented by `this` value.
    *
    * @throws FlixError if `this` value is not of int16 type.
    */
  def getInt16: Short

  /**
    * Returns the int32 represented by `this` value.
    *
    * @throws FlixError if `this` value is not of int32 type.
    */
  def getInt32: Int

  /**
    * Returns the int64 represented by `this` value.
    *
    * @throws FlixError if `this` value is not of int64 type.
    */
  def getInt64: Long

  /**
    * Returns `true` if `this` value is equal to the given int8 `i`.
    *
    * @throws FlixError if `this` value is not of int8 type.
    */
  def isInt8(i: Byte): Boolean

  /**
    * Returns `true` if `this` value is equal to the given int16 `i`.
    *
    * @throws FlixError if `this` value is not of int16 type.
    */
  def isInt16(i: Short): Boolean

  /**
    * Returns `true` if `this` value is equal to the given int32 `i`.
    *
    * @throws FlixError if `this` value is not of int32 type.
    */
  def isInt32(i: Int): Boolean

  /**
    * Returns `true` if `this` value is equal to the given int64 `i`.
    *
    * @throws FlixError if `this` value is not of int64 type.
    */
  def isInt64(i: Long): Boolean

  /////////////////////////////////////////////////////////////////////////////
  // String Extractors                                                       //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns the string represented by `this` value.
    *
    * @throws FlixError if `this` value is not of string type.
    */
  def getStr: String

  /**
    * Returns `true` if `this` value is equal to the given string `s`.
    *
    * @throws FlixError if `this` value is not of string type.
    */
  def isStr(s: String): Boolean

}
