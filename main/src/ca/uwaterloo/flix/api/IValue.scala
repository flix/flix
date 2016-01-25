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
  // Unit Extractors                                                         //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns `true` if `this` value is the unit value.
    */
  def isUnit: Boolean

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
    * Returns `true` if `this` value is the boolean `true` value.
    */
  def isTrue: Boolean

  /**
    * Returns `true` if `this` value is the boolean `false` value.
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

  /////////////////////////////////////////////////////////////////////////////
  // String Extractors                                                       //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the string represented by `this` value.
    *
    * @throws FlixError if `this` value is not of string type.
    */
  def getStr: String

  /////////////////////////////////////////////////////////////////////////////
  // Tuple Extractors                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the tuple represented by `this` value.
    *
    * @throws FlixError if `this` value is not of tuple type.
    */
  def getTuple: Array[IValue]


}
