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


}
