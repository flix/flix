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
  // Value Tests                                                             //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns the boolean represented by `this` value.
    *
    * @throws FlixError if `this` value is not of boolean type.
    */
  def getBool: Boolean

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




}
