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
    * Returns `true` if `this` type is the str type.
    */
  def isStr: Boolean

  /**
    * Returns `true` if `this` type is the enum type.
    */
  def isEnum: Boolean

  /**
    * Returns `true` if `this` type is the tuple type.
    */
  def isTuple: Boolean

  /**
    * Returns `true` if `this` type is the opt type.
    */
  def isOpt: Boolean

  /**
    * Returns `true` if `this` type is the list type.
    */
  def isList: Boolean

  /**
    * Returns `true` if `this` type is the set type.
    */
  def isSet: Boolean

  /**
    * Returns `true` if `this` type is the map type.
    */
  def isMap: Boolean

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

  /**
    * Returns the parametric type of `this` opt type.
    *
    * @throws UnsupportedOperationException if `this` type is not an opt.
    */
  def getOptParam: IType

  /**
    * Returns the parametric type of `this` list type.
    *
    * @throws UnsupportedOperationException if `this` type is not a list.
    */
  def getListParam: IType

  /**
    * Returns the parametric type of `this` set type.
    *
    * @throws UnsupportedOperationException if `this` type is not a set.
    */
  def getSetParam: IType

  /**
    * Returns the parametric type of the keys of `this` map type.
    *
    * @throws UnsupportedOperationException if `this` type is not a map.
    */
  def getMapKeyParam: IType

  /**
    * Returns the parametric type of the values of `this` map type.
    *
    * @throws UnsupportedOperationException if `this` type is not a map.
    */
  def getMapValueParam: IType

}
