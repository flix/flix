package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.Type

final class WrappedType(val tpe: Type) extends IType {

  def isUnit: Boolean =
    tpe == Type.Unit

  def isBool: Boolean =
    tpe == Type.Bool

  def isChar: Boolean =
    tpe == Type.Char

  def isFloat32: Boolean =
    tpe == Type.Float32

  def isFloat64: Boolean =
    tpe == Type.Float64

  def isInt8: Boolean =
    tpe == Type.Int8

  def isInt16: Boolean =
    tpe == Type.Int16

  def isInt32: Boolean =
    tpe == Type.Int32

  def isInt64: Boolean =
    tpe == Type.Int64

  def isStr: Boolean =
    tpe == Type.Str

  def isEnum: Boolean = tpe match {
    case Type.Enum(name, cases) => true
    case _ => false
  }

  def isFunction: Boolean = tpe match {
    case Type.Lambda(args, retTpe) => true
    case _ => false
  }

  def isTuple: Boolean = tpe match {
    case Type.Tuple(elms) => true
    case _ => false
  }

  def isOpt: Boolean = tpe match {
    case Type.FOpt(elm) => true
    case _ => false
  }

  def isList: Boolean = tpe match {
    case Type.FList(elm) => true
    case _ => false
  }

  def isSet: Boolean = tpe match {
    case Type.FSet(elm) => true
    case _ => false
  }

  def isMap: Boolean = tpe match {
    case Type.FMap(keys, values) => true
    case _ => false
  }

  def isNative: Boolean = tpe match {
    case Type.Native => true
    case _ => false
  }

  def getTupleParams: Array[IType] = tpe match {
    case Type.Tuple(elms) => elms.map(t => new WrappedType(t)).toArray
    case _ => throw new UnsupportedOperationException(s"Unexpected type: '$tpe'.")
  }

  def getOptParam: IType = tpe match {
    case Type.FOpt(elm) => new WrappedType(elm)
    case _ => throw new UnsupportedOperationException(s"Unexpected type: '$tpe'.")
  }

  def getListParam: IType = tpe match {
    case Type.FList(elm) => new WrappedType(elm)
    case _ => throw new UnsupportedOperationException(s"Unexpected type: '$tpe'.")
  }

  def getSetParam: IType = tpe match {
    case Type.FSet(elm) => new WrappedType(elm)
    case _ => throw new UnsupportedOperationException(s"Unexpected type: '$tpe'.")
  }

  def getMapKeyParam: IType = tpe match {
    case Type.FMap(k, v) => new WrappedType(k)
    case _ => throw new UnsupportedOperationException(s"Unexpected type: '$tpe'.")
  }

  def getMapValueParam: IType = tpe match {
    case Type.FMap(k, v) => new WrappedType(v)
    case _ => throw new UnsupportedOperationException(s"Unexpected type: '$tpe'.")
  }

  override def equals(other: Any): Boolean = other match {
    case that: WrappedType => tpe == that.tpe
    case _ => false
  }

  override def hashCode(): Int = tpe.hashCode()
}