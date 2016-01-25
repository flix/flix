package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.runtime.Value

protected final class ValueWrapper(private val value: Value) extends IValue {

  def isUnit: Boolean = value match {
    case Value.Unit => true
    case _ => false
  }

  def getType: IType = value match {
    case Value.Unit => new TypeWrapper(Type.Unit)
    case Value.True => new TypeWrapper(Type.Bool)
    case Value.False => new TypeWrapper(Type.Bool)
    case v: Value.Int => new TypeWrapper(Type.Int32)
    // TODO: rest
    case _ => throw new FlixApiError(s"Unexpected value: $value")
  }

  def getBool: Boolean = value match {
    case Value.True => true
    case Value.False => false
    case _ => throw new FlixApiError(s"Unexpected value: $value")
  }

  def isTrue: Boolean = value match {
    case Value.True => true
    case _ => false
  }

  def isFalse: Boolean = value match {
    case Value.False => true
    case _ => false
  }

  def getChar: Char = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  def getInt8: Byte = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  def getInt16: Short = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  def getInt32: Int = value match {
    case v: Value.Int => v.i
    case _ => throw new FlixApiError(s"Unexpected value: $value")
  }

  def getInt64: Long = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  def getStr: String = value match {
    case v: Value.Str => v.s
    case _ => throw new FlixApiError(s"Unexpected value: $value")
  }

  def getTuple: Array[IValue] = value match {
    case v: Value.Tuple => v.elms.map(e => new ValueWrapper(e))
    case _ => throw new FlixApiError(s"Unexpected value: $value")
  }

}
