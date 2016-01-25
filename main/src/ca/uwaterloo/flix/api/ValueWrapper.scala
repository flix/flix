package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.runtime.Value

protected final class ValueWrapper(private val value: Value) extends IValue {

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

  def isBool(b: Boolean): Boolean =
    getBool == b

  def isTrue: Boolean = getBool

  def isFalse: Boolean = !getBool

  def getChar: Char = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  def isChar(c: Char): Boolean =
    getChar == c

  def getInt8: Byte = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  def getInt16: Short = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  def getInt32: Int = value match {
    case v: Value.Int => v.i
    case _ => throw new FlixApiError(s"Unexpected value: $value")
  }

  def getInt64: Long = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  def isInt8(i: Byte): Boolean =
    getInt8 == i

  def isInt16(i: Short): Boolean =
    getInt16 == i

  def isInt32(i: Int): Boolean =
    getInt32 == i

  def isInt64(i: Long): Boolean =
    getInt64 == i


}
