package ca.uwaterloo.flix.language.phase.jvm

import org.objectweb.asm.Opcodes.{T_BOOLEAN, T_BYTE, T_CHAR, T_DOUBLE, T_FLOAT, T_INT, T_LONG, T_SHORT}

sealed trait PrimitiveType {

  def toTpe: BackendType = BackendType.Primitive(this)

  def toDescriptor: String = this match {
    case PrimitiveType.Bool => "Z"
    case PrimitiveType.Char => "C"
    case PrimitiveType.Int8 => "B"
    case PrimitiveType.Int16 => "S"
    case PrimitiveType.Int32 => "I"
    case PrimitiveType.Int64 => "J"
    case PrimitiveType.Float32 => "F"
    case PrimitiveType.Float64 => "D"
  }

  def toErasedString: String = this match {
    case PrimitiveType.Bool => "Bool"
    case PrimitiveType.Char => "Char"
    case PrimitiveType.Int8 => "Int8"
    case PrimitiveType.Int16 => "Int16"
    case PrimitiveType.Int32 => "Int32"
    case PrimitiveType.Int64 => "Int64"
    case PrimitiveType.Float32 => "Float32"
    case PrimitiveType.Float64 => "Float64"
  }

  def toArrayTypeCode: Int = this match {
    case PrimitiveType.Bool => T_BOOLEAN
    case PrimitiveType.Char => T_CHAR
    case PrimitiveType.Int8 => T_BYTE
    case PrimitiveType.Int16 => T_SHORT
    case PrimitiveType.Int32 => T_INT
    case PrimitiveType.Int64 => T_LONG
    case PrimitiveType.Float32 => T_FLOAT
    case PrimitiveType.Float64 => T_DOUBLE
  }
}

object PrimitiveType {

  case object Bool extends PrimitiveType

  case object Char extends PrimitiveType

  case object Int8 extends PrimitiveType

  case object Int16 extends PrimitiveType

  case object Int32 extends PrimitiveType

  case object Int64 extends PrimitiveType

  case object Float32 extends PrimitiveType

  case object Float64 extends PrimitiveType

}
