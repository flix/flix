package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers

import ca.uwaterloo.flix.language.ast.SemanticOperator._
import ca.uwaterloo.flix.language.ast._

object OperatorPrinter {

  private val eq: String = "=="
  private val neq: String = "!="
  private val not: String = "!"
  private val neg: String = "-"
  private val plus: String = "+"
  private val minus: String = "-"
  private val lt: String = "<"
  private val le: String = "<="
  private val gt: String = ">"
  private val ge: String = ">="
  private val and: String = "and"
  private val or: String = "or"
  private val mul: String = "*"
  private val div: String = "/"
  private val exp: String = "**"
  private val xor: String = "xor"
  private val rem: String = "rem"

  def print(uo: UnaryOperator): String = uo match {
    case UnaryOperator.LogicalNot => not
    case UnaryOperator.Plus => plus
    case UnaryOperator.Minus => minus
    case UnaryOperator.BitwiseNegate => "~"
  }

  def print(bo: BinaryOperator): String = bo match {
    case operator: ArithmeticOperator => operator match {
      case BinaryOperator.Plus => plus
      case BinaryOperator.Minus => minus
      case BinaryOperator.Times => mul
      case BinaryOperator.Divide => div
      case BinaryOperator.Remainder => rem
      case BinaryOperator.Exponentiate => exp
    }
    case operator: ComparisonOperator => operator match {
      case operator: EqualityOperator => operator match {
        case BinaryOperator.Equal => eq
        case BinaryOperator.NotEqual => neq
        case BinaryOperator.Spaceship => "<=>"
      }
      case BinaryOperator.Less => lt
      case BinaryOperator.LessEqual => le
      case BinaryOperator.Greater => gt
      case BinaryOperator.GreaterEqual => ge
    }
    case operator: LogicalOperator => operator match {
      case BinaryOperator.LogicalAnd => and
      case BinaryOperator.LogicalOr => or
    }
    case operator: BitwiseOperator => operator match {
      case BinaryOperator.BitwiseAnd => "b_and"
      case BinaryOperator.BitwiseOr => "b_or"
      case BinaryOperator.BitwiseXor => "b_xor"
      case BinaryOperator.BitwiseLeftShift => "b_shl"
      case BinaryOperator.BitwiseRightShift => "b_shr"
    }
  }

  def print(so: SemanticOperator): String = so match {
    case op: SemanticOperator.BoolOp => op match {
      case BoolOp.Not => not
      case BoolOp.And => and
      case BoolOp.Or => or
      case BoolOp.Eq => eq
      case BoolOp.Neq => neq
    }
    case op: SemanticOperator.CharOp => op match {
      case CharOp.Eq => eq
      case CharOp.Neq => neq
      case CharOp.Lt => lt
      case CharOp.Le => le
      case CharOp.Gt => gt
      case CharOp.Ge => ge
    }
    case op: SemanticOperator.Float32Op => op match {
      case Float32Op.Add => plus
      case Float32Op.Sub => minus
      case Float32Op.Mul => mul
      case Float32Op.Div => div
      case Float32Op.Exp => exp
      case Float32Op.Eq => eq
      case Float32Op.Neq => neq
      case Float32Op.Lt => lt
      case Float32Op.Le => le
      case Float32Op.Gt => gt
      case Float32Op.Ge => ge
    }
    case op: SemanticOperator.Float64Op => op match {
      case Float32Op.Neg => neg
      case Float64Op.Neg => neg
      case Float64Op.Add => plus
      case Float64Op.Sub => minus
      case Float64Op.Mul => mul
      case Float64Op.Div => div
      case Float64Op.Exp => exp
      case Float64Op.Eq => eq
      case Float64Op.Neq => neq
      case Float64Op.Lt => lt
      case Float64Op.Le => le
      case Float64Op.Gt => gt
      case Float64Op.Ge => ge
    }
    case op: SemanticOperator.BigDecimalOp => op match {
      case BigDecimalOp.Neg => neg
      case BigDecimalOp.Add => plus
      case BigDecimalOp.Sub => minus
      case BigDecimalOp.Mul => mul
      case BigDecimalOp.Div => div
      case BigDecimalOp.Exp => exp
      case BigDecimalOp.Eq => eq
      case BigDecimalOp.Neq => neq
      case BigDecimalOp.Lt => lt
      case BigDecimalOp.Le => le
      case BigDecimalOp.Gt => gt
      case BigDecimalOp.Ge => ge
    }
    case op: SemanticOperator.Int8Op => op match {
      case Int8Op.Neg => neg
      case Int8Op.Not => not
      case Int8Op.Add => plus
      case Int8Op.Sub => minus
      case Int8Op.Mul => mul
      case Int8Op.Div => div
      case Int8Op.Rem => rem
      case Int8Op.Exp => exp
      case Int8Op.And => and
      case Int8Op.Or => or
      case Int8Op.Xor => xor
      case Int8Op.Shl => "shl"
      case Int8Op.Shr => "shr"
      case Int8Op.Eq => eq
      case Int8Op.Neq => neq
      case Int8Op.Lt => lt
      case Int8Op.Le => le
      case Int8Op.Gt => gt
      case Int8Op.Ge => ge
    }
    case op: SemanticOperator.Int16Op => op match {
      case Int16Op.Neg => neg
      case Int16Op.Not => not
      case Int16Op.Add => plus
      case Int16Op.Sub => minus
      case Int16Op.Mul => mul
      case Int16Op.Div => div
      case Int16Op.Rem => rem
      case Int16Op.Exp => exp
      case Int16Op.And => and
      case Int16Op.Or => or
      case Int16Op.Xor => xor
      case Int16Op.Shl => "shl"
      case Int16Op.Shr => "shr"
      case Int16Op.Eq => eq
      case Int16Op.Neq => neq
      case Int16Op.Lt => lt
      case Int16Op.Le => le
      case Int16Op.Gt => gt
      case Int16Op.Ge => ge
    }
    case op: SemanticOperator.Int32Op => op match {
      case Int32Op.Neg => neg
      case Int32Op.Not => not
      case Int32Op.Add => plus
      case Int32Op.Sub => minus
      case Int32Op.Mul => mul
      case Int32Op.Div => div
      case Int32Op.Rem => rem
      case Int32Op.Exp => exp
      case Int32Op.And => and
      case Int32Op.Or => or
      case Int32Op.Xor => xor
      case Int32Op.Shl => "shl"
      case Int32Op.Shr => "shr"
      case Int32Op.Eq => eq
      case Int32Op.Neq => neq
      case Int32Op.Lt => lt
      case Int32Op.Le => le
      case Int32Op.Gt => gt
      case Int32Op.Ge => ge
    }
    case op: SemanticOperator.Int64Op => op match {
      case Int64Op.Neg => neg
      case Int64Op.Not => not
      case Int64Op.Add => plus
      case Int64Op.Sub => minus
      case Int64Op.Mul => mul
      case Int64Op.Div => div
      case Int64Op.Rem => rem
      case Int64Op.Exp => exp
      case Int64Op.And => plus
      case Int64Op.Or => or
      case Int64Op.Xor => xor
      case Int64Op.Shl => "shl"
      case Int64Op.Shr => "shr"
      case Int64Op.Eq => eq
      case Int64Op.Neq => neq
      case Int64Op.Lt => lt
      case Int64Op.Le => le
      case Int64Op.Gt => gt
      case Int64Op.Ge => ge
      case BigIntOp.Not => not
    }
    case op: SemanticOperator.BigIntOp => op match {
      case BigIntOp.Neg => not
      case BigIntOp.Add => plus
      case BigIntOp.Sub => minus
      case BigIntOp.Mul => mul
      case BigIntOp.Div => div
      case BigIntOp.Rem => rem
      case BigIntOp.Exp => exp
      case BigIntOp.And => and
      case BigIntOp.Or => or
      case BigIntOp.Xor => xor
      case BigIntOp.Shl => "shl"
      case BigIntOp.Shr => "shr"
      case BigIntOp.Eq => eq
      case BigIntOp.Neq => neq
      case BigIntOp.Lt => lt
      case BigIntOp.Le => le
      case BigIntOp.Gt => gt
      case BigIntOp.Ge => ge
    }
    case op: SemanticOperator.ObjectOp => op match {
      case ObjectOp.EqNull => "null == "
      case ObjectOp.NeqNull => "null != "
    }
    case op: SemanticOperator.StringOp => op match {
      case StringOp.Concat => plus
      case StringOp.Eq => eq
      case StringOp.Neq => neq
    }
  }
}
