package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.SemanticOperator.{BoolOp, CharOp, Float32Op, Float64Op}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._

object OperatorPrinter {

  def doc(uo: UnaryOperator): Doc = text(uo match {
    case UnaryOperator.LogicalNot => "!"
    case UnaryOperator.Plus => "+"
    case UnaryOperator.Minus => "-"
    case UnaryOperator.BitwiseNegate => "~"
  })

  def doc(bo: BinaryOperator): Doc = text(bo match {
    case operator: ArithmeticOperator => operator match {
      case BinaryOperator.Plus => "+"
      case BinaryOperator.Minus => "-"
      case BinaryOperator.Times => "*"
      case BinaryOperator.Divide => "/"
      case BinaryOperator.Remainder => "rem"
      case BinaryOperator.Exponentiate => "^"
    }
    case operator: ComparisonOperator => operator match {
      case operator: EqualityOperator => operator match {
        case BinaryOperator.Equal => "=="
        case BinaryOperator.NotEqual => "!="
        case BinaryOperator.Spaceship => "<=>"
      }
      case BinaryOperator.Less => "<"
      case BinaryOperator.LessEqual => "<="
      case BinaryOperator.Greater => ">"
      case BinaryOperator.GreaterEqual => ">="
    }
    case operator: LogicalOperator => operator match {
      case BinaryOperator.LogicalAnd => "and"
      case BinaryOperator.LogicalOr => "or"
    }
    case operator: BitwiseOperator => operator match {
      case BinaryOperator.BitwiseAnd => "b_and"
      case BinaryOperator.BitwiseOr => "b_or"
      case BinaryOperator.BitwiseXor => "b_xor"
      case BinaryOperator.BitwiseLeftShift => "b_shl"
      case BinaryOperator.BitwiseRightShift => "b_shr"
    }
  })

  def doc(so: SemanticOperator): Doc = text(so match {
    case op: SemanticOperator.BoolOp => op match {
      case BoolOp.Not => "!"
      case BoolOp.And => "and"
      case BoolOp.Or => "or"
      case BoolOp.Eq => "=="
      case BoolOp.Neq => "!="
    }
    case op: SemanticOperator.CharOp => op match {
      case CharOp.Eq => "=="
      case CharOp.Neq => "!="
      case CharOp.Lt => "<"
      case CharOp.Le => "<="
      case CharOp.Gt => ">"
      case CharOp.Ge => ">="
    }
    case op: SemanticOperator.Float32Op => op match {
      case Float32Op.Add => "+"
      case Float32Op.Sub => "-"
      case Float32Op.Mul => "*"
      case Float32Op.Div => "/"
      case Float32Op.Exp => "**"
      case Float32Op.Eq => "=="
      case Float32Op.Neq => "!="
      case Float32Op.Lt => "<"
      case Float32Op.Le => "<="
      case Float32Op.Gt => ">"
      case Float32Op.Ge => ">="
    }
    case op: SemanticOperator.Float64Op => op match {
      case Float32Op.Neg => "-"
      case Float64Op.Neg => "-"
      case Float64Op.Add => "+"
      case Float64Op.Sub => "-"
      case Float64Op.Mul => "*"
      case Float64Op.Div => "/"
      case Float64Op.Exp => "**"
      case Float64Op.Eq => "=="
      case Float64Op.Neq => "!="
      case Float64Op.Lt => "<"
      case Float64Op.Le => "<="
      case Float64Op.Gt => ">"
      case Float64Op.Ge => ">="
    }
    case op: SemanticOperator.BigDecimalOp => "BigDecimalOp"
    case op: SemanticOperator.Int8Op => "Int80p"
    case op: SemanticOperator.Int16Op => "Int16Op"
    case op: SemanticOperator.Int32Op => "Int32Op"
    case op: SemanticOperator.Int64Op => "Int64Op"
    case op: SemanticOperator.BigIntOp => "BigIntOp"
    case op: SemanticOperator.ObjectOp => "ObjectOp"
    case op: SemanticOperator.StringOp => "StringOp"
  })
}
