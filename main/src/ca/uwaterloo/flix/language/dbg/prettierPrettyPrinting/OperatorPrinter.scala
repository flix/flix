package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.{ArithmeticOperator, BinaryOperator, BitwiseOperator, ComparisonOperator, EqualityOperator, LogicalOperator, UnaryOperator}
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._

object OperatorPrinter {

  def doc(uo: UnaryOperator): Doc = text(uo match {
    case UnaryOperator.LogicalNot =>  "!"
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
  }
)
}
