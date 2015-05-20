package syntax

import impl.logic.{UnaryOperator, BinaryOperator, Operator}

/**
 * Embedded DSL syntax for symbols.
 */
object Operators {

  /**
   * Rich Operators.
   */
  implicit class RichOperator(op: Operator) {
    def fmt: String = op match {
      case UnaryOperator.Not => "!"
      case UnaryOperator.UnaryPlus => "+"
      case UnaryOperator.UnaryMinus => "-"

      case BinaryOperator.Plus => "+"
      case BinaryOperator.Minus => "-"
      case BinaryOperator.Times => "*"
      case BinaryOperator.Divide => "/"
      case BinaryOperator.Modulo => "%"

      case BinaryOperator.Less => "<"
      case BinaryOperator.LessEqual => "<="
      case BinaryOperator.Greater => ">"
      case BinaryOperator.GreaterEqual => ">="

      case BinaryOperator.Equal => "=="
      case BinaryOperator.NotEqual => "!="

      case BinaryOperator.And => "&&"
      case BinaryOperator.Or => "||"

      case BinaryOperator.Minimum => "Min"
      case BinaryOperator.Maximum => "Max"
    }
  }

}
