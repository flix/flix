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
      case BinaryOperator.Plus => "+"
      case BinaryOperator.Equal => "=="
      case BinaryOperator.Or => "||"
      case BinaryOperator.Times => "*"
      case BinaryOperator.Divide => "/"
      case BinaryOperator.Modulo => "%"
      case BinaryOperator.Plus => "+"
      case BinaryOperator.Minus => "-"
    }
  }

}
