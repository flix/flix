package syntax

import impl.logic.{BinaryOperator, Operator}

/**
 * Embedded DSL syntax for symbols.
 */
object Operators {

  /**
   * Rich Operators.
   */
  implicit class RichOperator(op: Operator) {
    def fmt: String = op match {
      case BinaryOperator.Eq => "=="
    }
  }

}
