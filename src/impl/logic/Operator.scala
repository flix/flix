package impl.logic

trait Operator

sealed trait UnaryOperator extends Operator

object UnaryOperator {
  // Logical
  case object Not extends BinaryOperator

  // Integer
  case object Plus extends UnaryOperator
  case object Minus extends UnaryOperator
  case object Times extends UnaryOperator
  case object Divide extends UnaryOperator
}

sealed trait BinaryOperator extends Operator

object BinaryOperator {
  // Comparison
  case object Eq extends BinaryOperator
  case object Neq extends BinaryOperator

  // Logical
  case object Or extends BinaryOperator
  case object And extends BinaryOperator
}
