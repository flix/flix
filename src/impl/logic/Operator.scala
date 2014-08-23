package impl.logic

trait Operator

sealed trait UnaryOperator extends Operator
object UnaryOperator {
  case object Not extends UnaryOperator
  case object UnaryPlus extends UnaryOperator
  case object UnaryMinus extends UnaryOperator
}

sealed trait BinaryOperator extends Operator
object BinaryOperator {
  case object Plus extends BinaryOperator
  case object Minus extends BinaryOperator
  case object Times extends BinaryOperator
  case object Divide extends BinaryOperator
  case object Modulo extends BinaryOperator
  case object Less extends BinaryOperator
  case object LessEqual extends BinaryOperator
  case object Greater extends BinaryOperator
  case object GreaterEqual extends BinaryOperator
  case object Equal extends BinaryOperator
  case object NotEqual extends BinaryOperator
}
