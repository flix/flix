package impl.logic

trait Operator

sealed trait UnaryOperator extends Operator

object UnaryOperator {

  case object Plus extends UnaryOperator

  case object Minus extends UnaryOperator

}

sealed trait BinaryOperator extends Operator

object BinaryOperator {

  case object Eq extends BinaryOperator

}
