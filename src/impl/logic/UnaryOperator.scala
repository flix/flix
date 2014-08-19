package impl.logic

sealed trait UnaryOperator

object UnaryOperator {

  case object Plus extends UnaryOperator

  case object Minus extends UnaryOperator

}