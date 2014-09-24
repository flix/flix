package impl.logic

trait Operator

sealed trait UnaryOperator extends Operator

object UnaryOperator {
  case object Not extends UnaryOperator
  case object UnaryPlus extends UnaryOperator
  case object UnaryMinus extends UnaryOperator
}

sealed trait BinaryOperator extends Operator

sealed trait ComparisonOperator extends BinaryOperator

object BinaryOperator {
  /**
   * Returns the binary operator corresponding with the given string `s`.
   */
  def valueOf(s: String): BinaryOperator = s match {
    case "+" => Plus
    case "-" => Minus
    case "*" => Times
    case "/" => Divide
    case "%" => Modulo
    case "<" => Less
    case "<=" => LessEqual
    case ">" => Greater
    case ">=" => GreaterEqual
    case "==" => Equal
    case "!=" => NotEqual
    case "and" => And
    case "or" => Or
    case "lte" => LessEqual
    case "gte" => GreaterEqual
    case "min" => Minimum
    case "max" => Maximum
  }

  case object Plus extends BinaryOperator
  case object Minus extends BinaryOperator
  case object Times extends BinaryOperator
  case object Divide extends BinaryOperator
  case object Modulo extends BinaryOperator

  case object Less extends ComparisonOperator
  case object LessEqual extends ComparisonOperator
  case object Greater extends ComparisonOperator
  case object GreaterEqual extends ComparisonOperator

  case object Equal extends ComparisonOperator
  case object NotEqual extends ComparisonOperator

  case object And extends BinaryOperator
  case object Or extends BinaryOperator

  case object Minimum extends BinaryOperator
  case object Maximum extends BinaryOperator

  case object Union extends BinaryOperator
  case object Subset extends BinaryOperator
}
