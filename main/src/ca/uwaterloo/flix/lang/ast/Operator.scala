package ca.uwaterloo.flix.lang.ast

/**
 * A common super-type for operators.
 */
trait Operator

/**
 * A common super-type for unary operators.
 */
sealed trait UnaryOperator extends Operator

object UnaryOperator {

  /**
   * Unary negation.
   */
  case object Not extends UnaryOperator

  /**
   * Unary plus.
   */
  case object UnaryPlus extends UnaryOperator

  /**
   * Unary minus.
   */
  case object UnaryMinus extends UnaryOperator

}

/**
 * A common super-type for binary operators.
 */
sealed trait BinaryOperator extends Operator

/**
 * A common super-type for comparison operators
 */
sealed trait ComparisonOperator extends BinaryOperator

object BinaryOperator {

  /**
   * Addition.
   */
  case object Plus extends BinaryOperator

  /**
   * Subtraction.
   */
  case object Minus extends BinaryOperator

  /**
   * Multiplication.
   */
  case object Times extends BinaryOperator

  /**
   * Division.
   */
  case object Divide extends BinaryOperator

  /**
   * Modulus.
   */
  case object Modulo extends BinaryOperator

  /**
   * Strictly less-than.
   */
  case object Less extends ComparisonOperator

  /**
   * Less than or equal.
   */
  case object LessEqual extends ComparisonOperator

  /**
   * Strictly greater.
   */
  case object Greater extends ComparisonOperator

  /**
   * Greater or equal.
   */
  case object GreaterEqual extends ComparisonOperator

  /**
   * Equality
   */
  case object Equal extends ComparisonOperator

  /**
   * Inequality.
   */
  case object NotEqual extends ComparisonOperator

  /**
   * Logical conjunction.
   */
  case object And extends BinaryOperator

  /**
   * Logical disjunction.
   */
  case object Or extends BinaryOperator

  @deprecated("replaced by builtin function", "0.1.0")
  case object Minimum extends BinaryOperator

  @deprecated("replaced by builtin function", "0.1.0")
  case object Maximum extends BinaryOperator

  @deprecated("replaced by builtin function", "0.1.0")
  case object Union extends BinaryOperator

  @deprecated("replaced by builtin function", "0.1.0")
  case object Subset extends BinaryOperator

}
