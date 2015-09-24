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
 * A common super-type for arithmetic operators.
 */
sealed trait ArithmeticOperator extends BinaryOperator

/**
 * A common super-type for comparison operators.
 */
sealed trait ComparisonOperator extends BinaryOperator

/**
 * A common super-type for equality operators
 */
sealed trait EqualityOperator extends BinaryOperator

/**
 * A common super-type for logical operators.
 */
sealed trait LogicalOperator extends BinaryOperator

object BinaryOperator {

  /**
   * Addition.
   */
  case object Plus extends ArithmeticOperator

  /**
   * Subtraction.
   */
  case object Minus extends ArithmeticOperator

  /**
   * Multiplication.
   */
  case object Times extends ArithmeticOperator

  /**
   * Division.
   */
  case object Divide extends ArithmeticOperator

  /**
   * Modulus.
   */
  case object Modulo extends ArithmeticOperator

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
  case object Equal extends EqualityOperator

  /**
   * Inequality.
   */
  case object NotEqual extends EqualityOperator

  /**
   * Logical conjunction.
   */
  case object And extends LogicalOperator

  /**
   * Logical disjunction.
   */
  case object Or extends LogicalOperator

  // TODO: Remove these
  @deprecated("replaced by builtin function", "0.1.0")
  case object Minimum extends BinaryOperator

  @deprecated("replaced by builtin function", "0.1.0")
  case object Maximum extends BinaryOperator

  @deprecated("replaced by builtin function", "0.1.0")
  case object Union extends BinaryOperator

  @deprecated("replaced by builtin function", "0.1.0")
  case object Subset extends BinaryOperator

}
