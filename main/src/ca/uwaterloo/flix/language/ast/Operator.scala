package ca.uwaterloo.flix.language.ast

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

  /**
   * A common super-type for unary set operations.
   */
  sealed trait SetOperator extends UnaryOperator

  object Set {

    /**
     * A unary operator that returns true if its argument is the empty set.
     */
    case object IsEmpty extends UnaryOperator.SetOperator

    /**
     * A unary operator that returns true if its argument is a non-empty set.
     */
    case object NonEmpty extends UnaryOperator.SetOperator

    /**
     * A unary operator that returns the size of a set.
     */
    case object Size extends UnaryOperator.SetOperator

  }

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

  /**
   * A common super-type for binary set operators.
   */
  sealed trait SetOperator extends BinaryOperator

  object Set {

    case object Insert extends BinaryOperator.SetOperator

    case object Remove extends BinaryOperator.SetOperator

    case object Member extends BinaryOperator.SetOperator

    case object Union extends BinaryOperator.SetOperator

    case object Intersection extends BinaryOperator.SetOperator

    case object Difference extends BinaryOperator.SetOperator

    case object SubsetOf extends BinaryOperator.SetOperator

    case object ProperSubsetOf extends BinaryOperator.SetOperator

  }


}
