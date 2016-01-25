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
  case object LogicalNot extends UnaryOperator

  /**
    * Unary plus.
    */
  case object Plus extends UnaryOperator

  /**
    * Unary minus.
    */
  case object Minus extends UnaryOperator

  /**
    * Bitwise negate.
    */
  case object BitwiseNegate extends UnaryOperator

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
sealed trait EqualityOperator extends ComparisonOperator

/**
  * A common super-type for logical operators.
  */
sealed trait LogicalOperator extends BinaryOperator

/**
  * A common super-type for bitwise operators.
  */
sealed trait BitwiseOperator extends BinaryOperator

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
  case object LogicalAnd extends LogicalOperator

  /**
    * Logical disjunction.
    */
  case object LogicalOr extends LogicalOperator

  /**
    * Logical implication.
    */
  case object Implication extends LogicalOperator

  /**
    * Logical Biconditional.
    */
  case object Biconditional extends LogicalOperator

  /**
    * Bitwise and.
    */
  case object BitwiseAnd extends BitwiseOperator

  /**
    * Bitwise or.
    */
  case object BitwiseOr extends BitwiseOperator

  /**
    * Bitwise xor.
    */
  case object BitwiseXor extends BitwiseOperator

  /**
    * Bitwise left shift.
    */
  case object BitwiseLeftShift extends BitwiseOperator

  /**
    * Bitwise right shift.
    */
  case object BitwiseRightShift extends BitwiseOperator

}
