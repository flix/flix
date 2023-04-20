/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.util.InternalCompilerException

sealed trait SemanticOperator

object SemanticOperator {

  /**
    * Boolean Operators.
    */
  sealed trait BoolOp extends SemanticOperator

  object BoolOp {

    /**
      * Boolean Not.
      */
    case object Not extends BoolOp

    /**
      * Boolean And.
      */
    case object And extends BoolOp

    /**
      * Boolean Or.
      */
    case object Or extends BoolOp

    /**
      * Equality.
      */
    case object Eq extends BoolOp

    /**
      * Inequality.
      */
    case object Neq extends BoolOp

  }

  /**
    * Char Operators.
    */
  sealed trait CharOp extends SemanticOperator

  object CharOp {

    /**
      * Equality.
      */
    case object Eq extends CharOp

    /**
      * Inequality.
      */
    case object Neq extends CharOp

    /**
      * Less than.
      */
    case object Lt extends CharOp

    /**
      * Less or equal.
      */
    case object Le extends CharOp

    /**
      * Greater than.
      */
    case object Gt extends CharOp

    /**
      * Greater or equal.
      */
    case object Ge extends CharOp

  }

  /**
    * Float32 Operators.
    */
  sealed trait Float32Op extends SemanticOperator

  object Float32Op {

    /**
      * Negation.
      */
    case object Neg extends Float32Op

    /**
      * Addition.
      */
    case object Add extends Float32Op

    /**
      * Subtraction.
      */
    case object Sub extends Float32Op

    /**
      * Multiplication.
      */
    case object Mul extends Float32Op

    /**
      * Division.
      */
    case object Div extends Float32Op

    /**
      * Exponentiate.
      */
    case object Exp extends Float32Op

    /**
      * Equality.
      */
    case object Eq extends Float32Op

    /**
      * Inequality.
      */
    case object Neq extends Float32Op

    /**
      * Less than.
      */
    case object Lt extends Float32Op

    /**
      * Less or equal.
      */
    case object Le extends Float32Op

    /**
      * Greater than.
      */
    case object Gt extends Float32Op

    /**
      * Greater or equal.
      */
    case object Ge extends Float32Op

  }

  /**
    * Float64 Operators.
    */
  sealed trait Float64Op extends SemanticOperator

  object Float64Op {

    /**
      * Negation.
      */
    case object Neg extends Float64Op

    /**
      * Addition.
      */
    case object Add extends Float64Op

    /**
      * Subtraction.
      */
    case object Sub extends Float64Op

    /**
      * Multiplication.
      */
    case object Mul extends Float64Op

    /**
      * Division.
      */
    case object Div extends Float64Op

    /**
      * Exponentiate.
      */
    case object Exp extends Float64Op

    /**
      * Equality.
      */
    case object Eq extends Float64Op

    /**
      * Inequality.
      */
    case object Neq extends Float64Op

    /**
      * Less than.
      */
    case object Lt extends Float64Op

    /**
      * Less or equal.
      */
    case object Le extends Float64Op

    /**
      * Greater than.
      */
    case object Gt extends Float64Op

    /**
      * Greater or equal.
      */
    case object Ge extends Float64Op

  }

  /**
    * BigDecimal Operators.
    */
  sealed trait BigDecimalOp extends SemanticOperator

  object BigDecimalOp {

    /**
      * Negation.
      */
    case object Neg extends BigDecimalOp

    /**
      * Addition.
      */
    case object Add extends BigDecimalOp

    /**
      * Subtraction.
      */
    case object Sub extends BigDecimalOp

    /**
      * Multiplication.
      */
    case object Mul extends BigDecimalOp

    /**
      * Division.
      */
    case object Div extends BigDecimalOp

    /**
      * Equality.
      */
    case object Eq extends BigDecimalOp

    /**
      * Inequality.
      */
    case object Neq extends BigDecimalOp

    /**
      * Less than.
      */
    case object Lt extends BigDecimalOp

    /**
      * Less or equal.
      */
    case object Le extends BigDecimalOp

    /**
      * Greater than.
      */
    case object Gt extends BigDecimalOp

    /**
      * Greater or equal.
      */
    case object Ge extends BigDecimalOp

  }

  /**
    * Int8 Operators.
    */
  sealed trait Int8Op extends SemanticOperator

  object Int8Op {

    /**
      * Negation.
      */
    case object Neg extends Int8Op

    /**
      * Bitwise Not.
      */
    case object Not extends Int8Op

    /**
      * Addition.
      */
    case object Add extends Int8Op

    /**
      * Subtraction.
      */
    case object Sub extends Int8Op

    /**
      * Multiplication.
      */
    case object Mul extends Int8Op

    /**
      * Division.
      */
    case object Div extends Int8Op

    /**
      * Remainder.
      */
    case object Rem extends Int8Op

    /**
      * Exponentiate.
      */
    case object Exp extends Int8Op

    /**
      * Bitwise And.
      */
    case object And extends Int8Op

    /**
      * Bitwise Or.
      */
    case object Or extends Int8Op

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int8Op

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int8Op

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int8Op

    /**
      * Equality.
      */
    case object Eq extends Int8Op

    /**
      * Inequality.
      */
    case object Neq extends Int8Op

    /**
      * Less than.
      */
    case object Lt extends Int8Op

    /**
      * Less or equal.
      */
    case object Le extends Int8Op

    /**
      * Greater than.
      */
    case object Gt extends Int8Op

    /**
      * Greater or equal.
      */
    case object Ge extends Int8Op

  }

  /**
    * Int16 Operators.
    */
  sealed trait Int16Op extends SemanticOperator

  object Int16Op {

    /**
      * Negation.
      */
    case object Neg extends Int16Op

    /**
      * Bitwise Not.
      */
    case object Not extends Int16Op

    /**
      * Addition.
      */
    case object Add extends Int16Op

    /**
      * Subtraction.
      */
    case object Sub extends Int16Op

    /**
      * Multiplication.
      */
    case object Mul extends Int16Op

    /**
      * Division.
      */
    case object Div extends Int16Op

    /**
      * Remainder.
      */
    case object Rem extends Int16Op

    /**
      * Exponentiate.
      */
    case object Exp extends Int16Op

    /**
      * Bitwise And.
      */
    case object And extends Int16Op

    /**
      * Bitwise Or.
      */
    case object Or extends Int16Op

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int16Op

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int16Op

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int16Op

    /**
      * Equality.
      */
    case object Eq extends Int16Op

    /**
      * Inequality.
      */
    case object Neq extends Int16Op

    /**
      * Less than.
      */
    case object Lt extends Int16Op

    /**
      * Less or equal.
      */
    case object Le extends Int16Op

    /**
      * Greater than.
      */
    case object Gt extends Int16Op

    /**
      * Greater or equal.
      */
    case object Ge extends Int16Op

  }

  /**
    * Int32 Operators.
    */
  sealed trait Int32Op extends SemanticOperator

  object Int32Op {

    /**
      * Negation.
      */
    case object Neg extends Int32Op

    /**
      * Bitwise Not.
      */
    case object Not extends Int32Op

    /**
      * Addition.
      */
    case object Add extends Int32Op

    /**
      * Subtraction.
      */
    case object Sub extends Int32Op

    /**
      * Multiplication.
      */
    case object Mul extends Int32Op

    /**
      * Division.
      */
    case object Div extends Int32Op

    /**
      * Remainder.
      */
    case object Rem extends Int32Op

    /**
      * Exponentiate.
      */
    case object Exp extends Int32Op

    /**
      * Bitwise And.
      */
    case object And extends Int32Op

    /**
      * Bitwise Or.
      */
    case object Or extends Int32Op

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int32Op

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int32Op

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int32Op

    /**
      * Equality.
      */
    case object Eq extends Int32Op

    /**
      * Inequality.
      */
    case object Neq extends Int32Op

    /**
      * Less than.
      */
    case object Lt extends Int32Op

    /**
      * Less or equal.
      */
    case object Le extends Int32Op

    /**
      * Greater than.
      */
    case object Gt extends Int32Op

    /**
      * Greater or equal.
      */
    case object Ge extends Int32Op

  }

  /**
    * Int64 Operators.
    */
  sealed trait Int64Op extends SemanticOperator

  object Int64Op {

    /**
      * Negation.
      */
    case object Neg extends Int64Op

    /**
      * Bitwise Not.
      */
    case object Not extends Int64Op

    /**
      * Addition.
      */
    case object Add extends Int64Op

    /**
      * Subtraction.
      */
    case object Sub extends Int64Op

    /**
      * Multiplication.
      */
    case object Mul extends Int64Op

    /**
      * Division.
      */
    case object Div extends Int64Op

    /**
      * Remainder.
      */
    case object Rem extends Int64Op

    /**
      * Exponentiate.
      */
    case object Exp extends Int64Op

    /**
      * Bitwise And.
      */
    case object And extends Int64Op

    /**
      * Bitwise Or.
      */
    case object Or extends Int64Op

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int64Op

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int64Op

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int64Op

    /**
      * Equality.
      */
    case object Eq extends Int64Op

    /**
      * Inequality.
      */
    case object Neq extends Int64Op

    /**
      * Less than.
      */
    case object Lt extends Int64Op

    /**
      * Less or equal.
      */
    case object Le extends Int64Op

    /**
      * Greater than.
      */
    case object Gt extends Int64Op

    /**
      * Greater or equal.
      */
    case object Ge extends Int64Op

  }

  /**
    * BigInt Operators.
    */
  sealed trait BigIntOp extends SemanticOperator

  object BigIntOp {

    /**
      * Negation.
      */
    case object Neg extends BigIntOp

    /**
      * Bitwise Not.
      */
    case object Not extends BigIntOp

    /**
      * Addition.
      */
    case object Add extends BigIntOp

    /**
      * Subtraction.
      */
    case object Sub extends BigIntOp

    /**
      * Multiplication.
      */
    case object Mul extends BigIntOp

    /**
      * Division.
      */
    case object Div extends BigIntOp

    /**
      * Remainder.
      */
    case object Rem extends BigIntOp

    /**
      * Bitwise And.
      */
    case object And extends BigIntOp

    /**
      * Bitwise Or.
      */
    case object Or extends BigIntOp

    /**
      * Bitwise Xor.
      */
    case object Xor extends BigIntOp

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends BigIntOp

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends BigIntOp

    /**
      * Equality.
      */
    case object Eq extends BigIntOp

    /**
      * Inequality.
      */
    case object Neq extends BigIntOp

    /**
      * Less than.
      */
    case object Lt extends BigIntOp

    /**
      * Less or equal.
      */
    case object Le extends BigIntOp

    /**
      * Greater than.
      */
    case object Gt extends BigIntOp

    /**
      * Greater or equal.
      */
    case object Ge extends BigIntOp

  }

  /**
    * Object Operators.
    */
  sealed trait ObjectOp extends SemanticOperator

  object ObjectOp {

    /**
      * Null equality.
      */
    case object EqNull extends ObjectOp

    /**
      * Null inequality.
      */
    case object NeqNull extends ObjectOp

  }

  /**
    * String Operators.
    */
  sealed trait StringOp extends SemanticOperator

  object StringOp {

    /**
      * Concatenate.
      */
    case object Concat extends StringOp

    /**
      * Equality.
      */
    case object Eq extends StringOp

    /**
      * Inequality.
      */
    case object Neq extends StringOp

  }

}

object SemanticOperatorOps {

  /**
    * Maps a semantic operator to a unary operator.
    *
    * @param sop The semantic operator.
    * @return The appropriate unary operator.
    */
  def toUnaryOp(sop: SemanticOperator, loc: SourceLocation): UnaryOperator = sop match {
    case SemanticOperator.BoolOp.Not => UnaryOperator.LogicalNot
    case SemanticOperator.Float32Op.Neg | SemanticOperator.Float64Op.Neg | SemanticOperator.BigDecimalOp.Neg
         | SemanticOperator.Int8Op.Neg | SemanticOperator.Int16Op.Neg | SemanticOperator.Int32Op.Neg | SemanticOperator.Int64Op.Neg
         | SemanticOperator.BigIntOp.Neg => UnaryOperator.Minus
    case SemanticOperator.Int8Op.Not | SemanticOperator.Int16Op.Not | SemanticOperator.Int32Op.Not | SemanticOperator.Int64Op.Not
         | SemanticOperator.BigIntOp.Not => UnaryOperator.BitwiseNegate
    case _ => throw InternalCompilerException(s"Unexpected unary operator: '$sop'.", loc)
  }

  /**
    * Maps a semantic operator to a binary operator.
    *
    * @param sop The semantic operator.
    * @return The appropriate binary operator.
    */
  def toBinaryOp(sop: SemanticOperator, loc: SourceLocation): BinaryOperator = sop match {
    case SemanticOperator.BoolOp.And => BinaryOperator.LogicalAnd
    case SemanticOperator.BoolOp.Or => BinaryOperator.LogicalOr

    case SemanticOperator.Float32Op.Add | SemanticOperator.Float64Op.Add | SemanticOperator.BigDecimalOp.Add
         | SemanticOperator.Int8Op.Add  | SemanticOperator.Int16Op.Add | SemanticOperator.Int16Op.Add
         | SemanticOperator.Int32Op.Add | SemanticOperator.Int64Op.Add | SemanticOperator.BigIntOp.Add => BinaryOperator.Plus

    case SemanticOperator.Float32Op.Sub | SemanticOperator.Float64Op.Sub | SemanticOperator.BigDecimalOp.Sub
         | SemanticOperator.Int8Op.Sub  | SemanticOperator.Int16Op.Sub | SemanticOperator.Int16Op.Sub
         | SemanticOperator.Int32Op.Sub | SemanticOperator.Int64Op.Sub | SemanticOperator.BigIntOp.Sub => BinaryOperator.Minus

    case SemanticOperator.Float32Op.Mul | SemanticOperator.Float64Op.Mul | SemanticOperator.BigDecimalOp.Mul
         | SemanticOperator.Int8Op.Mul  | SemanticOperator.Int16Op.Mul | SemanticOperator.Int16Op.Mul
         | SemanticOperator.Int32Op.Mul | SemanticOperator.Int64Op.Mul | SemanticOperator.BigIntOp.Mul => BinaryOperator.Times

    case SemanticOperator.Float32Op.Div | SemanticOperator.Float64Op.Div | SemanticOperator.BigDecimalOp.Div
         | SemanticOperator.Int8Op.Div  | SemanticOperator.Int16Op.Div | SemanticOperator.Int16Op.Div
         | SemanticOperator.Int32Op.Div | SemanticOperator.Int64Op.Div | SemanticOperator.BigIntOp.Div => BinaryOperator.Divide

    case SemanticOperator.Int8Op.Rem | SemanticOperator.Int16Op.Rem | SemanticOperator.Int16Op.Rem
         | SemanticOperator.Int32Op.Rem | SemanticOperator.Int64Op.Rem
         | SemanticOperator.BigIntOp.Rem => BinaryOperator.Remainder

    case SemanticOperator.Float32Op.Exp | SemanticOperator.Float64Op.Exp
         | SemanticOperator.Int8Op.Exp | SemanticOperator.Int16Op.Exp | SemanticOperator.Int16Op.Exp
         | SemanticOperator.Int32Op.Exp | SemanticOperator.Int64Op.Exp => BinaryOperator.Exponentiate

    case SemanticOperator.Int8Op.And | SemanticOperator.Int16Op.And | SemanticOperator.Int32Op.And
         | SemanticOperator.Int64Op.And | SemanticOperator.BigIntOp.And => BinaryOperator.BitwiseAnd

    case SemanticOperator.Int8Op.Or | SemanticOperator.Int16Op.Or | SemanticOperator.Int32Op.Or
         | SemanticOperator.Int64Op.Or | SemanticOperator.BigIntOp.Or => BinaryOperator.BitwiseOr

    case SemanticOperator.Int8Op.Xor | SemanticOperator.Int16Op.Xor | SemanticOperator.Int32Op.Xor
         | SemanticOperator.Int64Op.Xor | SemanticOperator.BigIntOp.Xor => BinaryOperator.BitwiseXor

    case SemanticOperator.Int8Op.Shl | SemanticOperator.Int16Op.Shl | SemanticOperator.Int32Op.Shl
         | SemanticOperator.Int64Op.Shl | SemanticOperator.BigIntOp.Shl => BinaryOperator.BitwiseLeftShift

    case SemanticOperator.Int8Op.Shr | SemanticOperator.Int16Op.Shr | SemanticOperator.Int32Op.Shr
         | SemanticOperator.Int64Op.Shr | SemanticOperator.BigIntOp.Shr => BinaryOperator.BitwiseRightShift

    case SemanticOperator.BoolOp.Eq | SemanticOperator.CharOp.Eq
         | SemanticOperator.Float32Op.Eq | SemanticOperator.Float64Op.Eq | SemanticOperator.BigDecimalOp.Eq
         | SemanticOperator.Int8Op.Eq | SemanticOperator.Int16Op.Eq | SemanticOperator.Int32Op.Eq
         | SemanticOperator.Int64Op.Eq | SemanticOperator.BigIntOp.Eq
         | SemanticOperator.StringOp.Eq => BinaryOperator.Equal

    case SemanticOperator.BoolOp.Neq | SemanticOperator.CharOp.Neq
         | SemanticOperator.Float32Op.Neq | SemanticOperator.Float64Op.Neq | SemanticOperator.BigDecimalOp.Neq
         | SemanticOperator.Int8Op.Neq | SemanticOperator.Int16Op.Neq | SemanticOperator.Int32Op.Neq
         | SemanticOperator.Int64Op.Neq | SemanticOperator.BigIntOp.Neq
         | SemanticOperator.StringOp.Neq => BinaryOperator.NotEqual

    case SemanticOperator.CharOp.Lt | SemanticOperator.Float32Op.Lt | SemanticOperator.Float64Op.Lt
         | SemanticOperator.BigDecimalOp.Lt
         | SemanticOperator.Int8Op.Lt | SemanticOperator.Int16Op.Lt | SemanticOperator.Int32Op.Lt
         | SemanticOperator.Int64Op.Lt | SemanticOperator.BigIntOp.Lt => BinaryOperator.Less

    case SemanticOperator.CharOp.Le | SemanticOperator.Float32Op.Le | SemanticOperator.Float64Op.Le
         | SemanticOperator.BigDecimalOp.Le
         | SemanticOperator.Int8Op.Le | SemanticOperator.Int16Op.Le | SemanticOperator.Int32Op.Le
         | SemanticOperator.Int64Op.Le | SemanticOperator.BigIntOp.Le => BinaryOperator.LessEqual

    case SemanticOperator.CharOp.Gt | SemanticOperator.Float32Op.Gt | SemanticOperator.Float64Op.Gt
         | SemanticOperator.BigDecimalOp.Gt
         | SemanticOperator.Int8Op.Gt | SemanticOperator.Int16Op.Gt | SemanticOperator.Int32Op.Gt
         | SemanticOperator.Int64Op.Gt | SemanticOperator.BigIntOp.Gt => BinaryOperator.Greater

    case SemanticOperator.CharOp.Ge | SemanticOperator.Float32Op.Ge | SemanticOperator.Float64Op.Ge
         | SemanticOperator.BigDecimalOp.Ge
         | SemanticOperator.Int8Op.Ge | SemanticOperator.Int16Op.Ge | SemanticOperator.Int32Op.Ge
         | SemanticOperator.Int64Op.Ge | SemanticOperator.BigIntOp.Ge => BinaryOperator.GreaterEqual

    case SemanticOperator.StringOp.Concat => BinaryOperator.Plus

    case _ => throw InternalCompilerException(s"Unexpected binary operator: '$sop'.", loc)
  }

}
