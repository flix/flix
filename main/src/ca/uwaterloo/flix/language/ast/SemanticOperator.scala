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
