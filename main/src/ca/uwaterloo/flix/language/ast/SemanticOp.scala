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

sealed trait SemanticOp

object SemanticOp {

  sealed trait UnaryOp extends SemanticOp

  sealed trait BinaryOp extends SemanticOp

  /**
    * Boolean Operators.
    */
  sealed trait BoolOp extends SemanticOp

  object BoolOp {

    /**
      * Boolean Not.
      */
    case object Not extends BoolOp with UnaryOp

    /**
      * Boolean And.
      */
    case object And extends BoolOp with BinaryOp

    /**
      * Boolean Or.
      */
    case object Or extends BoolOp with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends BoolOp with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends BoolOp with BinaryOp

  }

  /**
    * Char Operators.
    */
  sealed trait CharOp extends SemanticOp

  object CharOp {

    /**
      * Equality.
      */
    case object Eq extends CharOp with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends CharOp with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends CharOp with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends CharOp with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends CharOp with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends CharOp with BinaryOp

  }

  /**
    * Float32 Operators.
    */
  sealed trait Float32Op extends SemanticOp

  object Float32Op {

    /**
      * Negation.
      */
    case object Neg extends Float32Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Float32Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Float32Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Float32Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Float32Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Float32Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Float32Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Float32Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Float32Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Float32Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Float32Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Float32Op with BinaryOp

  }

  /**
    * Float64 Operators.
    */
  sealed trait Float64Op extends SemanticOp

  object Float64Op {

    /**
      * Negation.
      */
    case object Neg extends Float64Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Float64Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Float64Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Float64Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Float64Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Float64Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Float64Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Float64Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Float64Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Float64Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Float64Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Float64Op with BinaryOp

  }

  /**
    * Int8 Operators.
    */
  sealed trait Int8Op extends SemanticOp

  object Int8Op {

    /**
      * Negation.
      */
    case object Neg extends Int8Op with UnaryOp

    /**
      * Bitwise Not.
      */
    case object Not extends Int8Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Int8Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Int8Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Int8Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Int8Op with BinaryOp

    /**
      * Remainder.
      */
    case object Rem extends Int8Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Int8Op with BinaryOp

    /**
      * Bitwise And.
      */
    case object And extends Int8Op with BinaryOp

    /**
      * Bitwise Or.
      */
    case object Or extends Int8Op with BinaryOp

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int8Op with BinaryOp

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int8Op with BinaryOp

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int8Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Int8Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Int8Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Int8Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Int8Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Int8Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Int8Op with BinaryOp

  }

  /**
    * Int16 Operators.
    */
  sealed trait Int16Op extends SemanticOp

  object Int16Op {

    /**
      * Negation.
      */
    case object Neg extends Int16Op with UnaryOp

    /**
      * Bitwise Not.
      */
    case object Not extends Int16Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Int16Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Int16Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Int16Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Int16Op with BinaryOp

    /**
      * Remainder.
      */
    case object Rem extends Int16Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Int16Op with BinaryOp

    /**
      * Bitwise And.
      */
    case object And extends Int16Op with BinaryOp

    /**
      * Bitwise Or.
      */
    case object Or extends Int16Op with BinaryOp

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int16Op with BinaryOp

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int16Op with BinaryOp

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int16Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Int16Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Int16Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Int16Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Int16Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Int16Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Int16Op with BinaryOp

  }

  /**
    * Int32 Operators.
    */
  sealed trait Int32Op extends SemanticOp

  object Int32Op {

    /**
      * Negation.
      */
    case object Neg extends Int32Op with UnaryOp

    /**
      * Bitwise Not.
      */
    case object Not extends Int32Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Int32Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Int32Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Int32Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Int32Op with BinaryOp

    /**
      * Remainder.
      */
    case object Rem extends Int32Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Int32Op with BinaryOp

    /**
      * Bitwise And.
      */
    case object And extends Int32Op with BinaryOp

    /**
      * Bitwise Or.
      */
    case object Or extends Int32Op with BinaryOp

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int32Op with BinaryOp

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int32Op with BinaryOp

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int32Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Int32Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Int32Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Int32Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Int32Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Int32Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Int32Op with BinaryOp

  }

  /**
    * Int64 Operators.
    */
  sealed trait Int64Op extends SemanticOp

  object Int64Op {

    /**
      * Negation.
      */
    case object Neg extends Int64Op with UnaryOp

    /**
      * Bitwise Not.
      */
    case object Not extends Int64Op with UnaryOp

    /**
      * Addition.
      */
    case object Add extends Int64Op with BinaryOp

    /**
      * Subtraction.
      */
    case object Sub extends Int64Op with BinaryOp

    /**
      * Multiplication.
      */
    case object Mul extends Int64Op with BinaryOp

    /**
      * Division.
      */
    case object Div extends Int64Op with BinaryOp

    /**
      * Remainder.
      */
    case object Rem extends Int64Op with BinaryOp

    /**
      * Exponentiate.
      */
    case object Exp extends Int64Op with BinaryOp

    /**
      * Bitwise And.
      */
    case object And extends Int64Op with BinaryOp

    /**
      * Bitwise Or.
      */
    case object Or extends Int64Op with BinaryOp

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int64Op with BinaryOp

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int64Op with BinaryOp

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int64Op with BinaryOp

    /**
      * Equality.
      */
    case object Eq extends Int64Op with BinaryOp

    /**
      * Inequality.
      */
    case object Neq extends Int64Op with BinaryOp

    /**
      * Less than.
      */
    case object Lt extends Int64Op with BinaryOp

    /**
      * Less or equal.
      */
    case object Le extends Int64Op with BinaryOp

    /**
      * Greater than.
      */
    case object Gt extends Int64Op with BinaryOp

    /**
      * Greater or equal.
      */
    case object Ge extends Int64Op with BinaryOp

  }

  /**
    * String Operators.
    */
  sealed trait StringOp extends SemanticOp

  object StringOp {

    /**
      * Concatenate.
      */
    case object Concat extends StringOp with BinaryOp

  }

}
