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
    * Unit Operators.
    */
  sealed trait Unit extends SemanticOperator

  object Unit {

    /**
      * Equality.
      */
    case object Eq extends Unit

    /**
      * Inequality.
      */
    case object Neq extends Unit

  }

  /**
    * Boolean Operators.
    */
  sealed trait Bool extends SemanticOperator

  object Bool {

    /**
      * Boolean Negation.
      */
    case object Not extends Bool

    /**
      * Boolean And.
      */
    case object And extends Bool

    /**
      * Boolean Or.
      */
    case object Or extends Bool

    /**
      * Equality.
      */
    case object Eq extends Bool

    /**
      * Inequality.
      */
    case object Neq extends Bool

  }

  /**
    * Char Operators.
    */
  sealed trait Char extends SemanticOperator

  object Char {

    /**
      * Equality.
      */
    case object Eq extends Char

    /**
      * Inequality.
      */
    case object Neq extends Char

    /**
      * Less than.
      */
    case object Lt extends Char

    /**
      * Less or equal.
      */
    case object Le extends Char

    /**
      * Greater than.
      */
    case object Gt extends Char

    /**
      * Greater or equal.
      */
    case object Ge extends Char

  }

  /**
    * Float32 Operators.
    */
  sealed trait Float32 extends SemanticOperator

  object Float32 {

    /**
      * Unary Plus.
      */
    case object Plus extends Float32

    /**
      * Unary Minus.
      */
    case object Minus extends Float64

    /**
      * Addition.
      */
    case object Add extends Float32

    /**
      * Subtraction.
      */
    case object Sub extends Float32

    /**
      * Multiplication.
      */
    case object Mul extends Float32

    /**
      * Division.
      */
    case object Div extends Float32

    /**
      * Remainder.
      */
    case object Rem extends Float32

    /**
      * Exponentiate.
      */
    case object Exp extends Float32

    /**
      * Equality.
      */
    case object Eq extends Float32

    /**
      * Inequality.
      */
    case object Neq extends Float32

    /**
      * Less than.
      */
    case object Lt extends Float32

    /**
      * Less or equal.
      */
    case object Le extends Float32

    /**
      * Greater than.
      */
    case object Gt extends Float32

    /**
      * Greater or equal.
      */
    case object Ge extends Float32

  }

  /**
    * Float64 Operators.
    */
  sealed trait Float64 extends SemanticOperator

  object Float64 {

    /**
      * Unary Plus.
      */
    case object Plus extends Float64

    /**
      * Unary Minus.
      */
    case object Minus extends Float64

    /**
      * Addition.
      */
    case object Add extends Float64

    /**
      * Subtraction.
      */
    case object Sub extends Float64

    /**
      * Multiplication.
      */
    case object Mul extends Float64

    /**
      * Division.
      */
    case object Div extends Float64

    /**
      * Remainder.
      */
    case object Rem extends Float64

    /**
      * Exponentiate.
      */
    case object Exp extends Float64

    /**
      * Equality.
      */
    case object Eq extends Float64

    /**
      * Inequality.
      */
    case object Neq extends Float64

    /**
      * Less than.
      */
    case object Lt extends Float64

    /**
      * Less or equal.
      */
    case object Le extends Float64

    /**
      * Greater than.
      */
    case object Gt extends Float64

    /**
      * Greater or equal.
      */
    case object Ge extends Float64

  }

  /**
    * Int8 Operators.
    */
  sealed trait Int8 extends SemanticOperator

  object Int8 {

    /**
      * Unary Plus.
      */
    case object Plus extends Int8

    /**
      * Unary Minus.
      */
    case object Minus extends Int8

    /**
      * Addition.
      */
    case object Add extends Int8

    /**
      * Subtraction.
      */
    case object Sub extends Int8

    /**
      * Multiplication.
      */
    case object Mul extends Int8

    /**
      * Division.
      */
    case object Div extends Int8

    /**
      * Remainder.
      */
    case object Rem extends Int8

    /**
      * Exponentiate.
      */
    case object Exp extends Int8

    /**
      * Bitwise Negate.
      */
    case object Neg extends Int8

    /**
      * Bitwise And.
      */
    case object And extends Int8

    /**
      * Bitwise Or.
      */
    case object Or extends Int8

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int8

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int8

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int8

    /**
      * Equality.
      */
    case object Eq extends Int8

    /**
      * Inequality.
      */
    case object Neq extends Int8

    /**
      * Less than.
      */
    case object Lt extends Int8

    /**
      * Less or equal.
      */
    case object Le extends Int8

    /**
      * Greater than.
      */
    case object Gt extends Int8

    /**
      * Greater or equal.
      */
    case object Ge extends Int8

  }

  /**
    * Int16 Operators.
    */
  sealed trait Int16 extends SemanticOperator

  object Int16 {

    /**
      * Unary Plus.
      */
    case object Plus extends Int16

    /**
      * Unary Minus.
      */
    case object Minus extends Int16

    /**
      * Addition.
      */
    case object Add extends Int16

    /**
      * Subtraction.
      */
    case object Sub extends Int16

    /**
      * Multiplication.
      */
    case object Mul extends Int16

    /**
      * Division.
      */
    case object Div extends Int16

    /**
      * Remainder.
      */
    case object Rem extends Int16

    /**
      * Exponentiate.
      */
    case object Exp extends Int16

    /**
      * Bitwise Negate.
      */
    case object Neg extends Int16

    /**
      * Bitwise And.
      */
    case object And extends Int16

    /**
      * Bitwise Or.
      */
    case object Or extends Int16

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int16

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int16

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int16

    /**
      * Equality.
      */
    case object Eq extends Int16

    /**
      * Inequality.
      */
    case object Neq extends Int16

    /**
      * Less than.
      */
    case object Lt extends Int16

    /**
      * Less or equal.
      */
    case object Le extends Int16

    /**
      * Greater than.
      */
    case object Gt extends Int16

    /**
      * Greater or equal.
      */
    case object Ge extends Int16

  }

  /**
    * Int32 Operators.
    */
  sealed trait Int32 extends SemanticOperator

  object Int32 {

    /**
      * Unary Plus.
      */
    case object Plus extends Int32

    /**
      * Unary Minus.
      */
    case object Minus extends Int32

    /**
      * Addition.
      */
    case object Add extends Int32

    /**
      * Subtraction.
      */
    case object Sub extends Int32

    /**
      * Multiplication.
      */
    case object Mul extends Int32

    /**
      * Division.
      */
    case object Div extends Int32

    /**
      * Remainder.
      */
    case object Rem extends Int32

    /**
      * Exponentiate.
      */
    case object Exp extends Int32

    /**
      * Bitwise Negate.
      */
    case object Neg extends Int32

    /**
      * Bitwise And.
      */
    case object And extends Int32

    /**
      * Bitwise Or.
      */
    case object Or extends Int32

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int32

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int32

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int32

    /**
      * Equality.
      */
    case object Eq extends Int32

    /**
      * Inequality.
      */
    case object Neq extends Int32

    /**
      * Less than.
      */
    case object Lt extends Int32

    /**
      * Less or equal.
      */
    case object Le extends Int32

    /**
      * Greater than.
      */
    case object Gt extends Int32

    /**
      * Greater or equal.
      */
    case object Ge extends Int32

  }

  /**
    * Int64 Operators.
    */
  sealed trait Int64 extends SemanticOperator

  object Int64 {

    /**
      * Unary Plus.
      */
    case object Plus extends Int64

    /**
      * Unary Minus.
      */
    case object Minus extends Int64

    /**
      * Addition.
      */
    case object Add extends Int64

    /**
      * Subtraction.
      */
    case object Sub extends Int64

    /**
      * Multiplication.
      */
    case object Mul extends Int64

    /**
      * Division.
      */
    case object Div extends Int64

    /**
      * Remainder.
      */
    case object Rem extends Int64

    /**
      * Exponentiate.
      */
    case object Exp extends Int64

    /**
      * Bitwise Negate.
      */
    case object Neg extends Int64

    /**
      * Bitwise And.
      */
    case object And extends Int64

    /**
      * Bitwise Or.
      */
    case object Or extends Int64

    /**
      * Bitwise Xor.
      */
    case object Xor extends Int64

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends Int64

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends Int64

    /**
      * Equality.
      */
    case object Eq extends Int64

    /**
      * Inequality.
      */
    case object Neq extends Int64

    /**
      * Less than.
      */
    case object Lt extends Int64

    /**
      * Less or equal.
      */
    case object Le extends Int64

    /**
      * Greater than.
      */
    case object Gt extends Int64

    /**
      * Greater or equal.
      */
    case object Ge extends Int64

  }

  /**
    * BigInt Operators.
    */
  sealed trait BigInt extends SemanticOperator

  object BigInt {

    // TODO: What operations should we support?

    /**
      * Unary Plus.
      */
    case object Plus extends BigInt

    /**
      * Unary Minus.
      */
    case object Minus extends BigInt

    /**
      * Addition.
      */
    case object Add extends BigInt

    /**
      * Subtraction.
      */
    case object Sub extends BigInt

    /**
      * Multiplication.
      */
    case object Mul extends BigInt

    /**
      * Division.
      */
    case object Div extends BigInt

    /**
      * Remainder.
      */
    case object Rem extends BigInt

    /**
      * Exponentiate.
      */
    case object Exp extends BigInt

    /**
      * Bitwise Negate.
      */
    case object Neg extends Int64

    /**
      * Bitwise And.
      */
    case object And extends BigInt

    /**
      * Bitwise Or.
      */
    case object Or extends BigInt

    /**
      * Bitwise Xor.
      */
    case object Xor extends BigInt

    /**
      * Bitwise Left Shift.
      */
    case object Shl extends BigInt

    /**
      * Bitwise Right Shift.
      */
    case object Shr extends BigInt

    /**
      * Equality.
      */
    case object Eq extends BigInt

    /**
      * Inequality.
      */
    case object Neq extends BigInt

    /**
      * Less than.
      */
    case object Lt extends BigInt

    /**
      * Less or equal.
      */
    case object Le extends BigInt

    /**
      * Greater than.
      */
    case object Gt extends BigInt

    /**
      * Greater or equal.
      */
    case object Ge extends BigInt

  }

  /**
    * Tag Operators.
    */
  sealed trait Tag extends SemanticOperator

  object Tag {

    /**
      * Equality.
      */
    case object Eq extends Tag

    /**
      * Inequality.
      */
    case object Neq extends Tag

  }


  /**
    * Tuple Operators.
    */
  sealed trait Tuple extends SemanticOperator

  object Tuple {

    /**
      * Equality.
      */
    case object Eq extends Tuple

    /**
      * Inequality.
      */
    case object Neq extends Tuple

  }

  /**
    * String Operators.
    */
  sealed trait Str extends SemanticOperator

  object Str {

    /**
      * Concatenate.
      */
    case object Concat extends Str

    /**
      * Equality.
      */
    case object Eq extends Str

    /**
      * Inequality.
      */
    case object Neq extends Str

  }

}
