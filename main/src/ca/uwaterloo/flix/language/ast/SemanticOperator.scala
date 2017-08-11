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
  sealed trait Bool extends SemanticOperator

  object Bool {

    /**
      * Boolean Negation.
      */
    case object Not extends Bool

    /**
      * Boolean And.
      */
    case object Conj extends Bool

    /**
      * Boolean Or.
      */
    case object Disj extends Bool

  }

  /**
    * Float32 Operators.
    */
  sealed trait Float32 extends SemanticOperator

  object Float32 {

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
    * Int32 Operators.
    */
  sealed trait Int32Op extends SemanticOperator

  object Int32Op {

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

}
