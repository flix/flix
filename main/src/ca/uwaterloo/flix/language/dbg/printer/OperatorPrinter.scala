/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.SemanticOperator._
import ca.uwaterloo.flix.language.ast._

object OperatorPrinter {

  private val and = "and"
  private val div = "/"
  private val eq = "=="
  private val exp = "**"
  private val ge = ">="
  private val gt = ">"
  private val le = "<="
  private val lt = "<"
  private val minus = "-"
  private val mul = "*"
  private val neg = "-"
  private val neq = "!="
  private val not = "!"
  private val or = "or"
  private val plus = "+"
  private val rem = "rem"
  private val shl = "shl"
  private val shr = "shr"
  private val xor = "xor"

  /**
    * Returns the string representation of `uo`.
    */
  def print(uo: UnaryOperator): String = uo match {
    case UnaryOperator.LogicalNot => not
    case UnaryOperator.Plus => plus
    case UnaryOperator.Minus => minus
    case UnaryOperator.BitwiseNegate => "b_" + neg
  }

  /**
    * Returns the string representation of `bo`.
    */
  def print(bo: BinaryOperator): String = bo match {
    case BinaryOperator.Plus => plus
    case BinaryOperator.Minus => minus
    case BinaryOperator.Times => mul
    case BinaryOperator.Divide => div
    case BinaryOperator.Remainder => rem
    case BinaryOperator.Exponentiate => exp
    case BinaryOperator.Equal => eq
    case BinaryOperator.NotEqual => neq
    case BinaryOperator.Spaceship => "<=>"
    case BinaryOperator.Less => lt
    case BinaryOperator.LessEqual => le
    case BinaryOperator.Greater => gt
    case BinaryOperator.GreaterEqual => ge
    case BinaryOperator.LogicalAnd => and
    case BinaryOperator.LogicalOr => or
    case BinaryOperator.BitwiseAnd => "b_" + and
    case BinaryOperator.BitwiseOr => "b_" + or
    case BinaryOperator.BitwiseXor => "b_" + xor
    case BinaryOperator.BitwiseLeftShift => "b_" + shl
    case BinaryOperator.BitwiseRightShift => "b_" + shr
  }

  /**
    * Returns the string representation of `so`.
    */
  def print(so: SemanticOperator): String = so match {
    case BoolOp.Not |
         Int8Op.Not |
         Int16Op.Not |
         Int32Op.Not |
         Int64Op.Not |
         BigIntOp.Not => not
    case BoolOp.And |
         Int8Op.And |
         Int16Op.And |
         Int32Op.And |
         BigIntOp.And => and
    case BoolOp.Or |
         Int8Op.Or |
         Int16Op.Or |
         Int32Op.Or |
         Int64Op.Or |
         BigIntOp.Or => or
    case BoolOp.Eq |
         Float32Op.Eq |
         CharOp.Eq |
         Float64Op.Eq |
         BigDecimalOp.Eq |
         Int8Op.Eq |
         Int16Op.Eq |
         Int32Op.Eq |
         Int64Op.Eq |
         BigIntOp.Eq |
         StringOp.Eq => eq
    case BoolOp.Neq |
         CharOp.Neq |
         Float32Op.Neq |
         Float64Op.Neq |
         BigDecimalOp.Neq |
         Int8Op.Neq |
         Int16Op.Neq |
         Int32Op.Neq |
         Int64Op.Neq |
         BigIntOp.Neq |
         StringOp.Neq => neq
    case CharOp.Lt |
         Float32Op.Lt |
         Float64Op.Lt |
         BigDecimalOp.Lt |
         Int8Op.Lt |
         Int16Op.Lt |
         Int32Op.Lt |
         Int64Op.Lt |
         BigIntOp.Lt => lt
    case CharOp.Le |
         Float32Op.Le |
         Float64Op.Le |
         BigDecimalOp.Le |
         Int8Op.Le |
         Int16Op.Le |
         Int32Op.Le |
         Int64Op.Le |
         BigIntOp.Le => le
    case CharOp.Gt |
         Float32Op.Gt |
         Float64Op.Gt |
         BigDecimalOp.Gt |
         Int8Op.Gt |
         Int16Op.Gt |
         Int32Op.Gt |
         Int64Op.Gt |
         BigIntOp.Gt => gt
    case CharOp.Ge |
         Float32Op.Ge |
         Float64Op.Ge |
         BigDecimalOp.Ge |
         Int8Op.Ge |
         Int16Op.Ge |
         Int32Op.Ge |
         Int64Op.Ge |
         BigIntOp.Ge => ge
    case Float32Op.Add |
         Float64Op.Add |
         BigDecimalOp.Add |
         Int8Op.Add |
         Int16Op.Add |
         Int32Op.Add |
         Int64Op.Add |
         Int64Op.And |
         BigIntOp.Add |
         StringOp.Concat => plus
    case Float32Op.Sub |
         Float64Op.Sub |
         BigDecimalOp.Sub |
         Int8Op.Sub |
         Int16Op.Sub |
         Int32Op.Sub |
         Int64Op.Sub |
         BigIntOp.Sub => minus
    case Float32Op.Mul |
         Float64Op.Mul |
         BigDecimalOp.Mul |
         Int8Op.Mul |
         Int16Op.Mul |
         Int32Op.Mul |
         Int64Op.Mul |
         BigIntOp.Mul => mul
    case Float32Op.Div |
         Float64Op.Div |
         BigDecimalOp.Div |
         Int8Op.Div |
         Int16Op.Div |
         Int32Op.Div |
         Int64Op.Div |
         BigIntOp.Div => div
    case Float32Op.Exp |
         Float64Op.Exp |
         Int8Op.Exp |
         Int16Op.Exp |
         Int32Op.Exp |
         Int64Op.Exp => exp
    case Float32Op.Neg |
         Float64Op.Neg |
         BigDecimalOp.Neg |
         Int8Op.Neg |
         Int16Op.Neg |
         Int32Op.Neg |
         Int64Op.Neg |
         BigIntOp.Neg => neg
    case Int8Op.Rem |
         Int16Op.Rem |
         Int32Op.Rem |
         Int64Op.Rem |
         BigIntOp.Rem => rem
    case Int8Op.Xor |
         Int16Op.Xor |
         Int32Op.Xor |
         Int64Op.Xor |
         BigIntOp.Xor => xor
    case Int8Op.Shl |
         Int16Op.Shl |
         Int32Op.Shl |
         Int64Op.Shl |
         BigIntOp.Shl => shl
    case Int8Op.Shr |
         Int16Op.Shr |
         Int32Op.Shr |
         Int64Op.Shr |
         BigIntOp.Shr => shr
    case ObjectOp.EqNull => "null == "
    case ObjectOp.NeqNull => "null != "
  }

}
