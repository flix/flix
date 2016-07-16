/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.evaluator

import ca.uwaterloo.flix.language.ast.{Name, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * SMT expressions.
  */
sealed trait SmtExpr {
  def tpe: Type
}

object SmtExpr {

  /**
    * An Int8 constant.
    */
  case class Int8(lit: Byte) extends SmtExpr {
    def tpe: Type = Type.Int8
  }

  /**
    * An Int16 constant.
    */
  case class Int16(lit: Short) extends SmtExpr {
    def tpe: Type = Type.Int16
  }

  /**
    * An Int32 constant.
    */
  case class Int32(lit: Int) extends SmtExpr {
    def tpe: Type = Type.Int32
  }

  /**
    * Int64 constant.
    */
  case class Int64(lit: Long) extends SmtExpr {
    def tpe: Type = Type.Int64
  }

  /**
    * BigInt constant.
    */
  case class BigInt(lit: java.math.BigInteger) extends SmtExpr {
    def tpe: Type = Type.BigInt
  }

  /**
    * Variable.
    */
  case class Var(ident: Name.Ident, tpe: Type) extends SmtExpr

  /**
    * Addition.
    */
  case class Plus(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Subtraction.
    */
  case class Minus(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Multiplication.
    */
  case class Times(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Division.
    */
  case class Divide(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Modulus.
    */
  case class Modulo(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Exponentiation.
    */
  case class Exponentiate(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Less.
    */
  case class Less(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = Type.Bool
  }

  /**
    * Less-than or Equal.
    */
  case class LessEqual(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = Type.Bool
  }

  /**
    * Greater.
    */
  case class Greater(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = Type.Bool
  }

  /**
    * Greater-than or Equal.
    */
  case class GreaterEqual(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = Type.Bool
  }

  /**
    * Equality.
    */
  case class Equal(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == e2.tpe)

    def tpe: Type = Type.Bool
  }

  /**
    * Dis-equality.
    */
  case class NotEqual(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == e2.tpe)

    def tpe: Type = Type.Bool
  }

  /**
    * Negation.
    */
  case class Not(e: SmtExpr) extends SmtExpr {
    assert(e.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  /**
    * Conjunction.
    */
  case class LogicalAnd(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == Type.Bool && e2.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  /**
    * Disjunction.
    */
  case class LogicalOr(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == Type.Bool && e2.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  /**
    * Implication.
    */
  case class Implication(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == Type.Bool && e2.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  /**
    * Bicondition.
    */
  case class Bicondition(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == Type.Bool && e2.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  /**
    * Bitwise Negation.
    */
  case class BitwiseNegate(e: SmtExpr) extends SmtExpr {
    assertNum(e.tpe)

    def tpe: Type = e.tpe
  }

  /**
    * Bitwise And.
    */
  case class BitwiseAnd(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Bitwise Or.
    */
  case class BitwiseOr(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Bitwise Xor.
    */
  case class BitwiseXor(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Bitwise Left Shift.
    */
  case class BitwiseLeftShift(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Bitwise Right Shift.
    */
  case class BitwiseRightShift(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: Type = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Asserts that the two given types `tpe1` and `tpe2` are the same. Returns the type.
    */
  private def assertEq(tpe1: Type, tpe2: Type): Type =
    if (tpe1 == tpe2)
      tpe1
    else
      throw InternalCompilerException(s"Unexpected non-equal types: '$tpe1' and '$tpe2'.")

  /**
    * Asserts that the given type `type` is a numeric type, i.e. a float or an int.
    */
  private def assertNum(tpe: Type): Unit = tpe match {
    case Type.Float32 => // nop
    case Type.Float64 => // nop
    case Type.Int8 => // nop
    case Type.Int16 => // nop
    case Type.Int32 => // nop
    case Type.Int64 => // nop
    case Type.BigInt => // nop
    case _ => throw InternalCompilerException(s"Unexpected non-numeric type: '$tpe'.")
  }

}