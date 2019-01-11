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

import ca.uwaterloo.flix.language.ast.{Symbol, MonoType}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * SMT expressions.
  */
sealed trait SmtExpr {
  def tpe: MonoType
}

object SmtExpr {

  /**
    * Variable.
    */
  case class Var(sym: Symbol.VarSym, tpe: MonoType) extends SmtExpr

  /**
    * An Int8 constant.
    */
  case class Int8(lit: Byte) extends SmtExpr {
    def tpe: MonoType = MonoType.Int8
  }

  /**
    * An Int16 constant.
    */
  case class Int16(lit: Short) extends SmtExpr {
    def tpe: MonoType = MonoType.Int16
  }

  /**
    * An Int32 constant.
    */
  case class Int32(lit: Int) extends SmtExpr {
    def tpe: MonoType = MonoType.Int32
  }

  /**
    * Int64 constant.
    */
  case class Int64(lit: Long) extends SmtExpr {
    def tpe: MonoType = MonoType.Int64
  }

  /**
    * BigInt constant.
    */
  case class BigInt(lit: java.math.BigInteger) extends SmtExpr {
    def tpe: MonoType = MonoType.BigInt
  }

  /**
    * Addition.
    */
  case class Plus(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Subtraction.
    */
  case class Minus(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Multiplication.
    */
  case class Times(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Division.
    */
  case class Divide(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Modulus.
    */
    // TODO: Rename to remainder?
  case class Modulo(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Exponentiation.
    */
  case class Exponentiate(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Less.
    */
  case class Less(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Less-than or Equal.
    */
  case class LessEqual(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Greater.
    */
  case class Greater(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Greater-than or Equal.
    */
  case class GreaterEqual(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Equality.
    */
  case class Equal(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == e2.tpe)

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Dis-equality.
    */
  case class NotEqual(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == e2.tpe)

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Negation.
    */
  case class Not(e: SmtExpr) extends SmtExpr {
    assert(e.tpe == MonoType.Bool)

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Conjunction.
    */
  case class LogicalAnd(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == MonoType.Bool && e2.tpe == MonoType.Bool)

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Disjunction.
    */
  case class LogicalOr(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == MonoType.Bool && e2.tpe == MonoType.Bool)

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Implication.
    */
  case class Implication(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == MonoType.Bool && e2.tpe == MonoType.Bool)

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Bicondition.
    */
  case class Bicondition(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == MonoType.Bool && e2.tpe == MonoType.Bool)

    def tpe: MonoType = MonoType.Bool
  }

  /**
    * Bitwise Negation.
    */
  case class BitwiseNegate(e: SmtExpr) extends SmtExpr {
    assertNum(e.tpe)

    def tpe: MonoType = e.tpe
  }

  /**
    * Bitwise And.
    */
  case class BitwiseAnd(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Bitwise Or.
    */
  case class BitwiseOr(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Bitwise Xor.
    */
  case class BitwiseXor(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Bitwise Left Shift.
    */
  case class BitwiseLeftShift(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Bitwise Right Shift.
    */
  case class BitwiseRightShift(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assertNum(assertEq(e1.tpe, e2.tpe))

    def tpe: MonoType = assertEq(e1.tpe, e2.tpe)
  }

  /**
    * Asserts that the two given types `tpe1` and `tpe2` are the same. Returns the type.
    */
  private def assertEq(tpe1: MonoType, tpe2: MonoType): MonoType =
    if (tpe1 == tpe2)
      tpe1
    else
      throw InternalCompilerException(s"Unexpected non-equal types: '$tpe1' and '$tpe2'.")

  /**
    * Asserts that the given type `type` is a numeric type, i.e. a float or an int.
    */
  private def assertNum(tpe: MonoType): Unit = tpe match {
    case MonoType.Float32 => // nop
    case MonoType.Float64 => // nop
    case MonoType.Int8 => // nop
    case MonoType.Int16 => // nop
    case MonoType.Int32 => // nop
    case MonoType.Int64 => // nop
    case MonoType.BigInt => // nop
    case _ => throw InternalCompilerException(s"Unexpected non-numeric type: '$tpe'.")
  }

}