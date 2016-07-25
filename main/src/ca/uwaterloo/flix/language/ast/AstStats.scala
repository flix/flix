/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.ast

object AstStats {

  def statsOf(root: SimplifiedAst.Root): AstStats = {

    /**
      * Computes statistics for the given expression `exp0`.
      */
    def visitExp(exp0: SimplifiedAst.Expression): AstStats = exp0 match {
      case SimplifiedAst.Expression.Unit => AstStats(numberOfExpressions = 1, numberOfUnitLiterals = 1)
      case SimplifiedAst.Expression.True => AstStats(numberOfExpressions = 1, numberOfTrueLiterals = 1)
      case SimplifiedAst.Expression.False => AstStats(numberOfExpressions = 1, numberOfFalseLiterals = 1)

      // TODO: Rest
      case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) => op match {
        case UnaryOperator.Plus => visitExp(exp).incUnaryPlus
        case UnaryOperator.Minus => visitExp(exp).incUnaryMinus
        case UnaryOperator.LogicalNot => visitExp(exp).incUnaryLogicalNot
        case UnaryOperator.BitwiseNegate => visitExp(exp).incUnaryBitwiseNegate
      }
      case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) => op match {
        case BinaryOperator.Plus => (visitExp(exp1) + visitExp(exp2)).incBinaryPlus
        case BinaryOperator.Minus => (visitExp(exp1) + visitExp(exp2)).incBinaryMinus
        case BinaryOperator.Times => (visitExp(exp1) + visitExp(exp2)).incBinaryTimes
        case BinaryOperator.Divide => (visitExp(exp1) + visitExp(exp2)).incBinaryDivide
        case BinaryOperator.Modulo => (visitExp(exp1) + visitExp(exp2)).incBinaryModulo
        case BinaryOperator.Exponentiate => (visitExp(exp1) + visitExp(exp2)).incBinaryExponentiate
        case BinaryOperator.Less => (visitExp(exp1) + visitExp(exp2)).incBinaryLess
        case BinaryOperator.LessEqual => (visitExp(exp1) + visitExp(exp2)).incBinaryLessEqual
        case BinaryOperator.Greater => (visitExp(exp1) + visitExp(exp2)).incBinaryGreater
        case BinaryOperator.GreaterEqual => (visitExp(exp1) + visitExp(exp2)).incBinaryGreaterEqual
        case BinaryOperator.Equal => (visitExp(exp1) + visitExp(exp2)).incBinaryEqual
        case BinaryOperator.NotEqual => (visitExp(exp1) + visitExp(exp2)).incBinaryNotEqual
        case BinaryOperator.LogicalAnd => (visitExp(exp1) + visitExp(exp2)).incBinaryLogicalAnd
        case BinaryOperator.LogicalOr => (visitExp(exp1) + visitExp(exp2)).incBinaryLogicalOr
        case BinaryOperator.Implication => (visitExp(exp1) + visitExp(exp2)).incBinaryImplication
        case BinaryOperator.Biconditional => (visitExp(exp1) + visitExp(exp2)).incBinaryBiconditional
        case BinaryOperator.BitwiseAnd => (visitExp(exp1) + visitExp(exp2)).incBinaryBitwiseAnd
        case BinaryOperator.BitwiseOr => (visitExp(exp1) + visitExp(exp2)).incBinaryBitwiseOr
        case BinaryOperator.BitwiseXor => (visitExp(exp1) + visitExp(exp2)).incBinaryBitwiseXor
        case BinaryOperator.BitwiseLeftShift => (visitExp(exp1) + visitExp(exp2)).incBinaryBitwiseLeftShift
        case BinaryOperator.BitwiseRightShift => (visitExp(exp1) + visitExp(exp2)).incBinaryBitwiseRightShift
      }
      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        (visitExp(exp1) + visitExp(exp2) + visitExp(exp3)).incIfThenElse
      case SimplifiedAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
        (visitExp(exp1) + visitExp(exp2)).incLet



    }

    ??? // TODO
  }

}

/**
  * A collection of statistics about an abstract syntax tree.
  */
case class AstStats(numberOfExpressions: Int = 0,
                    numberOfUnitLiterals: Int = 0,
                    numberOfTrueLiterals: Int = 0,
                    numberOfFalseLiterals: Int = 0,
                    numberOfCharLiterals: Int = 0,
                    numberOfFloat32Literals: Int = 0,
                    numberOfFloat64Literals: Int = 0,
                    numberOfInt8Literals: Int = 0,
                    numberOfInt16Literals: Int = 0,
                    numberOfInt32Literals: Int = 0,
                    numberOfInt64Literals: Int = 0,
                    numberOfBigIntLiterals: Int = 0,
                    numberOfStrLiterals: Int = 0,
                    // TODO: Load and Store
                    numberOfVarExpressions: Int = 0,
                    numberOfRefExpressions: Int = 0,
                    numberOfLambdaExpressions: Int = 0,
                    numberOfHookExpressions: Int = 0,
                    numberOfMkClosureExpressions: Int = 0,
                    numberOfMkClosureRefExpressions: Int = 0,
                    numberOfApplyRefExpressions: Int = 0,
                    numberOfApplyHookExpressions: Int = 0,
                    numberOfApplyExpressions: Int = 0,
                    numberOfUnaryExpressions: Int = 0,
                    numberOfUnaryPlusExpressions: Int = 0,
                    numberOfUnaryMinusExpressions: Int = 0,
                    numberOfUnaryLogicalNotExpressions: Int = 0,
                    numberOfUnaryBitwiseNegateExpressions: Int = 0,
                    numberOfBinaryExpressions: Int = 0,
                    numberOfBinaryPlusExpressions: Int = 0,
                    numberOfBinaryMinusExpressions: Int = 0,
                    numberOfBinaryTimesExpressions: Int = 0,
                    numberOfBinaryDivideExpressions: Int = 0,
                    numberOfBinaryModuloExpressions: Int = 0,
                    numberOfBinaryExponentiateExpressions: Int = 0,
                    numberOfBinaryLessExpressions: Int = 0,
                    numberOfBinaryLessEqualExpressions: Int = 0,
                    numberOfBinaryGreaterExpressions: Int = 0,
                    numberOfBinaryGreaterEqualExpressions: Int = 0,
                    numberOfBinaryEqualExpressions: Int = 0,
                    numberOfBinaryNotEqualExpressions: Int = 0,
                    numberOfBinaryLogicalAndExpressions: Int = 0,
                    numberOfBinaryLogicalOrExpressions: Int = 0,
                    numberOfBinaryImplicationExpressions: Int = 0,
                    numberOfBinaryBiconditionalExpressions: Int = 0,
                    numberOfBinaryBitwiseOrExpressions: Int = 0,
                    numberOfBinaryBitwiseXorExpressions: Int = 0,
                    numberOfBinaryBitwiseAndExpressions: Int = 0,
                    numberOfBinaryBitwiseLeftShiftExpressions: Int = 0,
                    numberOfBinaryBitwiseRightShiftExpressions: Int = 0,
                    numberOfIfThenElseExpressions: Int = 0,
                    numberOfLetExpressions: Int = 0,
                    numberOfCheckTagExpressions: Int = 0,
                    numberOfGetTagValueExpressions: Int = 0,
                    numberOfTagExpressions: Int = 0,
                    numberOfGetTupleIndexExpressions: Int = 0,
                    numberOfTupleExpressions: Int = 0,
                    numberOfCheckNilExpressions: Int = 0,
                    numberOfCheckConsExpressions: Int = 0,
                    numberOfFSetExpressions: Int = 0,
                    numberOfExistentialExpressions: Int = 0,
                    numberOfUniversalExpressions: Int = 0,
                    numberOfMatchErrorExpressions: Int = 0,
                    numberOfUserErrorExpressions: Int = 0,
                    numberOfSwitchErrorExpressions: Int = 0
                   ) {

  def +(that: AstStats): AstStats = ???

  def incUnaryPlus: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfUnaryExpressions = numberOfUnaryExpressions + 1,
    numberOfUnaryPlusExpressions = numberOfUnaryPlusExpressions + 1
  )

  def incUnaryMinus: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfUnaryExpressions = numberOfUnaryExpressions + 1,
    numberOfUnaryMinusExpressions = numberOfUnaryMinusExpressions + 1
  )

  def incUnaryLogicalNot: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfUnaryExpressions = numberOfUnaryExpressions + 1,
    numberOfUnaryLogicalNotExpressions = numberOfUnaryLogicalNotExpressions + 1
  )

  def incUnaryBitwiseNegate: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfUnaryExpressions = numberOfUnaryExpressions + 1,
    numberOfUnaryBitwiseNegateExpressions = numberOfUnaryBitwiseNegateExpressions + 1
  )

  def incBinaryPlus: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryPlusExpressions = numberOfBinaryPlusExpressions + 1
  )

  def incBinaryMinus: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryMinusExpressions = numberOfBinaryMinusExpressions + 1
  )

  def incBinaryTimes: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryTimesExpressions = numberOfBinaryTimesExpressions + 1
  )

  def incBinaryDivide: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryDivideExpressions = numberOfBinaryDivideExpressions + 1
  )

  def incBinaryModulo: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryModuloExpressions = numberOfBinaryModuloExpressions + 1
  )

  def incBinaryExponentiate: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryExponentiateExpressions = numberOfBinaryExponentiateExpressions + 1
  )

  def incBinaryLess: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryLessExpressions = numberOfBinaryLessExpressions + 1
  )

  def incBinaryLessEqual: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryLessEqualExpressions = numberOfBinaryLessEqualExpressions + 1
  )

  def incBinaryGreater: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryGreaterExpressions = numberOfBinaryGreaterExpressions + 1
  )

  def incBinaryGreaterEqual: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryGreaterEqualExpressions = numberOfBinaryGreaterEqualExpressions + 1
  )

  def incBinaryEqual: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryEqualExpressions = numberOfBinaryEqualExpressions + 1
  )

  def incBinaryNotEqual: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryNotEqualExpressions = numberOfBinaryNotEqualExpressions + 1
  )

  def incBinaryLogicalAnd: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryLogicalAndExpressions = numberOfBinaryLogicalAndExpressions + 1
  )

  def incBinaryLogicalOr: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryLogicalOrExpressions = numberOfBinaryLogicalOrExpressions + 1
  )

  def incBinaryImplication: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryImplicationExpressions = numberOfBinaryImplicationExpressions + 1
  )

  def incBinaryBiconditional: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryBiconditionalExpressions = numberOfBinaryBiconditionalExpressions + 1
  )

  def incBinaryBitwiseAnd: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryBitwiseAndExpressions = numberOfBinaryBitwiseAndExpressions + 1
  )

  def incBinaryBitwiseOr: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryBitwiseOrExpressions = numberOfBinaryBitwiseOrExpressions + 1
  )

  def incBinaryBitwiseXor: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryBitwiseXorExpressions = numberOfBinaryBitwiseXorExpressions + 1
  )

  def incBinaryBitwiseLeftShift: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryBitwiseLeftShiftExpressions = numberOfBinaryBitwiseLeftShiftExpressions + 1
  )

  def incBinaryBitwiseRightShift: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfBinaryExpressions = numberOfBinaryExpressions + 1,
    numberOfBinaryBitwiseRightShiftExpressions = numberOfBinaryBitwiseRightShiftExpressions + 1
  )

  def incLet: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfLetExpressions = numberOfLetExpressions + 1
  )

  def incIfThenElse: AstStats = copy(
    numberOfExpressions = numberOfExpressions + 1,
    numberOfIfThenElseExpressions = numberOfIfThenElseExpressions + 1
  )

  // TODO: Rest

}

