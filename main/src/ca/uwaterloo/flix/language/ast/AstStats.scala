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

  /**
    * Computes statistics for the given program.
    */
  def statsOf(root: ExecutableAst.Root): AstStats = {

    /**
      * Computes statistics for the given expression `exp0`.
      */
    def visitExp(exp0: ExecutableAst.Expression): AstStats = exp0 match {
      case ExecutableAst.Expression.Unit => AstStats(unitLiterals = 1)
      case ExecutableAst.Expression.True => AstStats(trueLiterals = 1)
      case ExecutableAst.Expression.False => AstStats(falseLiterals = 1)
      case ExecutableAst.Expression.Char(lit) => AstStats(charLiterals = 1)
      case ExecutableAst.Expression.Float32(lit) => AstStats(float32Literals = 1)
      case ExecutableAst.Expression.Float64(lit) => AstStats(float64Literals = 1)
      case ExecutableAst.Expression.Int8(lit) => AstStats(int8Literals = 1)
      case ExecutableAst.Expression.Int16(lit) => AstStats(int16Literals = 1)
      case ExecutableAst.Expression.Int32(lit) => AstStats(int32Literals = 1)
      case ExecutableAst.Expression.Int64(lit) => AstStats(int64Literals = 1)
      case ExecutableAst.Expression.BigInt(lit) => AstStats(bigIntLiterals = 1)
      case ExecutableAst.Expression.Str(lit) => AstStats(strLiterals = 1)
      case ExecutableAst.Expression.LoadBool(exp, _) => visitExp(exp).incLoadBool
      case ExecutableAst.Expression.LoadInt8(exp, _) => visitExp(exp).incLoadInt8
      case ExecutableAst.Expression.LoadInt16(exp, _) => visitExp(exp).incLoadInt16
      case ExecutableAst.Expression.LoadInt32(exp, _) => visitExp(exp).incLoadInt32
      case ExecutableAst.Expression.StoreBool(exp1, _, exp2) => (visitExp(exp1) + visitExp(exp2)).incStoreBool
      case ExecutableAst.Expression.StoreInt8(exp1, _, exp2) => (visitExp(exp1) + visitExp(exp2)).incStoreInt8
      case ExecutableAst.Expression.StoreInt16(exp1, _, exp2) => (visitExp(exp1) + visitExp(exp2)).incStoreInt16
      case ExecutableAst.Expression.StoreInt32(exp1, _, exp2) => (visitExp(exp1) + visitExp(exp2)).incStoreInt32
      case ExecutableAst.Expression.Var(ident, _, tpe, loc) => AstStats(varExpressions = 1)
      case ExecutableAst.Expression.Ref(name, tpe, loc) => AstStats(refExpressions = 1)
      case ExecutableAst.Expression.MkClosureRef(ref, freeVars, tpe, loc) => AstStats(mkClosureRefExpressions = 1)
      case ExecutableAst.Expression.ApplyClosure(exp, args, tpe, loc) =>
        val s = args.foldLeft(visitExp(exp)) {
          case (acc, e) => acc + visitExp(e)
        }
        s.copy(applyClosureExpressions = s.applyClosureExpressions + 1)
      case ExecutableAst.Expression.ApplyRef(name, args, tpe, loc) => AstStats(applyRefExpressions = 1)
      case ExecutableAst.Expression.ApplyHook(hook, args, tpe, loc) => AstStats(applyHookExpressions = 1)
      case ExecutableAst.Expression.Unary(op, exp, tpe, loc) => op match {
        case UnaryOperator.Plus => visitExp(exp).incUnaryPlus
        case UnaryOperator.Minus => visitExp(exp).incUnaryMinus
        case UnaryOperator.LogicalNot => visitExp(exp).incUnaryLogicalNot
        case UnaryOperator.BitwiseNegate => visitExp(exp).incUnaryBitwiseNegate
      }
      case ExecutableAst.Expression.Binary(op, exp1, exp2, tpe, loc) => op match {
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
      case ExecutableAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        (visitExp(exp1) + visitExp(exp2) + visitExp(exp3)).incIfThenElse
      case ExecutableAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
        (visitExp(exp1) + visitExp(exp2)).incLet
      case ExecutableAst.Expression.CheckTag(tag, exp, loc) =>
        visitExp(exp).incCheckTag
      case ExecutableAst.Expression.Tag(enum, tag, exp, tpe, loc) => visitExp(exp).incTag
      case ExecutableAst.Expression.GetTagValue(tag, exp, tpe, loc) =>
        visitExp(exp).incGetTagValue
      case ExecutableAst.Expression.GetTupleIndex(base, offset, tpe, loc) => visitExp(base)
      case ExecutableAst.Expression.Tuple(elms, tpe, loc) =>
        val s = elms.foldLeft(AstStats()) {
          case (acc, e) => acc + visitExp(e)
        }
        s.incTuple
      case ExecutableAst.Expression.Existential(params, exp, loc) => visitExp(exp).incExistential
      case ExecutableAst.Expression.Universal(params, exp, loc) => visitExp(exp).incUniversal
      case ExecutableAst.Expression.MatchError(tpe, loc) => AstStats(matchErrorExpressions = 1)
      case ExecutableAst.Expression.SwitchError(tpe, loc) => AstStats(switchErrorExpressions = 1)
      case ExecutableAst.Expression.UserError(tpe, loc) => AstStats(userErrorExpressions = 1)
      case _ => AstStats()
    }

    /**
      * Visit each definition.
      */
    root.definitions.foldLeft(AstStats()) {
      case (acc, defn) => acc + visitExp(defn._2.exp)
    }
  }

}

/**
  * A collection of statistics about an abstract syntax tree.
  */
case class AstStats(unitLiterals: Int = 0,
                    trueLiterals: Int = 0,
                    falseLiterals: Int = 0,
                    charLiterals: Int = 0,
                    float32Literals: Int = 0,
                    float64Literals: Int = 0,
                    int8Literals: Int = 0,
                    int16Literals: Int = 0,
                    int32Literals: Int = 0,
                    int64Literals: Int = 0,
                    bigIntLiterals: Int = 0,
                    strLiterals: Int = 0,
                    loadBoolExpressions: Int = 0,
                    loadInt8Expressions: Int = 0,
                    loadInt16Expressions: Int = 0,
                    loadInt32Expressions: Int = 0,
                    storeBoolExpressions: Int = 0,
                    storeInt8Expressions: Int = 0,
                    storeInt16Expressions: Int = 0,
                    storeInt32Expressions: Int = 0,
                    varExpressions: Int = 0,
                    refExpressions: Int = 0,
                    mkClosureRefExpressions: Int = 0,
                    applyClosureExpressions: Int = 0,
                    applyRefExpressions: Int = 0,
                    applyHookExpressions: Int = 0,
                    unaryPlusExpressions: Int = 0,
                    unaryMinusExpressions: Int = 0,
                    unaryLogicalNotExpressions: Int = 0,
                    unaryBitwiseNegateExpressions: Int = 0,
                    plusExpressions: Int = 0,
                    minusExpressions: Int = 0,
                    timesExpressions: Int = 0,
                    divideExpressions: Int = 0,
                    moduloExpressions: Int = 0,
                    exponentiateExpressions: Int = 0,
                    lessExpressions: Int = 0,
                    lessEqualExpressions: Int = 0,
                    greaterExpressions: Int = 0,
                    greaterEqualExpressions: Int = 0,
                    equalExpressions: Int = 0,
                    notEqualExpressions: Int = 0,
                    logicalAndExpressions: Int = 0,
                    logicalOrExpressions: Int = 0,
                    implicationExpressions: Int = 0,
                    biconditionalExpressions: Int = 0,
                    bitwiseOrExpressions: Int = 0,
                    bitwiseXorExpressions: Int = 0,
                    bitwiseAndExpressions: Int = 0,
                    bitwiseLeftShiftExpressions: Int = 0,
                    bitwiseRightShiftExpressions: Int = 0,
                    ifThenElseExpressions: Int = 0,
                    letExpressions: Int = 0,
                    checkTagExpressions: Int = 0,
                    getTagValueExpressions: Int = 0,
                    tagExpressions: Int = 0,
                    getTupleIndexExpressions: Int = 0,
                    tupleExpressions: Int = 0,
                    checkNilExpressions: Int = 0,
                    checkConsExpressions: Int = 0,
                    existentialExpressions: Int = 0,
                    universalExpressions: Int = 0,
                    matchErrorExpressions: Int = 0,
                    userErrorExpressions: Int = 0,
                    switchErrorExpressions: Int = 0
                   ) {

  def +(that: AstStats): AstStats = AstStats(
    this.unitLiterals + that.unitLiterals,
    this.trueLiterals + that.trueLiterals,
    this.falseLiterals + that.falseLiterals,
    this.charLiterals + that.charLiterals,
    this.float32Literals + that.float32Literals,
    this.float64Literals + that.float64Literals,
    this.int8Literals + that.int8Literals,
    this.int16Literals + that.int16Literals,
    this.int32Literals + that.int32Literals,
    this.int64Literals + that.int64Literals,
    this.bigIntLiterals + that.bigIntLiterals,
    this.strLiterals + that.strLiterals,
    this.loadBoolExpressions + that.loadBoolExpressions,
    this.loadInt8Expressions + that.loadInt8Expressions,
    this.loadInt16Expressions + that.loadInt16Expressions,
    this.loadInt32Expressions + that.loadInt32Expressions,
    this.storeBoolExpressions + that.storeBoolExpressions,
    this.storeInt8Expressions + that.storeInt8Expressions,
    this.storeInt16Expressions + that.storeInt16Expressions,
    this.storeInt32Expressions + that.storeInt32Expressions,
    this.varExpressions + that.varExpressions,
    this.refExpressions + that.refExpressions,
    this.mkClosureRefExpressions + that.mkClosureRefExpressions,
    this.applyClosureExpressions + that.applyClosureExpressions,
    this.applyRefExpressions + that.applyRefExpressions,
    this.applyHookExpressions + that.applyHookExpressions,
    this.unaryPlusExpressions + that.unaryPlusExpressions,
    this.unaryMinusExpressions + that.unaryMinusExpressions,
    this.unaryLogicalNotExpressions + that.unaryLogicalNotExpressions,
    this.unaryBitwiseNegateExpressions + that.unaryBitwiseNegateExpressions,
    this.plusExpressions + that.plusExpressions,
    this.minusExpressions + that.minusExpressions,
    this.timesExpressions + that.timesExpressions,
    this.divideExpressions + that.divideExpressions,
    this.moduloExpressions + that.moduloExpressions,
    this.exponentiateExpressions + that.exponentiateExpressions,
    this.lessExpressions + that.lessExpressions,
    this.lessEqualExpressions + that.lessEqualExpressions,
    this.greaterExpressions + that.greaterExpressions,
    this.greaterEqualExpressions + that.greaterEqualExpressions,
    this.equalExpressions + that.equalExpressions,
    this.notEqualExpressions + that.notEqualExpressions,
    this.logicalAndExpressions + that.logicalAndExpressions,
    this.logicalOrExpressions + that.logicalOrExpressions,
    this.implicationExpressions + that.implicationExpressions,
    this.biconditionalExpressions + that.biconditionalExpressions,
    this.bitwiseOrExpressions + that.bitwiseOrExpressions,
    this.bitwiseXorExpressions + that.bitwiseXorExpressions,
    this.bitwiseAndExpressions + that.bitwiseAndExpressions,
    this.bitwiseLeftShiftExpressions + that.bitwiseLeftShiftExpressions,
    this.bitwiseRightShiftExpressions + that.bitwiseRightShiftExpressions,
    this.ifThenElseExpressions + that.ifThenElseExpressions,
    this.letExpressions + that.letExpressions,
    this.checkTagExpressions + that.checkTagExpressions,
    this.getTagValueExpressions + that.getTagValueExpressions,
    this.tagExpressions + that.tagExpressions,
    this.getTupleIndexExpressions + that.getTupleIndexExpressions,
    this.tupleExpressions + that.tupleExpressions,
    this.checkNilExpressions + that.checkNilExpressions,
    this.checkConsExpressions + that.checkConsExpressions,
    this.existentialExpressions + that.existentialExpressions,
    this.universalExpressions + that.universalExpressions,
    this.matchErrorExpressions + that.matchErrorExpressions,
    this.userErrorExpressions + that.userErrorExpressions,
    this.switchErrorExpressions + that.switchErrorExpressions
  )

  def incLoadBool: AstStats = copy(loadBoolExpressions = loadBoolExpressions + 1)

  def incLoadInt8: AstStats = copy(loadInt8Expressions = loadInt8Expressions + 1)

  def incLoadInt16: AstStats = copy(loadInt16Expressions = loadInt16Expressions + 1)

  def incLoadInt32: AstStats = copy(loadInt32Expressions = loadInt32Expressions + 1)

  def incStoreBool: AstStats = copy(storeBoolExpressions = storeBoolExpressions + 1)

  def incStoreInt8: AstStats = copy(storeInt8Expressions = storeInt8Expressions + 1)

  def incStoreInt16: AstStats = copy(storeInt16Expressions = storeInt16Expressions + 1)

  def incStoreInt32: AstStats = copy(storeInt32Expressions = storeInt32Expressions + 1)

  def incUnaryPlus: AstStats = copy(unaryPlusExpressions = unaryPlusExpressions + 1)

  def incUnaryMinus: AstStats = copy(unaryMinusExpressions = unaryMinusExpressions + 1)

  def incUnaryLogicalNot: AstStats = copy(unaryLogicalNotExpressions = unaryLogicalNotExpressions + 1)

  def incUnaryBitwiseNegate: AstStats = copy(unaryBitwiseNegateExpressions = unaryBitwiseNegateExpressions + 1)

  def incBinaryPlus: AstStats = copy(plusExpressions = plusExpressions + 1)

  def incBinaryMinus: AstStats = copy(minusExpressions = minusExpressions + 1)

  def incBinaryTimes: AstStats = copy(timesExpressions = timesExpressions + 1)

  def incBinaryDivide: AstStats = copy(divideExpressions = divideExpressions + 1)

  def incBinaryModulo: AstStats = copy(moduloExpressions = moduloExpressions + 1)

  def incBinaryExponentiate: AstStats = copy(exponentiateExpressions = exponentiateExpressions + 1)

  def incBinaryLess: AstStats = copy(lessExpressions = lessExpressions + 1)

  def incBinaryLessEqual: AstStats = copy(lessEqualExpressions = lessEqualExpressions + 1)

  def incBinaryGreater: AstStats = copy(greaterExpressions = greaterExpressions + 1)

  def incBinaryGreaterEqual: AstStats = copy(greaterEqualExpressions = greaterEqualExpressions + 1)

  def incBinaryEqual: AstStats = copy(equalExpressions = equalExpressions + 1)

  def incBinaryNotEqual: AstStats = copy(notEqualExpressions = notEqualExpressions + 1)

  def incBinaryLogicalAnd: AstStats = copy(logicalAndExpressions = logicalAndExpressions + 1)

  def incBinaryLogicalOr: AstStats = copy(logicalOrExpressions = logicalOrExpressions + 1)

  def incBinaryImplication: AstStats = copy(implicationExpressions = implicationExpressions + 1)

  def incBinaryBiconditional: AstStats = copy(biconditionalExpressions = biconditionalExpressions + 1)

  def incBinaryBitwiseAnd: AstStats = copy(bitwiseAndExpressions = bitwiseAndExpressions + 1)

  def incBinaryBitwiseOr: AstStats = copy(bitwiseOrExpressions = bitwiseOrExpressions + 1)

  def incBinaryBitwiseXor: AstStats = copy(bitwiseXorExpressions = bitwiseXorExpressions + 1)

  def incBinaryBitwiseLeftShift: AstStats = copy(bitwiseLeftShiftExpressions = bitwiseLeftShiftExpressions + 1)

  def incBinaryBitwiseRightShift: AstStats = copy(bitwiseRightShiftExpressions = bitwiseRightShiftExpressions + 1)

  def incLet: AstStats = copy(letExpressions = letExpressions + 1)

  def incCheckTag: AstStats = copy(checkTagExpressions = checkTagExpressions + 1)

  def incGetTagValue: AstStats = copy(getTagValueExpressions = getTagValueExpressions + 1)

  def incIfThenElse: AstStats = copy(ifThenElseExpressions = ifThenElseExpressions + 1)

  def incTag: AstStats = copy(tagExpressions = tagExpressions + 1)

  def incTuple: AstStats = copy(tupleExpressions = tupleExpressions + 1)

  def incExistential: AstStats = copy(existentialExpressions = existentialExpressions + 1)

  def incUniversal: AstStats = copy(universalExpressions = universalExpressions + 1)

}

