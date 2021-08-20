/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.SemanticOperator._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.EraserMonad.ToMonad
import ca.uwaterloo.flix.language.phase.sjvm.{ClosureInfo, NamespaceInfo, SjvmOps}
import ca.uwaterloo.flix.language.phase.{EraserMonad => EM}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

object Eraser extends Phase[FinalAst.Root, ErasedAst.Root] {

  def run(root: FinalAst.Root)(implicit flix: Flix): Validation[ErasedAst.Root, CompilationError] = flix.phase("Eraser") {
    val defns = EM.foldRight(root.defs)(Map[Symbol.DefnSym, ErasedAst.Def[_ <: PType]]().toMonad) {
      case ((k, v), mapp) =>
        visitDef[PType](v) map (defn => mapp + (k -> defn))
    }

    // TODO(JLS): add enums to root and collect tags based on it

    val closureSyms = defns.closures.map(_.sym)

    // TODO(JLS): should maybe be integrated to the other fold
    val defnsResult = defns.setNamespaces(defns.value.groupBy(_._1.namespace).map {
      case (ns, defs) =>
        // Collect all non-law definitions.
        val nonLaws = defs filter {
          case (sym, defn) => SjvmOps.nonLaw(defn) && !closureSyms.contains(sym)
        }
        NamespaceInfo(ns, nonLaws)
    }.toSet)

    // TODO(JLS): the function list should be split into closures and functions (and maybe include nonLaw checking)
    val result = ErasedAst.Root(defnsResult.value, root.reachable, root.sources, defnsResult.types, defnsResult.closures, defnsResult.enumSyms, defnsResult.namespaces)
    result.toSuccess
  }

  /**
    * Translates the given definition `def0` to the ErasedAst.
    */
  private def visitDef[T <: PType](def0: FinalAst.Def): EraserMonad[ErasedAst.Def[T]] = {
    for {
      formals0 <- EM.traverse(def0.formals)(visitFormalParam)
      exp <- visitExp[T](def0.exp)
      tpe <- visitTpe[PReference[PFunction[T]]](def0.tpe)
    } yield ErasedAst.Def(def0.ann, def0.mod, def0.sym, formals0, exp, tpe, def0.loc)
  }

  /**
    * Translates the given expression `exp0` to the ErasedAst.
    */
  private def visitExp[T <: PType](baseExp: FinalAst.Expression): EraserMonad[ErasedAst.Expression[T]] = baseExp match {
    case FinalAst.Expression.Unit(loc) =>
      ErasedAst.Expression.Unit(loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Null(tpe, loc) =>
      for {
        tpe0 <- visitTpe[PReference[PRefType]](tpe)
        expRes = ErasedAst.Expression.Null[PRefType](tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.True(loc) =>
      ErasedAst.Expression.True(loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.False(loc) =>
      ErasedAst.Expression.False(loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Char(lit, loc) =>
      ErasedAst.Expression.Char(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Float32(lit, loc) =>
      ErasedAst.Expression.Float32(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Float64(lit, loc) =>
      ErasedAst.Expression.Float64(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Int8(lit, loc) =>
      ErasedAst.Expression.Int8(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Int16(lit, loc) =>
      ErasedAst.Expression.Int16(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Int32(lit, loc) =>
      ErasedAst.Expression.Int32(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Int64(lit, loc) =>
      ErasedAst.Expression.Int64(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.BigInt(lit, loc) =>
      ErasedAst.Expression.BigInt(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Str(lit, loc) =>
      ErasedAst.Expression.Str(lit, loc).asInstanceOf[ErasedAst.Expression[T]].toMonad

    case FinalAst.Expression.Var(sym, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
      } yield ErasedAst.Expression.Var(sym, tpe0, loc)

    case FinalAst.Expression.Closure(sym, freeVars, _, tpe, loc) =>
      val freeVarsMonad = EM.traverse(freeVars)(fv => visitTpe[PType](fv.tpe) map (
        tpe0 => ErasedAst.FreeVar(fv.sym, tpe0))
      )
      EM.flatMapN(
        freeVarsMonad,
        visitTpe[PReference[PFunction[T]]](tpe)
      ) {
        case (freeVars0, tpe0) =>
          val expRes = ErasedAst.Expression.Closure(sym, freeVars0, tpe0, loc)
          val result = expRes.asInstanceOf[ErasedAst.Expression[T]]
          result.toMonad.setClosures(Set(ClosureInfo(sym, freeVars0, tpe0.asInstanceOf[RType[PReference[PFunction[_ <: PType]]]])))
      }

    case FinalAst.Expression.ApplyClo(exp, args, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        args0 <- EM.traverse(args)(visitExp[PType])
        exp0 <- visitExp[PReference[PFunction[T]]](exp)
        expRes = ErasedAst.Expression.ApplyClo(exp0, args0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyDef(sym, args, tpe, loc) =>
      for {
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        fnType <- visitTpe[PReference[PFunction[T]]](MonoType.Arrow(args.map(_.tpe), tpe))
        expRes = ErasedAst.Expression.ApplyDef(sym, args0, fnType, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PFunction[T]]](exp)
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.ApplyCloTail(exp0, args0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
      for {
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        fnType <- visitTpe[PReference[PFunction[T]]](MonoType.Arrow(args.map(_.tpe), tpe))
        expRes = ErasedAst.Expression.ApplyDefTail(sym, args0, fnType, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      for {
        formals0 <- EM.traverse(formals)(fp =>
          visitTpe[PType](fp.tpe) map (tpe0 => ErasedAst.FormalParam(fp.sym, tpe0))
        )
        actuals0 <- EM.traverse(actuals)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        fnType <- visitTpe[PReference[PFunction[T]]](MonoType.Arrow(actuals.map(_.tpe), tpe))
        expRes = ErasedAst.Expression.ApplySelfTail(sym, formals0, actuals0, fnType, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    // TODO(JLS): This could certainly be prettier
    case FinalAst.Expression.Unary(sop, _, exp, tpe, loc) =>
      def compileUnary
      [ExpType <: PType, TpeType <: PType]
      (combinator: (ErasedAst.Expression[ExpType], RType[TpeType]) => ErasedAst.Expression[_ <: PType]):
      EraserMonad[ErasedAst.Expression[T]] = for {
        exp0 <- visitExp[ExpType](exp)
        tpe0 <- visitTpe[TpeType](tpe)
      } yield combinator(exp0, tpe0).asInstanceOf[ErasedAst.Expression[T]]

      import ErasedAst.Expression._
      sop match {
        case SemanticOperator.BoolOp.Not =>
          compileUnary[PInt32, PInt32] { (exp0, tpe0) => BoolNot(exp0, tpe0, loc) }
        case SemanticOperator.Float32Op.Neg =>
          compileUnary[PFloat32, PFloat32] { (exp0, tpe0) => Float32Neg(exp0, tpe0, loc) }
        case SemanticOperator.Float64Op.Neg =>
          compileUnary[PFloat64, PFloat64] { (exp0, tpe0) => Float64Neg(exp0, tpe0, loc) }
        case SemanticOperator.Int8Op.Neg =>
          compileUnary[PInt8, PInt8] { (exp0, tpe0) => Int8Neg(exp0, tpe0, loc) }
        case SemanticOperator.Int16Op.Neg =>
          compileUnary[PInt16, PInt16] { (exp0, tpe0) => Int16Neg(exp0, tpe0, loc) }
        case SemanticOperator.Int32Op.Neg =>
          compileUnary[PInt32, PInt32] { (exp0, tpe0) => Int32Neg(exp0, tpe0, loc) }
        case SemanticOperator.Int64Op.Neg =>
          compileUnary[PInt64, PInt64] { (exp0, tpe0) => Int64Neg(exp0, tpe0, loc) }
        case SemanticOperator.BigIntOp.Neg =>
          compileUnary[PReference[PBigInt], PReference[PBigInt]] { (exp0, tpe0) => BigIntNeg(exp0, tpe0, loc) }
        case SemanticOperator.Int8Op.Not =>
          compileUnary[PInt8, PInt8] { (exp0, tpe0) => Int8Not(exp0, tpe0, loc) }
        case SemanticOperator.Int16Op.Not =>
          compileUnary[PInt16, PInt16] { (exp0, tpe0) => Int16Not(exp0, tpe0, loc) }
        case SemanticOperator.Int32Op.Not =>
          compileUnary[PInt32, PInt32] { (exp0, tpe0) => Int32Not(exp0, tpe0, loc) }
        case SemanticOperator.Int64Op.Not =>
          compileUnary[PInt64, PInt64] { (exp0, tpe0) => Int64Not(exp0, tpe0, loc) }
        case SemanticOperator.BigIntOp.Not =>
          compileUnary[PReference[PBigInt], PReference[PBigInt]] { (exp0, tpe0) => BigIntNot(exp0, tpe0, loc) }
        case SemanticOperator.ObjectOp.EqNull =>
          compileUnary[PReference[PRefType], PInt32] { (exp0, tpe0) => ObjEqNull(exp0, tpe0, loc) }
        case SemanticOperator.ObjectOp.NeqNull =>
          compileUnary[PReference[PRefType], PInt32] { (exp0, tpe0) => ObjNeqNull(exp0, tpe0, loc) }
        case _ => throw InternalCompilerException(s"Unary expression not Implemented ${sop.getClass.getCanonicalName}")
      }

    // TODO(JLS): This could certainly be prettier
    case FinalAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      def compileBinary
      [Exp1Type <: PType, Exp2Type <: PType, TpeType <: PType]
      (combinator: (ErasedAst.Expression[Exp1Type], ErasedAst.Expression[Exp2Type], RType[TpeType]) => ErasedAst.Expression[_ <: PType]):
      EraserMonad[ErasedAst.Expression[T]] = for {
        exp10 <- visitExp[Exp1Type](exp1)
        exp20 <- visitExp[Exp2Type](exp2)
        tpe0 <- visitTpe[TpeType](tpe)
        expRes = combinator(exp10, exp20, tpe0)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

      import ErasedAst.Expression._
      sop match {
        case op: SemanticOperator.BoolOp => op match {
          case BoolOp.Not => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case BoolOp.And => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => BoolLogicalOp(LogicalOp.And, exp10, exp20, tpe0, loc) }
          case BoolOp.Or => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => BoolLogicalOp(LogicalOp.Or, exp10, exp20, tpe0, loc) }
          case BoolOp.Eq => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => BoolEquality(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case BoolOp.Neq => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => BoolEquality(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
        }
        case op: SemanticOperator.CharOp => op match {
          case CharOp.Eq => compileBinary[PChar, PChar, PInt32] { (exp10, exp20, tpe0) => CharComparison(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case CharOp.Neq => compileBinary[PChar, PChar, PInt32] { (exp10, exp20, tpe0) => CharComparison(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
          case CharOp.Lt => compileBinary[PChar, PChar, PInt32] { (exp10, exp20, tpe0) => CharComparison(ComparisonOp.Lt, exp10, exp20, tpe0, loc) }
          case CharOp.Le => compileBinary[PChar, PChar, PInt32] { (exp10, exp20, tpe0) => CharComparison(ComparisonOp.Le, exp10, exp20, tpe0, loc) }
          case CharOp.Gt => compileBinary[PChar, PChar, PInt32] { (exp10, exp20, tpe0) => CharComparison(ComparisonOp.Gt, exp10, exp20, tpe0, loc) }
          case CharOp.Ge => compileBinary[PChar, PChar, PInt32] { (exp10, exp20, tpe0) => CharComparison(ComparisonOp.Ge, exp10, exp20, tpe0, loc) }
        }
        case op: SemanticOperator.Float32Op => op match {
          case Float32Op.Neg => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Float32Op.Add => compileBinary[PFloat32, PFloat32, PFloat32] { (exp10, exp20, tpe0) => Float32Arithmetic(ArithmeticOp.Add, exp10, exp20, tpe0, loc) }
          case Float32Op.Sub => compileBinary[PFloat32, PFloat32, PFloat32] { (exp10, exp20, tpe0) => Float32Arithmetic(ArithmeticOp.Sub, exp10, exp20, tpe0, loc) }
          case Float32Op.Mul => compileBinary[PFloat32, PFloat32, PFloat32] { (exp10, exp20, tpe0) => Float32Arithmetic(ArithmeticOp.Mul, exp10, exp20, tpe0, loc) }
          case Float32Op.Div => compileBinary[PFloat32, PFloat32, PFloat32] { (exp10, exp20, tpe0) => Float32Arithmetic(ArithmeticOp.Div, exp10, exp20, tpe0, loc) }
          case Float32Op.Rem => compileBinary[PFloat32, PFloat32, PFloat32] { (exp10, exp20, tpe0) => Float32Arithmetic(ArithmeticOp.Rem, exp10, exp20, tpe0, loc) }
          case Float32Op.Exp => compileBinary[PFloat32, PFloat32, PFloat32] { (exp10, exp20, tpe0) => Float32Arithmetic(ArithmeticOp.Exp, exp10, exp20, tpe0, loc) }
          case Float32Op.Eq => compileBinary[PFloat32, PFloat32, PInt32] { (exp10, exp20, tpe0) => Float32Comparison(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case Float32Op.Neq => compileBinary[PFloat32, PFloat32, PInt32] { (exp10, exp20, tpe0) => Float32Comparison(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
          case Float32Op.Lt => compileBinary[PFloat32, PFloat32, PInt32] { (exp10, exp20, tpe0) => Float32Comparison(ComparisonOp.Lt, exp10, exp20, tpe0, loc) }
          case Float32Op.Le => compileBinary[PFloat32, PFloat32, PInt32] { (exp10, exp20, tpe0) => Float32Comparison(ComparisonOp.Le, exp10, exp20, tpe0, loc) }
          case Float32Op.Gt => compileBinary[PFloat32, PFloat32, PInt32] { (exp10, exp20, tpe0) => Float32Comparison(ComparisonOp.Gt, exp10, exp20, tpe0, loc) }
          case Float32Op.Ge => compileBinary[PFloat32, PFloat32, PInt32] { (exp10, exp20, tpe0) => Float32Comparison(ComparisonOp.Ge, exp10, exp20, tpe0, loc) }
        }
        case op: SemanticOperator.Float64Op => op match {
          case Float64Op.Neg => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Float64Op.Add => compileBinary[PFloat64, PFloat64, PFloat64] { (exp10, exp20, tpe0) => Float64Arithmetic(ArithmeticOp.Add, exp10, exp20, tpe0, loc) }
          case Float64Op.Sub => compileBinary[PFloat64, PFloat64, PFloat64] { (exp10, exp20, tpe0) => Float64Arithmetic(ArithmeticOp.Sub, exp10, exp20, tpe0, loc) }
          case Float64Op.Mul => compileBinary[PFloat64, PFloat64, PFloat64] { (exp10, exp20, tpe0) => Float64Arithmetic(ArithmeticOp.Mul, exp10, exp20, tpe0, loc) }
          case Float64Op.Div => compileBinary[PFloat64, PFloat64, PFloat64] { (exp10, exp20, tpe0) => Float64Arithmetic(ArithmeticOp.Div, exp10, exp20, tpe0, loc) }
          case Float64Op.Rem => compileBinary[PFloat64, PFloat64, PFloat64] { (exp10, exp20, tpe0) => Float64Arithmetic(ArithmeticOp.Rem, exp10, exp20, tpe0, loc) }
          case Float64Op.Exp => compileBinary[PFloat64, PFloat64, PFloat64] { (exp10, exp20, tpe0) => Float64Arithmetic(ArithmeticOp.Exp, exp10, exp20, tpe0, loc) }
          case Float64Op.Eq => compileBinary[PFloat64, PFloat64, PInt32] { (exp10, exp20, tpe0) => Float64Comparison(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case Float64Op.Neq => compileBinary[PFloat64, PFloat64, PInt32] { (exp10, exp20, tpe0) => Float64Comparison(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
          case Float64Op.Lt => compileBinary[PFloat64, PFloat64, PInt32] { (exp10, exp20, tpe0) => Float64Comparison(ComparisonOp.Lt, exp10, exp20, tpe0, loc) }
          case Float64Op.Le => compileBinary[PFloat64, PFloat64, PInt32] { (exp10, exp20, tpe0) => Float64Comparison(ComparisonOp.Le, exp10, exp20, tpe0, loc) }
          case Float64Op.Gt => compileBinary[PFloat64, PFloat64, PInt32] { (exp10, exp20, tpe0) => Float64Comparison(ComparisonOp.Gt, exp10, exp20, tpe0, loc) }
          case Float64Op.Ge => compileBinary[PFloat64, PFloat64, PInt32] { (exp10, exp20, tpe0) => Float64Comparison(ComparisonOp.Ge, exp10, exp20, tpe0, loc) }
        }
        case op: SemanticOperator.Int8Op => op match {
          case Int8Op.Neg => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Int8Op.Not => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Int8Op.Add => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Arithmetic(ArithmeticOp.Add, exp10, exp20, tpe0, loc) }
          case Int8Op.Sub => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Arithmetic(ArithmeticOp.Sub, exp10, exp20, tpe0, loc) }
          case Int8Op.Mul => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Arithmetic(ArithmeticOp.Mul, exp10, exp20, tpe0, loc) }
          case Int8Op.Div => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Arithmetic(ArithmeticOp.Div, exp10, exp20, tpe0, loc) }
          case Int8Op.Rem => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Arithmetic(ArithmeticOp.Rem, exp10, exp20, tpe0, loc) }
          case Int8Op.Exp => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Arithmetic(ArithmeticOp.Exp, exp10, exp20, tpe0, loc) }
          case Int8Op.And => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Bitwise(BitwiseOp.And, exp10, exp20, tpe0, loc) }
          case Int8Op.Or => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Bitwise(BitwiseOp.Or, exp10, exp20, tpe0, loc) }
          case Int8Op.Xor => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Bitwise(BitwiseOp.Xor, exp10, exp20, tpe0, loc) }
          case Int8Op.Shl => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Bitwise(BitwiseOp.Shl, exp10, exp20, tpe0, loc) }
          case Int8Op.Shr => compileBinary[PInt8, PInt8, PInt8] { (exp10, exp20, tpe0) => Int8Bitwise(BitwiseOp.Shr, exp10, exp20, tpe0, loc) }
          case Int8Op.Eq => compileBinary[PInt8, PInt8, PInt32] { (exp10, exp20, tpe0) => Int8Comparison(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case Int8Op.Neq => compileBinary[PInt8, PInt8, PInt32] { (exp10, exp20, tpe0) => Int8Comparison(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
          case Int8Op.Lt => compileBinary[PInt8, PInt8, PInt32] { (exp10, exp20, tpe0) => Int8Comparison(ComparisonOp.Lt, exp10, exp20, tpe0, loc) }
          case Int8Op.Le => compileBinary[PInt8, PInt8, PInt32] { (exp10, exp20, tpe0) => Int8Comparison(ComparisonOp.Le, exp10, exp20, tpe0, loc) }
          case Int8Op.Gt => compileBinary[PInt8, PInt8, PInt32] { (exp10, exp20, tpe0) => Int8Comparison(ComparisonOp.Gt, exp10, exp20, tpe0, loc) }
          case Int8Op.Ge => compileBinary[PInt8, PInt8, PInt32] { (exp10, exp20, tpe0) => Int8Comparison(ComparisonOp.Ge, exp10, exp20, tpe0, loc) }
        }
        case op: SemanticOperator.Int16Op => op match {
          case Int16Op.Neg => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Int16Op.Not => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Int16Op.Add => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Arithmetic(ArithmeticOp.Add, exp10, exp20, tpe0, loc) }
          case Int16Op.Sub => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Arithmetic(ArithmeticOp.Sub, exp10, exp20, tpe0, loc) }
          case Int16Op.Mul => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Arithmetic(ArithmeticOp.Mul, exp10, exp20, tpe0, loc) }
          case Int16Op.Div => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Arithmetic(ArithmeticOp.Div, exp10, exp20, tpe0, loc) }
          case Int16Op.Rem => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Arithmetic(ArithmeticOp.Rem, exp10, exp20, tpe0, loc) }
          case Int16Op.Exp => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Arithmetic(ArithmeticOp.Exp, exp10, exp20, tpe0, loc) }
          case Int16Op.And => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Bitwise(BitwiseOp.And, exp10, exp20, tpe0, loc) }
          case Int16Op.Or => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Bitwise(BitwiseOp.Or, exp10, exp20, tpe0, loc) }
          case Int16Op.Xor => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Bitwise(BitwiseOp.Xor, exp10, exp20, tpe0, loc) }
          case Int16Op.Shl => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Bitwise(BitwiseOp.Shl, exp10, exp20, tpe0, loc) }
          case Int16Op.Shr => compileBinary[PInt16, PInt16, PInt16] { (exp10, exp20, tpe0) => Int16Bitwise(BitwiseOp.Shr, exp10, exp20, tpe0, loc) }
          case Int16Op.Eq => compileBinary[PInt16, PInt16, PInt32] { (exp10, exp20, tpe0) => Int16Comparison(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case Int16Op.Neq => compileBinary[PInt16, PInt16, PInt32] { (exp10, exp20, tpe0) => Int16Comparison(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
          case Int16Op.Lt => compileBinary[PInt16, PInt16, PInt32] { (exp10, exp20, tpe0) => Int16Comparison(ComparisonOp.Lt, exp10, exp20, tpe0, loc) }
          case Int16Op.Le => compileBinary[PInt16, PInt16, PInt32] { (exp10, exp20, tpe0) => Int16Comparison(ComparisonOp.Le, exp10, exp20, tpe0, loc) }
          case Int16Op.Gt => compileBinary[PInt16, PInt16, PInt32] { (exp10, exp20, tpe0) => Int16Comparison(ComparisonOp.Gt, exp10, exp20, tpe0, loc) }
          case Int16Op.Ge => compileBinary[PInt16, PInt16, PInt32] { (exp10, exp20, tpe0) => Int16Comparison(ComparisonOp.Ge, exp10, exp20, tpe0, loc) }
        }
        case op: SemanticOperator.Int32Op => op match {
          case Int32Op.Neg => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Int32Op.Not => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Int32Op.Add => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Arithmetic(ArithmeticOp.Add, exp10, exp20, tpe0, loc) }
          case Int32Op.Sub => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Arithmetic(ArithmeticOp.Sub, exp10, exp20, tpe0, loc) }
          case Int32Op.Mul => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Arithmetic(ArithmeticOp.Mul, exp10, exp20, tpe0, loc) }
          case Int32Op.Div => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Arithmetic(ArithmeticOp.Div, exp10, exp20, tpe0, loc) }
          case Int32Op.Rem => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Arithmetic(ArithmeticOp.Rem, exp10, exp20, tpe0, loc) }
          case Int32Op.Exp => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Arithmetic(ArithmeticOp.Exp, exp10, exp20, tpe0, loc) }
          case Int32Op.And => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Bitwise(BitwiseOp.And, exp10, exp20, tpe0, loc) }
          case Int32Op.Or => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Bitwise(BitwiseOp.Or, exp10, exp20, tpe0, loc) }
          case Int32Op.Xor => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Bitwise(BitwiseOp.Xor, exp10, exp20, tpe0, loc) }
          case Int32Op.Shl => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Bitwise(BitwiseOp.Shl, exp10, exp20, tpe0, loc) }
          case Int32Op.Shr => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Bitwise(BitwiseOp.Shr, exp10, exp20, tpe0, loc) }
          case Int32Op.Eq => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Comparison(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case Int32Op.Neq => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Comparison(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
          case Int32Op.Lt => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Comparison(ComparisonOp.Lt, exp10, exp20, tpe0, loc) }
          case Int32Op.Le => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Comparison(ComparisonOp.Le, exp10, exp20, tpe0, loc) }
          case Int32Op.Gt => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Comparison(ComparisonOp.Gt, exp10, exp20, tpe0, loc) }
          case Int32Op.Ge => compileBinary[PInt32, PInt32, PInt32] { (exp10, exp20, tpe0) => Int32Comparison(ComparisonOp.Ge, exp10, exp20, tpe0, loc) }
        }
        case op: SemanticOperator.Int64Op => op match {
          case Int64Op.Neg => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Int64Op.Not => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case Int64Op.Add => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Arithmetic(ArithmeticOp.Add, exp10, exp20, tpe0, loc) }
          case Int64Op.Sub => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Arithmetic(ArithmeticOp.Sub, exp10, exp20, tpe0, loc) }
          case Int64Op.Mul => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Arithmetic(ArithmeticOp.Mul, exp10, exp20, tpe0, loc) }
          case Int64Op.Div => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Arithmetic(ArithmeticOp.Div, exp10, exp20, tpe0, loc) }
          case Int64Op.Rem => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Arithmetic(ArithmeticOp.Rem, exp10, exp20, tpe0, loc) }
          case Int64Op.Exp => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Arithmetic(ArithmeticOp.Exp, exp10, exp20, tpe0, loc) }
          case Int64Op.And => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Bitwise(BitwiseOp.And, exp10, exp20, tpe0, loc) }
          case Int64Op.Or => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Bitwise(BitwiseOp.Or, exp10, exp20, tpe0, loc) }
          case Int64Op.Xor => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Bitwise(BitwiseOp.Xor, exp10, exp20, tpe0, loc) }
          case Int64Op.Shl => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Bitwise(BitwiseOp.Shl, exp10, exp20, tpe0, loc) }
          case Int64Op.Shr => compileBinary[PInt64, PInt64, PInt64] { (exp10, exp20, tpe0) => Int64Bitwise(BitwiseOp.Shr, exp10, exp20, tpe0, loc) }
          case Int64Op.Eq => compileBinary[PInt64, PInt64, PInt32] { (exp10, exp20, tpe0) => Int64Comparison(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case Int64Op.Neq => compileBinary[PInt64, PInt64, PInt32] { (exp10, exp20, tpe0) => Int64Comparison(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
          case Int64Op.Lt => compileBinary[PInt64, PInt64, PInt32] { (exp10, exp20, tpe0) => Int64Comparison(ComparisonOp.Lt, exp10, exp20, tpe0, loc) }
          case Int64Op.Le => compileBinary[PInt64, PInt64, PInt32] { (exp10, exp20, tpe0) => Int64Comparison(ComparisonOp.Le, exp10, exp20, tpe0, loc) }
          case Int64Op.Gt => compileBinary[PInt64, PInt64, PInt32] { (exp10, exp20, tpe0) => Int64Comparison(ComparisonOp.Gt, exp10, exp20, tpe0, loc) }
          case Int64Op.Ge => compileBinary[PInt64, PInt64, PInt32] { (exp10, exp20, tpe0) => Int64Comparison(ComparisonOp.Ge, exp10, exp20, tpe0, loc) }
        }
        case op: SemanticOperator.BigIntOp => op match {
          case BigIntOp.Neg => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case BigIntOp.Not => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case BigIntOp.Add => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntArithmetic(ArithmeticOp.Add, exp10, exp20, tpe0, loc) }
          case BigIntOp.Sub => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntArithmetic(ArithmeticOp.Sub, exp10, exp20, tpe0, loc) }
          case BigIntOp.Mul => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntArithmetic(ArithmeticOp.Mul, exp10, exp20, tpe0, loc) }
          case BigIntOp.Div => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntArithmetic(ArithmeticOp.Div, exp10, exp20, tpe0, loc) }
          case BigIntOp.Rem => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntArithmetic(ArithmeticOp.Rem, exp10, exp20, tpe0, loc) }
          case BigIntOp.Exp => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntArithmetic(ArithmeticOp.Exp, exp10, exp20, tpe0, loc) }
          case BigIntOp.And => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntBitwise(BitwiseOp.And, exp10, exp20, tpe0, loc) }
          case BigIntOp.Or => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntBitwise(BitwiseOp.Or, exp10, exp20, tpe0, loc) }
          case BigIntOp.Xor => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntBitwise(BitwiseOp.Xor, exp10, exp20, tpe0, loc) }
          case BigIntOp.Shl => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntBitwise(BitwiseOp.Shl, exp10, exp20, tpe0, loc) }
          case BigIntOp.Shr => compileBinary[PReference[PBigInt], PReference[PBigInt], PReference[PBigInt]] { (exp10, exp20, tpe0) => BigIntBitwise(BitwiseOp.Shr, exp10, exp20, tpe0, loc) }
          case BigIntOp.Eq => compileBinary[PReference[PBigInt], PReference[PBigInt], PInt32] { (exp10, exp20, tpe0) => BigIntComparison(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case BigIntOp.Neq => compileBinary[PReference[PBigInt], PReference[PBigInt], PInt32] { (exp10, exp20, tpe0) => BigIntComparison(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
          case BigIntOp.Lt => compileBinary[PReference[PBigInt], PReference[PBigInt], PInt32] { (exp10, exp20, tpe0) => BigIntComparison(ComparisonOp.Lt, exp10, exp20, tpe0, loc) }
          case BigIntOp.Le => compileBinary[PReference[PBigInt], PReference[PBigInt], PInt32] { (exp10, exp20, tpe0) => BigIntComparison(ComparisonOp.Le, exp10, exp20, tpe0, loc) }
          case BigIntOp.Gt => compileBinary[PReference[PBigInt], PReference[PBigInt], PInt32] { (exp10, exp20, tpe0) => BigIntComparison(ComparisonOp.Gt, exp10, exp20, tpe0, loc) }
          case BigIntOp.Ge => compileBinary[PReference[PBigInt], PReference[PBigInt], PInt32] { (exp10, exp20, tpe0) => BigIntComparison(ComparisonOp.Ge, exp10, exp20, tpe0, loc) }
        }
        case op: SemanticOperator.ObjectOp => op match {
          case ObjectOp.EqNull => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
          case ObjectOp.NeqNull => throw InternalCompilerException(s"Binary expression not Implemented ${sop.getClass.getCanonicalName}")
        }
        case op: SemanticOperator.StringOp => op match {
          case StringOp.Concat => compileBinary[PReference[PStr], PReference[PStr], PReference[PStr]] { (exp10, exp20, tpe0) => StringConcat(exp10, exp20, tpe0, loc) }
          case StringOp.Eq => compileBinary[PReference[PStr], PReference[PStr], PInt32] { (exp10, exp20, tpe0) => StringEquality(EqualityOp.Eq, exp10, exp20, tpe0, loc) }
          case StringOp.Neq => compileBinary[PReference[PStr], PReference[PStr], PInt32] { (exp10, exp20, tpe0) => StringEquality(EqualityOp.Ne, exp10, exp20, tpe0, loc) }
        }
      }

    case FinalAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      for {
        exp10 <- visitExp[PInt32](exp1)
        exp20 <- visitExp[T](exp2)
        exp30 <- visitExp[T](exp3)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.IfThenElse(exp10, exp20, exp30, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Branch(exp, branches, tpe, loc) =>
      for {
        branches0 <- EM.foldRight(branches)(Map[Symbol.LabelSym, ErasedAst.Expression[T]]().toMonad) {
          case ((label, branchExp), map) => visitExp[T](branchExp) map (exp0 => map + (label -> exp0))
        }
        exp0 <- visitExp[T](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Branch(exp0, branches0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.JumpTo(sym, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.JumpTo(sym, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
      for {
        exp10 <- visitExp[PType](exp1)
        exp20 <- visitExp[T](exp2)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Let(sym, exp10, exp20, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Is(sym, tag, exp, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        expRes = ErasedAst.Expression.Is(sym, tag, exp0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Tag(sym, tag, exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.Tag(sym, tag, exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Untag(sym, tag, exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Index(base, offset, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PTuple]](base)
        tpe0 <- visitTpe[T](tpe)
        // TODO(JLS): maybe add cast
        expRes = ErasedAst.Expression.Index(base0, offset, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Tuple(elms, tpe, loc) =>
      for {
        elms0 <- EM.traverse(elms)(visitExp[PType])
        tpe0 <- visitTpe[PReference[PTuple]](tpe)
        expRes = ErasedAst.Expression.Tuple(elms0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.RecordEmpty(tpe, loc) =>
      for {
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.RecordEmpty(tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.RecordSelect(exp, field, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.RecordSelect(exp0, field, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
      for {
        value0 <- visitExp[PType](value)
        res0 <- visitExp[PReference[PAnyObject]](rest)
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.RecordExtend(field, value0, res0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
      for {
        rest0 <- visitExp[PReference[PAnyObject]](rest)
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.RecordRestrict(field, rest0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayLit(elms, tpe, loc) =>
      for {
        elms0 <- EM.traverse(elms)(visitExp[PType])
        tpe0 <- visitTpe[PReference[PArray[PType]]](tpe)
        expRes = ErasedAst.Expression.ArrayLit(elms0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayNew(elm, len, tpe, loc) =>
      for {
        elm0 <- visitExp[T](elm)
        len0 <- visitExp[PInt32](len)
        tpe0 <- visitTpe[PReference[PArray[T]]](tpe)
        expRes = ErasedAst.Expression.ArrayNew(elm0, len0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayLoad(base, index, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PArray[T]]](base)
        index0 <- visitExp[PInt32](index)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.ArrayLoad(base0, index0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PArray[T]]](base)
        index0 <- visitExp[PInt32](index)
        elm0 <- visitExp[T](elm)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.ArrayStore(base0, index0, elm0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArrayLength(base, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PArray[T]]](base)
        tpe0 <- visitTpe[PInt32](tpe)
        expRes = ErasedAst.Expression.ArrayLength(base0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      for {
        base0 <- visitExp[PReference[PArray[T]]](base)
        beginIndex0 <- visitExp[PInt32](beginIndex)
        endIndex0 <- visitExp[PInt32](endIndex)
        tpe0 <- visitTpe[PReference[PArray[T]]](tpe)
        expRes = ErasedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Ref(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[PReference[PRef[PType]]](tpe)
        expRes = ErasedAst.Expression.Ref(exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Deref(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PRef[T]]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Deref(exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      for {
        exp10 <- visitExp[PReference[PRef[T]]](exp1)
        exp20 <- visitExp[T](exp2)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.Assign(exp10, exp20, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Existential(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      for {
        exp0 <- visitExp[PInt32](exp)
        tpe0 <- visitTpe[PType](tpe)
        fparam0 = ErasedAst.FormalParam(sym, tpe0)
        expRes = ErasedAst.Expression.Existential(fparam0, exp0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Universal(fparam, exp, loc) =>
      val FinalAst.FormalParam(sym, tpe) = fparam
      for {
        exp0 <- visitExp[PInt32](exp)
        tpe0 <- visitTpe[PType](tpe)
        fparam0 = ErasedAst.FormalParam(sym, tpe0)
        expRes = ErasedAst.Expression.Universal(fparam0, exp0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Cast(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Cast(exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.TryCatch(exp, rules, tpe, loc) =>
      for {
        rules0 <- EM.traverse(rules)(cr =>
          for {
            cexp0 <- visitExp[T](cr.exp)
          } yield ErasedAst.CatchRule[T](cr.sym, cr.clazz, cexp0)
        )
        exp0 <- visitExp[T](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.TryCatch(exp0, rules0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      for {
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[PReference[PAnyObject]](tpe)
        expRes = ErasedAst.Expression.InvokeConstructor(constructor, args0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.InvokeMethod(method, exp0, args0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      for {
        args0 <- EM.traverse(args)(visitExp[PType])
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.InvokeStaticMethod(method, args0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.GetField(field, exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PAnyObject]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.GetField(field, exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
      for {
        exp10 <- visitExp[PReference[PAnyObject]](exp1)
        exp20 <- visitExp[PType](exp2)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.PutField(field, exp10, exp20, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.GetStaticField(field, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.GetStaticField(field, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.PutStaticField(field, exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.PutStaticField(field, exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.NewChannel(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PInt32](exp)
        tpe0 <- visitTpe[PReference[PChan[T]]](tpe)
        expRes = ErasedAst.Expression.NewChannel(exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.GetChannel(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PChan[T]]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.GetChannel(exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
      for {
        exp10 <- visitExp[PReference[PChan[T]]](exp1)
        exp20 <- visitExp[T](exp2)
        tpe0 <- visitTpe[PReference[PChan[T]]](tpe)
        expRes = ErasedAst.Expression.PutChannel(exp10, exp20, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.SelectChannel(rules, default, tpe, loc) =>
      for {
        rules0 <- EM.traverse(rules)(rule => {
          for {
            exp0 <- visitExp[T](rule.exp)
            chan0 <- visitExp[PReference[PChan[PType]]](rule.chan)
          } yield ErasedAst.SelectChannelRule(rule.sym, chan0, exp0)
        })
        default0 <- default match {
          case Some(defaultExp) =>
            visitExp[T](defaultExp) map (v => Some(v))
          case None => None.toMonad
        }
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.SelectChannel(rules0, default0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.Spawn(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PType](exp)
        tpe0 <- visitTpe[PReference[PUnit]](tpe)
        expRes = ErasedAst.Expression.Spawn(exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Lazy(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PFunction[T]]](exp)
        tpe0 <- visitTpe[PReference[PLazy[T]]](tpe)
        expRes = ErasedAst.Expression.Lazy(exp0, tpe0, loc)
      } yield expRes.asInstanceOf[ErasedAst.Expression[T]]

    case FinalAst.Expression.Force(exp, tpe, loc) =>
      for {
        exp0 <- visitExp[PReference[PLazy[T]]](exp)
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.Force(exp0, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.HoleError(sym, tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.HoleError(sym, tpe0, loc)
      } yield expRes

    case FinalAst.Expression.MatchError(tpe, loc) =>
      for {
        tpe0 <- visitTpe[T](tpe)
        expRes = ErasedAst.Expression.MatchError(tpe0, loc)
      } yield expRes
  }

  /**
    * Translates the given formal param `p` to the ErasedAst.
    */
  private def visitFormalParam(p: FinalAst.FormalParam): EraserMonad[ErasedAst.FormalParam] = {
    visitTpe[PType](p.tpe) map (tpe0 => ErasedAst.FormalParam(p.sym, tpe0))
  }

  /**
    *
    * Translates the type 'tpe' to the ErasedType.
    */
  private def visitTpe[T <: PType](tpe: MonoType): EraserMonad[RType[T]] = {
    val mainMonad = tpe match {
      case MonoType.Unit => RReference(RUnit).asInstanceOf[RType[T]].toMonad
      case MonoType.Bool => RBool.asInstanceOf[RType[T]].toMonad
      case MonoType.Char => RChar.asInstanceOf[RType[T]].toMonad
      case MonoType.Float32 => RFloat32.asInstanceOf[RType[T]].toMonad
      case MonoType.Float64 => RFloat64.asInstanceOf[RType[T]].toMonad
      case MonoType.Int8 => RInt8.asInstanceOf[RType[T]].toMonad
      case MonoType.Int16 => RInt16.asInstanceOf[RType[T]].toMonad
      case MonoType.Int32 => RInt32.asInstanceOf[RType[T]].toMonad
      case MonoType.Int64 => RInt64.asInstanceOf[RType[T]].toMonad
      case MonoType.BigInt =>
        RReference(RBigInt).asInstanceOf[RType[T]].toMonad
      case MonoType.Str =>
        RReference(RStr).asInstanceOf[RType[T]].toMonad
      case MonoType.Array(tpe) =>
        for {
          tpe0 <- visitTpe[PType](tpe)
        } yield RReference(RArray[PType](tpe0)).asInstanceOf[RType[T]]
      case MonoType.Channel(tpe) =>
        for {
          tpe0 <- visitTpe[T](tpe)
        } yield RReference(RChannel(tpe0)).asInstanceOf[RType[T]]
      case MonoType.Lazy(tpe) =>
        for {
          tpe0 <- visitTpe[T](tpe)
        } yield RReference(RLazy(tpe0)).asInstanceOf[RType[T]]
      case MonoType.Ref(tpe) =>
        for {
          tpe0 <- visitTpe[T](tpe)
        } yield RReference(RRef(tpe0)).asInstanceOf[RType[T]]
      case MonoType.Tuple(elms) =>
        for {
          elms0 <- EM.traverse(elms)(visitTpe[PType])
          expRes = RReference(RTuple(elms0))
        } yield expRes.asInstanceOf[RType[T]]
      case MonoType.Enum(sym, args) =>
        EM.traverse(args)(visitTpe[PType]) flatMap (args0 => {
          val res = RReference(REnum(sym, args0)).asInstanceOf[RType[T]]
          res.toMonad.setEnumSyms(Set(sym))
        })
      case MonoType.Arrow(args, result) =>
        EM.mapN(
          EM.traverse(args)(visitTpe[PType]),
          visitTpe[PType](result)
        ) {
          case (args0, result0) =>
            val tpeRes = RReference(RArrow(args0, result0))
            tpeRes.asInstanceOf[RType[T]]
        }
      case MonoType.RecordEmpty() =>
        RReference(RRecordEmpty).asInstanceOf[RType[T]].toMonad
      case MonoType.RecordExtend(field, value, rest) =>
        for {
          value0 <- visitTpe[PType](value)
          rest0 <- visitTpe[PReference[PAnyObject]](rest)
          expRes = RReference(RRecordExtend(field, value0, rest0))
        } yield expRes.asInstanceOf[RType[T]]
      case MonoType.SchemaEmpty() =>
        RReference(RSchemaEmpty).asInstanceOf[RType[T]].toMonad
      case MonoType.SchemaExtend(name, tpe, rest) =>
        for {
          tpe0 <- visitTpe[PType](tpe)
          rest0 <- visitTpe[PReference[PAnyObject]](rest)
          expRes = RReference(RSchemaExtend(name, tpe0, rest0))
        } yield expRes.asInstanceOf[RType[T]]
      case MonoType.Relation(tpes) =>
        for {
          tpes0 <- EM.traverse(tpes)(visitTpe[PType])
        } yield RReference(RRelation(tpes0)).asInstanceOf[RType[T]]
      case MonoType.Lattice(tpes) =>
        for {
          tpes0 <- EM.traverse(tpes)(visitTpe[PType])
        } yield RReference(RLattice(tpes0)).asInstanceOf[RType[T]]
      case MonoType.Native(clazz) =>
        RReference(RNative(clazz)).asInstanceOf[RType[T]].toMonad
      case MonoType.Var(id) =>
        RReference(RVar(id)).asInstanceOf[RType[T]].toMonad
    }
    mainMonad.flatMap(tpe => tpe.toMonad.setTypes(Set(tpe)))
  }
}
