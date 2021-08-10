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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.language.ast.ErasedAst.Expression
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PRefType, PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.MethodVisitor

import scala.language.implicitConversions

object BytecodeCompiler {

  sealed trait Stack

  sealed trait StackNil extends Stack

  sealed trait StackCons[R <: Stack, T <: PType] extends Stack

  sealed trait StackEnd extends Stack

  type **[R <: Stack, T <: PType] = StackCons[R, T]

  sealed case class F[T <: Stack](visitor: MethodVisitor)

  trait Cat1[T]

  implicit val int8Cat1: PInt8 => Cat1[PInt8] = null
  implicit val int16Cat1: PInt16 => Cat1[PInt16] = null
  implicit val int32Cat1: PInt32 => Cat1[PInt32] = null
  implicit val charCat1: PChar => Cat1[PChar] = null
  implicit val float32Cat1: PFloat32 => Cat1[PFloat32] = null

  implicit def referenceCat1[T <: PRefType](t: PReference[T]): Cat1[PReference[T]] = null

  trait Cat2[T]

  implicit val int64Cat1: PInt64 => Cat2[PInt64] = null
  implicit val float64Cat1: PFloat64 => Cat2[PFloat64] = null

  def compileExp[R <: Stack, T <: PType](exp: Expression[T]): F[R] => F[R ** T] = exp match {
    case Expression.Unit(loc) =>
      WithSource[R](loc) ~
        pushUnit

    case Expression.Null(tpe, loc) =>
      WithSource[R](loc) ~ pushNull(tpe)

    case Expression.True(loc) =>
      WithSource[R](loc) ~ pushBool(true)

    case Expression.False(loc) =>
      WithSource[R](loc) ~ pushBool(false)

    case Expression.Char(lit, loc) =>
      WithSource[R](loc) ~
        pushChar(lit)

    case Expression.Float32(lit, loc) =>
      WithSource[R](loc) ~ pushFloat32(lit)

    case Expression.Float64(lit, loc) =>
      WithSource[R](loc) ~ pushFloat64(lit)

    case Expression.Int8(lit, loc) =>
      WithSource[R](loc) ~ pushInt8(lit)

    case Expression.Int16(lit, loc) =>
      WithSource[R](loc) ~ pushInt16(lit)

    case Expression.Int32(lit, loc) =>
      WithSource[R](loc) ~ pushInt32(lit)

    case Expression.Int64(lit, loc) =>
      WithSource[R](loc) ~ pushInt64(lit)

    case Expression.BigInt(lit, loc) => ???
    case Expression.Str(lit, loc) => ???
    case Expression.Var(sym, tpe, loc) =>
      WithSource[R](loc) ~
        XLOAD(tpe, sym.getStackOffset + symOffsetOffset) // TODO(JLS): make this offset exist in F, dependent on static(0)/object function(1)

    case Expression.Closure(sym, freeVars, _, loc) =>
      WithSource[R](loc) ~
        Instructions.CREATECLOSURE(freeVars, sym.cloName)

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        CALL(args, squeezeFunction(squeezeReference(exp.tpe)).functionInterfaceName, tpe)

    case Expression.ApplyDef(sym, args, tpe, loc) =>
      WithSource[R](loc) ~
        CREATEDEF(sym.defName) ~
        CALL(args, sym.defName, tpe)

    case Expression.ApplyCloTail(exp, args, _, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        TAILCALL(args, squeezeFunction(squeezeReference(exp.tpe)).functionInterfaceName, tagOf[T])

    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      WithSource[R](loc) ~
        CREATEDEF(sym.defName) ~
        TAILCALL(args, sym.defName, tpe.tagOf)

    case Expression.ApplySelfTail(sym, _, actuals, _, loc) =>
      WithSource[R](loc) ~
        SELFTAILCALL(actuals, sym.defName, tagOf[T])

    case Expression.Unary(sop, op, exp, tpe, loc) => ???
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => /*TODO(JLS): remove*/ println(sop.getClass.getCanonicalName, tpe); ???
    case Expression.Int16Eq(exp1, exp2, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        IF_ICMPEQ16(pushBool(true), pushBool(false))

    case Expression.Int32Eq(exp1, exp2, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        IF_ICMPEQ32(pushBool(true), pushBool(false))

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => ???
    case Expression.Branch(exp, branches, tpe, loc) => ???
    case Expression.JumpTo(sym, tpe, loc) => ???
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      //TODO(JLS): sym is unsafe. localvar stack + reg as types?
      WithSource[R](loc) ~
        compileExp(exp1) ~
        XStore(sym, exp1.tpe) ~
        compileExp(exp2)

    case Expression.Is(sym, tag, exp, loc) => ???
    case Expression.Tag(sym, tag, exp, tpe, loc) => ???
    case Expression.Untag(sym, tag, exp, tpe, loc) => ???
    case Expression.Index(base, offset, tpe, loc) => ???
    case Expression.Tuple(elms, tpe, loc) => ???
    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, loc) => ???
    case Expression.ArrayLit(elms, tpe, loc) =>
      def makeAndFillArray[R0 <: Stack, T0 <: PType]
      (elms: List[Expression[T0]], arrayType: RArray[T0]):
      F[R0 ** PReference[PArray[T0]]] => F[R0 ** PReference[PArray[T0]]] = {
        val elmType = arrayType.tpe
        elmType match {
          case RBool => ???
          case RInt8 => ???
          case RInt16 => ???
          case RInt32 =>
            START[R0 ** PReference[PArray[T0]]] ~
              multiComposition(elms.zipWithIndex) { case (elm, index) =>
                START[R0 ** PReference[PArray[T0]]] ~
                  DUP ~
                  pushInt32(index) ~
                  compileExp(elm) ~
                  XAStore(elmType)
              }
          case RInt64 => ???
          case RChar => ???
          case RFloat32 => ???
          case RFloat64 => ???
          case RReference(_) => ???
        }
      }

      WithSource[R](loc) ~
        pushInt32(elms.length) ~
        XNEWARRAY(tpe) ~
        makeAndFillArray(elms, squeezeArray(squeezeReference(tpe)))


    case Expression.ArrayNew(elm, len, tpe, loc) =>
      def elmArraySwitch
      [R0 <: Stack, T0 <: PType]
      (elmType: RType[T0]):
      F[R0 ** T0 ** PReference[PArray[T0]]] => F[R0 ** PReference[PArray[T0]] ** PReference[PArray[T0]] ** T0] =
        elmType match {
          case RBool | RInt8 | RInt16 | RInt32 | RChar | RFloat32 | RReference(_) => // Cat1
            //TODO(JLS): note: start here is needed because of some implicit overshadowing
            START[R0 ** T0 ** PReference[PArray[T0]]] ~
              DUP_X1 ~
              SWAP
          case RInt64 | RFloat64 => // Cat2
            START[R0 ** T0 ** PReference[PArray[T0]]] ~
              DUP_X2_onCat2 ~
              DUP_X2_onCat2 ~
              POP
        }

      WithSource[R](loc) ~
        compileExp(elm) ~
        compileExp(len) ~
        XNEWARRAY(tpe) ~
        elmArraySwitch(elm.tpe) ~
        arraysFill(elm.tpe)

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        compileExp(index) ~
        XALOAD(tpe)

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        compileExp(index) ~
        compileExp(elm) ~
        XAStore(elm.tpe) ~
        pushUnit

    case Expression.ArrayLength(base, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        arrayLength(base.tpe)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        compileExp(beginIndex) ~
        compileExp(endIndex) ~
        SWAP ~
        DUP_X1 ~
        ISUB ~
        DUP ~
        XNEWARRAY(tpe) ~
        DUP2_X2_cat1_onCat1 ~
        SWAP ~
        pushInt32(0) ~
        SWAP ~
        systemArrayCopy ~
        SWAP ~
        POP

    case Expression.Ref(exp, tpe, loc) =>
      val tpeRRef = squeezeReference(tpe)
      WithSource[R](loc) ~
        NEW(tpeRRef) ~
        DUP ~
        INVOKESPECIAL(tpeRRef, JvmName.nothingToVoid) ~
        DUP ~
        compileExp(exp) ~
        PUTFIELD(tpeRRef, GenRefClasses.ValueFieldName, exp.tpe)

    case Expression.Deref(exp, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(exp) ~
        XGETFIELD(squeezeReference(exp.tpe), GenRefClasses.ValueFieldName, tpe)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(exp1) ~
        compileExp(exp2) ~
        PUTFIELD(squeezeReference(exp1.tpe), GenRefClasses.ValueFieldName, exp2.tpe) ~
        pushUnit

    case Expression.Existential(fparam, exp, loc) => ???
    case Expression.Universal(fparam, exp, loc) => ???
    case Expression.Cast(exp, tpe, loc) => {
      // TODO(JLS): implement Cast
      def fixStack[R <: Stack, T1 <: PType, T2 <: PType]: F[R ** T1] => F[R ** T2] = f => f.asInstanceOf[F[R ** T2]]

      WithSource[R](loc) ~
        compileExp(exp) ~
        fixStack
    }

    case Expression.TryCatch(exp, rules, tpe, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, loc) => ???
    case Expression.GetField(field, exp, tpe, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, loc) => ???
    case Expression.GetStaticField(field, tpe, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, loc) => ???
    case Expression.NewChannel(exp, tpe, loc) =>
      WithSource[R](loc) ~
        pushNull(tpe)

    //    case Expression.GetChannel(exp, tpe, loc) => ???
    //    case Expression.PutChannel(exp1, exp2, tpe, loc) => ???
    //    case Expression.SelectChannel(rules, default, tpe, loc) => ???
    //    case Expression.Spawn(exp, tpe, loc) => ???
    //    case Expression.Lazy(exp, tpe, loc) => ???
    //    case Expression.Force(exp, tpe, loc) => ???
    //    case Expression.FixpointConstraintSet(cs, tpe, loc) => ???
    //    case Expression.FixpointCompose(exp1, exp2, tpe, loc) => ???
    //    case Expression.FixpointSolve(exp, stf, tpe, loc) => ???
    //    case Expression.FixpointProject(pred, exp, tpe, loc) => ???
    //    case Expression.FixpointFold(pred, init, f, constraints, tpe, loc) => ???
    //    case Expression.HoleError(sym, tpe, loc) => ???
    //    case Expression.MatchError(tpe, loc) => ???
    //    case ErasedAst.BoxInt8(exp, loc) => ???
    //    case ErasedAst.BoxInt16(exp, loc) => ???
    //    case ErasedAst.BoxInt32(exp, loc) => ???
    //    case ErasedAst.BoxInt64(exp, loc) => ???
    //    case ErasedAst.BoxChar(exp, loc) => ???
    //    case ErasedAst.BoxFloat32(exp, loc) => ???
    //    case ErasedAst.BoxFloat64(exp, loc) => ???
    //    case ErasedAst.UnboxInt8(exp, loc) => ???
    //    case ErasedAst.UnboxInt16(exp, loc) => ???
    //    case ErasedAst.UnboxInt32(exp, loc) => ???
    //    case ErasedAst.UnboxInt64(exp, loc) => ???
    //    case ErasedAst.UnboxChar(exp, loc) => ???
    //    case ErasedAst.UnboxFloat32(exp, loc) => ???
    //    case ErasedAst.UnboxFloat64(exp, loc) => ???
    case _ => ???
  }

}
