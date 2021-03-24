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
import ca.uwaterloo.flix.language.ast.{ErasedAst, PType}
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._

object BytecodeCompiler {

  sealed trait Stack

  sealed trait StackNil extends Stack

  sealed case class StackCons[R <: Stack, T <: PType](rest: R, top: T) extends Stack

  type **[R <: Stack, T <: PType] = StackCons[R, T]

  sealed trait F[T]

  def compileExp[R <: Stack, T <: PType](exp: Expression[T]): F[R] => F[R ** T] = exp match {
    case Expression.Unit(loc) =>
      WithSource[R](loc) ~
        pushUnit

    case Expression.Null(tpe, loc) =>
      WithSource[R](loc) ~ pushNull

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
    case Expression.Var(sym, tpe, loc) => ???
    case Expression.Closure(sym, freeVars, tpe, loc) => ???
    case Expression.ApplyClo(exp, args, tpe, loc) => ???
    case Expression.ApplyDef(sym, args, tpe, loc) => ???
    case Expression.ApplyCloTail(exp, args, tpe, loc) => ???
    case Expression.ApplyDefTail(sym, args, tpe, loc) => ???
    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => ???
    case Expression.Unary(sop, op, exp, tpe, loc) => ???
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => ???
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => ???
    case Expression.Branch(exp, branches, tpe, loc) => ???
    case Expression.JumpTo(sym, tpe, loc) => ???
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      // sym is unsafe. localvar stack + reg as types?
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
    case Expression.ArrayLit(elms, tpe, loc) => ???
    case Expression.ArrayNew(elm, len, tpe, loc) => ???
    case Expression.ArrayLoad(base, index, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        compileExp(index) ~
        XALoad(tpe)

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
        arrayLength

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
      WithSource[R](loc) ~
//        ??? ~[R ** PReference[PRef[T]] ** PReference[PRef[T]]]
//        DUP ~[R ** PReference[PRef[T]] ** PReference[PRef[T]] ** PType]
//        compileExp(exp) ~[R ** PReference[PRef[T]]]
        ???

    case Expression.Deref(exp, tpe, loc) => ???
    case Expression.Assign(exp1, exp2, tpe, loc) => ???
    case Expression.Existential(fparam, exp, loc) => ???
    case Expression.Universal(fparam, exp, loc) => ???
    case Expression.Cast(exp, tpe, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, loc) => ???
    case Expression.GetField(field, exp, tpe, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, loc) => ???
    case Expression.GetStaticField(field, tpe, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, loc) => ???
    case Expression.NewChannel(exp, tpe, loc) => ???
    case Expression.GetChannel(exp, tpe, loc) => ???
    case Expression.PutChannel(exp1, exp2, tpe, loc) => ???
    case Expression.SelectChannel(rules, default, tpe, loc) => ???
    case Expression.Spawn(exp, tpe, loc) => ???
    case Expression.Lazy(exp, tpe, loc) => ???
    case Expression.Force(exp, tpe, loc) => ???
    case Expression.FixpointConstraintSet(cs, tpe, loc) => ???
    case Expression.FixpointCompose(exp1, exp2, tpe, loc) => ???
    case Expression.FixpointSolve(exp, stf, tpe, loc) => ???
    case Expression.FixpointProject(pred, exp, tpe, loc) => ???
    case Expression.FixpointEntails(exp1, exp2, tpe, loc) => ???
    case Expression.FixpointFold(pred, init, f, constraints, tpe, loc) => ???
    case Expression.HoleError(sym, tpe, loc) => ???
    case Expression.MatchError(tpe, loc) => ???
    case ErasedAst.BoxInt8(exp, loc) => ???
    case ErasedAst.BoxInt16(exp, loc) => ???
    case ErasedAst.BoxInt32(exp, loc) => ???
    case ErasedAst.BoxInt64(exp, loc) => ???
    case ErasedAst.BoxChar(exp, loc) => ???
    case ErasedAst.BoxFloat32(exp, loc) => ???
    case ErasedAst.BoxFloat64(exp, loc) => ???
    case ErasedAst.UnboxInt8(exp, loc) => ???
    case ErasedAst.UnboxInt16(exp, loc) => ???
    case ErasedAst.UnboxInt32(exp, loc) => ???
    case ErasedAst.UnboxInt64(exp, loc) => ???
    case ErasedAst.UnboxChar(exp, loc) => ???
    case ErasedAst.UnboxFloat32(exp, loc) => ???
    case ErasedAst.UnboxFloat64(exp, loc) => ???
  }

}
