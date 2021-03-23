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
import ca.uwaterloo.flix.language.ast.PType
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

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(exp3) ~
        ???

    case Expression.Ref(exp, tpe, loc) =>
      //      NEW("class name") ~
      //      DUP ~
      //      compileExp(exp) ~
      //      INVOKESPECIAL("class name", "constructor signature")
      WithSource[R](loc) ~
        pushNull ~
        ???

    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      // sym is unsafe. localvar stack + reg as types?
      WithSource[R](loc) ~
        compileExp(exp1) ~
        XStore(sym, exp1.tpe) ~
        compileExp(exp2)

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

    case _ => ???
  }

}
