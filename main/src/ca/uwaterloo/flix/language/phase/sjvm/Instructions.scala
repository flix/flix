/*
 * Copyright 2021 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.language.ast.ErasedAst.{Cat1, Cat2, JType, ErasedType => ET}
import ca.uwaterloo.flix.language.ast.ErasedAst.JType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.util.InternalCompilerException

object Instructions {
  def WithSource[R <: Stack](loc: SourceLocation): F[R] => F[R] = ???

  // TODO QUESTION: which functions are "unsafe" irt. types? A: Leafs

  //todo: naming
  def ISWAP[R <: Stack, T1 <: JType with Cat1, T2 <: JType with Cat1]: F[R ** T2 ** T1] => F[R ** T1 ** T2] = ???

  // instead of covariance
  def upCast[R <: Stack, T <: JType]: F[R ** JArray[T]] => F[R ** JObject] = ???

  def NOP[R <: Stack]: F[R] => F[R] = x => x

  def POP[R <: Stack, T <: JType]: F[R ** T] => F[R] = ???

  def DUP_X1[R <: Stack, T1 <: JType with Cat1, T2 <: JType with Cat1]: F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = ???

  def DUP_X2_onCat1[R <: Stack, T1 <: JType with Cat1, T2 <: JType with Cat1, T3 <: JType with Cat1]: F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = ???

  def DUP_X2_onCat2[R <: Stack, T1 <: JType with Cat1, T2 <: JType with Cat2]: F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = ???

  def DUP2_X2_cat1_onCat1[R <: Stack, T1 <: JType with Cat1, T2 <: JType with Cat1, T3 <: JType with Cat1, T4 <: JType with Cat1]: F[R ** T4 ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T4 ** T3 ** T2 ** T1] = ???

  def DUP2_X2_cat2_onCat1[R <: Stack, T1 <: JType with Cat2, T2 <: JType with Cat1, T3 <: JType with Cat1]: F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = ???

  def DUP2_X2_cat1_onCat2[R <: Stack, T1 <: JType with Cat1, T2 <: JType with Cat1, T3 <: JType with Cat2]: F[R ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T3 ** T2 ** T1] = ???

  def DUP2_X2_cat2_onCat2[R <: Stack, T1 <: JType with Cat2, T2 <: JType with Cat2]: F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = ???

  def DUP[R <: Stack]: F[R ** PrimInt32] => F[R ** PrimInt32 ** PrimInt32] = ???

  def ISUB[R <: Stack]: F[R ** PrimInt32 ** PrimInt32] => F[R ** PrimInt32] = ???

  def pushUnit[R <: Stack]: F[R] => F[R ** JUnit] = ???

  def pushNull[R <: Stack]: F[R] => F[R ** JObject] = ???

  def pushBool[R <: Stack](b: Boolean): F[R] => F[R ** PrimInt32] = pushInt32(if (b) 1 else 0)

  def pushInt8[R <: Stack](n: Int): F[R] => F[R ** PrimInt8] = ???

  def pushInt16[R <: Stack](n: Int): F[R] => F[R ** PrimInt16] = ???

  def pushInt32[R <: Stack](n: Int): F[R] => F[R ** PrimInt32] = ???

  def pushInt64[R <: Stack](n: Long): F[R] => F[R ** PrimInt64] = ???

  def pushFloat32[R <: Stack](n: Float): F[R] => F[R ** PrimFloat32] = ???

  def pushFloat64[R <: Stack](n: Double): F[R] => F[R ** PrimFloat64] = ???

  def pushChar[R <: Stack](c: Char): F[R] => F[R ** PrimChar] = ???

  def BALoad[R <: Stack]: F[R ** JArray[PrimInt8] ** PrimInt32] => F[R ** PrimInt8] = ???

  def SALoad[R <: Stack]: F[R ** JArray[PrimInt16] ** PrimInt32] => F[R ** PrimInt16] = ???

  def IALoad[R <: Stack]: F[R ** JArray[PrimInt32] ** PrimInt32] => F[R ** PrimInt32] = ???

  def LALoad[R <: Stack]: F[R ** JArray[PrimInt64] ** PrimInt32] => F[R ** PrimInt64] = ???

  def CALoad[R <: Stack]: F[R ** JArray[PrimChar] ** PrimInt32] => F[R ** PrimChar] = ???

  def FALoad[R <: Stack]: F[R ** JArray[PrimFloat32] ** PrimInt32] => F[R ** PrimFloat32] = ???

  def DALoad[R <: Stack]: F[R ** JArray[PrimFloat64] ** PrimInt32] => F[R ** PrimFloat64] = ???

  def AALoad[R <: Stack]: F[R ** JArray[JObject] ** PrimInt32] => F[R ** JObject] = ???

  def XALoad[R <: Stack, T <: JType](tpe: ET[T]): F[R ** JArray[T] ** PrimInt32] => F[R ** T] = tpe match {
    case ET.Unit() => ???
    case ET.Bool() => ???
    case ET.BoxedBool() => ???
    case ET.Char() => CALoad
    case ET.BoxedChar() => ???
    case ET.Float32() => ???
    case ET.BoxedFloat32() => ???
    case ET.Float64() => ???
    case ET.BoxedFloat64() => ???
    case ET.Int8() => BALoad
    case ET.BoxedInt8() => ???
    case ET.Int16() => SALoad
    case ET.BoxedInt16() => ???
    case ET.Int32() => IALoad
    case ET.BoxedInt32() => ???
    case ET.Int64() => LALoad
    case ET.BoxedInt64() => ???
    case ET.Array(tpe) => ???
    case ET.Channel(tpe) => ???
    case ET.Lazy(tpe) => ???
    case ET.Ref(tpe) => ???
    case ET.ObjectT(o) => AALoad
    case ET.Var(id) => ???
  }

  def BAStore[R <: Stack]: F[R ** JArray[PrimInt8] ** PrimInt32 ** PrimInt8] => F[R] = ???

  def SAStore[R <: Stack]: F[R ** JArray[PrimInt16] ** PrimInt32 ** PrimInt16] => F[R] = ???

  def IAStore[R <: Stack]: F[R ** JArray[PrimInt32] ** PrimInt32 ** PrimInt32] => F[R] = ???

  def LAStore[R <: Stack]: F[R ** JArray[PrimInt64] ** PrimInt32 ** PrimInt64] => F[R] = ???

  def CAStore[R <: Stack]: F[R ** JArray[PrimChar] ** PrimInt32 ** PrimChar] => F[R] = ???

  def FAStore[R <: Stack]: F[R ** JArray[PrimFloat32] ** PrimInt32 ** PrimFloat32] => F[R] = ???

  def DAStore[R <: Stack]: F[R ** JArray[PrimFloat64] ** PrimInt32 ** PrimFloat64] => F[R] = ???

  def AAStore[R <: Stack]: F[R ** JArray[JObject] ** PrimInt32 ** JObject] => F[R] = ???

  def XAStore[R <: Stack, T <: JType](tpe: ET[T]): F[R ** JArray[T] ** PrimInt32 ** T] => F[R] = tpe match {
    case ET.Unit() => ???
    case ET.Bool() => ???
    case ET.BoxedBool() => ???
    case ET.Char() => CAStore
    case ET.BoxedChar() => ???
    case ET.Float32() => ???
    case ET.BoxedFloat32() => ???
    case ET.Float64() => ???
    case ET.BoxedFloat64() => ???
    case ET.Int8() => BAStore
    case ET.BoxedInt8() => ???
    case ET.Int16() => SAStore
    case ET.BoxedInt16() => ???
    case ET.Int32() => IAStore
    case ET.BoxedInt32() => ???
    case ET.Int64() => LAStore
    case ET.BoxedInt64() => ???
    case ET.Array(tpe) => ???
    case ET.Channel(tpe) => ???
    case ET.Lazy(tpe) => ???
    case ET.Ref(tpe) => ???
    case ET.ObjectT(o) => AAStore
    case ET.Var(id) => ???
  }

  def IStore[R <: Stack](sym: Symbol.VarSym): F[R ** PrimInt32] => F[R] = ???

  def LStore[R <: Stack](sym: Symbol.VarSym): F[R ** PrimInt64] => F[R] = ???

  def FStore[R <: Stack](sym: Symbol.VarSym): F[R ** PrimFloat32] => F[R] = ???

  def DStore[R <: Stack](sym: Symbol.VarSym): F[R ** PrimFloat64] => F[R] = ???

  def AStore[R <: Stack](sym: Symbol.VarSym): F[R ** JObject] => F[R] = ???

  def XStore[R <: Stack, T <: JType](sym: Symbol.VarSym, tpe: ET[T]): F[R ** T] => F[R] = tpe match {
    case ET.Unit() => ???
    case ET.Bool() => ???
    case ET.BoxedBool() => ???
    case ET.Char() => ???
    case ET.BoxedChar() => ???
    case ET.Float32() => FStore(sym)
    case ET.BoxedFloat32() => ???
    case ET.Float64() => DStore(sym)
    case ET.BoxedFloat64() => ???
    case ET.Int8() => ???
    case ET.BoxedInt8() => ???
    case ET.Int16() => ???
    case ET.BoxedInt16() => ???
    case ET.Int32() => IStore(sym)
    case ET.BoxedInt32() => ???
    case ET.Int64() => LStore(sym)
    case ET.BoxedInt64() => ???
    case ET.Array(tpe) => ???
    case ET.Channel(tpe) => ???
    case ET.Lazy(tpe) => ???
    case ET.Ref(tpe) => ???
    case ET.ObjectT(o) => AStore(sym)
    case ET.Var(id) => ???
  }

  // TODO Question: does this make sense? can values still be seen as int32?
  def BOOLNEWARRAY[R <: Stack]: F[R] => F[R ** JArray[PrimInt32]] = ???

  def CNEWARRAY[R <: Stack]: F[R] => F[R ** JArray[PrimChar]] = ???

  def FNEWARRAY[R <: Stack]: F[R] => F[R ** JArray[PrimFloat32]] = ???

  def DNEWARRAY[R <: Stack]: F[R] => F[R ** JArray[PrimFloat64]] = ???

  def BNEWARRAY[R <: Stack]: F[R] => F[R ** JArray[PrimInt8]] = ???

  def SNEWARRAY[R <: Stack]: F[R] => F[R ** JArray[PrimInt16]] = ???

  def INEWARRAY[R <: Stack]: F[R] => F[R ** JArray[PrimInt32]] = ???

  def LNEWARRAY[R <: Stack]: F[R] => F[R ** JArray[PrimInt64]] = ???

  def ANEWARRAY[R <: Stack]: F[R] => F[R ** JArray[JObject]] = {
    // from genExpression:
    // visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
    // should type be built?
    ???
  }

  def XNEWARRAY[R <: Stack, T <: JType](arrayType: ET[JArray[T]]): F[R] => F[R ** JArray[T]] = arrayType match {
    case ET.Array(tpe) => tpe match {
      case ET.Unit() => ???
      case ET.Bool() => BOOLNEWARRAY
      case ET.BoxedBool() => ???
      case ET.Char() => CNEWARRAY
      case ET.BoxedChar() => ???
      case ET.Float32() => FNEWARRAY
      case ET.BoxedFloat32() => ???
      case ET.Float64() => DNEWARRAY
      case ET.BoxedFloat64() => ???
      case ET.Int8() => BNEWARRAY
      case ET.BoxedInt8() => ???
      case ET.Int16() => SNEWARRAY
      case ET.BoxedInt16() => ???
      case ET.Int32() => INEWARRAY
      case ET.BoxedInt32() => ???
      case ET.Int64() => LNEWARRAY
      case ET.BoxedInt64() => ???
      case ET.Array(tpe) => ???
      case ET.Channel(tpe) => ???
      case ET.Lazy(tpe) => ???
      case ET.Ref(tpe) => ???
      case ET.ObjectT(o) => ANEWARRAY
      case ET.Var(id) => ???
    }
    case _ => throw InternalCompilerException("unexpected non-array type")
  }

  def systemArrayCopy[R <: Stack]: F[R ** JObject ** PrimInt32 ** JObject ** PrimInt32 ** PrimInt32] => F[R] = ???

  def arrayLength[R <: Stack]: F[R ** JArray[JType]] => F[R ** PrimInt32] = ???

  // also make void
  def defMakeFunction[R <: Stack, T <: JType](t: ET[T], x: F[R]): F[R ** T] = ???

  def foo = defMakeFunction(ET.Int32(), defMakeFunction(ET.Int32(), null))


  implicit class ComposeOps[A, B](f: F[A] => F[B]) {
    def ~[C](that: F[B] => F[C]): F[A] => F[C] = ???
  }

}
