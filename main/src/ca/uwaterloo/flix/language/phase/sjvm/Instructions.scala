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

import ca.uwaterloo.flix.language.ast.ErasedAst.{ErasedType, JType}
import ca.uwaterloo.flix.language.ast.ErasedAst.JType._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.util.InternalCompilerException

object Instructions {
  def WithSource[R <: Stack](loc: SourceLocation): F[R] => F[R] = ???

  // TODO QUESTION: which functions are "unsafe" irt. types? A: Leafs

  def TEMPSWAP[R <: Stack](): F[R ** PrimInt32 ** PrimInt32] => F[R ** PrimInt32 ** PrimInt32] = ???

  // instead of covariance
  def upCast[R <: Stack, T <: JType](): F[R ** JArray[T]] => F[R ** JObject] = ???

  // TODO QUESTION: match on given types or copy per type?
  //  def SWAP[R <: Stack, T <: JType, U <: JType](t: ErasedType[T], u: ErasedType[U]): F[R ** T ** U] => F[R ** U ** T] = (t, u) match {
  //    case (ErasedType.Int32(), ErasedType.Int32()) => ISWAP()
  //    case _ => ???
  //  }

  // TODO: reverse 1 2
  def DUP_X1[R <: Stack, T1 <: JType, T2 <: JType](t1: ErasedType[T1], t2: ErasedType[T2]): F[R ** T1 ** T2] => F[R ** T2 ** T1 ** T2] = (t1, t2) match {
    case (ErasedType.Int8() | ErasedType.Int32() | ErasedType.Int16(), ErasedType.Int8() | ErasedType.Int32() | ErasedType.Int16()) => ???
    case _ => ???
  }

  // TODO: This will have 4 versions, depending on the value categories
  def DUP2_X2[R <: Stack, T1 <: JType, T2 <: JType, T3 <: JType, T4 <: JType](t1: ErasedType[T1], t2: ErasedType[T2], t3: ErasedType[T3], t4: ErasedType[T4]): F[R ** T4 ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T4 ** T3 ** T2 ** T1] = (t1, t2, t3, t4) match {
    case (ErasedType.Array(tpe), ErasedType.Int32(), ErasedType.Int32(), ErasedType.Int32()) => ???
    case _ => ???
  }

  def systemArrayCopy[R <: Stack](): F[R ** JObject ** PrimInt32 ** JObject ** PrimInt32 ** PrimInt32] => F[R] = ???

  // also make void
  def defMakeFunction[R <: Stack, T <: JType](t: ErasedType[T], x: F[R]): F[R ** T] = ???

  def foo() = defMakeFunction(ErasedType.Int32(), defMakeFunction(ErasedType.Int32(), null))

  def DUP[R <: Stack](): F[R ** PrimInt32] => F[R ** PrimInt32 ** PrimInt32] = ???

  def ISUB[R <: Stack](): F[R ** PrimInt32 ** PrimInt32] => F[R ** PrimInt32] = ???

  def pushUnit[R <: Stack](): F[R] => F[R ** JUnit] = ???

  def pushNull[R <: Stack](): F[R] => F[R ** JObject] = ???

  def pushBool[R <: Stack](b: Boolean): F[R] => F[R ** PrimInt32] = pushInt32(if (b) 1 else 0)

  def pushInt8[R <: Stack](n: Int): F[R] => F[R ** PrimInt8] = ???

  def pushInt16[R <: Stack](n: Int): F[R] => F[R ** PrimInt16] = ???

  def pushInt32[R <: Stack](n: Int): F[R] => F[R ** PrimInt32] = ???

  def pushInt64[R <: Stack](n: Long): F[R] => F[R ** PrimInt64] = ???

  def pushFloat32[R <: Stack](n: Float): F[R] => F[R ** PrimFloat32] = ???

  def pushFloat64[R <: Stack](n: Double): F[R] => F[R ** PrimFloat64] = ???

  def pushChar[R <: Stack](c: Char): F[R] => F[R ** PrimChar] = ???

  def arrayLength[R <: Stack](): F[R ** JArray[JType]] => F[R ** PrimInt32] = ???

  def BALoad[R <: Stack](): F[R ** JArray[PrimInt8] ** PrimInt32] => F[R ** PrimInt8] = ???

  def SALoad[R <: Stack](): F[R ** JArray[PrimInt16] ** PrimInt32] => F[R ** PrimInt16] = ???

  def IALoad[R <: Stack](): F[R ** JArray[PrimInt32] ** PrimInt32] => F[R ** PrimInt32] = ???

  def LALoad[R <: Stack](): F[R ** JArray[PrimInt64] ** PrimInt32] => F[R ** PrimInt64] = ???

  def CALoad[R <: Stack](): F[R ** JArray[PrimChar] ** PrimInt32] => F[R ** PrimChar] = ???

  def FALoad[R <: Stack](): F[R ** JArray[PrimFloat32] ** PrimInt32] => F[R ** PrimFloat32] = ???

  def DALoad[R <: Stack](): F[R ** JArray[PrimFloat64] ** PrimInt32] => F[R ** PrimFloat64] = ???

  def AALoad[R <: Stack](): F[R ** JArray[JObject] ** PrimInt32] => F[R ** JObject] = ???

  def XALoad[R <: Stack, T <: JType](tpe: ErasedType[T]): F[R ** JArray[T] ** PrimInt32] => F[R ** T] = tpe match {
    case ErasedType.Unit() => ???
    case ErasedType.Bool() => ???
    case ErasedType.BoxedBool() => ???
    case ErasedType.Char() => CALoad()
    case ErasedType.BoxedChar() => ???
    case ErasedType.Float32() => ???
    case ErasedType.BoxedFloat32() => ???
    case ErasedType.Float64() => ???
    case ErasedType.BoxedFloat64() => ???
    case ErasedType.Int8() => BALoad()
    case ErasedType.BoxedInt8() => ???
    case ErasedType.Int16() => SALoad()
    case ErasedType.BoxedInt16() => ???
    case ErasedType.Int32() => IALoad()
    case ErasedType.BoxedInt32() => ???
    case ErasedType.Int64() => LALoad()
    case ErasedType.BoxedInt64() => ???
    case ErasedType.Array(tpe) => ???
    case ErasedType.Channel(tpe) => ???
    case ErasedType.Lazy(tpe) => ???
    case ErasedType.Ref(tpe) => ???
    case ErasedType.ObjectT(o) => AALoad()
    case ErasedType.Var(id) => ???
  }

  def BAStore[R <: Stack](): F[R ** JArray[PrimInt8] ** PrimInt32 ** PrimInt8] => F[R] = ???

  def SAStore[R <: Stack](): F[R ** JArray[PrimInt16] ** PrimInt32 ** PrimInt16] => F[R] = ???

  def IAStore[R <: Stack](): F[R ** JArray[PrimInt32] ** PrimInt32 ** PrimInt32] => F[R] = ???

  def LAStore[R <: Stack](): F[R ** JArray[PrimInt64] ** PrimInt32 ** PrimInt64] => F[R] = ???

  def CAStore[R <: Stack](): F[R ** JArray[PrimChar] ** PrimInt32 ** PrimChar] => F[R] = ???

  def FAStore[R <: Stack](): F[R ** JArray[PrimFloat32] ** PrimInt32 ** PrimFloat32] => F[R] = ???

  def DAStore[R <: Stack](): F[R ** JArray[PrimFloat64] ** PrimInt32 ** PrimFloat64] => F[R] = ???

  def AAStore[R <: Stack](): F[R ** JArray[JObject] ** PrimInt32 ** JObject] => F[R] = ???

  def XAStore[R <: Stack, T <: JType](tpe: ErasedType[T]): F[R ** JArray[T] ** PrimInt32 ** T] => F[R] = tpe match {
    case ErasedType.Unit() => ???
    case ErasedType.Bool() => ???
    case ErasedType.BoxedBool() => ???
    case ErasedType.Char() => CAStore()
    case ErasedType.BoxedChar() => ???
    case ErasedType.Float32() => ???
    case ErasedType.BoxedFloat32() => ???
    case ErasedType.Float64() => ???
    case ErasedType.BoxedFloat64() => ???
    case ErasedType.Int8() => BAStore()
    case ErasedType.BoxedInt8() => ???
    case ErasedType.Int16() => SAStore()
    case ErasedType.BoxedInt16() => ???
    case ErasedType.Int32() => IAStore()
    case ErasedType.BoxedInt32() => ???
    case ErasedType.Int64() => LAStore()
    case ErasedType.BoxedInt64() => ???
    case ErasedType.Array(tpe) => ???
    case ErasedType.Channel(tpe) => ???
    case ErasedType.Lazy(tpe) => ???
    case ErasedType.Ref(tpe) => ???
    case ErasedType.ObjectT(o) => AAStore()
    case ErasedType.Var(id) => ???
  }

  def IStore[R <: Stack](sym: Symbol.VarSym): F[R ** PrimInt32] => F[R] = ???

  def LStore[R <: Stack](sym: Symbol.VarSym): F[R ** PrimInt64] => F[R] = ???

  def FStore[R <: Stack](sym: Symbol.VarSym): F[R ** PrimFloat32] => F[R] = ???

  def DStore[R <: Stack](sym: Symbol.VarSym): F[R ** PrimFloat64] => F[R] = ???

  def AStore[R <: Stack](sym: Symbol.VarSym): F[R ** JObject] => F[R] = ???

  def XStore[R <: Stack, T <: JType](sym: Symbol.VarSym, tpe: ErasedType[T]): F[R ** T] => F[R] = tpe match {
    case ErasedType.Unit() => ???
    case ErasedType.Bool() => ???
    case ErasedType.BoxedBool() => ???
    case ErasedType.Char() => ???
    case ErasedType.BoxedChar() => ???
    case ErasedType.Float32() => FStore(sym)
    case ErasedType.BoxedFloat32() => ???
    case ErasedType.Float64() => DStore(sym)
    case ErasedType.BoxedFloat64() => ???
    case ErasedType.Int8() => ???
    case ErasedType.BoxedInt8() => ???
    case ErasedType.Int16() => ???
    case ErasedType.BoxedInt16() => ???
    case ErasedType.Int32() => IStore(sym)
    case ErasedType.BoxedInt32() => ???
    case ErasedType.Int64() => LStore(sym)
    case ErasedType.BoxedInt64() => ???
    case ErasedType.Array(tpe) => ???
    case ErasedType.Channel(tpe) => ???
    case ErasedType.Lazy(tpe) => ???
    case ErasedType.Ref(tpe) => ???
    case ErasedType.ObjectT(o) => AStore(sym)
    case ErasedType.Var(id) => ???
  }

  // TODO Question: does this make sense? can values still be seen as int32?
  def BOOLNEWARRAY[R <: Stack](): F[R] => F[R ** JArray[PrimInt32]] = ???

  def CNEWARRAY[R <: Stack](): F[R] => F[R ** JArray[PrimChar]] = ???

  def FNEWARRAY[R <: Stack](): F[R] => F[R ** JArray[PrimFloat32]] = ???

  def DNEWARRAY[R <: Stack](): F[R] => F[R ** JArray[PrimFloat64]] = ???

  def BNEWARRAY[R <: Stack](): F[R] => F[R ** JArray[PrimInt8]] = ???

  def SNEWARRAY[R <: Stack](): F[R] => F[R ** JArray[PrimInt16]] = ???

  def INEWARRAY[R <: Stack](): F[R] => F[R ** JArray[PrimInt32]] = ???

  def LNEWARRAY[R <: Stack](): F[R] => F[R ** JArray[PrimInt64]] = ???

  def ANEWARRAY[R <: Stack](): F[R] => F[R ** JArray[JObject]] = {
    // from genExpression:
    // visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
    // should type be built?
    ???
  }

  def XNEWARRAY[R <: Stack, T <: JType](arrayType: ErasedType[JArray[T]]): F[R] => F[R ** JArray[T]] = arrayType match {
          case ErasedType.Array(tpe) => tpe match {
            case ErasedType.Unit() => ???
            case ErasedType.Bool() => BOOLNEWARRAY()
            case ErasedType.BoxedBool() => ???
            case ErasedType.Char() => CNEWARRAY()
            case ErasedType.BoxedChar() => ???
            case ErasedType.Float32() => FNEWARRAY()
            case ErasedType.BoxedFloat32() => ???
            case ErasedType.Float64() => DNEWARRAY()
            case ErasedType.BoxedFloat64() => ???
            case ErasedType.Int8() => BNEWARRAY()
            case ErasedType.BoxedInt8() => ???
            case ErasedType.Int16() => SNEWARRAY()
            case ErasedType.BoxedInt16() => ???
            case ErasedType.Int32() => INEWARRAY()
            case ErasedType.BoxedInt32() => ???
            case ErasedType.Int64() => LNEWARRAY()
            case ErasedType.BoxedInt64() => ???
            case ErasedType.Array(tpe) => ???
            case ErasedType.Channel(tpe) => ???
            case ErasedType.Lazy(tpe) => ???
            case ErasedType.Ref(tpe) => ???
            case ErasedType.ObjectT(o) => ANEWARRAY()
            case ErasedType.Var(id) => ???
          }
    case _ => throw InternalCompilerException("unexpected non-array type")
  }

  implicit class ComposeOps[A, B](f: F[A] => F[B]) {
    def ~[C](that: F[B] => F[C]): F[A] => F[C] = ???
  }

}
