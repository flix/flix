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

import ca.uwaterloo.flix.language.ast.ErasedAst.{Cat1, Cat2, PType => PT, PRefType => PRT, EType => ET, ERefType => ERT}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.util.InternalCompilerException

object Instructions {
  def WithSource[R <: Stack](loc: SourceLocation): F[R] => F[R] = ???

  // TODO QUESTION: which functions are "unsafe" irt. types? A: Leafs

  //todo: naming
  def ISWAP[R <: Stack, T1 <: PT with Cat1, T2 <: PT with Cat1]: F[R ** T2 ** T1] => F[R ** T1 ** T2] = ???

  def NOP[R <: Stack]: F[R] => F[R] = x => x

  def POP[R <: Stack, T <: PT]: F[R ** T] => F[R] = ???

  def DUP_X1[R <: Stack, T1 <: PT with Cat1, T2 <: PT with Cat1]: F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = ???

  def DUP_X2_onCat1[R <: Stack, T1 <: PT with Cat1, T2 <: PT with Cat1, T3 <: PT with Cat1]: F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = ???

  def DUP_X2_onCat2[R <: Stack, T1 <: PT with Cat1, T2 <: PT with Cat2]: F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = ???

  def DUP2_X2_cat1_onCat1[R <: Stack, T1 <: PT with Cat1, T2 <: PT with Cat1, T3 <: PT with Cat1, T4 <: PT with Cat1]: F[R ** T4 ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T4 ** T3 ** T2 ** T1] = ???

  def DUP2_X2_cat2_onCat1[R <: Stack, T1 <: PT with Cat2, T2 <: PT with Cat1, T3 <: PT with Cat1]: F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = ???

  def DUP2_X2_cat1_onCat2[R <: Stack, T1 <: PT with Cat1, T2 <: PT with Cat1, T3 <: PT with Cat2]: F[R ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T3 ** T2 ** T1] = ???

  def DUP2_X2_cat2_onCat2[R <: Stack, T1 <: PT with Cat2, T2 <: PT with Cat2]: F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = ???

  def DUP[R <: Stack, T <: PT with Cat1]: F[R ** T] => F[R ** T ** T] = ???

  def ISUB[R <: Stack]: F[R ** PT.PrimInt32 ** PT.PrimInt32] => F[R ** PT.PrimInt32] = ???

  def pushUnit[R <: Stack]: F[R] => F[R ** PT.PReference[PRT.PUnit]] = ???

  def pushNull[R <: Stack, T <: PRT]: F[R] => F[R ** PT.PReference[T]] = ???

  def pushBool[R <: Stack](b: Boolean): F[R] => F[R ** PT.PrimInt32] = pushInt32(if (b) 1 else 0)

  def pushInt8[R <: Stack](n: Int): F[R] => F[R ** PT.PrimInt8] = ???

  def pushInt16[R <: Stack](n: Int): F[R] => F[R ** PT.PrimInt16] = ???

  def pushInt32[R <: Stack](n: Int): F[R] => F[R ** PT.PrimInt32] = ???

  def pushInt64[R <: Stack](n: Long): F[R] => F[R ** PT.PrimInt64] = ???

  def pushFloat32[R <: Stack](n: Float): F[R] => F[R ** PT.PrimFloat32] = ???

  def pushFloat64[R <: Stack](n: Double): F[R] => F[R ** PT.PrimFloat64] = ???

  def pushChar[R <: Stack](c: Char): F[R] => F[R ** PT.PrimChar] = ???

  def BALoad[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimInt8]] ** PT.PrimInt32] => F[R ** PT.PrimInt8] = ???

  def SALoad[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimInt16]] ** PT.PrimInt32] => F[R ** PT.PrimInt16] = ???

  def IALoad[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimInt32]] ** PT.PrimInt32] => F[R ** PT.PrimInt32] = ???

  def LALoad[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimInt64]] ** PT.PrimInt32] => F[R ** PT.PrimInt64] = ???

  def CALoad[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimChar]] ** PT.PrimInt32] => F[R ** PT.PrimChar] = ???

  def FALoad[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimFloat32]] ** PT.PrimInt32] => F[R ** PT.PrimFloat32] = ???

  def DALoad[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimFloat64]] ** PT.PrimInt32] => F[R ** PT.PrimFloat64] = ???

  def AALoad[R <: Stack, T <: PRT]: F[R ** PT.PReference[PRT.PArray[PT.PReference[T]]] ** PT.PrimInt32] => F[R ** PT.PReference[T]] = ???

  def XALoad[R <: Stack, T <: PT](tpe: ET[T]): F[R ** PT.PReference[PRT.PArray[T]] ** PT.PrimInt32] => F[R ** T] = tpe match {
    case ET.Bool() | ET.Int32() => IALoad
    case ET.Char() => CALoad
    case ET.Float32() => FALoad
    case ET.Float64() => DALoad
    case ET.Int8() => BALoad
    case ET.Int16() => SALoad
    case ET.Int64() => LALoad
    case ET.Reference(_) => AALoad //todo: try pop pop pushunit
  }

  def BAStore[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimInt8]] ** PT.PrimInt32 ** PT.PrimInt8] => F[R] = ???

  def SAStore[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimInt16]] ** PT.PrimInt32 ** PT.PrimInt16] => F[R] = ???

  def IAStore[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimInt32]] ** PT.PrimInt32 ** PT.PrimInt32] => F[R] = ???

  def LAStore[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimInt64]] ** PT.PrimInt32 ** PT.PrimInt64] => F[R] = ???

  def CAStore[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimChar]] ** PT.PrimInt32 ** PT.PrimChar] => F[R] = ???

  def FAStore[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimFloat32]] ** PT.PrimInt32 ** PT.PrimFloat32] => F[R] = ???

  def DAStore[R <: Stack]: F[R ** PT.PReference[PRT.PArray[PT.PrimFloat64]] ** PT.PrimInt32 ** PT.PrimFloat64] => F[R] = ???

  def AAStore[R <: Stack, T <: PRT]: F[R ** PT.PReference[PRT.PArray[PT.PReference[T]]] ** PT.PrimInt32 ** PT.PReference[T]] => F[R] = ???

  def XAStore[R <: Stack, T <: PT](tpe: ET[T]): F[R ** PT.PReference[PRT.PArray[T]] ** PT.PrimInt32 ** T] => F[R] = tpe match {
    case ET.Char() => CAStore
    case ET.Float32() => FAStore
    case ET.Float64() => DAStore
    case ET.Int8() => BAStore
    case ET.Int16() => SAStore
    case ET.Bool() | ET.Int32() => IAStore
    case ET.Int64() => LAStore
    case ET.Reference(_) => AAStore
  }

  // I/S/B/C-Store are all just jvm ISTORE
  def IStore[R <: Stack](sym: Symbol.VarSym): F[R ** PT.PrimInt32] => F[R] = ???

  def SStore[R <: Stack](sym: Symbol.VarSym): F[R ** PT.PrimInt16] => F[R] = ???

  def BStore[R <: Stack](sym: Symbol.VarSym): F[R ** PT.PrimInt8] => F[R] = ???

  def CStore[R <: Stack](sym: Symbol.VarSym): F[R ** PT.PrimChar] => F[R] = ???

  def LStore[R <: Stack](sym: Symbol.VarSym): F[R ** PT.PrimInt64] => F[R] = ???

  def FStore[R <: Stack](sym: Symbol.VarSym): F[R ** PT.PrimFloat32] => F[R] = ???

  def DStore[R <: Stack](sym: Symbol.VarSym): F[R ** PT.PrimFloat64] => F[R] = ???

  def AStore[R <: Stack, T <: PRT](sym: Symbol.VarSym): F[R ** PT.PReference[T]] => F[R] = ???

  def XStore[R <: Stack, T <: PT](sym: Symbol.VarSym, tpe: ET[T]): F[R ** T] => F[R] = tpe match {
    case ET.Char() => CStore(sym)
    case ET.Float32() => FStore(sym)
    case ET.Float64() => DStore(sym)
    case ET.Int8() => BStore(sym)
    case ET.Int16() => SStore(sym)
    case ET.Bool() | ET.Int32() => IStore(sym)
    case ET.Int64() => LStore(sym)
    case ET.Reference(_) => AStore(sym)
  }

  // TODO Question: does this make sense? can values still be seen as int32?
  def BOOLNEWARRAY[R <: Stack]: F[R] => F[R ** PT.PReference[PRT.PArray[PT.PrimInt32]]] = ???

  def CNEWARRAY[R <: Stack]: F[R] => F[R ** PT.PReference[PRT.PArray[PT.PrimChar]]] = ???

  def FNEWARRAY[R <: Stack]: F[R] => F[R ** PT.PReference[PRT.PArray[PT.PrimFloat32]]] = ???

  def DNEWARRAY[R <: Stack]: F[R] => F[R ** PT.PReference[PRT.PArray[PT.PrimFloat64]]] = ???

  def BNEWARRAY[R <: Stack]: F[R] => F[R ** PT.PReference[PRT.PArray[PT.PrimInt8]]] = ???

  def SNEWARRAY[R <: Stack]: F[R] => F[R ** PT.PReference[PRT.PArray[PT.PrimInt16]]] = ???

  def INEWARRAY[R <: Stack]: F[R] => F[R ** PT.PReference[PRT.PArray[PT.PrimInt32]]] = ???

  def LNEWARRAY[R <: Stack]: F[R] => F[R ** PT.PReference[PRT.PArray[PT.PrimInt64]]] = ???

  def ANEWARRAY[R <: Stack, T <: PRT]: F[R] => F[R ** PT.PReference[PRT.PArray[PT.PReference[T]]]] = {
    // from genExpression:
    // visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
    // should type be built?
    ???
  }

  def XNEWARRAY[R <: Stack, T <: PT](arrayType: ET[PT.PReference[PRT.PArray[T]]]): F[R] => F[R ** PT.PReference[PRT.PArray[T]]] = arrayType match {
    case ET.Reference(ERT.Array(tpe)) => tpe match {
      case ET.Bool() => BOOLNEWARRAY
      case ET.Char() => CNEWARRAY
      case ET.Float32() => FNEWARRAY
      case ET.Float64() => DNEWARRAY
      case ET.Int8() => BNEWARRAY
      case ET.Int16() => SNEWARRAY
      case ET.Int32() => INEWARRAY
      case ET.Int64() => LNEWARRAY
      case ET.Reference(_) => ANEWARRAY
    }
    case _ => throw InternalCompilerException("unexpected non-array type")
  }

  def systemArrayCopy[R <: Stack]: F[R ** PT.PReference[PRT.AnyObject] ** PT.PrimInt32 ** PT.PReference[PRT.AnyObject] ** PT.PrimInt32 ** PT.PrimInt32] => F[R] = ???

  def arrayLength[R <: Stack, T <: PT]: F[R ** PT.PReference[PRT.PArray[T]]] => F[R ** PT.PrimInt32] = ???

  // also make void
  def defMakeFunction[R <: Stack, T <: PT](t: ET[T], x: F[R]): F[R ** T] = ???

  def foo = defMakeFunction(ET.Int32(), defMakeFunction(ET.Int32(), null))


  implicit class ComposeOps[A, B](f: F[A] => F[B]) {
    def ~[C](that: F[B] => F[C]): F[A] => F[C] = ???
  }

}
