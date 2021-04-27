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

import ca.uwaterloo.flix.language.ast.ERefType._
import ca.uwaterloo.flix.language.ast.EType._
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.{Cat1, Cat2, EType, PRefType, PType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.util.InternalCompilerException

object Instructions {
  def WithSource[R <: Stack](loc: SourceLocation): F[R] => F[R] = ???

  // NATIVE
  def SWAP
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  // META
  def NOP
  [R <: Stack]:
  F[R] => F[R] =
    x => x

  // NATIVE
  def XGETFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String, fieldType: EType[T1]):
  F[R ** PReference[T2]] => F[R ** T1] =
    fieldType match {
      case Bool() =>  ???
      case Int8() => ???
      case Int16() => ???
      case Int32() => ???
      case Int64() => ???
      case Char() => ???
      case Float32() => ???
      case Float64() => ???
      case Reference(referenceType) => ???
    }

  // NATIVE
  def GetInt32Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String, fieldType: EType[T1]):
  F[R ** PReference[T2]] => F[R ** PInt32] =
    ???

  // NATIVE
  def PUTFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String, fieldType: EType[T1]):
  F[R ** PReference[T2] ** T1] => F[R] =
    ???

  // NATIVE
  def CAST
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PAnyObject]] => F[R ** PReference[T]] =
    ???

  // NATIVE
  def NEW
  [R <: Stack, T <: PRefType]
  (className: String):
  F[R] => F[R ** PReference[T]] =
    ???

  // TODO: type should be constructed along with descriptor string
  // NATIVE
  def INVOKESPECIAL
  [R <: Stack, T <: PRefType]
  (className: String, constructorDescriptor: String):
  F[R ** PReference[T]] => F[R] =
    ???

  // todo delete
  def SCAFFOLD
  [R1 <: Stack, R2 <: Stack]:
  F[R1] => F[R2] =
    ???

  // TODO: What should happen here
  // NATIVE
  // def RETURN[R <: Stack]: F[R] => R[???] =

  // NATIVE
  def POP
  [R <: Stack, T <: PType with Cat1]:
  F[R ** T] => F[R] =
    ???

  // NATIVE
  def DUP_X1
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] =
    ???

  // NATIVE
  def DUP_X2_onCat1
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1, T3 <: PType with Cat1]:
  F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] =
    ???

  // NATIVE
  def DUP_X2_onCat2
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat2]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] =
    ???

  // NATIVE
  def DUP2_X2_cat1_onCat1
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1, T3 <: PType with Cat1, T4 <: PType with Cat1]:
  F[R ** T4 ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T4 ** T3 ** T2 ** T1] =
    ???

  // NATIVE
  def DUP2_X2_cat2_onCat1
  [R <: Stack, T1 <: PType with Cat2, T2 <: PType with Cat1, T3 <: PType with Cat1]:
  F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] =
    ???

  // NATIVE
  def DUP2_X2_cat1_onCat2
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1, T3 <: PType with Cat2]:
  F[R ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T3 ** T2 ** T1] =
    ???

  // NATIVE
  def DUP2_X2_cat2_onCat2
  [R <: Stack, T1 <: PType with Cat2, T2 <: PType with Cat2]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] =
    ???

  // NATIVE
  def DUP
  [R <: Stack, T <: PType with Cat1]:
  F[R ** T] => F[R ** T ** T] =
    ???

  // NATIVE
  def ISUB
  [R <: Stack]:
  F[R ** PInt32 ** PInt32] => F[R ** PInt32] =
    ???

  // NATIVE
  def pushUnit
  [R <: Stack]:
  F[R] => F[R ** PReference[PUnit]] = {
    ???
    // flix/runtime/value/Unit
  }

  // NATIVE
  def pushNull
  [R <: Stack, T <: PRefType]:
  F[R] => F[R ** PReference[T]] =
    ???

  // NATIVE
  def pushBool
  [R <: Stack]
  (b: Boolean):
  F[R] => F[R ** PInt32] =
    pushInt32(if (b) 1 else 0)

  // NATIVE
  def pushInt8
  [R <: Stack]
  (n: Int):
  F[R] => F[R ** PInt8] =
    ???

  // NATIVE
  def pushInt16
  [R <: Stack]
  (n: Int):
  F[R] => F[R ** PInt16] =
    ???

  // NATIVE
  def pushInt32
  [R <: Stack]
  (n: Int):
  F[R] => F[R ** PInt32] =
    ???

  // NATIVE
  def pushInt64
  [R <: Stack]
  (n: Long):
  F[R] => F[R ** PInt64] =
    ???

  // NATIVE
  def pushFloat32
  [R <: Stack]
  (n: Float):
  F[R] => F[R ** PFloat32] =
    ???

  // NATIVE
  def pushFloat64
  [R <: Stack]
  (n: Double):
  F[R] => F[R ** PFloat64] =
    ???

  // NATIVE
  def pushChar
  [R <: Stack]
  (c: scala.Char):
  F[R] => F[R ** PChar] =
    ???

  // NATIVE
  def ALOAD
  [R <: Stack, T <: PRefType]
  (index: Int):
  F[R] => F[R ** PReference[T]] =
    ???

  // NATIVE
  def FLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PFloat32] =
    ???

  // NATIVE
  def DLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PFloat64] =
    ???

  // NATIVE
  def ILOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PInt32] =
    ???

  // NATIVE
  def LLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PInt64] =
    ???

  // META
  def XLOAD
  [R <: Stack, T <: PType]
  (tpe: EType[T], index: Int):
  F[R] => F[R ** T] =
    tpe match {
      case Bool() => ???
      case Int8() => ???
      case Int16() => ???
      case Int32() => ILOAD(index)
      case Int64() => LLOAD(index)
      case Char() => ???
      case Float32() => FLOAD(index)
      case Float64() => DLOAD(index)
      case Reference(_) => ALOAD(index)
    }

  // NATIVE
  def BALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PInt8]] ** PInt32] => F[R ** PInt8] =
    ???

  // NATIVE
  def SALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PInt16]] ** PInt32] => F[R ** PInt16] =
    ???

  // NATIVE
  def IALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PInt32]] ** PInt32] => F[R ** PInt32] =
    ???

  // NATIVE
  def LALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PInt64]] ** PInt32] => F[R ** PInt64] =
    ???

  // NATIVE
  def CALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PChar]] ** PInt32] => F[R ** PChar] =
    ???

  // NATIVE
  def FALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat32]] ** PInt32] => F[R ** PFloat32] =
    ???

  // NATIVE
  def DALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat64]] ** PInt32] => F[R ** PFloat64] =
    ???

  // NATIVE
  def AALoad
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PArray[PReference[T]]] ** PInt32] => F[R ** PReference[T]] =
    ???

  // META
  def XALoad
  [R <: Stack, T <: PType]
  (tpe: EType[T]):
  F[R ** PReference[PArray[T]] ** PInt32] => F[R ** T] =
    tpe match {
      case Bool() | Int32() => IALoad
      case Char() => CALoad
      case Float32() => FALoad
      case Float64() => DALoad
      case Int8() => BALoad
      case Int16() => SALoad
      case Int64() => LALoad
      case Reference(_) => AALoad
    }

  // NATIVE
  def BAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt8]] ** PInt32 ** PInt8] => F[R] =
    ???

  // NATIVE
  def SAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt16]] ** PInt32 ** PInt16] => F[R] =
    ???

  // NATIVE
  def IAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt32]] ** PInt32 ** PInt32] => F[R] =
    ???

  // NATIVE
  def LAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt64]] ** PInt32 ** PInt64] => F[R] =
    ???

  // NATIVE
  def CAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PChar]] ** PInt32 ** PChar] => F[R] =
    ???

  // NATIVE
  def FAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat32]] ** PInt32 ** PFloat32] => F[R] =
    ???

  // NATIVE
  def DAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat64]] ** PInt32 ** PFloat64] => F[R] =
    ???

  // NATIVE
  def AAStore
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PArray[PReference[T]]] ** PInt32 ** PReference[T]] => F[R] =
    ???

  // META
  def XAStore
  [R <: Stack, T <: PType]
  (tpe: EType[T]):
  F[R ** PReference[PArray[T]] ** PInt32 ** T] => F[R] =
    tpe match {
      case Char() => CAStore
      case Float32() => FAStore
      case Float64() => DAStore
      case Int8() => BAStore
      case Int16() => SAStore
      case Bool() | Int32() => IAStore
      case Int64() => LAStore
      case Reference(_) => AAStore
    }

  // I/S/B/C-Store are all just jvm ISTORE
  // NATIVE
  def IStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt32] => F[R] =
    ???

  // NATIVE
  def SStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt16] => F[R] =
    ???

  // NATIVE
  def BStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt8] => F[R] =
    ???

  // NATIVE
  def CStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PChar] => F[R] =
    ???

  // NATIVE
  def LStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt64] => F[R] =
    ???

  // NATIVE
  def FStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat32] => F[R] =
    ???

  // NATIVE
  def DStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat64] => F[R] =
    ???

  // NATIVE
  def AStore
  [R <: Stack, T <: PRefType]
  (sym: Symbol.VarSym):
  F[R ** PReference[T]] => F[R] =
    ???

  // META
  def XStore
  [R <: Stack, T <: PType]
  (sym: Symbol.VarSym, tpe: EType[T]):
  F[R ** T] => F[R] =
    tpe match {
      case Char() => CStore(sym)
      case Float32() => FStore(sym)
      case Float64() => DStore(sym)
      case Int8() => BStore(sym)
      case Int16() => SStore(sym)
      case Bool() | Int32() => IStore(sym)
      case Int64() => LStore(sym)
      case Reference(_) => AStore(sym)
    }

  // NATIVE
  def BOOLNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt32]]] =
    ???

  // NATIVE
  def CNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PChar]]] =
    ???

  // NATIVE
  def FNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PFloat32]]] =
    ???

  // NATIVE
  def DNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PFloat64]]] =
    ???

  // NATIVE
  def BNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt8]]] =
    ???

  // NATIVE
  def SNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt16]]] =
    ???

  // NATIVE
  def INEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt32]]] =
    ???

  // NATIVE
  def LNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt64]]] =
    ???

  // NATIVE
  def ANEWARRAY
  [R <: Stack, T <: PRefType]:
  F[R ** PInt32] => F[R ** PReference[PArray[PReference[T]]]] =
  // from genExpression:
  // visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
  // should type be built?
    ???

  // META
  def XNEWARRAY
  [R <: Stack, T <: PType]
  (arrayType: EType[PReference[PArray[T]]]):
  F[R ** PInt32] => F[R ** PReference[PArray[T]]] =
    arrayType match {
      case Reference(Array(tpe)) => tpe match {
        case Bool() => BOOLNEWARRAY
        case Char() => CNEWARRAY
        case Float32() => FNEWARRAY
        case Float64() => DNEWARRAY
        case Int8() => BNEWARRAY
        case Int16() => SNEWARRAY
        case Int32() => INEWARRAY
        case Int64() => LNEWARRAY
        case Reference(_) => ANEWARRAY
      }
      case _ => throw InternalCompilerException("unexpected non-array type")
    }

  def systemArrayCopy
  [R <: Stack, S <: PRefType]:
  F[R ** PReference[S] ** PInt32 ** PReference[S] ** PInt32 ** PInt32] => F[R] =
    ???

  def arrayLength
  [R <: Stack, T <: PType]:
  F[R ** PReference[PArray[T]]] => F[R ** PInt32] =
    ???

  // also make void
  def defMakeFunction
  [R <: Stack, T <: PType]
  (x: F[R], t: EType[T]):
  F[R ** T] = {
    //todo where is this string stored
    EType.toInternalName(t)
    ???
  }

  def foo[R <: Stack] = (r: F[R]) => defMakeFunction(defMakeFunction(r, Int32()), Int32())

  // META
  implicit class ComposeOps[A, B](f: F[A] => F[B]) {
    def ~[C](that: F[B] => F[C]): F[A] => F[C] = ???
  }

}
