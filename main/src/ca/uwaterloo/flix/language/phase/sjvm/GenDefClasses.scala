/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
 * Copyright 2017 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, PType, RType, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm.Opcodes

object GenDefClasses {

  def gen(defs: Map[Symbol.DefnSym, ErasedAst.Def[_ <: PType]], nonClosureFunctions: Set[Symbol.DefnSym])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    // TODO(JLS): check parops for all gens
    ParOps.parAgg(defs, Map[JvmName, JvmClass]())({
      case (macc, (sym, defn)) =>
        if (SjvmOps.nonLaw(defn) && nonClosureFunctions.contains(sym)) {
          val functionType = squeezeFunction(squeezeReference(defn.tpe))
          // TODO(JLS): type var in genByteCode gets set to PType... without casts does not work
          macc + (sym.defName -> JvmClass(sym.defName, genByteCode(defn.asInstanceOf[ErasedAst.Def[PType]], sym.defName, functionType.asInstanceOf[RArrow[PType]])))
        } else macc
    }, _ ++ _)
  }

  private def genByteCode[T <: PType](defn: ErasedAst.Def[T], defName: JvmName, functionType: RArrow[T])(implicit root: Root, flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(defName, addSource = false, Some(functionType.jvmName))
    classMaker.mkSuperConstructor()
    classMaker.mkMethod(genInvokeFunction(defn, defName), GenContinuationInterfaces.invokeMethodName, functionType.result.nothingToContMethodDescriptor, Mod.isPublic)
    classMaker.closeClassMaker
  }

  def genInvokeFunction[T <: PType](defn: ErasedAst.Def[T], defName: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      (multiComposition(defn.formals.zipWithIndex) {
        case (ErasedAst.FormalParam(sym, tpe), index) =>
          magicStoreArg(index, tpe, defName, sym)
      }) ~
      compileExp(defn.exp) ~
      THISLOAD(tagOf[PAnyObject]) ~
      magicReversePutField(defName, defn.exp.tpe) ~
      RETURNNULL
  }

  def magicReversePutField[R <: Stack, T <: PType](className: JvmName, resultType: RType[T]): F[R ** T ** PReference[PAnyObject]] => F[R] = f => {
    resultType match {
      case RType.RInt64 | RType.RFloat64 =>
        f.visitor.visitInsn(Opcodes.DUP_X2)
        f.visitor.visitInsn(Opcodes.POP)
      case _ =>
        f.visitor.visitInsn(Opcodes.SWAP)
    }
    f.visitor.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, GenContinuationInterfaces.resultFieldName, resultType.erasedDescriptor)
    f.asInstanceOf[F[R]]
  }

  def magicStoreArg[R <: Stack, T <: PType](index: Int, tpe: RType[T], defName: JvmName, sym: Symbol.VarSym): F[R] => F[R] = {
    ((f: F[R]) => {
      f.visitor.visitVarInsn(Opcodes.ALOAD, 0)
      f.visitor.visitFieldInsn(Opcodes.GETFIELD, defName.toInternalName, GenFunctionInterfaces.argFieldName(index), tpe.erasedDescriptor)
      undoErasure(tpe, f.visitor)
      f.asInstanceOf[F[R ** T]]
    }) ~ XStore(sym, tpe)
  }

}
