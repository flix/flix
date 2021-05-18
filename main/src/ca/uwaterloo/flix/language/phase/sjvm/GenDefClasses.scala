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
import ca.uwaterloo.flix.language.ast.RRefType.RArrow
import ca.uwaterloo.flix.language.ast.{ErasedAst, PType, RType, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.Opcodes

object GenDefClasses {

  def gen(defs: Map[Symbol.DefnSym, ErasedAst.Def])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    defs.foldLeft(Map[JvmName, JvmClass]()) {
      case (macc, (sym, defn)) =>
        if (SjvmOps.nonLaw(defn)) {
          RType.getRReference(defn.tpe).referenceType match {
            case functionType@RArrow(_, _) =>
              macc + (sym.defName -> JvmClass(sym.defName, genByteCode(defn, sym.defName, functionType)))
          }
        } else macc
    }
  }

  private def genByteCode(defn: ErasedAst.Def, defName: JvmName, functionType: RArrow)(implicit root: Root, flix: Flix): Array[Byte] = {

    val classMaker = ClassMaker.mkClass(defName, addSource = false, Some(functionType.functionInterfaceName))
    classMaker.mkSuperConstructor()
    classMaker.mkMethod(genApplyFunction(defn, defName, functionType), GenContinuationInterfaces.invokeMethodName, functionType.result.nothingToThisMethodDescriptor, Mod.isPublic)
    classMaker.closeClassMaker
  }

  def genApplyFunction(defn: ErasedAst.Def, defName: JvmName, functionType: RArrow): F[StackNil] => F[StackEnd] = {

    START[StackNil] ~
      (defn.formals.zipWithIndex.foldLeft(START[StackNil]) {
        case (prev, (ErasedAst.FormalParam(sym, tpe), index)) =>
          prev ~ storeArg(index, tpe, defName, sym)
      }) ~ RETURN
    // TODO(JLS): Finish This
  }

  def storeArg[R <: Stack, T <: PType](index: Int, tpe: RType[T], defName: JvmName, sym: Symbol.VarSym): F[R] => F[R] = {
    ((f: F[R]) => {
      f.visitor.visitVarInsn(Opcodes.ALOAD, 0)
      f.visitor.visitFieldInsn(Opcodes.GETFIELD, defName.toInternalName, GenFunctionInterfaces.argFieldName(index), tpe.toDescriptor)
      f.asInstanceOf[F[R ** T]]
    }) ~ XStore(sym, tpe)
  }

}
