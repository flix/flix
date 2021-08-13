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


// TODO(JLS): refactoring against GenDefClasses
object GenClosureClasses {

  def cloArgFieldName(index: Int): String = s"clo$index"

  def gen(closures: Set[ClosureInfo[_ <: PType]])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    ParOps.parAgg(closures, Map.empty[JvmName, JvmClass])({
      case (macc, closure) =>
        val cloSym = closure.sym
        val cloName = cloSym.cloName
        val cloDef = root.functions(cloSym)
        // TODO(JLS): type var in genByteCode gets set to PType... without casts does not work. problem with all gen functions
        val bytecode = genByteCode(cloDef.asInstanceOf[ErasedAst.Def[PType]], cloName, closure.freeVars, squeezeFunction(squeezeReference(cloDef.tpe)).asInstanceOf[RArrow[PType]])
        macc + (cloName -> JvmClass(cloName, bytecode))
    }, _ ++ _)

  }

  private def genByteCode[T <: PType](defn: ErasedAst.Def[T], cloName: JvmName, freeVars: List[ErasedAst.FreeVar], functionType: RArrow[T])(implicit root: Root, flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(cloName, addSource = false, Some(functionType.jvmName))
    classMaker.mkSuperConstructor()
    classMaker.mkMethod(genInvokeFunction(defn, defn.exp, cloName, freeVars), GenContinuationInterfaces.invokeMethodName, functionType.result.nothingToContMethodDescriptor, Mod.isPublic)
    for ((fv, index) <- freeVars.zipWithIndex) {
      classMaker.mkField(GenClosureClasses.cloArgFieldName(index), fv.tpe.erasedType, Mod.isPublic)
    }
    classMaker.closeClassMaker
  }

  def genInvokeFunction[T <: PType](defn: ErasedAst.Def[T], functionBody: ErasedAst.Expression[T], cloName: JvmName, freeVars: List[ErasedAst.FreeVar]): F[StackNil] => F[StackEnd] = {
    // TODO(JLS): maybe frees should not be in formals?
    // Free variables
    val frees = defn.formals.take(freeVars.length).map(x => ErasedAst.FreeVar(x.sym, x.tpe))
    // Function parameters
    val params = defn.formals.takeRight(defn.formals.length - freeVars.length)

    START[StackNil] ~
      (multiComposition(frees.zipWithIndex) { case (freeVar, index) =>
        magicStoreArg(index, freeVar.tpe, cloName, freeVar.sym, GenClosureClasses.cloArgFieldName)
      }) ~
      (multiComposition(params.zipWithIndex) { case (formalParam, index) =>
        magicStoreArg(index, formalParam.tpe, cloName, formalParam.sym, GenFunctionInterfaces.argFieldName)
      }) ~
      compileExp(functionBody) ~
      THISLOAD(tagOf[PAnyObject]) ~
      magicReversePutField(cloName, functionBody.tpe) ~
      RETURNNULL
  }

  // TODO(JLS): could be SWAP_cat1_onSomething(..) ~ PUTFIELD(..)
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

  def magicStoreArg[R <: Stack, T <: PType](index: Int, tpe: RType[T], defName: JvmName, sym: Symbol.VarSym, fieldName: Int => String): F[R] => F[R] = {
    ((f: F[R]) => {
      f.visitor.visitVarInsn(Opcodes.ALOAD, 0)
      f.visitor.visitFieldInsn(Opcodes.GETFIELD, defName.toInternalName, fieldName(index), tpe.erasedDescriptor)
      undoErasure(tpe, f.visitor)
      f.asInstanceOf[F[R ** T]]
    }) ~ XStore(sym, tpe)
  }

}