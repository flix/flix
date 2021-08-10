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
import ca.uwaterloo.flix.language.ast.{PType, RType, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.Opcodes

/**
 * Generates bytecode for the namespace classes.
 */
object GenNamespaces {

  /**
   * Returns the set of namespaces classes for the given set of namespaces.
   */
  def gen(namespaces: Set[NamespaceInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a namespace class for each namespace and collect the results in a map.
    //
    namespaces.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, ns) =>
        val className = SjvmOps.getNamespaceClassType(ns)
        val bytecode = genBytecode(className, ns)
        macc + (className -> JvmClass(className, bytecode))
    }
  }

  /**
   * Returns the namespace class for the given namespace `ns`.
   */
  private def genBytecode(className: JvmName, ns: NamespaceInfo)(implicit root: Root, flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(className, addSource = false, None)
    classMaker.mkSuperConstructor()
    for ((sym, defn) <- ns.defs) {
      val arrow = squeezeFunction(squeezeReference(defn.tpe))
      classMaker.mkMethod(compileShimMethod(sym, arrow, arrow.result), sym.nsMethodName, defn.tpe.toDescriptor, ClassMaker.Mod.isPublic.isFinal.isStatic)
    }
    classMaker.closeClassMaker
  }

  /**
   * Adding a shim for the function `defn` on namespace `ns`
   */
  private def compileShimMethod[T <: PType](sym: Symbol.DefnSym, functionType: RArrow, resType: RType[T])(implicit root: Root, flix: Flix): F[StackNil] => F[StackEnd] = {
    //TODO(JLS): largely the same as CALL/TAILCALL except compiling arguments versus the loading of arguments here
    START[StackNil] ~ { f: F[StackNil] =>
      f.visitor.visitTypeInsn(Opcodes.NEW, sym.defName.toInternalName)
      f.visitor.visitInsn(Opcodes.DUP)
      f.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, sym.defName.toInternalName, JvmName.constructorMethod, JvmName.nothingToVoid, false)
      f.asInstanceOf[F[StackNil ** PReference[PFunction]]]
    } ~ { f: F[StackNil ** PReference[PFunction]] =>
      var nextIndex = 0
      for ((argType, argIndex) <- functionType.args.zipWithIndex) {
        f.visitor.visitInsn(Opcodes.DUP)
        XLOAD(functionType.args(argIndex), nextIndex)(f)
        val inc = if (argType.isCat1) 1 else 2
        nextIndex += inc
        f.visitor.visitFieldInsn(Opcodes.PUTFIELD, sym.defName.toInternalName, GenFunctionInterfaces.argFieldName(argIndex), functionType.args(argIndex).erasedDescriptor)
      }
      f.asInstanceOf[F[StackNil ** PReference[PFunction]]]
    } ~ { f: F[StackNil ** PReference[PFunction]] =>
      f.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, resType.contName.toInternalName, GenContinuationInterfaces.unwindMethodName, resType.nothingToThisMethodDescriptor, false)
      RType.undoErasure(resType, f.visitor)
      f.asInstanceOf[F[StackNil ** T]]
    } ~
      XRETURN(resType)
  }

}
