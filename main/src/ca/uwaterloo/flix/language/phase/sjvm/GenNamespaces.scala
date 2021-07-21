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
import ca.uwaterloo.flix.language.ast.{PRefType, PType, RType, Symbol}
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
      val arrow = getArrow(defn.tpe)
      classMaker.mkMethod(compileShimMethod(sym, arrow, arrow.result), sym.nsMethodName, defn.tpe.toDescriptor, ClassMaker.Mod.isPublic.isFinal.isStatic)
    }
    classMaker.closeClassMaker
  }

  def getArrow(tpe: RType[PReference[PFunction]]): RArrow =
    RType.getRReference(tpe).referenceType match {
      case r: RArrow => r
    }

  /**
    * Adding a shim for the function `defn` on namespace `ns`
    */
  private def compileShimMethod[T <: PType](sym: Symbol.DefnSym, functionType: RArrow, resType: RType[T])(implicit root: Root, flix: Flix): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      { f: F[StackNil] =>
        f.visitor.visitTypeInsn(Opcodes.NEW, sym.defName.toInternalName)
        f.visitor.visitInsn(Opcodes.DUP)
        f.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, sym.defName.toInternalName, JvmName.constructorMethod, JvmName.nothingToVoid, false)
        f.asInstanceOf[F[StackNil ** PReference[PRefType.PAnyObject]]]
      } ~
      {f: F[StackNil ** PReference[PAnyObject]] =>
        for (argIndex <- functionType.args.indices) {
          f.visitor.visitInsn(Opcodes.DUP)
          XLOAD(functionType.args(argIndex), argIndex)(f) // TODO(JLS): This does not work for cat 2 types
          f.visitor.visitFieldInsn(Opcodes.PUTFIELD, sym.defName.toInternalName, GenFunctionInterfaces.argFieldName(argIndex), functionType.args(argIndex).toDescriptor)
        }
        f.asInstanceOf[F[StackNil ** PReference[PAnyObject]]]
      } ~
      { f: F[StackNil ** PReference[PAnyObject]] =>
        f.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, resType.contName.toInternalName, GenContinuationInterfaces.unwindMethodName, resType.nothingToThisMethodDescriptor, false)
        f.asInstanceOf[F[StackNil ** T]]
      } ~
      XRETURN(resType)
  }

}
