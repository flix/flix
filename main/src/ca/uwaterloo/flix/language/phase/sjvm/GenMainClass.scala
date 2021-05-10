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
import ca.uwaterloo.flix.language.ast.ErasedAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType.PReference
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType.RReference
import ca.uwaterloo.flix.language.ast.{PRefType, PType, RRefType, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler.{F, StackEnd, StackNil}
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import ca.uwaterloo.flix.util.InternalCompilerException

/**
 * Generates bytecode for the main class.
 */
object GenMainClass {

  val mainMethod: String = "main"

  val mainMethodClassName: JvmName = JvmName.main

  /**
   * Returns the main class.
   */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = getMain(root) match {
    case None => Map.empty
    case Some(defn) =>
      def mainTypeMatch[T <: PRefType](m: RRefType[T]) = m match {
        case r: RArrow => r
        case _ => throw InternalCompilerException(s"The type of main has to be a function, not ${m.toInternalName}")
      }

      val mainType = defn.tpe match {
        case RReference(referenceType) => RReference(mainTypeMatch(referenceType))
        case _ => throw InternalCompilerException(s"The type of main cannot be a primitive, ${defn.tpe}")
      }

      val bytecode = genByteCode(mainType)
      Map(mainMethodClassName -> JvmClass(mainMethodClassName, bytecode))
  }

  def genByteCode(mainType: RReference[PFunction])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val classMaker = ClassMaker.mkClass(JvmName.main, addSource = true)

    // Emit the code for the main method
    classMaker.mkMethod(compileMainMethod(mainType), mainMethod, JvmName.javaMainDescriptor, Mod.isPublic.isStatic)

    classMaker.closeClassMaker
  }

    /**
     * Emits code for the main method in the main class. The emitted (byte)code should satisfy the following signature for the method:
     * public static void main(String[])
     *
     * The method itself needs simply invoke the m_main method which is in the root namespace.
     *
     * The emitted code for the method should correspond to:
     *
     * Ns.m_main((Object)null);
     */
    def compileMainMethod(mainType: RReference[PFunction])(implicit root: Root, flix: Flix): F[StackNil] => F[StackEnd] = {

      //Get the root namespace in order to get the class type when invoking m_main
      val ns = SjvmOps.getNamespace(Symbol.Main)

      // Call Ns.m_main(args)

      // Push the args array on the stack.
//      main.visitVarInsn(ALOAD, 0)
      START[StackNil] ~
        ALOAD(0, tag[PArray[PReference[PStr]]]) ~
        POP ~
        RETURN
//      THISLOAD(tag[PArray[PReference[PStr]]]) ~
//        ???
//      Invoke m_main
//      main.visitMethodInsn(INVOKESTATIC, JvmOps.getNamespaceClassType(ns).name.toInternalName, "m_main",
//        AsmOps.getMethodDescriptor(List(/* TODO: Should be string array */ JvmType.Object), JvmType.PrimInt), false)
//
//      main.visitInsn(RETURN)
//      main.visitMaxs(1,1)
//      main.visitEnd()
    }

  /**
   * Optionally returns the main definition in the given AST `root`.
   */
  private def getMain(root: Root): Option[Def[_ <: PType]] = {
    // The main function must be called `main` and occur in the root namespace.
    val sym = Symbol.Main

    // Check if the main function exists.
    root.defs.get(sym) flatMap {
      case defn =>
        // The main function must take zero arguments.
        Some(defn)
    }
  }

}
