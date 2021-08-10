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
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PRefType, RRefType, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.Opcodes

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
      def mainTypeMatch[T <: PRefType](m: RRefType[T]): RRefType[PFunction] = m match {
        case r: RArrow => r // TODO(JLS): maybe match more here on type etc
        case _ => throw InternalCompilerException(s"The type of main has to be a function, not ${m.toInternalName}")
      }

      val mainType = defn.tpe match {
        case RReference(referenceType) => RReference(mainTypeMatch(referenceType))
        case _ => throw InternalCompilerException(s"The type of main cannot be a primitive, ${defn.tpe}")
      }

      // TODO(JLS): should get a namespace and a name. maybe its always Ns.m_main(args)
      val bytecode = genByteCode(defn, mainType)
      Map(mainMethodClassName -> JvmClass(mainMethodClassName, bytecode))
  }

  def genByteCode(defn: Def, mainType: RReference[PFunction])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val classMaker = ClassMaker.mkClass(JvmName.main, addSource = true, None)

    // Emit the code for the main method
    classMaker.mkMethod(compileMainMethod(defn, mainType), mainMethod, JvmName.javaMainDescriptor, Mod.isPublic.isStatic)

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
  def compileMainMethod(defn: Def, mainType: RReference[PFunction])(implicit root: Root, flix: Flix): F[StackNil] => F[StackEnd] = {

    //Get the root namespace in order to get the class type when invoking m_main
    // Call Ns.m_main(args)
    // Push the args array on the stack.
    //      main.visitVarInsn(ALOAD, 0)
    // TODO(JLS): This could just call NS.m_main()
    START[StackNil] ~ { f: F[StackNil] =>
      f.visitor.visitTypeInsn(Opcodes.NEW, defn.sym.defName.toInternalName)
      f.visitor.visitInsn(Opcodes.DUP)
      f.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, defn.sym.defName.toInternalName, JvmName.constructorMethod, JvmName.nothingToVoid, false)
      f.asInstanceOf[F[StackNil ** PReference[PFunction]]]
    } ~
      DUP ~
      ALOAD(0, tagOf[PArray[PReference[PStr]]]) ~ { f: F[StackNil ** PReference[PFunction] ** PReference[PFunction] ** PReference[PArray[PReference[PStr]]]] =>
      f.visitor.visitFieldInsn(Opcodes.PUTFIELD, defn.sym.defName.toInternalName, GenFunctionInterfaces.argFieldName(0), JvmName.Java.Lang.Object.toDescriptor)
      f.asInstanceOf[F[StackNil ** PReference[PFunction]]]
    } ~ { f: F[StackNil ** PReference[PFunction]] =>
      f.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, RInt32.contName.toInternalName, GenContinuationInterfaces.unwindMethodName, RInt32.nothingToThisMethodDescriptor, false)
      f.asInstanceOf[F[StackNil ** PInt32]]
    } ~ { f: F[StackNil ** PInt32] =>
      f.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/System", "exit", "(I)V", false);
      f.asInstanceOf[F[StackNil]]
    } ~
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
  private def getMain(root: Root): Option[Def] = {
    // The main function must be called `main` and occur in the root namespace.
    val sym = Symbol.Main

    // Check if the main function exists.
    root.defs.get(sym) flatMap {
      case defn =>
        // The main function must take zero arguments.
        // TODO(JLS): is this always the correct main?
        Some(defn)
    }
  }

}
