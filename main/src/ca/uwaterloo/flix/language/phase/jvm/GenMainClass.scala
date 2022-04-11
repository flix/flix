/*
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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the main class.
  */
object GenMainClass {

  /**
    * Returns the main class.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = getMain(root) match {
    case None => Map.empty
    case Some(defn) =>
      val jvmType = JvmOps.getMainClassType()
      val jvmName = jvmType.name
      val arrowType = defn.tpe.asInstanceOf[MonoType.Arrow]
      // returnType, giveArgs, and their use can be inlined if m_main return type is known
      val giveArgs = arrowType.args.headOption match {
        case None => false
        case Some(_) => true // assume its array of string
      }
      val returnType = JvmOps.getErasedJvmType(arrowType.result)
      val bytecode = genByteCode(defn.sym, jvmType, giveArgs, returnType)
      Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  /**
    * Optionally returns the main definition in the given AST `root`.
    */
  private def getMain(root: Root): Option[Def] = {
    root.entryPoint match {
      case None => None
      case Some(sym) => root.defs.get(sym)
    }
  }

  private def genByteCode(sym: Symbol.DefnSym, jvmType: JvmType.Reference, giveArgs: Boolean, returnType: JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, jvmType.name.toInternalName, null, superClass, null)

    // Source of the class
    visitor.visitSource(jvmType.name.toInternalName, null)

    // Emit the code for the main method
    compileMainMethod(sym, visitor, giveArgs, returnType)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Emits code for the main method in the main class. The emitted (byte)code should satisfy the following signature for the method:
    * public static void main(String[])
    *
    * The method itself needs simply invoke the m_entry method which is in the root namespace.
    *
    * The emitted code for the method should correspond to:
    *
    * Ns.m_entry((Object)null);
    */
  private def compileMainMethod(sym: Symbol.DefnSym, visitor: ClassWriter, giveArgs: Boolean, returnType: JvmType)(implicit root: Root, flix: Flix): Unit = {

    //Get the (argument) descriptor, since the main argument is of type String[], we need to get it's corresponding descriptor
    val argumentDescriptor = AsmOps.getArrayType(JvmType.String)

    //Get the (result) descriptor, since main method returns void, we need to get the void type descriptor
    val resultDescriptor = JvmType.Void.toDescriptor

    //Emit the main method signature
    val main = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", s"($argumentDescriptor)$resultDescriptor", null, null)

    main.visitCode()

    //Get the root namespace in order to get the class type when invoking m_<entry>
    val ns = JvmOps.getNamespace(sym)

    // Call Ns.m_<entry>(args)

    // Push the args array on the stack.
    main.visitVarInsn(ALOAD, 0)

    // Save the args in `dev.flix.runtime.Global.setArgs(..)
    main.visitMethodInsn(INVOKESTATIC, JvmName.Global.toInternalName, GenGlobalClass.SetArgsMethodName,
      s"([${BackendObjType.String.toDescriptor})V", false)

    val nsClassName = JvmOps.getNamespaceClassType(ns).name.toInternalName
    val mainMethodName = JvmOps.getDefMethodNameInNamespaceClass(sym)
    if (giveArgs) {
      // give the args
      main.visitVarInsn(ALOAD, 0)
      main.visitMethodInsn(INVOKESTATIC, nsClassName, mainMethodName,
        AsmOps.getMethodDescriptor(List(JvmType.Object), returnType), false)
    } else {
      // no args
      main.visitMethodInsn(INVOKESTATIC, nsClassName, mainMethodName,
        AsmOps.getMethodDescriptor(Nil, returnType), false)
    }

    // The return value is ignored

    main.visitInsn(RETURN)
    main.visitMaxs(1, 1)
    main.visitEnd()
  }

}
