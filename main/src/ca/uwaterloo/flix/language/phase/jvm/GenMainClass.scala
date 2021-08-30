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
import ca.uwaterloo.flix.language.ast.FinalAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.Symbol
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
      val retJvmType = JvmOps.getErasedJvmType(defn.tpe)
      val bytecode = genByteCode(jvmType, retJvmType)
      Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  def genByteCode(jvmType: JvmType.Reference, retJvmType: JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, jvmType.name.toInternalName, null, superClass, null)

    // Source of the class
    visitor.visitSource(jvmType.name.toInternalName, null)

    // Emit the code for the main method
    compileMainMethod(visitor, jvmType, retJvmType)

    visitor.visitEnd()
    visitor.toByteArray
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
  def compileMainMethod(visitor: ClassWriter, jvmType: JvmType.Reference, retJvmType: JvmType)(implicit root: Root, flix: Flix): Unit = {

    //Get the (argument) descriptor, since the main argument is of type String[], we need to get it's corresponding descriptor
    val argumentDescriptor = AsmOps.getArrayType(JvmType.String)

    //Get the (result) descriptor, since main method returns void, we need to get the void type descriptor
    val resultDescriptor = JvmType.Void.toDescriptor

    //Emit the main method signature
    val main = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, "main",s"($argumentDescriptor)$resultDescriptor", null, null)

    main.visitCode()

    //Get the root namespace in order to get the class type when invoking m_main
    val ns = JvmOps.getNamespace(Symbol.Main)

    // Call Ns.m_main(args)

    // Push the args array on the stack.
    main.visitVarInsn(ALOAD, 0)

    //Invoke m_main
    main.visitMethodInsn(INVOKESTATIC, JvmOps.getNamespaceClassType(ns).name.toInternalName, "m_main",
      AsmOps.getMethodDescriptor(List(/* TODO: Should be string array */ JvmType.Object), JvmType.PrimInt), false)

    main.visitMethodInsn(INVOKESTATIC, "java/lang/System", "exit",
      AsmOps.getMethodDescriptor(List(JvmType.PrimInt), JvmType.Void), false)

    main.visitInsn(RETURN)
    main.visitMaxs(1,1)
    main.visitEnd()
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
        Some(defn)
    }
  }

}
