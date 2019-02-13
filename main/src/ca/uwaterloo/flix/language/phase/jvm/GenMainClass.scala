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
      // TODO: Ramin: Emit a main class with the appropriate method that calls the main shim.
      val jvmType = JvmOps.getMainClassType()
      val jvmName = jvmType.name
      val targs = List[JvmType](JvmType.Array(JvmType.String))
      val bytecode = genByteCode(jvmType, targs)
      Map(jvmName -> JvmClass(jvmName, bytecode))
  }



  def genByteCode(jvmType: JvmType.Reference, targs: List[JvmType])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, jvmType.name.toInternalName, null, superClass, null)

    // Source of the class
    visitor.visitSource(jvmType.name.toInternalName, null)

    // Emit the code for the main method
    compileMainMethod(visitor, jvmType, targs)

    visitor.visitEnd()
    visitor.toByteArray
  }

  def compileMainMethod(visitor: ClassWriter, jvmType: JvmType.Reference, targs: List[JvmType])(implicit root: Root, flix: Flix): Unit = {

    val main = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", AsmOps.getMethodDescriptor(targs, JvmType.Void), null, null)

    main.visitCode()
    main.visitTypeInsn(NEW, JvmType.Context.name.toInternalName)
    main.visitInsn(DUP)

    main.visitMethodInsn(INVOKESPECIAL, JvmName.Context.toInternalName,
      "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)


    main.visitMethodInsn(INVOKEVIRTUAL, JvmName.Context.toInternalName, "m_main", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    main.visitInsn(RETURN)
  }

  /**
    * Optionally returns the main definition in the given AST `root`.
    */
  private def getMain(root: Root): Option[Def] = {
    // The main function must be called `main` and occur in the root namespace.
    val sym = Symbol.mkDefnSym("main")

    // Check if the main function exists.
    root.defs.get(sym) flatMap {
      case defn =>
        // The main function must take zero arguments.
//        if (defn.formals.isEmpty) Some(defn) else None TODO: def main() is not generating empty formals. Why?
        println(defn.formals)
      Some(defn)
    }
  }

}
