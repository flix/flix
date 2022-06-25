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
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the main class.
  */
object GenMainClass {

  /**
    * Returns the main class if `root` has an entrypoint and `root.defs` has a
    * corresponding method.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = getMain(root) match {
    case None => Map.empty
    case Some(defn) =>
      checkMainType(defn)

      val jvmType = JvmOps.getMainClassType()
      val jvmName = jvmType.name
      val bytecode = genByteCode(defn.sym, jvmType)

      Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  /**
    * Throws `InternalCompilerException` if the type  of `defn` is not `Unit -> Unit`.
    */
  private def checkMainType(defn: Def): Unit = defn.tpe match {
    case MonoType.Arrow(List(MonoType.Unit), MonoType.Unit) => ()
    case other => throw InternalCompilerException(s"Entrypoint function should have type Unit -> Unit not '$other'")
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

  private def genByteCode(sym: Symbol.DefnSym, jvmType: JvmType.Reference)(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = BackendObjType.JavaObject.jvmName.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL,
      jvmType.name.toInternalName, null, superClass, null)

    // Source of the class
    visitor.visitSource(jvmType.name.toInternalName, null)

    // Emit the code for the main method
    compileMainMethod(sym, visitor)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Emits code for the main method in the main class. The emitted (byte)code
    * should satisfy the following signature for the method:
    * `public static void main(String[])`
    *
    * The method itself needs simply invoke the m_entrypoint method which is in
    * the root namespace.
    *
    * The emitted code for the method should correspond to:
    *
    * `public static void main(String[] args) = {`
    *
    * `dev.flix.runtime.Global.setArgs(args);`
    *
    * `Ns.m_entrypoint(Unit.INSTANCE);`
    *
    * `}`
    */
  private def compileMainMethod(sym: Symbol.DefnSym, visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    // The required java main signature `Array[String] -> Void`.
    val javaMainDescriptor = s"(${AsmOps.getArrayType(JvmType.String)})${JvmType.Void.toDescriptor}"
    // `public static void main(String[] args)`.
    val main = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, "main",
      javaMainDescriptor, null, null)

    main.visitCode()

    // Push the args array on the stack.
    main.visitVarInsn(ALOAD, 0)

    // Save the args in `dev.flix.runtime.Global.setArgs(..)`.
    val setArgsDescriptor = s"(${AsmOps.getArrayType(JvmType.String)})${JvmType.Void.toDescriptor}"
    main.visitMethodInsn(INVOKESTATIC, BackendObjType.Global.jvmName.toInternalName,
      BackendObjType.Global.SetArgsMethod.name, setArgsDescriptor, false)

    // Push `Unit` on the stack.
    main.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName,
      BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)

    // Call the `Ns.m_entrypoint` method.
    val nsClassName = JvmOps.getNamespaceClassType(JvmOps.getNamespace(sym)).name.toInternalName
    val mainMethodName = JvmOps.getDefMethodNameInNamespaceClass(sym)
    val erasedUnit = JvmOps.getErasedJvmType(MonoType.Unit)
    val mainDescriptor = AsmOps.getMethodDescriptor(List(erasedUnit), erasedUnit)
    main.visitMethodInsn(INVOKESTATIC, nsClassName, mainMethodName, mainDescriptor, false)
    // The return value is ignored.

    main.visitInsn(RETURN)
    main.visitMaxs(1, 1)
    main.visitEnd()
  }

}
