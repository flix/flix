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
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root

/**
  * Generates bytecode for the `Context` class.
  */
object GenContext {

  /**
    * Returns the `Context` class.
    */
  def gen(ns: Set[NamespaceInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, JvmName.Context.toInternalName, null,
      JvmName.Object.toInternalName, null)

    // Adding continuation field
    AsmOps.compileField(visitor, "continuation", JvmType.Object, isStatic = false, isPrivate = false)

    // Adding field for each namespace
    for (namespace <- ns) {
      // JvmType of the namespace
      val namespaceRef = JvmOps.getNamespaceClassType(namespace)

      // Name of the field for the `namespace` on the Context object
      val fieldName = JvmOps.getNamespaceFieldNameInContextClass(namespace)

      // Adding the field
      AsmOps.compileField(visitor, fieldName, namespaceRef, isStatic = false, isPrivate = false)
    }

    // Add the constructor
    compileContextConstructor(visitor, ns)

    visitor.visitEnd()
    Map(JvmType.Context.name -> JvmClass(JvmType.Context.name, visitor.toByteArray))
  }

  /**
    * Add the constructor for the class which initializes each field
    */
  private def compileContextConstructor(visitor: ClassWriter, ns: Set[NamespaceInfo])(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val constructor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "<init>",
      AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    constructor.visitCode()

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Initializing each field
    for (namespace <- ns) {
      // JvmType of the namespace
      val namespaceRef = JvmOps.getNamespaceClassType(namespace)

      // Name of the field for the `namespace` on the Context object
      val fieldName = JvmOps.getNamespaceFieldNameInContextClass(namespace)

      // Setting the field for `namespace`
      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitTypeInsn(NEW, namespaceRef.name.toInternalName)
      constructor.visitInsn(DUP)

      // Calling the constructor of `namespace` class
      constructor.visitMethodInsn(INVOKESPECIAL, namespaceRef.name.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

      constructor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, fieldName, namespaceRef.toDescriptor)
    }

    // Return
    constructor.visitInsn(RETURN)
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

}
