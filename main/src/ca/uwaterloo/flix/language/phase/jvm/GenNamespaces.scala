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
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

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
        val jvmType = JvmOps.getNamespaceClassType(ns)
        val jvmName = jvmType.name
        val bytecode = genBytecode(ns)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
    }
  }

  /**
    * Returns the namespace class for the given namespace `ns`.
    */
  private def genBytecode(ns: NamespaceInfo)(implicit root: Root, flix: Flix): Array[Byte] = {
    // JvmType for namespace
    val namespaceRef = JvmOps.getNamespaceClassType(ns)

    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class header
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, namespaceRef.name.toInternalName, null,
      JvmName.Object.toInternalName, null)

    // Adding fields for each function in `ns`
    for ((sym, defn) <- ns.defs) {
      // JvmType of `defn`
      val jvmType = JvmOps.getFunctionDefinitionClassType(sym)

      // Name of the field on namespace
      val fieldName = JvmOps.getNamespaceFieldName(sym)

      // Adding the field for functional interface for `tpe`
      AsmOps.compileField(visitor, fieldName, jvmType.toDescriptor, isStatic = false, isPrivate = false)
    }

    // Add the constructor
    compileNamespaceConstructor(visitor, ns)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Add the constructor for the class which initializes each field
    */
  private def compileNamespaceConstructor(visitor: ClassWriter, ns: NamespaceInfo)(implicit root: Root, flix: Flix): Unit = {

    // JvmType for `ns`
    val namespaceRef = JvmOps.getNamespaceClassType(ns)

    // Method header
    val constructor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "<init>", AsmOps.getMethodDescriptor(Nil), null, null)
    constructor.visitCode()

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil), false)

    // Initializing each field
    for ((sym, defn) <- ns.defs) {

      // JvmType for the `sym`
      val jvmType = JvmOps.getFunctionDefinitionClassType(sym)

      // Name of the field on namespace
      val fieldName = JvmOps.getNamespaceFieldName(sym)

      // Instantiating a new instance of the class
      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitTypeInsn(NEW, jvmType.name.toInternalName)
      constructor.visitInsn(DUP)

      // Calling the constructor of `namespace` class
      constructor.visitMethodInsn(INVOKESPECIAL, jvmType.name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil), false)

      // Initializing the field
      constructor.visitFieldInsn(PUTFIELD, namespaceRef.name.toInternalName, fieldName, jvmType.toDescriptor)
    }

    // Return
    constructor.visitInsn(RETURN)
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

}
