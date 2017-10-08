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
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Type}
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._
import ca.uwaterloo.flix.language.ast.Symbol

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

  def genNameSpace(prefix: List[String], defns: List[ExecutableAst.Def])(implicit flix: Flix): JvmClass = {

    // JvmType for namespace
    val namespace = JvmOps.getNamespaceType(prefix)

    // Class visitor
    val visitor = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
      override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
        JvmType.Obj.name.toInternalName
      }
    }

    // Class header
    visitor.visit(JvmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, namespace.name.toInternalName, null,
      JvmType.Obj.name.toInternalName, null)

    // Adding fields for functional interfaces
    defns.foreach{ defn =>

      // JvmType of the `tpe` //TODO: wrong type, needs to be the type of the class, not the interface
      val jvmType = JvmOps.getFunctionType(defn.tpe)

      // Adding the field for functional interface for `tpe`
      JvmOps.compileField(visitor, jvmType.name.name, jvmType.toDescriptor, isStatic = false, isPrivate = false)
    }

    // Add the constructor
    compileNamespaceConstructor(visitor, namespace, defns)

    visitor.visitEnd()
    JvmClass(namespace.name, visitor.toByteArray)
  }

  /**
    * Add the constructor for the class which initializes each field
    */
  private def compileNamespaceConstructor(visitor: ClassWriter, reference: JvmType.Reference, defns: List[ExecutableAst.Def]): Unit = {

    // Method header
    val constructor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "<init>", "()V", null, null)
    constructor.visitCode()

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmType.Obj.name.toInternalName, "<init>",
      "()V", false)

    // Initializing each field
    defns.foreach{ defn =>

      // JvmType of the `tpe`
      val jvmType = JvmOps.getFunctionType(defn.tpe)

      // TODO: wrong type, needs to be the type of the class, not the interface
      // Setting the field for `tpe`
      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitTypeInsn(NEW, jvmType.name.toInternalName)
      constructor.visitInsn(DUP)
      // TODO: construction call

      constructor.visitFieldInsn(PUTFIELD, reference.name.toInternalName, jvmType.name.name, jvmType.toDescriptor)
    }

    // Return
    constructor.visitInsn(RETURN)
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

  /**
    * Returns the namespace class for the given namespace `ns`.
    */
  private def genBytecode(ns: NamespaceInfo)(implicit root: Root, flix: Flix): Array[Byte] = {
    List(0xCA.toByte, 0xFE.toByte, 0xBA.toByte, 0xBE.toByte).toArray
  }

}
