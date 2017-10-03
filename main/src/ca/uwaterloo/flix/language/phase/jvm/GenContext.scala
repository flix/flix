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

object GenContext {

  // TODO: Documentation and signature
  def gen(defns: Map[Symbol.DefnSym, ExecutableAst.Def])(implicit flix: Flix): Map[JvmName, JvmClass] = {

    // Class visitor
    val visitor = new ClassWriter(ClassWriter.COMPUTE_FRAMES){
      override def getCommonSuperClass(tpe1: String, tpe2: String) : String = {
        JvmType.Obj.name.toInternalName
      }
    }

    // Class header
    visitor.visit(JvmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, JvmType.Context.name.toInternalName, null,
      JvmType.Obj.name.toInternalName, null)

    // Namespaces
    val namespaces = defns.keys.map(_.prefix).toSet

    // Adding continuation field
    JvmOps.compileField(visitor, "continuation", JvmType.Obj.toDescriptor, isStatic = false, isPrivate = false)

    // Adding field for each namespace
    namespaces.foreach{ namespace =>
      val namespaceRef = JvmOps.getNamespaceType(namespace)
      JvmOps.compileField(visitor, namespaceRef.name.name, namespaceRef.toDescriptor, isStatic = false, isPrivate = false)
    }


    // Add the constructor
    compileContextConstructor(visitor, namespaces)

    visitor.visitEnd()
    Map(JvmType.Context.name -> JvmClass(JvmType.Context.name, visitor.toByteArray))
  }

  /**
    * Add the constructor for the class which initializes each field
    */
  private def compileContextConstructor(visitor: ClassWriter, namespaces: Set[List[String]]): Unit = {

    // Method header
    val constructor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "<init>", "()V", null, null)
    constructor.visitCode()

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmType.Obj.name.toInternalName, "<init>",
      "()V", false)

    // Initializing each field
    namespaces.foreach{ namespace =>

      // JvmType of the namespace
      val namespaceRef = JvmOps.getNamespaceType(namespace)

      // Setting the field for `namespace`
      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitTypeInsn(NEW, namespaceRef.name.toInternalName)
      constructor.visitInsn(DUP)

      // Calling the constructor of `namespace` class
      constructor.visitMethodInsn(INVOKESPECIAL, namespaceRef.name.toInternalName, "<init>",
        "()V", false)

      constructor.visitFieldInsn(PUTFIELD, JvmType.Context.name.toInternalName, namespaceRef.name.name, namespaceRef.toDescriptor)
    }

    // Return
    constructor.visitInsn(RETURN)
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

}
