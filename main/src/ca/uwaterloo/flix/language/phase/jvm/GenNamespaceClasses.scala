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
import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the namespace classes.
  */
object GenNamespaceClasses {

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
    val namespaceClassType = JvmOps.getNamespaceClassType(ns)

    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class header
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, namespaceClassType.name.toInternalName, null,
      BackendObjType.JavaObject.jvmName.toInternalName, null)

    // Adding an IFO field and a shim method for each function in `ns` with no captured args
    for ((_, defn) <- ns.defs if !defn.isClo) {
      // Compile the shim method.
      compileShimMethod(visitor, defn)
    }

    // Add the constructor
    compileNamespaceConstructor(visitor)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Adding a shim for the function `defn` on namespace `ns`
    */
  private def compileShimMethod(visitor: ClassWriter, defn: Def)(implicit root: Root, flix: Flix): Unit = {
    // TODO: This can probably be removed (used in GenMain and other places)
    // Name of the shim
    val name = JvmOps.getDefMethodNameInNamespaceClass(defn.sym)

    // Jvm type of method args
    val MonoType.Arrow(targs, tresult) = defn.tpe
    val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(tresult))

    // Erased argument and result type.
    val erasedArgs = targs map JvmOps.getErasedJvmType
    val erasedResult = JvmOps.getErasedJvmType(tresult)

    // Method header
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, name, AsmOps.getMethodDescriptor(erasedArgs, erasedResult), null, null)
    method.visitCode()

    val functionType = JvmOps.getFunctionInterfaceType(defn.tpe)

    // Offset for each parameter
    var offset: Int = 0

    AsmOps.compileDefSymbol(defn.sym, method)

    // Set arguments for the IFO
    for ((arg, index) <- erasedArgs.zipWithIndex) {
      method.visitInsn(DUP)

      // Get the argument from the field
      val iLoad = AsmOps.getLoadInstruction(arg)
      method.visitVarInsn(iLoad, offset)

      // put the arg field
      method.visitFieldInsn(PUTFIELD, functionType.name.toInternalName, s"arg$index", arg.toDescriptor)

      // Incrementing the offset
      offset += AsmOps.getStackSize(arg)
    }
    method.visitMethodInsn(INVOKEVIRTUAL, functionType.name.toInternalName, backendContinuationType.UnwindMethod.name, AsmOps.getMethodDescriptor(Nil, erasedResult), false)
    // no erasure here because the ns function works on erased values

    // Return
    method.visitInsn(AsmOps.getReturnInstruction(erasedResult))

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(65535, 65535)
    method.visitEnd()
  }

  /**
    * Add the constructor for the class which initializes each field
    */
  private def compileNamespaceConstructor(visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, BackendObjType.JavaObject.jvmName.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    constructor.visitInsn(RETURN)

    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

}
