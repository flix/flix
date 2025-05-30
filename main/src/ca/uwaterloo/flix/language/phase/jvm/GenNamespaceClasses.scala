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
import ca.uwaterloo.flix.language.ast.ReducedAst.{Def, Root}
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.MethodEnricher
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.*

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
    ParOps.parMap(namespaces) {
      case ns =>
        val jvmName = JvmOps.getNamespaceClassType(ns)
        jvmName -> JvmClass(jvmName, genBytecode(jvmName, ns))
    }.toMap
  }

  /**
    * Returns the namespace class for the given namespace `ns`.
    */
  private def genBytecode(jvmName: JvmName, ns: NamespaceInfo)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class header
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, jvmName.toInternalName, null,
      BackendObjType.JavaObject.jvmName.toInternalName, null)

    // Adding an IFO field and a shim method for each function in `ns` with no captured args
    for ((sym, defn) <- ns.defs if root.entryPoints.contains(sym)) {
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
  private def compileShimMethod(visitor: ClassWriter, defn: Def): Unit = {
    // Name of the shim
    val name = JvmOps.getDefMethodNameInNamespaceClass(defn)

    // Erased argument and result type.
    val erasedArgs = defn.fparams.map(_.tpe).map(BackendType.toErasedBackendType)
    val erasedResult = BackendType.toErasedBackendType(defn.unboxedType.tpe)

    // Method header
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, name, MethodDescriptor(erasedArgs, erasedResult).toDescriptor, null, null)
    method.visitCode()

    val functionInterface = JvmOps.getFunctionInterfaceName(defn.arrowType)

    // Offset for each parameter
    var offset: Int = 0

    AsmOps.compileDefSymbol(defn.sym, method)

    // Set arguments for the IFO
    for ((arg, index) <- erasedArgs.zipWithIndex) {
      method.visitInsn(DUP)

      // Get the argument from the field
      method.visitByteIns(BytecodeInstructions.xLoad(arg, offset))

      // put the arg field
      method.visitFieldInsn(PUTFIELD, functionInterface.toInternalName, s"arg$index", arg.toDescriptor)

      // Incrementing the offset
      offset += arg.stackSlots
    }
    method.visitByteIns(BackendObjType.Result.unwindSuspensionFreeThunkToType(BackendType.toErasedBackendType(defn.unboxedType.tpe), s"in shim method of $name", defn.loc))
    // no erasure here because the ns function works on erased values

    // Return
    method.visitByteIns(BytecodeInstructions.xReturn(erasedResult))

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(65535, 65535)
    method.visitEnd()
  }

  /**
    * Add the constructor for the class which initializes each field
    */
  private def compileNamespaceConstructor(visitor: ClassWriter): Unit = {
    // Method header
    val constructor = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    constructor.visitCode()
    constructor.visitByteIns({
      import BytecodeInstructions.*
      ALOAD(0) ~
        INVOKESPECIAL(BackendObjType.JavaObject.Constructor) ~
        RETURN()
    })
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

}
