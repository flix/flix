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
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Def, Root}
import org.objectweb.asm.{ClassWriter, Label}
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
    val namespaceClassType = JvmOps.getNamespaceClassType(ns)

    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class header
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, namespaceClassType.name.toInternalName, null,
      JvmName.Object.toInternalName, null)

    // Adding an IFO field and a shill method for each function in `ns`
    for ((sym, defn) <- ns.defs) {
      // JvmType of `defn`
      val jvmType = JvmOps.getFunctionDefinitionClassType(sym)

      // Name of the field on namespace
      val fieldName = JvmOps.getDefFieldNameInNamespaceClass(sym)

      // Adding the field for functional interface for `tpe`
      AsmOps.compileField(visitor, fieldName, jvmType, isStatic = false, isPrivate = false)

      // TODO: Same problem as GenFunctionClasses, we should handle not defs with non arrow type
      if (defn.tpe.isArrow) {
        // Args of the function
        compileShillMethod(visitor, defn, jvmType, ns)
      }
    }

    // Add the constructor
    compileNamespaceConstructor(visitor, ns)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Adding a shill for the function `defn` on namespace `ns`
    */
  private def compileShillMethod(visitor: ClassWriter,
                                 defn: Def,
                                 ifoType: JvmType.Reference,
                                 ns: NamespaceInfo)(implicit root: Root, flix: Flix): Unit = {
    // Name of the shill
    val name = JvmOps.getDefMethodNameInNamespaceClass(defn.sym)

    // JvmType for namespace
    val namespaceClassType = JvmOps.getNamespaceClassType(ns)

    // Jvm type of method args
    val args = defn.tpe.typeArguments.map(JvmOps.getErasedType)

    // Length of args in local
    val stackSize = args.init.map(AsmOps.getStackSpace).sum

    // Address of continuation
    val contextAddr = stackSize

    // Address of the current IFO
    val ifoAddr = stackSize + 1

    // Method header
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, name, AsmOps.getMethodDescriptor(args.init, args.last), null, null)
    method.visitCode()

    // Creating a context object
    method.visitTypeInsn(NEW, JvmName.Context.toInternalName)
    method.visitInsn(DUP)

    // Calling the constructor of context object
    method.visitMethodInsn(INVOKESPECIAL, JvmName.Context.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Putting another reference of context on top of the stack
    method.visitInsn(DUP)

    // Storing Context on the variable
    method.visitVarInsn(ASTORE, contextAddr)

    // Name of the field for namespace on context object
    val nsFieldName = JvmOps.getNamespaceFieldNameInContextClass(ns)

    // Name of the field for IFO of defn on namespace
    val defnFieldName = JvmOps.getDefFieldNameInNamespaceClass(defn.sym)

    // Extracting the namespace field from the context object
    method.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, nsFieldName, namespaceClassType.toDescriptor)

    // Extracting the ifo from namespace
    method.visitFieldInsn(GETFIELD, namespaceClassType.name.toInternalName, defnFieldName, ifoType.toDescriptor)

    // Strong the IFO on a local variable
    method.visitVarInsn(ASTORE, ifoAddr)

    // Offset for each parameter
    var offset: Int = 0

    // Set arguments for the IFO
    for ((arg, index) <- args.init.zipWithIndex) {
      // Duplicate the IFO reference
      method.visitVarInsn(ALOAD, ifoAddr)

      // Get the argument from the field
      val iLoad = AsmOps.getLoadInstruction(arg)
      method.visitVarInsn(iLoad, offset)

      // Call the setter for the argument
      method.visitMethodInsn(INVOKEVIRTUAL, ifoType.name.toInternalName, s"setArg$index",
        AsmOps.getMethodDescriptor(List(arg), JvmType.Void), false)

      // Incrementing the offset
      offset += AsmOps.getStackSpace(arg)
    }
    // Label for the loop
    val loop = new Label
    // Type of the continuation interface
    val cont = JvmOps.getContinuationInterfaceType(defn.tpe)
    // Type of the function interface
    val functionInterface = JvmOps.getFunctionInterfaceType(defn.tpe)
    // Result type
    val resultType = args.last
    // Put the closure on `continuation` field of `Context`
    method.visitVarInsn(ALOAD, contextAddr)
    method.visitVarInsn(ALOAD, ifoAddr)

    // Casting to JvmType of FunctionInterface
    method.visitTypeInsn(CHECKCAST, functionInterface.name.toInternalName)
    method.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // This is necessary since the loop has to pop a value from the stack!
    method.visitInsn(ACONST_NULL)
    method.visitVarInsn(ASTORE, ifoAddr)

    // Begin of the loop
    method.visitLabel(loop)

    // Getting `continuation` field on `Context`
    method.visitVarInsn(ALOAD, contextAddr)
    method.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Setting `continuation` field of global to `null`
    method.visitVarInsn(ALOAD, contextAddr)
    method.visitInsn(ACONST_NULL)
    method.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Cast to the continuation
    method.visitTypeInsn(CHECKCAST, cont.name.toInternalName)
    // Duplicate
    method.visitInsn(DUP)
    // Storing the continuation on a local variable
    method.visitVarInsn(ASTORE, ifoAddr)

    // Call apply
    method.visitVarInsn(ALOAD, contextAddr)
    method.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "apply", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)

    // Getting `continuation` field on `Context`
    method.visitVarInsn(ALOAD, contextAddr)
    method.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    method.visitJumpInsn(IFNONNULL, loop)

    // Loading the IFO
    method.visitVarInsn(ALOAD, ifoAddr)
    // Invoking the `getResult` method
    method.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "getResult", AsmOps.getMethodDescriptor(Nil, resultType), true)

    // Return
    method.visitInsn(AsmOps.getReturnInsn(args.last))

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(65535, 65535)
    method.visitEnd()
  }

  /**
    * Add the constructor for the class which initializes each field
    */
  private def compileNamespaceConstructor(visitor: ClassWriter, ns: NamespaceInfo)(implicit root: Root, flix: Flix): Unit = {

    // JvmType for `ns`
    val namespaceRef = JvmOps.getNamespaceClassType(ns)

    // Method header
    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Initializing each field
    for ((sym, defn) <- ns.defs) {

      // JvmType for the `sym`
      val jvmType = JvmOps.getFunctionDefinitionClassType(sym)

      // Name of the field on namespace
      val fieldName = JvmOps.getDefFieldNameInNamespaceClass(sym)

      // Instantiating a new instance of the class
      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitTypeInsn(NEW, jvmType.name.toInternalName)
      constructor.visitInsn(DUP)

      // Calling the constructor of `namespace` class
      constructor.visitMethodInsn(INVOKESPECIAL, jvmType.name.toInternalName, "<init>",
        AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

      // Initializing the field
      constructor.visitFieldInsn(PUTFIELD, namespaceRef.name.toInternalName, fieldName, jvmType.toDescriptor)
    }

    // Return
    constructor.visitInsn(RETURN)
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

}
