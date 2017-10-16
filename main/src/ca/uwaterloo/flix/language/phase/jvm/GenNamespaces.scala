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
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._
import ca.uwaterloo.flix.language.ast.Symbol
import org.objectweb.asm.Label

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
      if(defn.tpe.isArrow) {
        // Args of the function
    //    compileShillMethod(visitor, defn, sym, namespaceClassType, jvmType, ns)
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
                                 sym: Symbol.DefnSym,
                                 nsClassType: JvmType.Reference,
                                 ifoType: JvmType.Reference,
                                 ns: NamespaceInfo)(implicit root: Root, flix: Flix): Unit = {
    // Name of the shill
    val name = JvmOps.getDefMethodNameInNamespaceClass(sym)

    // Jvm type of method args
    val args = defn.tpe.typeArguments.map(JvmOps.getErasedType)

    // Length of args in local
    val varLen = args.init.map{
      case JvmType.PrimLong | JvmType.PrimDouble => 2
      case _ => 1
    }.sum


    // Method header
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, name, AsmOps.getMethodDescriptor(args.init, args.last), null, null)
    method.visitCode()

    // Creating a context object
    method.visitTypeInsn(NEW, JvmName.Context.toInternalName)
    method.visitInsn(DUP)

    // Calling the constructor of context object
    method.visitMethodInsn(INVOKESPECIAL, JvmName.Context.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)


    // creating a local variable for context
    val label = new Label
    method.visitLabel(label)
    method.visitLocalVariable("context", JvmType.Context.toDescriptor, null, label, label, varLen)

    // Putting another reference of context on top of the stack
    method.visitInsn(DUP)

    // Storing Context on the variable
    method.visitVarInsn(ASTORE, varLen)

    // Name of the field for namespace on context object
    val nsFieldName = JvmOps.getNamespaceFieldNameInContextClass(ns)

    // Name of the field for IFO of defn on namespace
    val defnFieldName = JvmOps.getDefFieldNameInNamespaceClass(sym)

    // Extracting the namespace field from the context object
    method.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, nsFieldName, nsClassType.toDescriptor)

    // Extracting the ifo from namespace
    method.visitFieldInsn(GETFIELD, nsClassType.name.toInternalName, defnFieldName, ifoType.toDescriptor)

    // Offset for each parameter
    var offset: Int = 0

    // Set arguments for the IFO
    for((arg, index) <- args.init.zipWithIndex) {
      // Duplicate the IFO reference
      method.visitInsn(DUP)

      // Get the argument from the field
      val iLoad = AsmOps.getLoadInstruction(arg)
      method.visitVarInsn(iLoad, offset)

      // Call the setter for the argument
      method.visitMethodInsn(INVOKEVIRTUAL, ifoType.name.toInternalName, s"setArg$index",
        AsmOps.getMethodDescriptor(List(arg), JvmType.Void), false)

      // Incrementing the offset
      arg match {
        case JvmType.PrimLong | JvmType.PrimDouble => offset += 2
        case _ => offset += 1
      }
    }

    // TODO: Replace this with an appropriate while loop, for now we just call the apply
    // Loading context object
    method.visitInsn(DUP)
    method.visitVarInsn(ALOAD, varLen)
    method.visitMethodInsn(INVOKEVIRTUAL, ifoType.name.toInternalName, "apply",
      AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), false)

    // Calling `getResult` method on IFO
    method.visitMethodInsn(INVOKEVIRTUAL, ifoType.name.toInternalName, "getResult",
      AsmOps.getMethodDescriptor(Nil, args.last), false)

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
