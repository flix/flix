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
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.language.ast.Symbol
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label}

/**
  * Generates bytecode for the function classes.
  */
object GenFunctionClasses {

  /**
    * Returns the set of function classes for the given set of definitions `defs`.
    */
  def gen(defs: Map[Symbol.DefnSym, Def])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a function class for each def and collect the results in a map.
    //
    defs.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, (sym, defn)) if JvmOps.nonLaw(defn) =>
        // `JvmType` of the interface for `def.tpe`
        val functionInterface = JvmOps.getFunctionInterfaceType(defn.tpe)

        // `JvmType` of the class for `defn`
        val classType = JvmOps.getFunctionDefinitionClassType(sym)

        // Name of the class
        val className = classType.name
        macc + (className -> JvmClass(className, genByteCode(classType, functionInterface, defn)))

      case (macc, (sym, defn)) => macc
    }
  }

  /**
    * Returns the bytecode for the class for the given `defn`
    */
  private def genByteCode(classType: JvmType.Reference,
                          functionInterface: JvmType.Reference,
                          defn: Def)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Args of the function
    val args = defn.tpe.typeArguments

    // The super interface.
    val superInterface = Array(functionInterface.name.toInternalName)

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      JvmName.Object.toInternalName, superInterface)

    // Adding a setter and a field for each argument of the function
    for ((arg, index) <- args.init.zipWithIndex) {
      // `JvmType` of `arg`
      val argType = JvmOps.getErasedJvmType(arg)

      // `arg$index` field
      AsmOps.compileField(visitor, s"arg$index", argType, isStatic = false, isPrivate = true)

      // `setArg$index()` method
      AsmOps.compileSetFieldMethod(visitor, classType.name, s"arg$index", s"setArg$index", argType)
    }

    // Jvm type of the result of the function
    val resultType = JvmOps.getErasedJvmType(args.last)

    // Field for the result
    AsmOps.compileField(visitor, "result", resultType, isStatic = false, isPrivate = true)

    // Getter for the result field
    AsmOps.compileGetFieldMethod(visitor, classType.name, "result", "getResult", resultType)

    // Apply method of the class
    compileApplyMethod(visitor, classType, defn, resultType)

    // Constructor of the class
    compileConstructor(visitor)

    visitor.toByteArray
  }

  /**
    * Apply method for the given `defn` and `classType`.
    */
  private def compileApplyMethod(visitor: ClassWriter,
                                 classType: JvmType.Reference,
                                 defn: Def,
                                 resultType: JvmType)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val applyMethod = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "apply",
      AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), null, null)

    // Enter label
    val enterLabel = new Label()
    applyMethod.visitCode()

    // visiting the label
    applyMethod.visitLabel(enterLabel)

    // Saving parameters on variable stack
    for ((FormalParam(sym, tpe), ind) <- defn.formals.zipWithIndex) {
      // Erased type of the parameter
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the parameter from the field
      applyMethod.visitVarInsn(ALOAD, 0)
      applyMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"arg$ind", erasedType.toDescriptor)

      // Storing the parameter on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      applyMethod.visitVarInsn(iSTORE, sym.getStackOffset + 3)
    }

    // Generating the expression
    GenExpression.compileExpression(defn.exp, applyMethod, classType, Map(), enterLabel)

    // Loading `this`
    applyMethod.visitVarInsn(ALOAD, 0)

    // Swapping `this` and result of the expression
    if (AsmOps.getStackSize(resultType) == 1) {
      applyMethod.visitInsn(SWAP)
    } else {
      applyMethod.visitInsn(DUP_X2)
      applyMethod.visitInsn(POP)
    }

    // Saving the result on the `result` field of IFO
    applyMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "result", resultType.toDescriptor)

    // Return
    applyMethod.visitInsn(RETURN)
    applyMethod.visitMaxs(65535, 65535)
    applyMethod.visitEnd()
  }

  /**
    * Constructor of the class
    */
  private def compileConstructor(visitor: ClassWriter): Unit = {
    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)

    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    constructor.visitInsn(RETURN)
    constructor.visitMaxs(1, 1)
    constructor.visitEnd()
  }

}
