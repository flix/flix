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
import ca.uwaterloo.flix.language.ast.ExecutableAst._
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
    defs.foldLeft(Map.empty[JvmName, JvmClass]) { case (macc, (sym, defn)) =>
      // TODO Magnus: Should we make all the defs a function like before? Not sure what to do when there is non arrow function.
      // TODO We filter laws here.
      if(defn.tpe.isArrow && !defn.ann.isLaw) {

        // `JvmType` of the interface for `def.tpe`
        val functionInterface = JvmOps.getFunctionInterfaceType(defn.tpe)

        // `JvmType` of the class for `defn`
        val classType = JvmOps.getFunctionDefinitionClassType(sym)

        // Name of the class
        val className = classType.name
        macc + (className -> JvmClass(className, genByteCode(classType, functionInterface, defn)))
      } else {
        macc
      }
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
    for((arg, index) <- args.init.zipWithIndex) {
      // `JvmType` of `arg`
      val argType = JvmOps.getErasedType(arg)

      // `arg$index` field
      AsmOps.compileField(visitor, s"arg$index", argType, isStatic = false, isPrivate = true)

      // `setArg$index()` method
      AsmOps.compileSetFieldMethod(visitor, classType.name, argType, s"arg$index", s"setArg$index")
    }

    // Jvm type of the result of the function
    val resultType = JvmOps.getErasedType(args.last)

    // Field for the result
    AsmOps.compileField(visitor, "result", resultType, isStatic = false, isPrivate = true)

    // Getter for the result field
    AsmOps.compileGetFieldMethod(visitor, classType.name, resultType, "result", "getResult")

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
    val applyMethod = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "apply",
      AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), null, null)

    val enterLabel = new Label()
    applyMethod.visitCode()
    applyMethod.visitVarInsn(ALOAD, 0)
    applyMethod.visitLabel(enterLabel)
    GenExpression.compileExpression(defn.exp, classType, Map(), enterLabel, Nil, defn.formals.map(_.sym).toList, applyMethod)
    applyMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName , "result", resultType.toDescriptor)
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
