/*
 * Copyright 2017 Magnus Madsen
 * Copyright 2021 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.ParOps
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
    ParOps.parAgg(defs, Map.empty[JvmName, JvmClass])({
      case (macc, (sym, defn)) if defn.cparams.nonEmpty =>
        macc // do nothing, these defns should be Closure classes
      case (macc, (sym, defn)) =>
        flix.subtask(sym.toString, sample = true)

        // `JvmType` of the interface for `def.tpe`
        val functionInterface = JvmOps.getFunctionInterfaceType(defn.tpe)

        // `JvmType` of the class for `defn`
        val classType = JvmOps.getFunctionDefinitionClassType(sym)

        // Name of the class
        val className = classType.name
        macc + (className -> JvmClass(className, genByteCode(classType, functionInterface, defn)))
    }, _ ++ _)
  }

  /**
    * Returns the bytecode for the class for the given `defn`
    */
  private def genByteCode(classType: JvmType.Reference,
                          functionInterface: JvmType.Reference,
                          defn: Def)(implicit root: Root, flix: Flix): Array[Byte] = {
    // example for def checkLength(Int, String): Bool = ...
    // public final class Def$checkLength extends Fn2$Int$Obj$Bool {
    //   public Def$checkLength() { ... }
    //   public final Cont$Bool invoke() { ... }
    // }

    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Args of the function
    val MonoType.Arrow(_, tresult) = defn.tpe

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      functionInterface.name.toInternalName, null)

    // Constructor of the class
    compileConstructor(functionInterface, visitor)

    // Invoke method of the class
    compileInvokeMethod(visitor, classType, defn, tresult)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Constructor of the class
    */
  private def compileConstructor(superClass: JvmType.Reference, visitor: ClassWriter): Unit = {
    val constructor = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, superClass.name.toInternalName, JvmName.ConstructorMethod,
      MethodDescriptor.NothingToVoid.toDescriptor, false)
    constructor.visitInsn(RETURN)

    constructor.visitMaxs(999, 999)
    constructor.visitEnd()
  }

  /**
    * Invoke method for the given `defn` and `classType`.
    */
  private def compileInvokeMethod(visitor: ClassWriter,
                                  classType: JvmType.Reference,
                                  defn: Def,
                                  resultType: MonoType)(implicit root: Root, flix: Flix): Unit = {
    // Continuation class
    val continuationType = JvmOps.getContinuationInterfaceType(defn.tpe)

    // previous JvmOps function are already partial pattern matches
    val MonoType.Arrow(_, closureResultType) = defn.tpe
    val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(closureResultType))

    // Method header
    val m = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, backendContinuationType.InvokeMethod.name,
      AsmOps.getMethodDescriptor(Nil, continuationType), null, null)

    // Enter label
    val enterLabel = new Label()
    m.visitCode()

    // visiting the label
    m.visitLabel(enterLabel)

    // Saving parameters on variable stack
    for ((FormalParam(sym, tpe), ind) <- (defn.cparams ++ defn.fparams).zipWithIndex) {
      // Erased type of the parameter
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the parameter from the field
      m.visitVarInsn(ALOAD, 0)
      m.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"arg$ind", erasedType.toDescriptor)

      // Storing the parameter on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      m.visitVarInsn(iSTORE, sym.getStackOffset + 1)
    }

    // Generating the expression
    val ctx = GenExpression.MethodContext(classType, enterLabel, Map())
    GenExpression.compileStmt(defn.stmt)(m, ctx, root, flix)

    // Loading `this`
    m.visitVarInsn(ALOAD, 0)

    // Swapping `this` and result of the expression
    val resultJvmType = JvmOps.getErasedJvmType(resultType)
    if (AsmOps.getStackSize(resultJvmType) == 1) {
      m.visitInsn(SWAP)
    } else {
      m.visitInsn(DUP_X2)
      m.visitInsn(POP)
    }

    m.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendContinuationType.ResultField.name, resultJvmType.toDescriptor)

    // Return
    m.visitInsn(ACONST_NULL)
    m.visitInsn(ARETURN)
    m.visitMaxs(999, 999)
    m.visitEnd()
  }

}
