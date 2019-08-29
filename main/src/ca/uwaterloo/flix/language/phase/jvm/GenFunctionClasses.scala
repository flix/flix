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
import ca.uwaterloo.flix.language.ast.{Symbol, MonoType}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor}
import scala.collection.parallel.CollectionConverters._

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
    defs.par.aggregate(Map.empty[JvmName, JvmClass])( {
      case (macc, (sym, defn)) if JvmOps.nonLaw(defn) =>
        // `JvmType` of the interface for `def.tpe`
        val functionInterface = JvmOps.getFunctionInterfaceType(defn.tpe)

        // `JvmType` of the class for `defn`
        val classType = JvmOps.getFunctionDefinitionClassType(sym)

        // Name of the class
        val className = classType.name
        macc + (className -> JvmClass(className, genByteCode(classType, functionInterface, defn)))

      case (macc, (sym, defn)) => macc
    }, _ ++ _)
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
    val MonoType.Arrow(targs, tresult) = defn.tpe

    // The super interface.
    val superInterface = Array(functionInterface.name.toInternalName)

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      JvmName.Object.toInternalName, superInterface)

    // Adding a setter and a field for each argument of the function
    for ((arg, index) <- targs.zipWithIndex) {
      // `JvmType` of `arg`
      val argType = JvmOps.getErasedJvmType(arg)

      // `arg$index` field
      AsmOps.compileField(visitor, s"arg$index", argType, isStatic = false, isPrivate = true)

      // `setArg$index()` method
      AsmOps.compileSetFieldMethod(visitor, classType.name, s"arg$index", s"setArg$index", argType)
    }

    // Jvm type of the result of the function
    val jvmResultType = JvmOps.getErasedJvmType(tresult)

    // Field for the result
    AsmOps.compileField(visitor, "result", jvmResultType, isStatic = false, isPrivate = true)

    // Getter for the result field
    AsmOps.compileGetFieldMethod(visitor, classType.name, "result", "getResult", jvmResultType)

    // Constructor of the class
    compileConstructor(visitor)

    // Invoke method of the class
    compileInvokeMethod(visitor, classType, defn, jvmResultType)

    // Apply method of the class
    compileApplyMethod(visitor, classType, defn, tresult)

    // Eval method of the class
    compileEvalMethod(visitor, classType, defn, tresult)

    visitor.toByteArray
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

  /**
    * Invoke method for the given `defn` and `classType`.
    */
  private def compileInvokeMethod(visitor: ClassWriter,
                                  classType: JvmType.Reference,
                                  defn: Def,
                                  resultType: JvmType)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val invokeMethod = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "invoke",
      AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), null, null)

    // Enter label
    val enterLabel = new Label()
    invokeMethod.visitCode()

    // visiting the label
    invokeMethod.visitLabel(enterLabel)

    // Saving parameters on variable stack
    for ((FormalParam(sym, tpe), ind) <- defn.formals.zipWithIndex) {
      // Erased type of the parameter
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the parameter from the field
      invokeMethod.visitVarInsn(ALOAD, 0)
      invokeMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"arg$ind", erasedType.toDescriptor)

      // Storing the parameter on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      invokeMethod.visitVarInsn(iSTORE, sym.getStackOffset + 3)
    }

    // Generating the expression
    GenExpression.compileExpression(defn.exp, invokeMethod, classType, Map(), enterLabel)

    // Loading `this`
    invokeMethod.visitVarInsn(ALOAD, 0)

    // Swapping `this` and result of the expression
    if (AsmOps.getStackSize(resultType) == 1) {
      invokeMethod.visitInsn(SWAP)
    } else {
      invokeMethod.visitInsn(DUP_X2)
      invokeMethod.visitInsn(POP)
    }

    // Saving the result on the `result` field of IFO
    invokeMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "result", resultType.toDescriptor)

    // Return
    invokeMethod.visitInsn(RETURN)
    invokeMethod.visitMaxs(65535, 65535)
    invokeMethod.visitEnd()
  }

  /**
    * Apply method for the given `defn` and `classType`.
    */
  private def compileApplyMethod(cw: ClassWriter, classType: JvmType.Reference, defn: Def, resultType: MonoType)(implicit root: Root, flix: Flix): Unit = {
    // The JVM result type
    val jvmResultType = JvmOps.getErasedJvmType(resultType)

    // Method header
    val mv = cw.visitMethod(ACC_PUBLIC + ACC_FINAL, "apply", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Object), null, null)

    // Emit code to invoke setArgX for every argument in the array.
    compileArguments(defn, classType, mv)

    // TODO: Temporary instantiate a new context object and call the invoke method.
    // TODO: Need to load it from thread local.

    //
    // Call this.apply(new Context)
    //

    // Put `this` on the stack.
    mv.visitVarInsn(ALOAD, 0)

    // Allocate a fresh context object.
    mv.visitTypeInsn(NEW, JvmName.Context.toInternalName)
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, JvmName.Context.toInternalName, "<init>", "()V", false)

    // Store the context object in local variable 1.
    mv.visitInsn(DUP)
    mv.visitVarInsn(ASTORE, 1)

    // Call the invoke method.
    mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "eval", "(LContext;)Ljava/lang/Object;", false)

    // Construct a proxy object.
    resultType match {
      case arrayType: MonoType.Array => AsmOps.newProxyArray(arrayType, mv)
      case nonArrayType => AsmOps.newProxyObject(resultType, mv)
    }

    // Return the proxy object.
    mv.visitInsn(ARETURN)

    mv.visitMaxs(65535, 65535)
    mv.visitEnd()
  }

  /**
    * Emits code for a functional that fully evaluates the current function, including tail calls.
    */
  private def compileEvalMethod(cw: ClassWriter, classType: JvmType.Reference, defn: Def, resultType: MonoType)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val mv = cw.visitMethod(ACC_PUBLIC + ACC_FINAL, "eval", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Object), null, null)

    // The jvm result type.
    val jvmResultType = JvmOps.getErasedJvmType(resultType)

    // Label for the loop
    val loop = new Label

    // Type of the function
    val fnType = root.defs(defn.sym).tpe

    // Type of the continuation interface
    val cont = JvmOps.getContinuationInterfaceType(fnType)

    // Store this ifo to the continuation field.
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Begin of the loop
    mv.visitLabel(loop)

    // Getting `continuation` field on `Context`
    mv.visitVarInsn(ALOAD, 1)
    mv.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Setting `continuation` field of global to `null`
    mv.visitVarInsn(ALOAD, 1)
    mv.visitInsn(ACONST_NULL)
    mv.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)

    // Cast to the continuation
    mv.visitTypeInsn(CHECKCAST, cont.name.toInternalName)

    // Duplicate
    mv.visitInsn(DUP)

    // Save it on the IFO local variable
    mv.visitVarInsn(ASTORE, 2)

    // Call invoke
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "invoke", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)

    // Getting `continuation` field on `Context`
    mv.visitVarInsn(ALOAD, 1)
    mv.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    mv.visitJumpInsn(IFNONNULL, loop)

    // Load IFO from local variable and invoke `getResult` on it
    mv.visitVarInsn(ALOAD, 2)
    mv.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "getResult", AsmOps.getMethodDescriptor(Nil, jvmResultType), true)
    AsmOps.boxIfPrim(jvmResultType, mv)

    // Return the result.
    mv.visitInsn(ARETURN)

    mv.visitMaxs(65535, 65535)
    mv.visitEnd()
  }

  /**
    * Emits code to invoke setArgX for every value in the passed arguments array.
    */
  private def compileArguments(defn: Def, classType: JvmType.Reference, mv: MethodVisitor)(implicit root: Root, flix: Flix): Unit = {
    // The local variable where the object argument is stored.
    val ArgumentLocalVar = 1

    // The local variable for the array of objects.
    val ArrayLocalVar = 2

    // Load the arguments array.
    mv.visitVarInsn(ALOAD, ArgumentLocalVar)

    // Cast the object to an array of objects.
    mv.visitTypeInsn(CHECKCAST, "[Ljava/lang/Object;")

    // Store the array in a local variable.
    mv.visitVarInsn(ASTORE, ArrayLocalVar)

    // Iterate through each formal argument and invoke `setArg`.
    for ((FormalParam(sym, tpe), index) <- defn.formals.zipWithIndex) {
      // Load the `this` value (to be used for the call below).
      mv.visitVarInsn(ALOAD, 0)

      // Load the array.
      mv.visitVarInsn(ALOAD, ArrayLocalVar)

      // Push the array index.
      mv.visitIntInsn(BIPUSH, index)

      // Load the element at the index.
      mv.visitInsn(AALOAD)

      // Invoke the setArgX method on `this`.
      val argErasedType = JvmOps.getErasedJvmType(tpe)

      // Cast and unbox.
      AsmOps.castIfNotPrimAndUnbox(argErasedType, mv)

      // Invoke setArgX.
      mv.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, s"setArg$index", AsmOps.getMethodDescriptor(List(argErasedType), JvmType.Void), false)
    }
  }

}
