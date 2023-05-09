/*
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
import ca.uwaterloo.flix.language.ast.ErasedAst.{Def, FormalParam, Root}
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label}

/**
  * Generates byte code for the closure classes.
  */
object GenClosureClasses {

  /**
    * Returns the set of closures classes for the given set of definitions `defs`.
    */
  def gen(closures: Set[ClosureInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a closure class for each closure and collect the results in a map.
    //
    ParOps.parAgg(closures, Map.empty[JvmName, JvmClass])({
      case (macc, closure) =>
        val jvmType = JvmOps.getClosureClassType(closure.sym)
        val jvmName = jvmType.name
        val bytecode = genByteCode(closure)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
    }, _ ++ _)
  }

  /**
    * Returns the byte code for the closure.
    *
    * For example, given the symbol `mkAdder` with type (Int32, Int32) -> Int32 and the free variable `x`, we create:
    *
    * public final class Clo$mkAdder implements Clo2$Int32$Int32$Int32 {
    * public int clo0;
    * public int arg0; // from Fn2$...
    * public int arg1; // from Fn2$...
    * public int result; // from Cont$...
    *
    * public Clo$mkAdder() { }
    *
    * public Clo2$Int32$Int32$Int32 getUniqueThreadClosure() {
    *   Clo$mkAdder res = new Clo$mkAdder();
    *   res.clo0 = this.clo0;
    *   return res;
    *
    * public Cont$Int32 invoke() {
    *   this.res = this.x + this.arg0;
    *   return null;
    * }
    */
  private def genByteCode(closure: ClosureInfo)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Args of the function
    val MonoType.Arrow(_, tresult) = closure.tpe

    // `JvmType` of the interface for `closure.tpe`
    val functionInterface = JvmOps.getClosureAbstractClassType(closure.tpe)

    // `JvmType` of the class for `defn`
    val classType = JvmOps.getClosureClassType(closure.sym)

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      functionInterface.name.toInternalName, null)

    // Generate a field for each captured variable.
    for ((argType, index) <- closure.closureArgTypes.zipWithIndex) {
      // `JvmType` of `arg`
      val erasedArgType = JvmOps.getErasedJvmType(argType)

      // `clo$index` field
      AsmOps.compileField(visitor, s"clo$index", erasedArgType, isStatic = false, isPrivate = false, isVolatile = false)
    }

    val defn = root.defs(closure.sym)

    // Invoke method of the class
    compileInvokeMethod(visitor, classType, defn, closure.closureArgTypes, tresult)

    // getUniqueThreadClosure method of the class
    compileGetUniqueThreadClosureMethod(visitor, classType, defn, closure.closureArgTypes)

    // Constructor of the class
    compileConstructor(visitor, functionInterface)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Invoke method for the given `defn`, `classType`, and `resultType`.
    */
  private def compileInvokeMethod(visitor: ClassWriter, classType: JvmType.Reference, defn: Def,
                                  closureArgTypes: List[MonoType], resultType: MonoType)(implicit root: Root, flix: Flix): Unit = {
    // Continuation class
    val continuationType = JvmOps.getContinuationInterfaceType(defn.tpe)
    val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(resultType))

    // Method header
    val invokeMethod = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, backendContinuationType.InvokeMethod.name,
      AsmOps.getMethodDescriptor(Nil, continuationType), null, null)
    invokeMethod.visitCode()

    // Free variables
    val closureFormals = defn.formals.take(closureArgTypes.length)

    // Function parameters
    val params = defn.formals.takeRight(defn.formals.length - closureArgTypes.length)

    // Enter label
    val enterLabel = new Label()

    // Saving closure args variables on variable stack
    for ((f, ind) <- closureFormals.zipWithIndex) {
      // Erased type of the closure variable
      val erasedType = JvmOps.getErasedJvmType(f.tpe)

      // Getting the closure variable from IFO
      invokeMethod.visitVarInsn(ALOAD, 0)
      invokeMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"clo$ind", erasedType.toDescriptor)

      // Saving the closure variable on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      invokeMethod.visitVarInsn(iSTORE, f.sym.getStackOffset + 1)
    }

    // Saving parameters on variable stack
    for ((FormalParam(sym, tpe), ind) <- params.zipWithIndex) {
      // Erased type of the parameter
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the parameter from IFO
      invokeMethod.visitVarInsn(ALOAD, 0)
      invokeMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"arg$ind", erasedType.toDescriptor)

      // Saving the parameter on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      invokeMethod.visitVarInsn(iSTORE, sym.getStackOffset + 1)
    }

    // Generating the expression
    GenExpression.compileStmt(defn.stmt, invokeMethod, classType, Map(), enterLabel)

    // Loading `this`
    invokeMethod.visitVarInsn(ALOAD, 0)

    // Swapping `this` and result of the expression
    val resultJvmType = JvmOps.getErasedJvmType(resultType)
    if (AsmOps.getStackSize(resultJvmType) == 1) {
      invokeMethod.visitInsn(SWAP)
    } else {
      invokeMethod.visitInsn(DUP_X2)
      invokeMethod.visitInsn(POP)
    }

    // Saving the result on the `result` field of IFO
    invokeMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, backendContinuationType.ResultField.name, resultJvmType.toDescriptor)

    // Return
    invokeMethod.visitInsn(ACONST_NULL)
    invokeMethod.visitInsn(ARETURN)
    invokeMethod.visitMaxs(999, 999)
    invokeMethod.visitEnd()
  }

  private def compileGetUniqueThreadClosureMethod(visitor: ClassWriter, classType: JvmType.Reference, defn: Def,
                                                  closureArgTypes: List[MonoType])(implicit root: Root, flix: Flix): Unit = {

    val closureAbstractClass = JvmOps.getClosureAbstractClassType(defn.tpe)

    val m = visitor.visitMethod(ACC_PUBLIC, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName, AsmOps.getMethodDescriptor(Nil, closureAbstractClass), null, null)

    // Create the new clo object
    m.visitTypeInsn(NEW, classType.name.toInternalName)
    m.visitInsn(DUP)
    m.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)

    // transfer the closure arguments
    for ((argType, i) <- closureArgTypes.zipWithIndex) {
      m.visitInsn(DUP)
      val fieldDescriptor = JvmOps.getErasedJvmType(argType).toDescriptor
      m.visitIntInsn(ALOAD, 0)
      m.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"clo$i", fieldDescriptor)
      m.visitFieldInsn(PUTFIELD, classType.name.toInternalName, s"clo$i", fieldDescriptor)
    }

    m.visitInsn(ARETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  /**
    * Constructor of the class
    */
  private def compileConstructor(visitor: ClassWriter, superClass: JvmType.Reference)(implicit root: Root, flix: Flix): Unit = {
    // Constructor header
    val constructor = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    // Calling constructor of super
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, superClass.name.toInternalName, JvmName.ConstructorMethod,
      MethodDescriptor.NothingToVoid.toDescriptor, false)
    constructor.visitInsn(RETURN)

    constructor.visitMaxs(999, 999)
    constructor.visitEnd()
  }
}
