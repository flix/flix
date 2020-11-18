/*
 * Copyright 2020 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor}
import org.objectweb.asm.Opcodes._

/**
 * Generates bytecode for the lazy classes.
 */
object GenLazyClasses {

  /**
   * Returns the set of lazy classes for the given set of types `ts`.
   */
  def gen(ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tpe@MonoType.Lazy(valueType)) =>
        // Case 1: The type constructor is a lazy value.
        // Construct lazy class.
        val fullType = JvmOps.getLazyClassType(tpe)
        val jvmName = fullType.name
        if (!macc.contains(jvmName)) {
          val bytecode = genByteCode(fullType, valueType)
          macc + (jvmName -> JvmClass(jvmName, bytecode))
        } else {
          macc
        }
      case (macc, tpe) =>
        // Case 2: The type constructor is a non-tuple.
        // Nothing to be done. Return the map.
        macc
    }
  }

  /**
   * This method creates the class for each lazy value.
   * The specific lazy class has an associated value type (tpe) which
   * is either a jvm primitive or object.
   *
   * The lazy class has three fields - initialized: bool, expression: () -> tpe,
   * and value: tpe. These are all private. force(context) is the only public
   * method, which retuns a value of type tpe given a context to call the
   * expression closure in.
   *
   * force will only evaluate the expression the first time, based on the flag initialized.
   * After that point it will store the result in value and just return that.
   */
  private def genByteCode(classType: JvmType.Reference, valueType: MonoType)(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null, superClass, null)

    AsmOps.compileField(visitor, "initialized", JvmType.PrimBool, isStatic = false, isPrivate = true)
    AsmOps.compileField(visitor, "expression", JvmType.Object, isStatic = false, isPrivate = true)
    AsmOps.compileField(visitor, "value", JvmOps.getErasedJvmType(valueType), isStatic = false, isPrivate = true)
    compileForceMethod(visitor, classType, valueType)

    // Emit the code for the constructor
    compileLazyConstructor(visitor, classType)

    // Generate `toString` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String),
      "toString method shouldn't be called")

    // Generate `hashCode` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
      "hashCode method shouldn't be called")

    // Generate `equals` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void),
      "equals method shouldn't be called")

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
   * !OBS!: This code was copied from AsmOps to omit the last cast in the function.
   *
   * Emits code to call a closure (not in tail position). fType is the type of the called
   * closure. argsType is the type of its arguments, and resultType is the type of its result.
   */
  private def compileClosureApplication(visitor: MethodVisitor, valueType: MonoType)(implicit root: Root, flix: Flix) = {
    val fType = MonoType.Arrow(List(MonoType.Unit), valueType)
    // Type of the continuation interface
    val cont = JvmOps.getContinuationInterfaceType(fType)
    // Label for the loop
    val loop = new Label
    visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    // Begin of the loop
    visitor.visitLabel(loop)
    // Getting `continuation` field on `Context`
    visitor.visitVarInsn(ALOAD, 1)
    visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    // Setting `continuation` field of global to `null`
    visitor.visitVarInsn(ALOAD, 1)
    visitor.visitInsn(ACONST_NULL)
    visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    // Cast to the continuation
    visitor.visitTypeInsn(CHECKCAST, cont.name.toInternalName)
    // Duplicate
    visitor.visitInsn(DUP)
    // Save it on the IFO local variable
    visitor.visitVarInsn(ASTORE, 2)
    // Call invoke
    visitor.visitVarInsn(ALOAD, 1)
    visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "invoke", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)
    // Getting `continuation` field on `Context`
    visitor.visitVarInsn(ALOAD, 1)
    visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
    visitor.visitJumpInsn(IFNONNULL, loop)
    // Load IFO from local variable and invoke `getResult` on it
    visitor.visitVarInsn(ALOAD, 2)
    visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "getResult", AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(valueType)), true)
  }

  /**
   * The force method takes a context as argument to call the expression closure in.
   * The result of the expression given in the constructor is then returned.
   * This is only actually evaluated the first time, and saved to return directly
   * afterwards.
   *
   * If lazy has associated type of Obj, the returned object needs to be casted
   * to whatever expected type.
   */
  private def compileForceMethod(visitor: ClassWriter, classType: JvmType.Reference, valueType: MonoType)(implicit root: Root, flix: Flix): Unit = {
    /*
    force(context) :=

    lock(this)
    if (!this.initialized) {
      this.value = this.expression()
      this.initialized = true
    }
    tpe result = this.value
    unlock(this)
    return result
     */

    val erasedValueType = JvmOps.getErasedJvmType(valueType)
    val erasedValueTypeDescriptor = erasedValueType.toDescriptor
    val internalClassType = classType.name.toInternalName

    // Header of the method.
    val returnDescription = AsmOps.getMethodDescriptor(List(JvmType.Context), erasedValueType)
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "force", returnDescription, null, null)
    method.visitCode()

    // [this] This is pushed to lock the object.
    method.visitVarInsn(ALOAD, 0)
    // [] The object is now locked.
    method.visitInsn(MONITORENTER)

    // [this] this is pushed to retrieve initialized to check if the Lazy object has already been evaluated.
    method.visitVarInsn(ALOAD, 0)
    // [this.initialized] the condition can now be checked.
    method.visitFieldInsn(GETFIELD, internalClassType, "initialized", JvmType.PrimBool.toDescriptor)

    // [] If expression has been evaluated, skip to returning this.value.
    val skip = new Label
    method.visitJumpInsn(IFNE, skip)

    // [context] Load the context to give as an argument to the expression closure.
    method.visitVarInsn(ALOAD, 1)
    // [context, this] push this to get the expression.
    method.visitVarInsn(ALOAD, 0)
    // [context, expression] now ready to call the expression closure.
    method.visitFieldInsn(GETFIELD, internalClassType, "expression", JvmType.Object.toDescriptor)
    // [value] The result of expression remains on the stack.
    compileClosureApplication(method, valueType)
    // [value, this] this is pushed to assign this.value = value.
    method.visitVarInsn(ALOAD, 0)
    // [this, value] the two stack elements are swapped, technique depending on values type category.
    if (AsmOps.getStackSize(erasedValueType) == 1) {
      method.visitInsn(SWAP)
    } else {
      method.visitInsn(DUP_X2)
      method.visitInsn(POP)
    }
    // [] this.value now stores the result from expression.
    method.visitFieldInsn(PUTFIELD, internalClassType, "value", erasedValueTypeDescriptor)

    // [this] this is pushed to update initialized such that evaluation is skipped the next call.
    method.visitVarInsn(ALOAD, 0)
    // [this, 1] true is pushed to assign this.initialized = true.
    method.visitInsn(ICONST_1)
    // [] initialized is now true.
    method.visitFieldInsn(PUTFIELD, internalClassType, "initialized", JvmType.PrimBool.toDescriptor)

    // This is the return point if initialized was true at the start of force.
    method.visitLabel(skip)
    // [this] this is pushed to retrieve this.value.
    method.visitVarInsn(ALOAD, 0)
    // [this.value] The return value is now on the stack.
    method.visitFieldInsn(GETFIELD, internalClassType, "value", erasedValueTypeDescriptor)

    // [this.value, this] this is pushed to unlock the object.
    method.visitVarInsn(ALOAD, 0)
    // [this.value] we are now ready to return.
    method.visitInsn(MONITOREXIT)

    // [] Return the value of appropriate type.
    method.visitInsn(AsmOps.getReturnInstruction(erasedValueType))

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
   * The constructor takes a expression object, which should be a function that takes
   * no argument and returns something of type tpe, related to the type of the lazy class.
   */
  def compileLazyConstructor(visitor: ClassWriter, classType: JvmType.Reference)(implicit root: Root, flix: Flix): Unit = {
    /*
    Lazy$tpe(expression) :=

    this.initialized = false
    this.expression = expression.
     */
    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void), null, null)

    constructor.visitCode()
    // [this] push this to call the object constructor.
    constructor.visitVarInsn(ALOAD, 0)

    // [] Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // [this] this is pushed to assign initialized to false.
    constructor.visitVarInsn(ALOAD, 0)
    // [this, false] now ready to put field.
    constructor.visitInsn(ICONST_0)
    // [] initialized = false.
    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "initialized", JvmType.PrimBool.toDescriptor)

    // [this] save the argument to expression.
    constructor.visitVarInsn(ALOAD, 0)
    // [this, expression] now ready to put field.
    constructor.visitVarInsn(ALOAD, 1)
    // [] expression has the value of the argument.
    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "expression", JvmType.Object.toDescriptor)

    // [] Return nothing.
    constructor.visitInsn(RETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }
}
