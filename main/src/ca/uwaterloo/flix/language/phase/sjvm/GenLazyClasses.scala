/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

/**
  * Generates bytecode for the lazy classes.
  */
object GenLazyClasses {

  /**
    * Returns the set of lazy classes for the given set of types `ts`.
    */
  def gen()(implicit root: Root, flix: Flix): Map[String, JvmClass] = {

    // Generating each lazy class
    def genAUX[T <: PType](tpe: RType[T]): (String, JvmClass) = {
      val eeType = RReference(RLazy(tpe))
      val className = getInternalName(eeType)
      val bytecode = genByteCode(className, tpe)
      className -> JvmClass(className, bytecode)
    }

    //Type that we need a cell class for
    Map() +
      genAUX(RBool()) +
      genAUX(RInt8()) +
      genAUX(RInt16()) +
      genAUX(RInt32()) +
      genAUX(RInt64()) +
      genAUX(RChar()) +
      genAUX(RFloat32()) +
      genAUX(RFloat64()) +
      genAUX(RReference(null))
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
  private def genByteCode[T <: PType](className: String, innerType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, className, null, "java/lang/Object", null)

    AsmOps.compileField(visitor, "initialized", getDescriptor(RBool()), isStatic = false, isPrivate = true)
    AsmOps.compileField(visitor, "expression", getDescriptor(RReference(null)), isStatic = false, isPrivate = true)
    AsmOps.compileField(visitor, "value", s"L$className;", isStatic = false, isPrivate = true)
    compileForceMethod(visitor, className, innerType)

    // Emit the code for the constructor
    compileLazyConstructor(className)


    visitor.visitEnd()
    visitor.toByteArray
  }

  private def compileForceMethod[T <: PType](visitor: ClassWriter, className: String, innerType: RType[T])(implicit root: Root, flix: Flix): Unit = {
    val erasedValueTypeDescriptor = getDescriptor(innerType)

    // Header of the method.
    val returnDescription = s"(LContext;)$erasedValueTypeDescriptor"
    val method = visitor.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, "force", returnDescription, null, null)
    method.visitCode()
    val t = compileForceMethod(className, innerType)
    t(F(method))
    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * !OBS!: This code was copied from AsmOps to omit the last cast in the function.
    *
    * Emits code to call a closure (not in tail position). fType is the type of the called
    * closure. argsType is the type of its arguments, and resultType is the type of its result.
    */
  private def compileClosureApplication[T <: PType](visitor: MethodVisitor, valueType: RType[T])(implicit root: Root, flix: Flix) = ()

  //  {
  //    val fType = MonoType.Arrow(List(MonoType.Unit), valueType)
  //    // Type of the continuation interface
  //    val cont = JvmOps.getContinuationInterfaceType(fType)
  //    // Label for the loop
  //    val loop = new Label
  //    visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
  //    // Begin of the loop
  //    visitor.visitLabel(loop)
  //    // Getting `continuation` field on `Context`
  //    visitor.visitVarInsn(ALOAD, 1)
  //    visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
  //    // Setting `continuation` field of global to `null`
  //    visitor.visitVarInsn(ALOAD, 1)
  //    visitor.visitInsn(ACONST_NULL)
  //    visitor.visitFieldInsn(PUTFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
  //    // Cast to the continuation
  //    visitor.visitTypeInsn(CHECKCAST, cont.name.toInternalName)
  //    // Duplicate
  //    visitor.visitInsn(DUP)
  //    // Save it on the IFO local variable
  //    visitor.visitVarInsn(ASTORE, 2)
  //    // Call invoke
  //    visitor.visitVarInsn(ALOAD, 1)
  //    visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "invoke", AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), true)
  //    // Getting `continuation` field on `Context`
  //    visitor.visitVarInsn(ALOAD, 1)
  //    visitor.visitFieldInsn(GETFIELD, JvmName.Context.toInternalName, "continuation", JvmType.Object.toDescriptor)
  //    visitor.visitJumpInsn(IFNONNULL, loop)
  //    // Load IFO from local variable and invoke `getResult` on it
  //    visitor.visitVarInsn(ALOAD, 2)
  //    visitor.visitMethodInsn(INVOKEINTERFACE, cont.name.toInternalName, "getResult", AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(valueType)), true)
  //  }

  /**
    * The force method takes a context as argument to call the expression closure in.
    * The result of the expression given in the constructor is then returned.
    * This is only actually evaluated the first time, and saved to return directly
    * afterwards.
    *
    * If lazy has associated type of Obj, the returned object needs to be casted
    * to whatever expected type.
    */
  private def compileForceMethod[T <: PType](className: String, valueType: RType[T])(implicit root: Root, flix: Flix): F[StackNil] => F[StackEnd] = {
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


    // [this] This is pushed to lock the object.
    //      f.visitor.visitVarInsn(ALOAD, 0)

    //      // [] The object is now locked.
    //      method.visitInsn(MONITORENTER)

    //      // [this] this is pushed to retrieve initialized to check if the Lazy object has already been evaluated.
    //      method.visitVarInsn(ALOAD, 0)

    //      // [this.initialized] the condition can now be checked.
    //      method.visitFieldInsn(GETFIELD, internalClassType, "initialized", JvmType.PrimBool.toDescriptor)

    //
    //      // [] If expression has been evaluated, skip to returning this.value.
    //      val skip = new Label
    //      method.visitJumpInsn(IFNE, skip)
    //
    //      // [context] Load the context to give as an argument to the expression closure.
    //      method.visitVarInsn(ALOAD, 1)
    //      // [context, this] push this to get the expression.
    //      method.visitVarInsn(ALOAD, 0)
    //      // [context, expression] now ready to call the expression closure.
    //      method.visitFieldInsn(GETFIELD, internalClassType, "expression", JvmType.Object.toDescriptor)
    //      // [value] The result of expression remains on the stack.
    //      compileClosureApplication(method, valueType)
    //      // [value, this] this is pushed to assign this.value = value.
    //      method.visitVarInsn(ALOAD, 0)
    //      // [this, value] the two stack elements are swapped, technique depending on values type category.
    //      if (AsmOps.getStackSize(erasedValueType) == 1) {
    //        method.visitInsn(SWAP)
    //      } else {
    //        method.visitInsn(DUP_X2)
    //        method.visitInsn(POP)
    //      }
    //      // [] this.value now stores the result from expression.
    //      method.visitFieldInsn(PUTFIELD, internalClassType, "value", erasedValueTypeDescriptor)
    //
    //      // [this] this is pushed to update initialized such that evaluation is skipped the next call.
    //      method.visitVarInsn(ALOAD, 0)
    //      // [this, 1] true is pushed to assign this.initialized = true.
    //      method.visitInsn(ICONST_1)
    //      // [] initialized is now true.
    //      method.visitFieldInsn(PUTFIELD, internalClassType, "initialized", JvmType.PrimBool.toDescriptor)
    //
    //      // This is the return point if initialized was true at the start of force.
    //      method.visitLabel(skip)
    //      // [this] this is pushed to retrieve this.value.
    //      method.visitVarInsn(ALOAD, 0)
    //      // [this.value] The return value is now on the stack.
    //      method.visitFieldInsn(GETFIELD, internalClassType, "value", erasedValueTypeDescriptor)
    //
    //      // [this.value, this] this is pushed to unlock the object.
    //      method.visitVarInsn(ALOAD, 0)
    //      // [this.value] we are now ready to return.
    //      method.visitInsn(MONITOREXIT)
    //
    //      // [] Return the value of appropriate type.
    //      method.visitInsn(AsmOps.getReturnInstruction(erasedValueType))
    f => f.asInstanceOf[F[StackEnd]]
  }

  /**
    * The constructor takes a expression object, which should be a function that takes
    * no argument and returns something of type tpe, related to the type of the lazy class.
    */
  def compileLazyConstructor(classType: String)(implicit root: Root, flix: Flix): Unit = ()

  //  {
  //    /*
  //    Lazy$tpe(expression) :=
  //
  //    this.initialized = false
  //    this.expression = expression.
  //     */
  //    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void), null, null)
  //
  //    constructor.visitCode()
  //    // [this] push this to call the object constructor.
  //    constructor.visitVarInsn(ALOAD, 0)
  //
  //    // [] Call the super (java.lang.Object) constructor
  //    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
  //
  //    // [this] this is pushed to assign initialized to false.
  //    constructor.visitVarInsn(ALOAD, 0)
  //    // [this, false] now ready to put field.
  //    constructor.visitInsn(ICONST_0)
  //    // [] initialized = false.
  //    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "initialized", JvmType.PrimBool.toDescriptor)
  //
  //    // [this] save the argument to expression.
  //    constructor.visitVarInsn(ALOAD, 0)
  //    // [this, expression] now ready to put field.
  //    constructor.visitVarInsn(ALOAD, 1)
  //    // [] expression has the value of the argument.
  //    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "expression", JvmType.Object.toDescriptor)
  //
  //    // [] Return nothing.
  //    constructor.visitInsn(RETURN)
  //
  //    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
  //    constructor.visitMaxs(65535, 65535)
  //    constructor.visitEnd()
  //  }
}
