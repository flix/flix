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
import ca.uwaterloo.flix.language.ast.ReducedAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label}

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
          val bytecode = genByteCode(fullType, JvmOps.getErasedJvmType(valueType), valueType)
          macc + (jvmName -> JvmClass(jvmName, bytecode))
        } else {
          macc
        }
      case (macc, _) =>
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
    * The lazy class has three fields - expression: () -> tpe, value: tpe,
    * and lock. The first two are public. force(context) is a public
    * method, which returns a value of type tpe given a context to call the
    * expression closure in. It will set expression = null, which can be checked in
    * order to check the validity of value.
    *
    * force will only evaluate the expression the first time, based on expression == null.
    * After that point it will store the result in value and just return that. Since force
    * claims a lock (which is expensive) this check should be done inline and not through
    * force, unless expression != null.
    * Note that expression is volatile to ensure that this check is correctly synchronized.
    */
  private def genByteCode(classType: JvmType.Reference, erasedType: JvmType, valueType: MonoType)(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = BackendObjType.JavaObject.jvmName.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null, superClass, null)

    AsmOps.compileField(visitor, "expression", JvmType.Object, isStatic = false, isPrivate = false, isVolatile = true)
    AsmOps.compileField(visitor, "value", erasedType, isStatic = false, isPrivate = false, isVolatile = false)
    AsmOps.compileField(visitor, "lock", JvmType.Reference(JvmName.ReentrantLock), isStatic = false, isPrivate = true, isVolatile = false)
    compileForceMethod(visitor, classType, erasedType, valueType)

    // Emit the code for the constructor
    compileLazyConstructor(visitor, classType)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * The force method takes a context as argument to call the expression closure in.
    * The result of the expression given in the constructor is then returned.
    * This is only actually evaluated the first time, and saved to return directly
    * afterwards.
    *
    * If lazy has associated type of Obj, the returned object needs to be casted
    * to whatever expected type.
    *
    * The generated code is of the form (assuming that the valueType is String)
    *
    * public String force() {
    *   lock.lockInterruptibly();
    *   try {
    *     if (expression != null) {
    *       value = expression();
    *       expression = null;
    *     }
    *     return value;
    *   } finally {
    *     lock.unlock();
    *   }
    * }
    */
  private def compileForceMethod(visitor: ClassWriter, classType: JvmType.Reference, erasedType: JvmType, valueType: MonoType)(implicit root: Root, flix: Flix): Unit = {
    val erasedValueTypeDescriptor = erasedType.toDescriptor
    val internalClassType = classType.name.toInternalName
    val returnIns = AsmOps.getReturnInstruction(erasedType)
    val functionType = JvmType.Reference(BackendObjType.Thunk.jvmName)

    // Header of the method.
    val returnDescription = AsmOps.getMethodDescriptor(Nil, erasedType)
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "force", returnDescription, null, null)
    method.visitCode()

    // lock.lockInterruptibly()
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, internalClassType, "lock", JvmName.ReentrantLock.toDescriptor)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.ReentrantLock.toInternalName, "lockInterruptibly", JvmName.MethodDescriptor.NothingToVoid.toDescriptor, false)

    // Try block
    val beforeTryBlock = new Label()
    val afterTryBlock = new Label()
    val finallyBlock = new Label()
    method.visitTryCatchBlock(beforeTryBlock, afterTryBlock, finallyBlock, null)

    method.visitLabel(beforeTryBlock)
    // [this] this is pushed to retrieve initialized to check if the Lazy object has already been evaluated.
    method.visitVarInsn(ALOAD, 0)
    // [this.initialized] the condition can now be checked.
    method.visitFieldInsn(GETFIELD, internalClassType, "expression", JvmType.Object.toDescriptor)

    // [] if expression is null (multiple threads tried to initialize) return value field, else continue
    val continue = new Label
    method.visitJumpInsn(IFNULL, continue)

    // [this] to assign the expression value
    method.visitVarInsn(ALOAD, 0)
    // [this, this] push this to get the expression.
    method.visitVarInsn(ALOAD, 0)
    // [this, expression] now ready to call the expression closure.
    method.visitFieldInsn(GETFIELD, internalClassType, "expression", JvmType.Object.toDescriptor)
    method.visitTypeInsn(CHECKCAST, functionType.name.toInternalName)
    // [this, value] the result of expression remains on the stack.
    BackendObjType.Result.unwindThunkToType(BackendType.toErasedBackendType(valueType))(new BytecodeInstructions.F(method))
    // [] this.value now stores the result from expression.
    method.visitFieldInsn(PUTFIELD, internalClassType, "value", erasedValueTypeDescriptor)

    // [this] this is pushed to update expression such that evaluation is skipped the next call.
    method.visitVarInsn(ALOAD, 0)
    // [this, null] null is pushed to assign this.expression = null.
    method.visitInsn(ACONST_NULL)
    // [] expression is now null.
    method.visitFieldInsn(PUTFIELD, internalClassType, "expression", JvmType.Object.toDescriptor)

    method.visitLabel(continue)
    // [this] this is pushed to retrieve this.value.
    method.visitVarInsn(ALOAD, 0)
    // [this.value] the return value is now on the stack.
    method.visitFieldInsn(GETFIELD, internalClassType, "value", erasedValueTypeDescriptor)

    // Finally block for non-exception case
    method.visitLabel(afterTryBlock)

    // lock.unlock()
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, internalClassType, "lock", JvmName.ReentrantLock.toDescriptor)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.ReentrantLock.toInternalName, "unlock", JvmName.MethodDescriptor.NothingToVoid.toDescriptor, false)

    // [] Return the value of appropriate type.
    method.visitInsn(returnIns)

    // Finally block for exception case
    method.visitLabel(finallyBlock)

    // lock.unlock()
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, internalClassType, "lock", JvmName.ReentrantLock.toDescriptor)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.ReentrantLock.toInternalName, "unlock", JvmName.MethodDescriptor.NothingToVoid.toDescriptor, false)

    // Rethrow exception
    method.visitInsn(ATHROW)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * The constructor takes a expression object, which should be a function that takes
    * no argument and returns something of type tpe, related to the type of the lazy class.
    */
  def compileLazyConstructor(visitor: ClassWriter, classType: JvmType.Reference)(implicit root: Root, flix: Flix): Unit = {
    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void), null, null)

    constructor.visitCode()
    // [this] push this to call the object constructor.
    constructor.visitVarInsn(ALOAD, 0)

    // [] Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, BackendObjType.JavaObject.jvmName.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // [this] save the argument to expression.
    constructor.visitVarInsn(ALOAD, 0)
    // [this, expression] now ready to put field.
    constructor.visitVarInsn(ALOAD, 1)
    // [] expression has the value of the argument.
    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "expression", JvmType.Object.toDescriptor)

    // lock = new ReentrantLock()
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitTypeInsn(NEW, JvmName.ReentrantLock.toInternalName)
    constructor.visitInsn(DUP)
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.ReentrantLock.toInternalName, "<init>",
      JvmName.MethodDescriptor.NothingToVoid.toDescriptor, false)
    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "lock", JvmName.ReentrantLock.toDescriptor)

    // [] Return nothing.
    constructor.visitInsn(RETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }
}
