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
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.ClassWriter
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
        val lambdaType = MonoType.Arrow(List(MonoType.Unit), valueType)
        val bytecode = genByteCode(fullType, lambdaType, JvmOps.getErasedJvmType(valueType))
        macc + (jvmName -> JvmClass(jvmName, bytecode))
      case (macc, tpe) =>
        // Case 2: The type constructor is a non-tuple.
        // Nothing to be done. Return the map.
        macc
    }
  }

  /**
   * TODO: make documentation
   */
  private def genByteCode(classType: JvmType.Reference, lambdaType: MonoType, valueType: JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null, superClass, null)

    // Source of the class TODO: what is the point of this?
    visitor.visitSource(classType.name.toInternalName, null)

    // Should always be JvmType.Object because its () -> targetType
    val erasedLambdaType = JvmOps.getErasedJvmType(lambdaType)

    AsmOps.compileField(visitor, "initialized", JvmType.PrimBool, isStatic = false, isPrivate = true)
    AsmOps.compileField(visitor, "expression", erasedLambdaType, isStatic = false, isPrivate = true)
    AsmOps.compileField(visitor, "value", valueType, isStatic = false, isPrivate = true)
    compileGetValueMethod(visitor, classType, valueType)

    // Emit the code for the constructor
    compileLazyConstructor(visitor, classType, valueType, erasedLambdaType)


    // TODO: Should these be supported?
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
   * TODO: make documentation
   */
  private def compileGetValueMethod(visitor: ClassWriter, classType: JvmType.Reference, valueType: JvmType)(implicit root: Root, flix: Flix): Unit = {
    // header of the method
    val returnDescription = AsmOps.getMethodDescriptor(List(), valueType)
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getValue", returnDescription, null, null)

    method.visitCode()

    /*
    if (!initialized) {
      this.value = expression()
      this.initialized = true
    }
    return this.value
     */

    // TODO: conditionally run value = expression()

    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, classType.name.toInternalName, "value", valueType.toDescriptor)
    method.visitInsn(AsmOps.getReturnInstruction(valueType))

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
   * TODO: make documentation
   */
  def compileLazyConstructor(visitor: ClassWriter, classType: JvmType.Reference, valueType: JvmType, erasedLambdaType: JvmType)(implicit root: Root, flix: Flix): Unit = {

    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(erasedLambdaType), JvmType.Void), null, null)

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    /*
    Lazy$<valueType>(exp: () -> T) =
      // this.value uninitialized
      this.initialized = false
      this.expression = exp
      return
     */

    // this.uninitialized = false
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitInsn(ICONST_0)
    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "initialized", JvmType.PrimBool.toDescriptor)

    // this.expression = expression
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitVarInsn(ALOAD, 1)
    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "expression", JvmType.Object.toDescriptor)

    // Return
    constructor.visitInsn(RETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

}
