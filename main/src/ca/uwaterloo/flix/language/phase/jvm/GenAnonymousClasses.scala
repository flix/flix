/*
 * Copyright 2022 Paul Butcher
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
import ca.uwaterloo.flix.language.phase.jvm.JvmName.{MethodDescriptor, RootPackage}
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label}

/**
  * Generates bytecode for anonymous classes (created through NewObject)
  */
object GenAnonymousClasses {

  /**
    * Returns the set of anonymous classes for the given set of objects
    */
  def gen(objs: Set[Expression.NewObject])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate an anonymous class for each object and collect the results in a map.
    //
    ParOps.parAgg(objs, Map.empty[JvmName, JvmClass])({
      case (macc, obj) =>
        val className = JvmName(RootPackage, obj.name)
        flix.subtask(className.toInternalName, sample = true)

        macc + (className -> JvmClass(className, genByteCode(className, obj)))
    }, _ ++ _)
  }

  /**
    * Returns the bytecode for the anonoymous class
    */
  private def genByteCode(className: JvmName, obj: Expression.NewObject)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = AsmOps.mkClassWriter()

    val superClass = BackendObjType.JavaObject.jvmName
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, className.toInternalName, null,
      superClass.toInternalName, Array(asm.Type.getInternalName(obj.clazz)))

    val currentClass = JvmType.Reference(className)
    compileConstructor(currentClass, superClass, obj.methods, visitor)

    obj.methods.zipWithIndex.foreach { case (m, i) => compileMethod(currentClass, m, i, visitor) }

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Constructor of the class
    */
  private def compileConstructor(currentClass: JvmType.Reference, superClass: JvmName, methods: List[JvmMethod], visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val constructor = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, superClass.toInternalName, JvmName.ConstructorMethod,
      MethodDescriptor.NothingToVoid.toDescriptor, false)

    methods.zipWithIndex.foreach { case (m, i) => 
      GenExpression.compileExpression(m.clo, constructor, currentClass, Map(), new Label())

      // Loading `this`
      constructor.visitVarInsn(ALOAD, 0)

      // Swapping `this` and result of the expression
      val resultJvmType = JvmOps.getErasedJvmType(m.retTpe)
      if (AsmOps.getStackSize(resultJvmType) == 1) {
        constructor.visitInsn(SWAP)
      } else {
        constructor.visitInsn(DUP_X2)
        constructor.visitInsn(POP)
      }

      constructor.visitFieldInsn(PUTFIELD, currentClass.name.toInternalName, s"m$i", JvmOps.getClosureAbstractClassType(m.clo.tpe).toDescriptor)
    }

    constructor.visitInsn(RETURN)

    constructor.visitMaxs(999, 999)
    constructor.visitEnd()
  }

  /**
    * Method
    */
  private def compileMethod(currentClass: JvmType.Reference, method: JvmMethod, i: Int, classVisitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = method match {
    case JvmMethod(ident, fparams, clo, tpe, loc) =>
      val closureAbstractClass = JvmOps.getClosureAbstractClassType(method.clo.tpe)
      val functionInterface = JvmOps.getFunctionInterfaceType(method.clo.tpe)
      val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(method.retTpe))

      AsmOps.compileField(classVisitor, s"m$i", closureAbstractClass, isStatic = false, isPrivate = false)

      // Drop the first formal parameter (which always represents `this`)
      val paramTypes = fparams.tail.map(f => JvmOps.getJvmType(f.tpe))
      val returnType = JvmOps.getJvmType(tpe)
      val methodVisitor = classVisitor.visitMethod(ACC_PUBLIC, ident.name, AsmOps.getMethodDescriptor(paramTypes, returnType), null, null)

      methodVisitor.visitVarInsn(ALOAD, 0)
      methodVisitor.visitFieldInsn(GETFIELD, currentClass.name.toInternalName, s"m$i", closureAbstractClass.toDescriptor)

      methodVisitor.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.name.toInternalName, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName, AsmOps.getMethodDescriptor(Nil, closureAbstractClass), false)

      fparams.zipWithIndex.foreach { case (arg, i) => 
        methodVisitor.visitInsn(DUP)
        methodVisitor.visitVarInsn(ALOAD, i)
        methodVisitor.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }
      methodVisitor.visitMethodInsn(INVOKEVIRTUAL, functionInterface.name.toInternalName,
        backendContinuationType.UnwindMethod.name, AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe)), false)
      AsmOps.castIfNotPrim(methodVisitor, JvmOps.getJvmType(tpe))

      methodVisitor.visitInsn(AsmOps.getReturnInstruction(JvmOps.getJvmType(method.retTpe)))

      methodVisitor.visitMaxs(999, 999)
      methodVisitor.visitEnd()
  }
}
