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
import ca.uwaterloo.flix.language.ast.MonoType
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

    val superClass = if (obj.clazz.isInterface()) 
        BackendObjType.JavaObject.jvmName.toInternalName 
      else 
        asm.Type.getInternalName(obj.clazz)

    val interfaces = if (obj.clazz.isInterface()) 
        Array(asm.Type.getInternalName(obj.clazz)) 
      else
        Array[String]()

    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, className.toInternalName, null,
      superClass, interfaces)

    val currentClass = JvmType.Reference(className)
    compileConstructor(currentClass, superClass, obj.methods, visitor)

    obj.methods.zipWithIndex.foreach { case (m, i) => compileMethod(currentClass, m, s"clo$i", visitor) }

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Constructor of the class
    */
  private def compileConstructor(currentClass: JvmType.Reference, superClass: String, methods: List[JvmMethod], visitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = {
    val constructor = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    // Invoke the superclass constructor
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, superClass, JvmName.ConstructorMethod,
      MethodDescriptor.NothingToVoid.toDescriptor, false)

    constructor.visitInsn(RETURN)

    constructor.visitMaxs(999, 999)
    constructor.visitEnd()
  }

  /**
    * Returns a JVM type descriptor for the given `MonoType`
    * 
    * Hacked to half-work for array types. In the new backend we should handle all types, including multidim arrays.
    */
  def getDescriptorHacked(tpe: MonoType)(implicit root: Root, flix: Flix): String = tpe match {
    case MonoType.Array(t) => s"[${JvmOps.getJvmType(t).toDescriptor}"
    case MonoType.Unit => JvmType.Void.toDescriptor
    case _ => JvmOps.getJvmType(tpe).toDescriptor
  }

  /**
    * Returns a JVM method descriptor for the given parameters and return types
    * 
    * Hacked to half-work for array types. In the new backend we should handle all types, including multidim arrays.
    */
  def getMethodDescriptorHacked(paramTypes: List[MonoType], retType: MonoType)(implicit root: Root, flix: Flix): String = {
    val resultDescriptor = getDescriptorHacked(retType)
    val argumentDescriptor = paramTypes.map(getDescriptorHacked).mkString
    s"($argumentDescriptor)$resultDescriptor"
  }

  /**
    * Method
    */
  private def compileMethod(currentClass: JvmType.Reference, method: JvmMethod, cloName: String, classVisitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = method match {
    case JvmMethod(ident, fparams, clo, tpe, loc) =>
      val closureAbstractClass = JvmOps.getClosureAbstractClassType(method.clo.tpe)
      val functionInterface = JvmOps.getFunctionInterfaceType(method.clo.tpe)
      val backendContinuationType = BackendObjType.Continuation(BackendType.toErasedBackendType(method.retTpe))

      // Create the field that will store the closure implementing the body of the method
      AsmOps.compileField(classVisitor, cloName, closureAbstractClass, isStatic = false, isPrivate = false, isVolatile = false)

      // Drop the first formal parameter (which always represents `this`)
      val paramTypes = fparams.tail.map(_.tpe)
      val methodVisitor = classVisitor.visitMethod(ACC_PUBLIC, ident.name, getMethodDescriptorHacked(paramTypes, tpe), null, null)

      // Retrieve the closure that implements this method
      methodVisitor.visitVarInsn(ALOAD, 0)
      methodVisitor.visitFieldInsn(GETFIELD, currentClass.name.toInternalName, cloName, closureAbstractClass.toDescriptor)

      methodVisitor.visitMethodInsn(INVOKEVIRTUAL, closureAbstractClass.name.toInternalName, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName,
        AsmOps.getMethodDescriptor(Nil, closureAbstractClass), false)

      // Push arguments onto the stack
      var offset = 0
      fparams.zipWithIndex.foreach { case (arg, i) => 
        methodVisitor.visitInsn(DUP)
        val argType = JvmOps.getJvmType(arg.tpe)
        methodVisitor.visitVarInsn(AsmOps.getLoadInstruction(argType), offset)
        offset += AsmOps.getStackSize(argType)
        methodVisitor.visitFieldInsn(PUTFIELD, functionInterface.name.toInternalName,
          s"arg$i", JvmOps.getErasedJvmType(arg.tpe).toDescriptor)
      }

      // Invoke the closure
      methodVisitor.visitMethodInsn(INVOKEVIRTUAL, functionInterface.name.toInternalName,
        backendContinuationType.UnwindMethod.name, AsmOps.getMethodDescriptor(Nil, JvmOps.getErasedJvmType(tpe)), false)

      tpe match {
        case MonoType.Array(_) => methodVisitor.visitTypeInsn(CHECKCAST, getDescriptorHacked(tpe))
        case _ => AsmOps.castIfNotPrim(methodVisitor, JvmOps.getJvmType(tpe))
      }

      val returnInstruction = tpe match {
        case MonoType.Unit => RETURN
        case MonoType.Array(_) => ARETURN
        case _ => AsmOps.getReturnInstruction(JvmOps.getJvmType(tpe))
      }
      methodVisitor.visitInsn(returnInstruction)

      methodVisitor.visitMaxs(999, 999)
      methodVisitor.visitEnd()
  }
}
