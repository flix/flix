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

    compileConstructor(JvmType.Reference(className), superClass, obj.methods, visitor)

    obj.methods.zipWithIndex.foreach { case (m, i) => compileMethod(m, i, visitor) }

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
      constructor.visitInsn(DUP)
      constructor.visitFieldInsn(PUTFIELD, currentClass.name.toInternalName, s"m$i", JvmOps.getErasedJvmType(m.clo.tpe).toDescriptor)
    }

    constructor.visitInsn(RETURN)

    constructor.visitMaxs(999, 999)
    constructor.visitEnd()
  }

  /**
    * Method
    */
  private def compileMethod(method: JvmMethod, i: Int, classVisitor: ClassWriter)(implicit root: Root, flix: Flix): Unit = method match {
    case JvmMethod(ident, fparams, clo, tpe, loc) =>
      AsmOps.compileField(classVisitor, s"m$i", JvmOps.getClosureAbstractClassType(method.clo.tpe), isStatic = false, isPrivate = false)

      // Drop the first formal parameter (which always represents `this`)
      val paramTypes = fparams.tail.map(f => JvmOps.getJvmType(f.tpe))
      val returnType = JvmOps.getJvmType(tpe)
      val methodVisitor = classVisitor.visitMethod(ACC_PUBLIC, ident.name, AsmOps.getMethodDescriptor(paramTypes, returnType), null, null)

      methodVisitor.visitTypeInsn(NEW, JvmName.UnsupportedOperationException.toInternalName)
      methodVisitor.visitInsn(DUP)
      methodVisitor.visitMethodInsn(INVOKESPECIAL, JvmName.UnsupportedOperationException.toInternalName, "<init>", MethodDescriptor.NothingToVoid.toDescriptor, false)
      methodVisitor.visitInsn(ATHROW)

      methodVisitor.visitMaxs(999, 999)
      methodVisitor.visitEnd()
  }
}
