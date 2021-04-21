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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmTarget}
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

class ClassMaker(visitor: ClassWriter) {
  def makeField(fieldName: String, fieldDescriptor: String, isStatic: Boolean, isPublic: Boolean): Unit = {
    val visibility = if (isPublic) ACC_PUBLIC else ACC_PRIVATE
    val access = if (isStatic) ACC_STATIC else 0
    val field = visitor.visitField(visibility + access, fieldName, fieldDescriptor, null, null)
    field.visitEnd()
  }

  def makeConstructor(f: F[StackNil] => F[StackEnd], descriptor: String): Unit =
    makeMethod(f, constructorMethod, descriptor, isFinal = false, isPublic = true)

  def makeMethod(f: F[StackNil] => F[StackEnd], methodName: String, descriptor: String, isFinal: Boolean, isPublic: Boolean): Unit = {
    val visibility = if (isPublic) ACC_PUBLIC else ACC_PRIVATE
    val finality = if (isFinal) ACC_FINAL else 0
    val methodVisitor = visitor.visitMethod(visibility + finality, methodName, descriptor, null, null)
    methodVisitor.visitCode()
    f(F(methodVisitor))
    methodVisitor.visitMaxs(1, 1)
    methodVisitor.visitEnd()
  }

  def closeClassMaker: Array[Byte] = {
    visitor.visitEnd()
    visitor.toByteArray
  }
}

object ClassMaker {

  /**
    * Returns the target JVM version.
    */
  private def JavaVersion(implicit flix: Flix): Int = flix.options.target match {
    case JvmTarget.Version16 => V1_6
    case JvmTarget.Version17 => V1_7
    case JvmTarget.Version18 => V1_8
    case JvmTarget.Version19 => throw InternalCompilerException(s"Unsupported Java version: '1.9'.")
  }

  /**
    * Returns a freshly created class writer object.
    *
    * The object is constructed to compute stack map frames automatically.
    */
  private def makeClassWriter(): ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
    override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
      "java/lang/Object"
    }
  }

  def openClassWriter(className: String, isFinal: Boolean)(implicit flix: Flix): ClassMaker = {
    val visibility = ACC_PUBLIC
    val finality = if (isFinal) ACC_FINAL else 0
    val visitor = makeClassWriter()
    visitor.visit(JavaVersion, visibility + finality, className, null, objectName, null)
    new ClassMaker(visitor)
  }

}
