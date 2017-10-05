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
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import ca.uwaterloo.flix.language.ast.Type
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object GenContinuationInterfaces {

  /**
    * Returns the set of continuation interfaces for the given set of types `ts`.
    */
  def gen(ts: Set[Type])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tpe) if tpe.typeConstructor.isArrow =>
        // Case 1: The type constructor is an arrow.
        // Construct continuation interface.
        val jvmType = JvmOps.getContinuationInterfaceType(tpe)
        val jvmName = jvmType.name
        val resultType = JvmOps.getResultType(tpe)
        val bytecode = genByteCode(jvmType, resultType)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
      case (macc, tpe) =>
        // Case 2: The type constructor is a non-arrow.
        // Nothing to be done. Return the map.
        macc
    }
  }

  /**
    * Returns the bytecode for the given continuation interface.
    */
  private def genByteCode(interfaceType: JvmType.Reference, resultType: JvmType): Array[Byte] = {

    // Pseudo code to generate:
    //
    // interface Cont$Bool {
    //   boolean getResult();
    //   void apply(Context c);
    // }
    //
    // The names `getResult` and `apply` are fixed and can be used as strings.
    //
    // The type Cont$Bool is available as the jvmType argument.
    //
    // From the type, the JvmName can be extract and from this we can get the internal name of Cont$Bool.
    //
    // The result type (here bool) we is provided as an argument. It could be primitive or some compound type.
    // We can use `getErasedType` to map it down into one of the primitive types or to Object.
    //

    // Class visitor
    val visitor = new ClassWriter(ClassWriter.COMPUTE_FRAMES){
      override def getCommonSuperClass(tpe1: String, tpe2: String) : String = {
        JvmType.Obj.name.toInternalName
      }
    }

    // Class header
    visitor.visit(JvmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, interfaceType.name.toInternalName, null,
      JvmType.Obj.name.toInternalName, null)

    // `getResult()` method
    val getResultMethod = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "getResult", s"()${resultType.toDescriptor}", null, null)
    getResultMethod.visitEnd()
    getResultMethod.visitEnd()

    // `apply()` method
    val applyMethod = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "apply", s"(${JvmType.Context.toDescriptor})V", null, null)
    applyMethod.visitEnd()

    visitor.visitEnd()
    visitor.toByteArray
  }

}
