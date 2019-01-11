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
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the continuation interfaces.
  */
object GenContinuationInterfaces {

  /**
    * Returns the set of continuation interfaces for the given set of types `ts`.
    */
  def gen(ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tpe@MonoType.Arrow(targs, tresult)) =>
        // Case 1: The type constructor is an arrow.
        // Construct continuation interface.
        val jvmType = JvmOps.getContinuationInterfaceType(tpe)
        val jvmName = jvmType.name
        val resultType = JvmOps.getErasedJvmType(tresult)
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
  private def genByteCode(interfaceType: JvmType.Reference, resultType: JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {

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
    val visitor = AsmOps.mkClassWriter()

    // Class header
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE,
      interfaceType.name.toInternalName, null, JvmName.Object.toInternalName, null)

    // `getResult()` method
    val getResultMethod = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "getResult",
      AsmOps.getMethodDescriptor(Nil, resultType), null, null)
    getResultMethod.visitEnd()

    // `invoke()` method
    val invokeMethod = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "invoke",
      AsmOps.getMethodDescriptor(List(JvmType.Context), JvmType.Void), null, null)
    invokeMethod.visitEnd()

    visitor.visitEnd()
    visitor.toByteArray
  }

}
