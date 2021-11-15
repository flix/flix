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
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the function interfaces.
  */
object GenFunctionInterfaces {

  /**
    * Returns the set of function interfaces for the given set of types `ts`.
    */
  def gen(ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a function interface for each type and collect the results in a map.
    //
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tpe: MonoType.Arrow) =>
        // Case 1: The type constructor is an arrow type.
        // Construct the functional interface.
        val clazz = genFunctionalInterface(tpe)
        macc + (clazz.name -> clazz)
      case (macc, _) =>
        // Case 2: The type constructor is a non-arrow.
        // Nothing to be done. Return the map.
        macc
    }
  }

  /**
    * Returns the function interface of the given type `tpe`.
    */
  private def genFunctionalInterface(tpe: MonoType)(implicit root: Root, flix: Flix): JvmClass = tpe match {
    case MonoType.Arrow(targs, _) =>
      // `JvmType` of the continuation interface for `tpe`
      val continuationSuperInterface = JvmOps.getContinuationInterfaceType(tpe)

      // `JvmType` of the java.util.functions.Function
      val javaFunctionSuperInterface = JvmType.Function

      // `JvmType` of the functional interface for `tpe`
      val functionType = JvmOps.getFunctionInterfaceType(tpe)

      // Class visitor
      val visitor = AsmOps.mkClassWriter()

      // The super interface.
      val superInterfaces = Array(continuationSuperInterface.name.toInternalName, javaFunctionSuperInterface.name.toInternalName)

      // Class visitor
      visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, functionType.name.toInternalName, null,
        JvmName.Object.toInternalName, superInterfaces)

      // Adding setters for each argument of the function
      for ((arg, index) <- targs.zipWithIndex) {
        // `JvmType` of `arg`
        val argType = JvmOps.getErasedJvmType(arg)

        // `setArg$ind()` method
        val setArgMethod = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, s"setArg$index",
          AsmOps.getMethodDescriptor(List(argType), JvmType.Void), null, null)
        setArgMethod.visitEnd()
      }

      visitor.visitEnd()

      // `JvmClass` of the interface
      JvmClass(functionType.name, visitor.toByteArray)

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

}
