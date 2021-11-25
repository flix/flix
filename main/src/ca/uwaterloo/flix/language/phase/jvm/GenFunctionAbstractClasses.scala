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
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the function abstract classes.
  */
object GenFunctionAbstractClasses {

  /**
    * Returns the set of function abstract classes for the given set of types `ts`.
    */
  def gen(ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a function abstract class for each type and collect the results in a map.
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
    * Returns the function abstract class of the given type `tpe`.
    */
  private def genFunctionalInterface(tpe: MonoType.Arrow)(implicit root: Root, flix: Flix): JvmClass = {
    // (Int, String) -> Bool example:
    // public abstract class Fn2$Int$Obj$Bool extends Cont$Bool implements java.util.function.Function {
    //   public abstract int arg0;
    //   public abstract Object arg1;
    //   public Fn2$Int$Obj$Bool() { ... }
    // }

    // TODO: this or subclasses do not implement Function::apply?


    // `JvmType` of the continuation class for `tpe`
    val continuationSuperInterface = JvmOps.getContinuationInterfaceType(tpe)

    // `JvmType` of the java.util.functions.Function
    val javaFunctionSuperInterface = JvmType.Function

    // `JvmType` of the functional interface for `tpe`
    val functionType = JvmOps.getFunctionInterfaceType(tpe)

    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // The super interface.
    val superInterfaces = Array(javaFunctionSuperInterface.name.toInternalName)

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT, functionType.name.toInternalName, null,
      continuationSuperInterface.name.toInternalName, superInterfaces)

    // Adding fields for each argument of the function
    for ((arg, index) <- tpe.args.zipWithIndex) {
      // `JvmType` of `arg`
      val argType = JvmOps.getErasedJvmType(arg)

      // arg fields
      visitor.visitField(ACC_PUBLIC + ACC_ABSTRACT, s"arg$index",
        argType.toDescriptor, null, null).visitEnd()
    }

    genConstructor(visitor, continuationSuperInterface)

    visitor.visitEnd()

    // `JvmClass` of the interface
    JvmClass(functionType.name, visitor.toByteArray)
  }

  private def genConstructor(visitor: ClassWriter, superClass: JvmType.Reference): Unit = {
    val m = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    m.visitVarInsn(ALOAD, 0)
    m.visitMethodInsn(INVOKESPECIAL, superClass.name.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
    m.visitInsn(RETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

}
