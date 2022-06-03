/*
 * Copyright 2017 Magnus Madsen
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
  def gen(arrows: Iterable[BackendObjType.Arrow])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    arrows.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, arrow) =>
        val clazz = genFunctionalInterface(arrow)
        macc + (clazz.name -> clazz)
    }
  }

  /**
    * Returns the function abstract class of the given type `arrow`.
    */
  private def genFunctionalInterface(arrow: BackendObjType.Arrow)(implicit root: Root, flix: Flix): JvmClass = {
    // (Int, String) -> Bool example:
    // public abstract class Fn2$Int$Obj$Bool extends Cont$Bool implements java.util.function.Function {
    //   public abstract int arg0;
    //   public abstract Object arg1;
    //   public Fn2$Int$Obj$Bool() { ... }
    // }

    // TODO: this or subclasses do not implement Function::apply?


    // `JvmType` of the continuation class for `tpe`
    val continuationSuperInterface = arrow.continuation

    // `JvmType` of the java.util.functions.Function
    val javaFunctionSuperInterface = JvmType.Function

    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // The super interface.
    val superInterfaces = Array(javaFunctionSuperInterface.name.toInternalName)

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT, arrow.jvmName.toInternalName, null,
      continuationSuperInterface.jvmName.toInternalName, superInterfaces)

    // Adding fields for each argument of the function
    for ((arg, index) <- arrow.args.zipWithIndex) {
      // arg fields
      visitor.visitField(ACC_PUBLIC + ACC_ABSTRACT, s"arg$index",
        arg.toDescriptor, null, null).visitEnd()
    }

    genConstructor(visitor, continuationSuperInterface)

    visitor.visitEnd()

    // `JvmClass` of the interface
    JvmClass(arrow.jvmName, visitor.toByteArray)
  }

  private def genConstructor(visitor: ClassWriter, superClass: BackendObjType): Unit = {
    val m = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    m.visitVarInsn(ALOAD, 0)
    m.visitMethodInsn(INVOKESPECIAL, superClass.jvmName.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
    m.visitInsn(RETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

}
