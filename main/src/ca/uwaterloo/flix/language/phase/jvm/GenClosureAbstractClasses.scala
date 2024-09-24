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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object GenClosureAbstractClasses {

  val GetUniqueThreadClosureFunctionName: String = "getUniqueThreadClosure"


  /** Returns the set of function abstract classes for the given set of types `ts`. */
  def gen(ts: Set[MonoType])(implicit flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tpe: MonoType.Arrow) =>
        // Case 1: The type constructor is an arrow type.
        // Construct the functional interface.
        val clazz = genClosureAbstractClass(tpe)
        macc + (clazz.name -> clazz)
      case (macc, _) =>
        // Case 2: The type constructor is a non-arrow.
        // Nothing to be done. Return the map.
        macc
    }
  }

  private def genClosureAbstractClass(tpe: MonoType.Arrow)(implicit flix: Flix): JvmClass = {
    // (Int, String) -> Bool example:
    // public abstract class Clo2$Int$Obj$Bool extends Fn2$Int$Obj$Bool {
    //   public Clo2$Int$Obj$Bool() { ... }
    //   public abstract Clo2$Int$Obj$Bool getUniqueThreadClosure();
    // }

    val classType = JvmOps.getClosureAbstractClassType(tpe)
    val superClass = JvmOps.getFunctionInterfaceType(tpe)
    val cw = AsmOps.mkClassWriter()
    cw.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT, classType.name.toInternalName, null, superClass.name.toInternalName, null)

    genConstructor(cw, superClass)

    cw.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, GetUniqueThreadClosureFunctionName, AsmOps.getMethodDescriptor(Nil, classType), null, null)

    cw.visitEnd()

    // `JvmClass` of the interface
    JvmClass(classType.name, cw.toByteArray)
  }

  private def genConstructor(cw: ClassWriter, superClass: JvmType.Reference): Unit = {
    val m = cw.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)
    m.visitCode()

    m.visitIntInsn(ALOAD, 0)
    m.visitMethodInsn(INVOKESPECIAL, superClass.name.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
    m.visitInsn(RETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }
}
