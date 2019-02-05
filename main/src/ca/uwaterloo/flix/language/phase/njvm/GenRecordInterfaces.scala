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

package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm._
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the tuple interfaces.
  */
object GenRecordInterfaces {

  /**
    * Returns the set of tuple interfaces for the given set of types `ts`.
    */
  def gen(ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, MonoType.RecordEmpty()) =>
        // Case 1: The type constructor is a tuple.
        // Construct tuple interface.
        val jvmType = JvmOps.getRecordInterfaceType()
        val jvmName = jvmType.name
        val targs = List()
        val bytecode = genByteCode(jvmType, targs)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
      case (macc, MonoType.RecordExtend(_,_,_)) =>
        // Case 1: The type constructor is a tuple.
        // Construct tuple interface.
        val jvmType = JvmOps.getRecordInterfaceType()
        val jvmName = jvmType.name
        val targs = List()
        val bytecode = genByteCode(jvmType, targs)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
      case (macc, tpe) =>
        // Case 2: The type constructor is a non-tuple.
        // Nothing to be done. Return the map.
        macc
    }
  }

  /**
    * This method will generate code for a tuple interface.
    * There is a getter and a setter method for each element of `fields` on this interface.
    * After creating a tuple object using a tuple class which corresponds to the same tuple type as this interface,
    * the class type should never be used to reference to that object and this interface should be used for all interactions
    * with that object.
    */
  private def genByteCode(interfaceType: JvmType.Reference, targs: List[JvmType])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // Super descriptor
    val superClass = JvmName.Object.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, interfaceType.name.toInternalName, null, superClass, Array())

    // Source of the class
    visitor.visitSource(interfaceType.name.toInternalName, null)

    val getter = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "getField", AsmOps.getMethodDescriptor(Nil, JvmType.Object), null, null)
    getter.visitEnd()

    visitor.visitEnd()
    visitor.toByteArray
  }

}
