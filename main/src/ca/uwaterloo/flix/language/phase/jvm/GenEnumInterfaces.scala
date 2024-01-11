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
import ca.uwaterloo.flix.language.ast.ReducedAst.Enum
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the Enum interfaces.
  */
object GenEnumInterfaces {

  /**
    * Returns the set of enum interfaces for the given enums.
    */
  def gen(enums: Iterable[Enum])(implicit flix: Flix): Map[JvmName, JvmClass] = {
    enums.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, e) =>
        // Construct enum interface.
        val jvmType = JvmOps.getEnumInterfaceType(e.sym)
        val jvmName = jvmType.name
        val bytecode = genByteCode(jvmType)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
    }
  }

  /**
    * Generates an interface for the given type.
    */
  private def genByteCode(interfaceType: JvmType.Reference)(implicit flix: Flix): Array[Byte] = {
    // Create a new class writer.
    val visitor = AsmOps.mkClassWriter()

    // The super class of the generated interface.
    val superClass = BackendObjType.JavaObject.jvmName.toInternalName

    // The interface header.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, interfaceType.name.toInternalName, null, superClass, Array())

    // The source of the generated interface.
    visitor.visitSource(interfaceType.name.toInternalName, null)

    // Complete the visitor and get the bytecode.
    visitor.visitEnd()
    visitor.toByteArray
  }
}
