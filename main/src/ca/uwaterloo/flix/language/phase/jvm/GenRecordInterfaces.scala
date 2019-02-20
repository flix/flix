/*
 * Copyright 2019 Miguel Fialho
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
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the record interface.
  */
object GenRecordInterfaces {

  /**
    * Returns a Map with a single entry, for the record interface
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val jvmType = JvmOps.getRecordInterfaceType()
    val jvmName = jvmType.name
    val targs = List()
    val bytecode = genByteCode(jvmType, targs)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  /**
    * This method will generate code for a record interface.
    * There is a getRecordWithField method which returns the Record (Object) with the given label
    * There is also a restrictField which given a label removes said label from the record
    * After creating a record object using a record class,
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

    //Emitting a getRecordWithField method
    val getRecordWithField = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "getRecordWithField",
      AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType), null, null)
    getRecordWithField.visitEnd()

    //Emitting a restrictField method
    val restrictField = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "restrictField",
      AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType), null, null)
    restrictField.visitEnd()


    visitor.visitEnd()
    visitor.toByteArray
  }

}
