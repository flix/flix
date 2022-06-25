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
import ca.uwaterloo.flix.language.ast.ErasedAst.Root

/**
  * Generates bytecode for the record interface.
  */
object GenRecordInterface {
  /**
    * Returns a Map with a single entry, for the record interface
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    Map(BackendObjType.Record.jvmName -> JvmClass(BackendObjType.Record.jvmName, genByteCode()))
  }

  /**
    * This method will generate code for a record interface.
    * There is a lookupField method which returns the Record (Object) with the given label
    * There is also a restrictField which given a label removes said label from the record.
    * After creating a record object using a record class,
    * the class type should never be used to reference to that object and this interface should be used for all interactions
    * with that object.
    */
  private def genByteCode()(implicit root: Root, flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkInterface(BackendObjType.Record.jvmName)

    cm.mkInterfaceMethod(BackendObjType.Record.LookupFieldMethod)
    cm.mkInterfaceMethod(BackendObjType.Record.RestrictFieldMethod)

    cm.closeClassMaker()
  }
}
