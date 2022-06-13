/*
 * Copyright 2019 Miguel Fialho
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
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType.RecordEmpty
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

/**
  * Generates bytecode for the empty record class.
  */
object GenRecordEmptyClass {
  /**
    * Returns a Map with a single entry, for the empty record class.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    Map(RecordEmpty.jvmName -> JvmClass(RecordEmpty.jvmName, genByteCode()))
  }

  /**
    * This method creates the class RecordEmpty that implements IRecord$.
    * It follows the singleton pattern.
    *
    * public static final RecordEmpty INSTANCE = new RecordEmpty();
    *
    * private RecordEmpty() { }
    *
    * public final IRecord$ lookupField(String var1) {
    * throw new UnsupportedOperationException("lookupField method shouldn't be called");
    * }
    *
    * public final IRecord$ restrictField(String var1) {
    * throw new UnsupportedOperationException("restrictField method shouldn't be called");
    * }
    */
  private def genByteCode()(implicit root: Root, flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(RecordEmpty.jvmName, IsFinal,
      interfaces = List(RecordEmpty.interface.jvmName))

    cm.mkStaticConstructor(genStaticConstructor())
    cm.mkObjectConstructor(IsPrivate)
    cm.mkField(RecordEmpty.InstanceField)
    RecordEmpty.LookupFieldMethod.mkMethod(cm, genLookupFieldMethod(), IsPublic, IsFinal)
    RecordEmpty.RestrictFieldMethod.mkMethod(cm, genRestrictFieldMethod(), IsPublic, IsFinal)

    cm.closeClassMaker()
  }

  private def genStaticConstructor(): InstructionSet =
    NEW(RecordEmpty.jvmName) ~
      DUP() ~
      invokeConstructor(RecordEmpty.jvmName, MethodDescriptor.NothingToVoid) ~
      PUTSTATIC(RecordEmpty.InstanceField) ~
      RETURN()

  private def genLookupFieldMethod(): InstructionSet =
    throwUnsupportedOperationException(
      s"${BackendObjType.Record.LookupFieldMethod.name} method shouldn't be called")

  private def genRestrictFieldMethod(): InstructionSet =
    throwUnsupportedOperationException(
      s"${BackendObjType.Record.RestrictFieldMethod.name} method shouldn't be called")

  private def throwUnsupportedOperationException(msg: String): InstructionSet =
    NEW(JvmName.UnsupportedOperationException) ~
      DUP() ~
      pushString(msg) ~
      INVOKESPECIAL(JvmName.UnsupportedOperationException, JvmName.ConstructorMethod,
        mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void)) ~
      ATHROW()
}
