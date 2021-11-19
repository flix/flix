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
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.InstructionSet
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{Finality, Instancing, Visibility}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

/**
  * Generates bytecode for the empty record class.
  */
object GenRecordEmpty {

  /**
    * Returns a Map with a single entry, for the empty record class
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    Map(BackendObjType.RecordEmpty.jvmName -> JvmClass(BackendObjType.RecordEmpty.jvmName, genByteCode()))
  }

  /**
    * This method creates the class for RecordEmpty.
    *
    * We then define the interfaces which this class implements (RecordEmpty).
    *
    * First, we will generate the `lookupField(String)` method which will always throws an exception,
    * since `lookupField` should not be called.
    *
    * public final IRecord lookupField(String var1) {
    * throw new UnsupportedOperationException("lookupField method shouldn't be called");
    * }
    *
    * Afterwards, we will generate the `restrictField(String)` method which will always throws an exception, since `restrictField` should not be called.
    *
    * public final IRecord restrictField(String var1) {
    * throw new UnsupportedOperationException("restrictField method shouldn't be called");
    * }
    */
  private def genByteCode()(implicit root: Root, flix: Flix): Array[Byte] = {
    val recordInterface = BackendObjType.RecordEmpty.interface
    val cm = ClassMaker.mkClass(BackendObjType.RecordEmpty.jvmName, Visibility.Public, Finality.Final, interfaces = List(recordInterface))

    cm.mkObjectConstructor(Visibility.Public)
    cm.mkMethod(genLookupFieldMethod(), GenRecordInterfaces.LookupFieldFunctionName, mkDescriptor(BackendObjType.String.toTpe)(recordInterface.toObjTpe.toTpe), Visibility.Public, Finality.Final, Instancing.NonStatic)
    cm.mkMethod(genRestrictFieldMethod(), GenRecordInterfaces.RestrictFieldFunctionName, mkDescriptor(BackendObjType.String.toTpe)(recordInterface.toObjTpe.toTpe), Visibility.Public, Finality.Final, Instancing.NonStatic)

    //    // Generate 'lookupField' method
    //    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "lookupField",
    //      AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType),
    //      "lookupField method shouldn't be called")
    //
    //    // Generate 'restrictField' method
    //    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "restrictField",
    //      AsmOps.getMethodDescriptor(List(JvmType.String), JvmOps.getRecordInterfaceType()),
    //      "restrictField method shouldn't be called")

    cm.closeClassMaker
  }

  private def genLookupFieldMethod(): InstructionSet = ???

  private def genRestrictFieldMethod(): InstructionSet = ???
}
