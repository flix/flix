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
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.Branch.{FalseBranch, TrueBranch}
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{Final, Static, Visibility}
import ca.uwaterloo.flix.language.phase.jvm.GenRecordInterface.{LookupFieldFunctionName, RestrictFieldFunctionName}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

object GenRecordExtendClasses {

  val LabelFieldName: String = "label"
  val ValueFieldName: String = "value"
  val RestFieldName: String = "rest"

  /**
    * Returns a Map with an extended record class entry for each element in `ts`.
    */
  def gen(ts: Iterable[BackendObjType.RecordExtend])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, extendType) =>
        macc + (extendType.jvmName -> JvmClass(extendType.jvmName, genByteCode(extendType)))
    }
  }

  /**
    * This method creates a `RecordExtend$V` class with some value type `V`.
    * This implements the record interface.
    *
    * We generate the following fields in the class:
    *
    * private String label;
    * private V value;
    * private IRecord rest;
    *
    * The constructor just calls the object constructor so fields have to be set directly from external code.
    *
    * public RecordExtend$V() {}
    *
    * We generate the lookupField method. The method receives one argument, the field label.
    * The method should check if the current record label is equal to the provided label. If it is equal it should return this
    * (The record object which has the given label). In case the provided label is not equal we recursively call lookupField on the rest of the record,
    * and return the value provided by the recursive call.
    *
    * We generate the restrictField method. The method receives one argument, the field label.
    * The method should check if the current record label is equal to the provided label. If it is equal it should return the rest of the record
    * (field2).In case the provided label is not equal we recursively call restrictField on the rest of the record.
    * Then we should set our 'rest' field(field2) to what was returned by the recursive call.
    * Because we might need to update our 'rest' pointer since if the provided label is equal to the next field label,
    * then this field should no longer be in the record. We then return 'this'.
    */
  private def genByteCode(extendType: BackendObjType.RecordExtend)(implicit root: Root, flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(extendType.jvmName, IsFinal, interfaces = List(extendType.interface))

    cm.mkField(LabelFieldName, BackendObjType.String.toTpe, IsPublic, NotFinal)
    cm.mkField(ValueFieldName, extendType.value, IsPublic, NotFinal)
    cm.mkField(RestFieldName, extendType.interface.toObjTpe.toTpe, IsPublic, NotFinal)

    cm.mkObjectConstructor(IsPublic)

    val stringToRecordInterface = mkDescriptor(BackendObjType.String.toTpe)(extendType.interface.toObjTpe.toTpe)
    cm.mkMethod(genLookupFieldMethod(extendType), LookupFieldFunctionName, stringToRecordInterface, IsPublic, IsFinal)
    cm.mkMethod(genRestrictFieldMethod(extendType), RestrictFieldFunctionName, stringToRecordInterface, IsPublic, IsFinal)

    cm.closeClassMaker
  }

  /**
    * Compares the label of `this`and `ALOAD(1)` and executes the designated branch.
    */
  private def caseOnLabelEquality(extendType: BackendObjType.RecordExtend)(cases: Branch => InstructionSet): InstructionSet =
    loadThis() ~
      GETFIELD(extendType.jvmName, LabelFieldName, BackendObjType.String.toTpe) ~
      ALOAD(1) ~
      INVOKEVIRTUAL(BackendObjType.String.jvmName, "equals", mkDescriptor(JvmName.Object.toObjTpe.toTpe)(BackendType.Bool)) ~
      matchBool(cases)

  private def genLookupFieldMethod(extendType: BackendObjType.RecordExtend)(implicit root: Root, flix: Flix): InstructionSet =
    caseOnLabelEquality(extendType) {
      case TrueBranch =>
        loadThis() ~ ARETURN()
      case FalseBranch =>
        loadThis() ~
          GETFIELD(extendType.jvmName, RestFieldName, extendType.interface.toObjTpe.toTpe) ~
          ALOAD(1) ~
          INVOKEINTERFACE(extendType.interface, LookupFieldFunctionName, mkDescriptor(BackendObjType.String.toTpe)(extendType.interface.toObjTpe.toTpe)) ~
          ARETURN()
    }

  private def genRestrictFieldMethod(extendType: BackendObjType.RecordExtend)(implicit root: Root, flix: Flix): InstructionSet =
    caseOnLabelEquality(extendType) {
      case TrueBranch =>
        loadThis() ~
          GETFIELD(extendType.jvmName, RestFieldName, extendType.interface.toObjTpe.toTpe) ~
          ARETURN()
      case FalseBranch =>
        loadThis() ~
          DUP() ~
          GETFIELD(extendType.jvmName, RestFieldName, extendType.interface.toObjTpe.toTpe) ~
          ALOAD(1) ~
          INVOKEINTERFACE(extendType.interface, RestrictFieldFunctionName, mkDescriptor(BackendObjType.String.toTpe)(extendType.interface.toObjTpe.toTpe)) ~
          PUTFIELD(extendType.jvmName, RestFieldName, extendType.interface.toObjTpe.toTpe) ~
          loadThis() ~
          ARETURN()
    }
}
