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
package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm._
import ca.uwaterloo.flix.language.phase.njvm.Api.Java
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.interfaces.RecordInterface
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import scala.reflect.runtime.universe._

class RecordExtend[T <: MnemonicsTypes : TypeTag](map: Map[JvmName, MnemonicsClass])(implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Setup
  private val ct: Reference = getRecordExtendClassType[T]
  private val cg: ClassGenerator = new ClassGenerator(ct, List(getRecordInterfaceType))

  //Fields each variable represents a field which can be accessed
  //while generating code for this class
  private val field0: Field[Ref[MString]] = cg.mkField("field0")
  private val field1: Field[T] = cg.mkField("field1")
  private val field2: Field[Ref[RecordInterface]] = cg.mkField("field2")

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method

  /**
    * Generate the constructor for the RecordExtend class. This constructor receives three arguments, the field label,
    * value and the rest of the record.
    * Variable contains the capability to call the constructor
    * For example for RecordExtend$Obj(String, Object, Object) creates the following constructor:
    *
    * public RecordExtend$Obj(String var1, Object var2, Object var3) {
    *   this.field0 = var1;
    *   this.field1 = var2;
    *   this.field2 = var3;
    * }
    *
    */
  val defaultConstructor: VoidMethod4[Ref[RecordExtend[T]], Ref[MString], T, Ref[RecordInterface]] = {
    cg.mkConstructor4(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          cg.SUPER |>>
          sig.getArg1.LOAD |>>
          sig.getArg2.LOAD |>>
          field0.PUT_FIELD |>>

          sig.getArg1.LOAD |>>
          sig.getArg3.LOAD |>>
          field1.PUT_FIELD |>>

          sig.getArg1.LOAD |>>
          sig.getArg4.LOAD |>>
          field2.PUT_FIELD |>>
          RETURN_VOID
    )
  }

  /**
    * Gennerate the getField method in the RecordExtend class. The method receives no arguments.
    * It should simply return the record field (value) which is store in the variable field2.
    *
    * We store the capability to call getField in getFieldMethod
    */
  val getFieldMethod: Method1[Ref[RecordExtend[T]], T] = {
    cg.mkMethod1("getField",
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          field1.GET_FIELD |>>
          RETURN
    )
  }


  /**
    * Gennerate the lookupField method in the RecordExtend class. The method receives one argument, the field label.
    * The method should check if the current record label is equal to the provided label. If it is equal it should return this
    * (The record object which has the given label). In case the provided label is not equal we recursively call lookupField on the rest of the record,
    * and return the value provided by the recursive call.
    *
    * We store the capability to call lookupField in lookupFieldMethod
    */
  val lookupFieldMethod: Method2[Ref[RecordExtend[T]], Ref[MString], Ref[RecordInterface]] = {
    val recordInterface: RecordInterface =
      map(getRecordInterfaceType.name).asInstanceOf[RecordInterface]

    cg.mkMethod2("lookupField",
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          field0.GET_FIELD |>>
          sig.getArg2.LOAD |>>
          Java.Lang.String.equals.INVOKE |>>
          IFEQ(
            sig.getArg1.LOAD[StackNil] |>>
              RETURN) |>>
          sig.getArg1.LOAD |>>
          field2.GET_FIELD |>>
          sig.getArg2.LOAD |>>
          recordInterface.lookupFieldMethod.INVOKE |>>
          RETURN
    )
  }

  /**
    * Generate the restrictField method in the RecordExtend class. The method receives one argument, the field label.
    * The method should check if the current record label is equal to the provided label. If it is equal it should return the rest of the record
    * (field2).In case the provided label is not equal we recursively call restrictField on the rest of the record.
    * Then we should set our 'rest' field(field2) to what was returned by the recursive call.
    * Because we might need to update our 'rest' pointer since if the provided label is equal to the next field label,
    * then this field should no longer be in the record. We then return 'this'.
    *
    * We store the capability to call restrictField in restrictFieldMethod
    */
  val restrictFieldMethod: Method2[Ref[RecordExtend[T]], Ref[MString], Ref[RecordInterface]] = {
    val recordInterface: RecordInterface =
      map(getRecordInterfaceType.name).asInstanceOf[RecordInterface]

    cg.mkMethod2("restrictField",
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          field0.GET_FIELD |>>
          sig.getArg2.LOAD |>>
          Java.Lang.String.equals.INVOKE |>>
          IFEQ(
            sig.getArg1.LOAD[StackNil] |>>
              field2.GET_FIELD |>>
              RETURN) |>>
          sig.getArg1.LOAD |>>
          DUP |>>
          field2.GET_FIELD |>>
          sig.getArg2.LOAD |>>
          recordInterface.restrictFieldMethod.INVOKE |>>
          field2.PUT_FIELD |>>
          sig.getArg1.LOAD |>>
          RETURN
    )
  }

  /**
    * Generate the `toString()` method which will always throws an exception, since `toString` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `toString` method is always the following:
    *
    * public string toString() throws Exception {
    * throw new Exception("toString method shouldn't be called");
    * }
    */
  val toStringMethod: Method1[Ref[RecordExtend[T]], Ref[MString]] =
    cg.mkMethod1("toString", _ => toStringNotImplemented)

  /** Generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `hashCode` method is always the following:
    *
    * public int hashCode() throws Exception {
    * throw new Exception("hashCode method shouldn't be called");
    * }
    */
  val hashCodeMethod: Method1[Ref[RecordExtend[T]], MInt] =
    cg.mkMethod1("hashCode", _ => hashCodeNotImplemented)

  /**
    * Generate the `equals(Obj)` method which will always throws an exception, since `equals` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `equals` method is always the following:
    *
    * public boolean equals(Object var1) throws Exception {
    * throw new Exception("equals method shouldn't be called");
    * }
    *
    */
  val equalsMethod: Method2[Ref[RecordExtend[T]], Ref[MObject], MBool] =
    cg.mkMethod2("equal", _ => equalsNotImplemented)

  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass = JvmClass(ct.name, cg.compile())

  // TODO: Miguel: Why do we need a getter here?
  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this
}
