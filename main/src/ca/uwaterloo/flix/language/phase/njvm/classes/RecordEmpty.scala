
/* * Copyright 2019 Miguel Fialho
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
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.interfaces.RecordInterface
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._

class RecordEmpty(map: Map[JvmName, MnemonicsClass])(implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Setup
  private val ct: Reference = getRecordEmptyClassType
  private val cg: ClassGenerator = new ClassGenerator(ct, List(getJvmType[Ref[RecordInterface]].asInstanceOf[Reference]))

  //Fields
  //Class with no fields

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method

  /**
    * Generates the constructor for the RecordEmpty class. This constructor doesn't receive any arguments.
    * Variable contains the capability to call the constructor
    * For example for RecordEmpty() creates the following constructor:
    *
    * public RecordEmpty() {}
    */
  val defaultConstructor: VoidMethod1[Ref[RecordEmpty]] = {
    cg.mkConstructor1(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          cg.SUPER |>>
          RETURN_VOID)
  }

  /**
    * Generate the `lookupField(String)` method which will always throws an exception,
    * since `getRecordWithField` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `lookupField` method is always the following:
    *
    * public Object lookupField(String var1) throws Exception {
    * throw new Exception("lookupField method shouldn't be called");
    * }
    */
  val lookupFieldMethod: Method2[Ref[RecordEmpty], Ref[MString], Ref[RecordInterface]] =
    cg.mkMethod2("lookupField",
      _ =>
        newUnsupportedOperationExceptionInstructions("lookupField shouldn't be called")
    )

  /**
    * Generate the `restrictField(String)` method which will always throws an exception, since `restrictField` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `restrictField` method is always the following:
    *
    * public string getField(String var1) throws Exception {
    * throw new Exception("restrictField method shouldn't be called");
    * }
    */
  val restrictFieldMethod: Method2[Ref[RecordEmpty], Ref[MString], Ref[RecordInterface]] =
    cg.mkMethod2("restrictField",
      _ =>
        newUnsupportedOperationExceptionInstructions("restrictField shouldn't be called")
    )

  /**
    * Generate the `toString()` method which will always throws an exception, since `toString` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `toString` method is always the following:
    *
    * public string toString() throws Exception {
    * throw new Exception("toString method shouldn't be called");
    * }
    */
  val toStringMethod: Method1[Ref[RecordEmpty], Ref[MString]] =
    cg.mkMethod1("toString", _ => toStringNotImplemented)

  /** Generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `hashCode` method is always the following:
    *
    * public int hashCode() throws Exception {
    * throw new Exception("hashCode method shouldn't be called");
    * }
    */
  val hashCodeMethod: Method1[Ref[RecordEmpty], MInt] =
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
  val equalsMethod: Method2[Ref[RecordEmpty],Ref[MObject], MBool] =
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
