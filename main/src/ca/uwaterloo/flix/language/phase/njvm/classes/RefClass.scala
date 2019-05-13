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
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import scala.reflect.runtime.universe._

class RefClass[T <:MnemonicsTypes : TypeTag](implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Setup
  private val ct: Reference = getRefClassType[T]

  private val cg: ClassGenerator = new ClassGenerator(ct, List())

  //Fields each variable represents a field which can be acessed
  //while generating code for this class
  private val field0: Field[T] = cg.mkField("field0")

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method
  /**
    * Generate the constructor for the current RefClass(depends on the type paramater)
    * we are generating. Stotring the capability to invoke the constructor
    *
    * For example When generating a constructor for a RefClass which encapsualtes an integer the construtor will be:
    *
    * public Ref$Int32(int var1) {
    *   this.field0 = var1;
    * }
    *
    */
  val defaultConstructor: VoidMethod2[Ref[RefClass[T]], T] = {

    cg.mkConstructor2(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          cg.SUPER |>>
          sig.getArg1.LOAD |>>
          sig.getArg2.LOAD |>>
          field0.PUT_FIELD |>>
          RETURN_VOID)
  }

  /**
    * Generate the getValue method for the current RefClass(depends on the type paramater)
    * we are generating. Storing the capability to invoke the getValue method
    *
    * For example When generating a getValue for a RefClass which encapsualtes an integer getValue will be:
    *
    * public final int getValue() {
    * return this.field0;
    * }
    *
    */
  val getValueMethod: Method1[Ref[RefClass[T]], T] =
    cg.mkMethod1("getValue",
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          field0.GET_FIELD |>>
          RETURN
    )

  /**
    * Generate the setValue method for the current RefClass(depends on the type paramater)
    * we are generating. Storing the capability to invoke the setValue method
    *
    * For example When generating a getValue for a RefClass which encapsualtes an integer setValue will be:
    *
    * public final void setValue(int var1) {
    *   this.field0 = var1;
    * }
    *
    */
  val setValueMethod: VoidMethod2[Ref[RefClass[T]], T] =
    cg.mkVoidMethod2("setValue",
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          sig.getArg2.LOAD |>>
          field0.PUT_FIELD |>>
          RETURN_VOID
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
  val toStringMethod: Method1[Ref[RefClass[T]],Ref[MString]] =
    cg.mkMethod1("toString", _ => toStringNotImplemented)

  /** Generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `hashCode` method is always the following:
    *
    * public int hashCode() throws Exception {
    * throw new Exception("hashCode method shouldn't be called");
    * }
    */
  val hashCodeMethod: Method1[Ref[RefClass[T]], MInt] =
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
  val equalsMethod: Method2[Ref[RefClass[T]], Ref[MObject], MBool] =
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
