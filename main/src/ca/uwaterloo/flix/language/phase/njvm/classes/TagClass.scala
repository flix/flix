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
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier.{Public, Static}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Api
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.reflect.runtime.universe._

/**
  * Class which generates a tag class
  *
  * The class will extend the enum case interface generated for the same enum case with the same field type.
  * The class contains one field: `value`. `value` contains the field of the case.
  * If the field is primitive, then `value` is of the type of that primitive, otherwise, `value` is just an object.
  * For example, for the case `Ok[Int32]` we generate:
  *
  * public int value;
  *
  * but for the case `Ok[List[Int32]]` we generate:
  *
  * public Object value;
  *
  * Classes generated at this step implements the interface corresponding the symbol of the enum case and they include
  * implementations of following methods: `getTag()`, `getValue()`, `getBoxedTagValue()`,`toString()`, `hashCode()` and
  * `equals(Object)`.
  * `getTag()` is the function which returns the name of the enum case. `getValue()` returns the value of `value` field.
  * `getBoxedTagValue()` returns the `value` field but the result is boxed inside an object. As an example, `getValue()` and
  * `getBoxedTagValue()` of the class representing `Ok[Int32]` is as follows:
  *
  * public final int getValue() {
  * return this.value;
  * }
  *
  * public final Object getBoxedTagValue() {
  * return new Integer(this.value);
  * }
  */
class TagClass[T <: MnemonicsTypes : TypeTag](tag: TagInfo)(implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Initiliaze the class reference and class generator
  private val ct: Reference = getTagClassType(tag)
  private val cg: ClassGenerator = tag.enumType match {
    case MonoType.Enum(sym, elms) =>
      val args = elms.map(getErasedJvmType)
      //the class implements the respective enum interface
      new ClassGenerator(ct, List(getEnumInterfaceType(sym, args)))
    case _ => throw InternalCompilerException("Unexpected type " + tag.enumType)
  }

  //Generate the value field
  private val value: Field[T] = cg.mkField("value")

  /**
    * Method which generates the capability to LOAD/STORE the value field,
    * since it is a prim field it also allow boxing
    */
  private def getPrimField[T1 <: MnemonicsPrimTypes : TypeTag]: PrimField[T1] =
    new PrimField[T1]("value")

  /**
    * Creates the single argument constructor of the enum case class
    */
  val defaultConstructor: VoidMethod2[Ref[TagClass[T]], T] =
    cg.mkConstructor2(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          cg.SUPER |>>
          sig.getArg1.LOAD |>>
          sig.getArg2.LOAD |>>
          value.PUT_FIELD |>>
          RETURN_VOID
    )

  /**
    * If the `value` field can be `Unit`
    * Then the class will be a singleton for which
    * we have an instance variable. Therefore this field is optional.
    * However, if it exists then we need to initiliaze which is what is being done here
    */
  val instance: Option[UncheckedStaticField] =
    if (tag.tagType == MonoType.Unit) {
      val unitInstance = cg.mkUncheckedStaticField("unitInstance", ct, List(Public, Static))
      cg.UncheckedStaticFieldInit(unitInstance,
        //if singleton then class constructor accepts Objects as an argument
        NEW[StackNil, Ref[TagClass[MObject]]](ct) |>>
          DUP |>>
          Api.Java.Runtime.Value.Unit.getInstance.INVOKE |>>
          defaultConstructor.asInstanceOf[VoidMethod2[Ref[TagClass[MObject]], Ref[MUnit]]].INVOKE |>>
          unitInstance.PUT_STATIC |>>
          RETURN_VOID
      )
      Some(unitInstance)
    }
    else
      None

  /**
    * Generate the getValue method.
    * It simply returns value
    */
  val getValueMethod: Method1[Ref[TagClass[T]], T] =
    cg.mkMethod1("getValue",
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          value.GET_FIELD |>>
          RETURN
    )

  /**
    * Generate the getBoxedValue method.
    * If it's a primitive type then we first box it
    */
  val getBoxedValueMethod: Method1[Ref[TagClass[T]], Ref[T]] = {

    cg.mkMethod1("getBoxedTagValue",
      sig =>
        typeOf[T] match {
          case t if t =:= typeOf[MBool] => getPrimField[MBool].GET_BOXED_FIELD[StackNil] |>> RETURN
          case t if t =:= typeOf[MChar] => getPrimField[MChar].GET_BOXED_FIELD[StackNil] |>> RETURN
          case t if t =:= typeOf[MByte] => getPrimField[MByte].GET_BOXED_FIELD[StackNil] |>> RETURN
          case t if t =:= typeOf[MShort] => getPrimField[MShort].GET_BOXED_FIELD[StackNil] |>> RETURN
          case t if t =:= typeOf[MInt] => getPrimField[MInt].GET_BOXED_FIELD[StackNil] |>> RETURN
          case t if t =:= typeOf[MLong] => getPrimField[MLong].GET_BOXED_FIELD[StackNil] |>> RETURN
          case t if t =:= typeOf[MFloat] => getPrimField[MFloat].GET_BOXED_FIELD[StackNil] |>> RETURN
          case t if t =:= typeOf[MDouble] => getPrimField[MDouble].GET_BOXED_FIELD[StackNil] |>> RETURN
          case _ => sig.getArg1.LOAD[StackNil] |>> value.GET_FIELD |>> RETURN
        }
    )
  }


  /**
    * Generates the `getTag()` method of the class which is the implementation of `getTag` method on `tagInterface`.
    * This methods returns an string containing the tag name.
    * For example, `Val[Char]` has following `getTag()`method:
    *
    * public final String getTag() {
    * return "Var";
    */
  val getTagMethod: Method1[Ref[TagClass[T]], Ref[MString]] =
    cg.mkMethod1("getTag",
      _ =>
        LDC_STRING[StackNil](tag.tag) |>>
          RETURN
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
  val toStringMethod: Method1[Ref[TagClass[T]], Ref[MString]] =
    cg.mkMethod1("toString",
      _ =>
        newUnsupportedOperationExceptionInstructions("toString shouldn't be called")
    )

  /** Generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * Despite this in order to stay in line with our format we still store the capability to call the method
    * The `hashCode` method is always the following:
    *
    * public int hashCode() throws Exception {
    * throw new Exception("hashCode method shouldn't be called");
    * }
    */
  val hashCodeMethod: Method1[Ref[TagClass[T]], MInt] =
    cg.mkMethod1("hashCode",
      _ =>
        newUnsupportedOperationExceptionInstructions("hashCode shouldn't be called")
    )

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
  val equalsMethod: Method2[Ref[TagClass[T]], Ref[MObject], MBool] =
    cg.mkMethod2("equal",
      _ =>
        newUnsupportedOperationExceptionInstructions("equals shouldn't be called")
    )

  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass = JvmClass(ct.name, cg.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this
}

