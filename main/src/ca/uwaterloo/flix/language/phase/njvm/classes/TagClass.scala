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
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{F, _}
import ca.uwaterloo.flix.language.phase.njvm.{Api, NJvmType}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.reflect.runtime.universe._

class TagClass[T <: MnemonicsTypes : TypeTag](tag : TagInfo)(implicit root: Root, flix : Flix) extends MnemonicsClass {
  //Setup
  private val ct: Reference = getTagClassType(tag)
  private val cg: ClassGenerator = tag.enumType match {
    case MonoType.Enum(sym, elms) =>
      // Case 1: The type constructor is an enum.
      // Construct enum interface.
      val args = elms.map(getErasedJvmType)
      new ClassGenerator(ct, List(getEnumInterfaceType(sym, args)))
    case _ => ???
  }

  private val field0  : Field[T] = cg.mkField("field0")

  private def getPrimField[T1 <: MnemonicsPrimTypes : TypeTag] : PrimField[T1] =
    new PrimField[T1]("field0")

  val defaultConstructor : VoidMethod2[Ref[TagClass[T]],T] =
    cg.mkConstructor2(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          cg.SUPER |>>
          sig.getArg1.LOAD |>>
          sig.getArg2.LOAD |>>
          field0.PUT_FIELD |>>
          RETURN_VOID
    )

  val instance : Option[UncheckedStaticField] =
    if(tag.tagType == MonoType.Unit) {
      val unitInstance = cg.mkUncheckedStaticField("unitInstance", ct, List(Public, Static))
      cg.UncheckedStaticFieldInit(unitInstance,
        //if singleton then class constructor accepts Objects as an argument
        NEW[StackNil, Ref[TagClass[MObject]]](ct) |>>
        DUP|>>
        Api.Java.Runtime.Value.Unit.getInstance.INVOKE|>>
        defaultConstructor.asInstanceOf[VoidMethod2[Ref[TagClass[MObject]], Ref[MUnit]]].INVOKE |>>
        unitInstance.PUT_STATIC |>>
        RETURN_VOID
      )
      Some(unitInstance)
    }
    else
      None

  val getValueMethod: Method1[Ref[TagClass[T]], T] =
    cg.mkMethod1("getValue",
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          field0.GET_FIELD |>>
          RETURN
    )

  val getBoxedValueMethod : Method1[Ref[TagClass[T]], Ref[T]] = {

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
          case _ => sig.getArg1.LOAD[StackNil] |>> field0.GET_FIELD |>> RETURN
        }
    )
  }

  val getTagMethod : Method1[Ref[TagClass[T]], Ref[MString]] =
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
  val toStringMethod: Method1[Ref[TagClass[T]],Ref[MString]] =
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

