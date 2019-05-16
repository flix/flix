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
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, JvmOps}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import scala.reflect.runtime.universe._

/**
  * Generate a main class of a given return type, which is specified in the type parameter
  *
  * @pre this class should only be called if there is a main definition
  */
class Main[T <: MnemonicsTypes : TypeTag](map: Map[JvmName, MnemonicsClass])(implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Initiliaze the main reference and the class generator
  private val ct: Reference = getMainClassType
  private val cg: ClassGenerator = new ClassGenerator(ct, List())

  //The class has only one method with the following signature public static void main(String[] args)
  val mainMethod: VoidMethod1[MArray[MString]] = {
    //Get the root namespace in order to get the class type when invoking m_main
    val ns = getNamespace(Symbol.mkDefnSym("main"))

    //generate the capability to call the m_main method in the root namespace
    val m_main_method = new Method1[Ref[MObject], T](JvmModifier.InvokeStatic, getNamespaceClassType(ns), "m_main")

    cg.mkStaticVoidMethod1("main",
      _ =>
        //TODO: actually apss in the console arguments
        CONST_NULL[StackNil, Ref[MObject]] |>>
          m_main_method.INVOKE |>>
          RETURN_VOID[T]
    )
  }

  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass = JvmClass(ct.name, cg.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this
}

