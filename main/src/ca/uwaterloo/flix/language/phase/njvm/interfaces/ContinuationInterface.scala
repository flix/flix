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
package ca.uwaterloo.flix.language.phase.njvm.interfaces

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{InterfaceGenerator, _}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.classes.Context
import scala.reflect.runtime.universe._

class ContinuationInterface[T1 <: MnemonicsTypes : TypeTag](implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Setup
  private val it: Reference = getContinuationInterfaceType[T1]
  private val ig: InterfaceGenerator = new InterfaceGenerator(it, List())

  //Fields
  //Interface has no fields ig doesn't even allow to compile fields

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method

  /**
    * Generate the getResult interface method. Stores the capability to call the method
    */
  val getResultMethod: Method1[Ref[ContinuationInterface[T1]], T1] = ig.mkMethod1("getResult")


  /**
    * Generate the invoke interface method. Stores the capability to call the method
    */
  val invokeMethod: VoidMethod2[Ref[ContinuationInterface[T1]], Ref[Context]] = ig.mkVoidMethod2("invoke")

  private val jvmClass: JvmClass = JvmClass(it.name, ig.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    it.name -> this
}
