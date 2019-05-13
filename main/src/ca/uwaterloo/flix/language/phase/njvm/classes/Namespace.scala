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
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, NamespaceInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._

class Namespace(ns : NamespaceInfo)(implicit root: Root, flix : Flix) extends MnemonicsClass {
  //Setup
  private val ct: Reference = getNamespaceClassType(ns)
  private val cg: ClassGenerator = new ClassGenerator(ct, List())

  private val continuation : Field[Ref[MObject]] = cg.mkField("continuation")

  for ((sym, defn) <- ns.defs) {
    // JvmType of `defn`
    val fieldType = getFunctionDefinitionClassType(sym)

    // Name of the field on namespace
    val fieldName = getDefFieldNameInNamespaceClass(sym)

    cg.mkUncheckedField(fieldName, fieldType)

//    // Compile the shim method.
//    cg.mkMethod(visitor, defn, jvmType, ns)
  }

  def getUncheckedField(sym:  Symbol.DefnSym): UncheckedField ={
    // JvmType of `defn`
    val fieldType = getFunctionDefinitionClassType(sym)

    // Name of the field on namespace
    val fieldName = getDefFieldNameInNamespaceClass(sym)
    new UncheckedField(fieldName, fieldType)
  }

  val defaultConstrutor : VoidMethod1[Ref[Namespace]] = ???
  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass = JvmClass(ct.name, cg.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this
}

