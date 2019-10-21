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
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, NamespaceInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier.Public
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._

/**
  * This class generates code for the context class, according to the namespace information
  *
  * @param ns is the set of all the namespace information
  */
class Context(ns: Set[NamespaceInfo])(implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Initialize the class reference and the generator
  private val ct: Reference = getContextClassType
  //The class doesn't implement any interfaces
  private val cg: ClassGenerator = new ClassGenerator(ct, List())

  //Generate the continuation field, which is a public field of type object
  private val continuation: Field[Ref[MObject]] = cg.mkField("continuation", List(Public))

  //For each namespace generate code for a field
  for (namespace <- ns) {
    // JvmType of the namespace
    val namespaceRef = getNamespaceClassType(namespace)

    // Name of the field for the `namespace` on the Context object
    val fieldName = getNamespaceFieldNameInContextClass(namespace)

    //Generating the field, this is an unchecked field as at compile time
    //we do not know the it's type.
    cg.mkUncheckedField(fieldName, namespaceRef, List(Public))
  }

  /**
    * Method which generates the capability to LOAD/STORE a namespace field
    *
    * @param namespace of the field we want to obtain
    */
  private def getUncheckedField(namespace: NamespaceInfo): UncheckedField = {
    // JvmType of the namespace
    val namespaceRef = getNamespaceClassType(namespace)

    // Name of the field for the `namespace` on the Context object
    val fieldName = getNamespaceFieldNameInContextClass(namespace)
    new UncheckedField(fieldName, namespaceRef)
  }

  //Generate code for the Context constructor.
  //The constructor simply calls super and initializes each of the namespace fields
  val defaultConstrutor: VoidMethod1[Ref[Context]] = {
    //We need initialize each of the namespaces in the context
    //Therefore we need to accumulate the instructions
    //To do this we use a foldleft.
    val initNamespaces =
    (sig: FunSig1[Ref[Context], MVoid]) => {
      ns.foldLeft(NO_OP[StackNil]) {
        case (ins, namespace) => {}
          val namespaceRef = getNamespaceClassType(namespace)
          val namespaceConstructor = new VoidMethod1[Ref[MObject]](JvmModifier.InvokeSpecial, namespaceRef, "<init>")
          ins |>>
            sig.getArg1.LOAD |>>
            NEW(namespaceRef) |>>
            DUP |>>
            namespaceConstructor.INVOKE |>>
            getUncheckedField(namespace).PUT_FIELD
      }
    }

    //Generate the constructor
    cg.mkConstructor1(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          cg.SUPER |>>
          initNamespaces(sig) |>>
          RETURN_VOID
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

