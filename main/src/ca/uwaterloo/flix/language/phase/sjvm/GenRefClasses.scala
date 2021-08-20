/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler.StackNil
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._

/**
  * Generates bytecode for the ref classes.
  */
object GenRefClasses {
  val ValueFieldName: String = "value"

  /**
    * Returns the bytecode for the ref classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //Type that we need a cell class for
    RType.baseTypes.foldLeft(Map[JvmName, JvmClass]()) {
      case (macc, tpe) =>
        val refType = RReference(RRef(tpe))
        macc + (refType.jvmName -> JvmClass(refType.jvmName, genByteCode(refType, tpe)))
    }
  }

  /**
    * Generating class `className` with value of type `innerType`
    */
  private def genByteCode[T <: PType](refType: RReference[PRef[T]], valueFieldType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(refType.jvmName, addSource = false, None)

    // Generate the instance field
    classMaker.mkField(ValueFieldName, valueFieldType, Mod.isPublic)

    classMaker.mkConstructor(START[StackNil] ~ THISINIT(JvmName.Java.Lang.Object) ~ RETURN, JvmName.nothingToVoid)

    classMaker.closeClassMaker
  }
}
