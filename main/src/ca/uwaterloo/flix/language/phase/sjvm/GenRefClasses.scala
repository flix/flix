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
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
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

    // Generating each ref class
    def genAUX[T <: PType](valueFieldType: RType[T]): (JvmName, JvmClass) = {
      val refType = RReference(RRef(valueFieldType))
      refType.jvmName -> JvmClass(refType.jvmName, genByteCode(refType, valueFieldType))
    }

    //Type that we need a cell class for
    Map() +
      genAUX(RBool()) +
      genAUX(RInt8()) +
      genAUX(RInt16()) +
      genAUX(RInt32()) +
      genAUX(RInt64()) +
      genAUX(RChar()) +
      genAUX(RFloat32()) +
      genAUX(RFloat64()) +
      genAUX(RReference(RObject()))
  }

  /**
   * Generating class `className` with value of type `innerType`
   */
  private def genByteCode[T <: PType](refType: RReference[PRef[T]], valueFieldType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(refType.jvmName, addSource = false)

    // Generate the instance field
    classMaker.mkField(ValueFieldName, valueFieldType, Mod.isPublic)

    val constructorDescriptor = JvmName.nothingToVoid
    classMaker.mkConstructor(genConstructor(valueFieldType), constructorDescriptor)

    classMaker.closeClassMaker
  }

  /**
   * Generating constructor for the class with value of type `innerType`
   */
  def genConstructor[T <: PType](valueFieldType: RType[T]): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      THISLOAD(tag[PRef[T]]) ~
      INVOKEOBJECTCONSTRUCTOR ~
      RETURN
  }
}
