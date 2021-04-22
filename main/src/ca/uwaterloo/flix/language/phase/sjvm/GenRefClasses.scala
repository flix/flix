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
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._

/**
  * Generates bytecode for the ref classes.
  */
object GenRefClasses {
  val fieldName: String = "value"

  /**
    * Returns the bytecode for the ref classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[String, JvmClass] = {

    // Generating each ref class
    def genAUX[T <: PType](tpe: RType[T]): (String, JvmClass) = {
      val eeType = RReference(RRef(tpe))
      val className: String = Instructions.getInternalName(eeType)
      className -> JvmClass(className, genByteCode(className, tpe))
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
  private def genByteCode[T <: PType](className: String, innerType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.openClassWriter(className, isFinal = true)

    // Generate the instance field
    val innerTypeString = Instructions.getDescriptor(innerType)
    classMaker.makeField(fieldName, innerTypeString, isStatic = false, isPublic = true)

    val constructorDescriptor = Instructions.getDescriptor(innerTypeString, JvmName.voidDescriptor)
    classMaker.makeConstructor(genConstructor(innerType), constructorDescriptor)

    classMaker.closeClassMaker
  }

  /**
    * Generating constructor for the class with value of type `innerType`
    */
  def genConstructor[T <: PType](eType: RType[T]): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
    THISLOAD(tag[PRef[T]]) ~
      INVOKESPECIAL(JvmName.Java.Lang.Object.toInternalName, JvmName.nothingToVoid) ~
      RETURN
  }
}
